{-# LANGUAGE ConstraintKinds, FlexibleContexts, KindSignatures, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PolyKinds, RankNTypes, TupleSections, RecordWildCards #-}

module ParserMonad
  ( alexMove
  , AlexInput(AlexInput, charPos, charPrev, pending, str)
  , ignorePendingBytes
  , alexGetByte
  , alexInputPrevChar
  , Impredicative(Impredicative)
  , runImpredicative
  , mapImpredicative
  , sequenceImpredicative
  , EnvM'(EnvM', runEnvM')
  , mapEnvM'
  , concatEnvM'
  , ParserState(..)
  , initialParserState
  , ParserMonad
  , prettyPosn
  , Locatable(Locatable)
  , throwLocalError
  , throwLocalErrorAt
  , throwGlobalError
  , runParser'
  , printError
  , runParser
  ) where

import AlexPosn (AlexPosn(AlexPosn), Locatable(Locatable))

import Core

import Control.Arrow ((***))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.State (StateT, runStateT)

import Data.Char (ord)
import qualified Data.Bits
import Data.Text (Text, uncons)
import qualified Data.Text as Text
import Data.Word (Word8)

import GHC.Exts (Constraint)

-- | Encode a Haskell Text to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> (Word8, [Word8])
utf8Encode = (fromIntegral *** map fromIntegral) . go . ord
 where go :: Int -> (Int, [Int])
       go oc
         | oc <= 0x7f       = (oc, [])
         | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                              , [0x80 + oc Data.Bits..&. 0x3f]
                              )
         | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                              , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
                              )
         | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                              , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
                              )

-- | The starting position
alexStartPos :: AlexPosn
alexStartPos = AlexPosn 0 1 1

-- | The width of a tab - used for column numbers
alexTabSize :: Int
alexTabSize = 8

-- | Advance the position by the given character
alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPosn a l c) '\t' = AlexPosn (a + 1)  l       (c + alexTabSize - ((c - 1) `mod` alexTabSize))
alexMove (AlexPosn a l _) '\n' = AlexPosn (a + 1) (l + 1)   1
alexMove (AlexPosn a l c) _    = AlexPosn (a + 1)  l       (c + 1)

-- | The current input to the lexer
data AlexInput = AlexInput
  { charPos  :: AlexPosn -- ^ The position of the character
  , charPrev :: Char     -- ^ The previous character
  , pending  :: [Word8]  -- ^ The pending bytes on the current character
  , str      :: Text     -- ^ The current input string
  }

-- | Ignore any remaining bytes in the current character
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes input = input { pending = [] }

-- | Return the previously read character
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = charPrev

-- | Get another byte from the input
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput{..} = case (pending, uncons str) of
  (b:bs, _         ) -> Just (b,AlexInput{ pending = bs, .. })
  ([]  , Nothing   ) -> Nothing
  ([]  , Just (c,s)) ->
    let p = alexMove charPos c in
    let (b, bs) = utf8Encode c in
    p `seq`  Just (b, AlexInput p c bs s)

data Impredicative (constraint :: k -> Constraint) (f :: k -> *) = Impredicative (forall a. constraint a => f a)

runImpredicative :: constraint a => Impredicative constraint f -> f a
runImpredicative (Impredicative x) = x

mapImpredicative :: (forall a. constraints a => f a -> g a) -> Impredicative constraints f -> Impredicative constraints g
mapImpredicative f (Impredicative x) = Impredicative $ f x

sequenceImpredicative :: (forall a. constraints a => [f a] -> g a) -> [Impredicative constraints f] -> Impredicative constraints g
sequenceImpredicative f xs = Impredicative $ f $ helper xs
  where
    helper :: constraints a => [Impredicative constraints f] -> [f a]
    helper [] = []
    helper (Impredicative y : ys) = y : helper ys

newtype EnvM' m = EnvM' { runEnvM' :: EnvM m }

mapEnvM' :: (EnvM m -> EnvM m) -> EnvM' m -> EnvM' m
mapEnvM' f (EnvM' env) = EnvM' $ f env

concatEnvM' :: [EnvM' m] -> EnvM' m
concatEnvM' = EnvM' . concat . map runEnvM'

-- | The current state to be threaded through the parser in the monad
data ParserState = ParserState
  { stateInput :: AlexInput
  , stateEnv   :: Impredicative EvalMonad EnvM'
  }

-- | The state to be threaded into the beginning of the parser
initialParserState :: Text -> Impredicative EvalMonad EnvM' -> ParserState
initialParserState s = ParserState (AlexInput alexStartPos '\n' [] s)

-- | The monad to be threaded through the lexer and the parser
type ParserMonad a = StateT ParserState (Either (Text, Maybe (AlexPosn, AlexPosn))) a

-- | Print a position in a nice way
prettyPosn :: Maybe (AlexPosn, AlexPosn) -> Text
prettyPosn (Just (AlexPosn _ l1 c1, AlexPosn _ l2 c2))
  | l1 == l2  = "Line " <> tshow l1 <> " Column " <> tshow c1 <> "-" <> tshow c2
  | otherwise = "Line " <> tshow l1 <> " Column " <> tshow c1 <> " to Line " <> tshow l2 <> " Column " <> tshow c2
prettyPosn Nothing = "Unknown Position"

-- | Throw an error in the `ParserMonad' in the location of the given `Locatable'
throwLocalError :: Locatable a -> Text -> ParserMonad b
throwLocalError (Locatable _ ps) s = throwError (s, ps)

-- | Throw an error in the `ParserMonad' in the given location
throwLocalErrorAt :: Maybe (AlexPosn, AlexPosn) -> Text -> ParserMonad a
throwLocalErrorAt ps s = throwError (s, ps)

-- | Throw an error in the `ParserMonad' without a location
throwGlobalError :: MonadError (Text, Maybe (AlexPosn, AlexPosn)) m => Text -> m a
throwGlobalError = throwError . (, Nothing)

runParser' :: ParserMonad a -> Text -> Impredicative EvalMonad EnvM' -> Either (Text, Maybe (AlexPosn, AlexPosn)) a
runParser' m s env = fst <$> (runStateT m $ initialParserState s env)

printError :: Text -> (Text, Maybe (AlexPosn, AlexPosn)) -> Text
printError _ (e, Nothing) = Text.append e "\n"
printError s (e, Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2))
  | l1 == l2 =
    let (line1, line2) = (Text.reverse . Text.takeWhile (/= '\n') . Text.reverse *** Text.takeWhile (/= '\n')) . Text.splitAt a1 $ s in
    (prettyPosn $ Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)) <> ": " <> e <> "\n" <> line1 <> line2 <> "\n" <> Text.replicate (c1 - 1) " " <> Text.replicate (c2 - c1) "^" <> "\n"
  | otherwise =
    (prettyPosn $ Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)) <> ": " <> e <> "\nError spanning mulitple lines, I don't yet know how to display that!!!\n"

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right

-- | Run the given parser on the given input string and either return a nicely formatted error or the output of the parser
runParser :: ParserMonad a -> Text -> Impredicative EvalMonad EnvM' -> Either Text a
runParser m s env = mapLeft (printError s) $ runParser' m s env

