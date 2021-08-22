{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lexer
  ( Token(..)
  , readToken
  ) where

import AlexPosn (AlexPosn, Locatable(Locatable))
import Core
import ParserMonad

import qualified Control.Monad.Trans.State as State
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (read)
}

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$lower = [a-z]     -- lower case characters
$upper = [A-Z]     -- upper case characters

tokens :-
  ($white # \n)+;
  \n;
  ";"                             { wrapPlainToken Semi }
  "--".*\n                        { wrapFuncToken $ CommentTok . fmap (SimpleCommentV . Text.drop 2) }
  "{-"[^\-]*(\-[^\}][^\-]*)*"-}"  { wrapFuncToken $ CommentTok . fmap (SimpleCommentV . Text.drop 2 . Text.dropEnd 2) }
  "{!-"                           { wrapPlainToken BeginExecComment }
  "-!}"                           { wrapPlainToken EndExecComment }
  "#"                             { wrapPlainToken Hash }
  ":"                             { wrapPlainToken Colon }
  "="                             { wrapPlainToken Equals }
  "\\"                            { wrapPlainToken LambdaTok }
  "->"                            { wrapPlainToken RightArrow }
  "$"                             { wrapPlainToken Dollar }
  "+"                             { wrapPlainToken Plus }
  "-"                             { wrapPlainToken Minus }
  "*"                             { wrapPlainToken Times }
  "/"                             { wrapPlainToken Slash }
  "&"                             { wrapPlainToken And }
  "|"                             { wrapPlainToken Or }
  "++"                            { wrapPlainToken Concat }
  "=="                            { wrapPlainToken Equality }
  "!="                            { wrapPlainToken Inequality }
  "<"                             { wrapPlainToken OpenAngle }
  ">"                             { wrapPlainToken CloseAngle }
  "{"                             { wrapPlainToken OpenCurly }
  "}"                             { wrapPlainToken CloseCurly }
  "("                             { wrapPlainToken OpenParen }
  ")"                             { wrapPlainToken CloseParen }
  "["                             { wrapPlainToken OpenSquare }
  "]"                             { wrapPlainToken CloseSquare }
  $digit+                         { wrapFuncToken $ Int . (read . Text.unpack <$>) }
  $lower [$alpha $digit \_]*      { wrapFuncToken (IdentTok . fmap Ident) }

{
-- | The tokens to lex
data Token
  = Semi                               -- ^ @;@
  | CommentTok (Locatable SimpleValue) -- ^ A comment
  | BeginExecComment                   -- ^ A comment
  | EndExecComment                     -- ^ A comment
  | Hash                               -- ^ @#@
  | Colon                              -- ^ @:@
  | Equals                             -- ^ @=@
  | LambdaTok                          -- ^ @\@
  | RightArrow                         -- ^ @->@
  | Dollar                             -- ^ @$@
  | Plus                               -- ^ @+@
  | Minus                              -- ^ @-@
  | Times                              -- ^ @*@
  | Slash                              -- ^ @/@
  | And                                -- ^ @&@
  | Or                                 -- ^ @|@
  | Xor                                -- ^ @^@
  | Concat                             -- ^ @++@
  | Equality                           -- ^ @==@
  | Inequality                         -- ^ @!=@
  | OpenAngle                          -- ^ @<@
  | CloseAngle                         -- ^ @>@
  | OpenCurly                          -- ^ @{@
  | CloseCurly                         -- ^ @}@
  | OpenParen                          -- ^ @(@
  | CloseParen                         -- ^ @)@
  | OpenSquare                         -- ^ @[@
  | CloseSquare                        -- ^ @]@
  | Int (Locatable Int)                -- ^ An integer
  | IdentTok (Locatable Ident)         -- ^ An identifier starting with a lower case letter
  | EOF                                -- ^ The end of the file
  deriving (Show)

wrapFuncToken :: (Locatable b -> a) -> b -> (AlexPosn, AlexPosn) -> ParserMonad (Locatable a)
wrapFuncToken f s ps = pure $ Locatable (f . Locatable s . Just $ ps) (Just ps)

wrapPlainToken :: a -> b -> (AlexPosn, AlexPosn) -> ParserMonad (Locatable a)
wrapPlainToken = wrapFuncToken . const

-- | Read a `Token' from the input
readToken :: ParserMonad (Locatable Token)
readToken = do
  ParserState{..} <- State.get
  case alexScan stateInput 0 of
    AlexEOF                ->
      return . Locatable EOF . Just $ (charPos stateInput, charPos stateInput)
    AlexError input'       -> do
      State.put ParserState{ stateInput = input', .. }
      throwLocalErrorAt (Just (charPos stateInput, alexMove (charPos input') (Text.head . str $ input')))
        $ Text.concat ["Could not lex token: ", Text.pack $ show $ Text.head $ str $ input']
    AlexSkip input' _      -> do
      State.put ParserState{ stateInput = input', .. }
      readToken
    AlexToken input' len t -> do
      State.put ParserState{ stateInput = input', .. }
      (t . Text.take len . str $ stateInput) (charPos stateInput, charPos input')
}
