{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import Data.Bifunctor (second)
import Data.Char (isAlpha, isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import System.Console.Haskeline
import System.Console.Wizard
import System.Console.Wizard.Haskeline
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec hiding (match, token)
import Text.Megaparsec.Char
import Text.Regex.TDFA

newtype CommentMarker = LineComment T.Text
  deriving (Eq, Ord, Show)

type VarName = T.Text

data VarDef = RegexVar T.Text Regex

type VarVal = T.Text

instance Show VarDef where
  show (RegexVar r _) = "RegexVar " <> T.unpack r

type Directive = T.Text

lineCommentHeader :: T.Text
lineCommentHeader = "AUTOCOMF COMMENT LINE "

findCommentMarker :: T.Text -> CommentMarker
findCommentMarker s
  | (_, s') <- T.breakOn lineCommentHeader s,
    c <- T.takeWhile (not . isSpace) $ T.drop (T.length lineCommentHeader) s' =
    LineComment c
  | otherwise =
    LineComment "#"

findDirective :: CommentMarker -> T.Text -> Maybe (Directive, T.Text)
findDirective (LineComment c) s = do
  let (_bef, s') =
        second (T.stripStart . T.drop (T.length c)) $ T.breakOn c s
  guard $ not $ T.null s'
  if "AUTOCOMF" `T.isPrefixOf` s'
    then pure $ T.breakOn "\n" s'
    else findDirective (LineComment c) s'

findDirectives :: CommentMarker -> T.Text -> [Directive]
findDirectives c s =
  case findDirective c s of
    Nothing -> []
    Just (dir, s') -> dir : findDirectives c s'

type Parser = Parsec Void T.Text

token :: T.Text -> Parser ()
token s = void $ chunk s <* space

pVarName :: Parser VarName
pVarName = takeWhileP (Just "letter") isAlpha <* space

pVarDef :: Parser VarDef
pVarDef = do
  token "REGEX"
  r <- takeRest
  r' <- makeRegexOptsM blankCompOpt defaultExecOpt $ T.unpack r
  pure $ RegexVar r r'

pVarDir :: Parser (VarName, VarDef)
pVarDir = do
  token "AUTOCOMF"
  token "VAR"
  (,) <$> pVarName <*> pVarDef

findVars :: [Directive] -> M.Map VarName VarDef
findVars = foldMap varFromDir
  where
    varFromDir dir =
      maybe mempty (uncurry M.singleton) $ parseMaybe pVarDir dir

data Chunk
  = ChunkVar VarName
  | ChunkText T.Text
  deriving (Show)

type UseDir = [Chunk]

pUseDir :: CommentMarker -> Parser UseDir
pUseDir (LineComment c) = do
  token c
  token "AUTOCOMF"
  token "USE"
  many pUseChunk
  where
    pUseChunk =
      choice
        [ ChunkVar <$> (chunk "¤" *> takeWhileP Nothing (/= '¤') <* chunk "¤"),
          ChunkText <$> takeWhile1P Nothing (/= '¤')
        ]

inferVarValues :: [Chunk] -> T.Text -> M.Map VarName VarVal
inferVarValues (ChunkText t : cs) y = inferVarValues cs $ T.drop (T.length t) y
inferVarValues [ChunkVar v] y =
  M.singleton v y
inferVarValues (ChunkVar v : ChunkText t : cs) y =
  let (v_val, y') = T.breakOn t y
   in M.singleton v v_val <> inferVarValues (ChunkText t : cs) y'
inferVarValues _ _ =
  -- Ambiguous; stupid user.
  mempty

currentVarValues :: CommentMarker -> T.Text -> M.Map VarName VarVal
currentVarValues c = recurse . T.lines
  where
    recurse [] = mempty
    recurse (l1 : l2 : ls)
      | Just usedir <- parseMaybe (pUseDir c) l1 =
        inferVarValues usedir l2 <> recurse ls
    recurse (_ : ls) =
      recurse ls

wizardForVar :: (VarName, VarDef) -> Maybe VarVal -> Wizard Haskeline T.Text
wizardForVar (v, RegexVar r regex) maybe_curval =
  retryMsg scold . validator ok . fmap T.pack $ case maybe_curval of
    Nothing -> line prompt
    Just curval -> linePrewritten prompt (T.unpack curval) mempty
  where
    scold = "No.  I already told you what was expected.  Try again."
    prompt = T.unpack $ v <> " (" <> r <> ")" <> ": "
    ok = match regex

getVarValsFromUser ::
  M.Map VarName VarDef ->
  M.Map VarName VarVal ->
  IO (Maybe (M.Map VarName VarVal))
getVarValsFromUser m curvals =
  runInputT defaultSettings (run $ haskeline $ fmap M.fromList . mapM f . M.toList $ m)
  where
    f (v, def) =
      (v,) <$> wizardForVar (v, def) (M.lookup v curvals)

instantiateUseDir :: M.Map VarName VarVal -> [Chunk] -> T.Text
instantiateUseDir _ [] =
  mempty
instantiateUseDir vals (ChunkText t : cs) =
  t <> instantiateUseDir vals cs
instantiateUseDir vals (ChunkVar v : cs) =
  fromMaybe ("¤" <> v) (M.lookup v vals) <> instantiateUseDir vals cs

indentAs :: T.Text -> T.Text -> T.Text
indentAs x y = T.takeWhile isSpace x <> y

substituteVars :: CommentMarker -> M.Map VarName VarVal -> T.Text -> T.Text
substituteVars c vals = T.unlines . recurse . T.lines
  where
    recurse [] = []
    recurse (l1 : l2 : ls)
      | Just usedir <- parseMaybe (pUseDir c) l1 =
        l1 : indentAs l2 (instantiateUseDir vals usedir) : recurse ls
    recurse (l : ls) =
      l : recurse ls

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
    [f] -> pure f
    _ -> do
      T.hPutStrLn stderr "Usage: autocomf <file>"
      exitFailure
  input <- T.readFile f
  let c = findCommentMarker input
      dirs = findDirectives c input
      vars = findVars dirs

  maybe_vals <- getVarValsFromUser vars (currentVarValues c input)
  case maybe_vals of
    Nothing -> exitFailure
    Just vals -> T.writeFile f $ substituteVars c vals input
