-- PARSES RESTRICTED TUTLE SUBSET INTO CANONICAL TRIPLES

{-# LANGUAGE LambdaCase #-}
module Turtle
  ( loadTurtleGraph
  ) where

import Data.Char (isDigit, isSpace)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Syntax

data PState = PState
  { stBase   :: Maybe String
  , stPrefix :: M.Map String String
  }

loadTurtleGraph :: FilePath -> IO [Triple]
loadTurtleGraph fp = do
  src <- readFile fp
  case parseTurtle src of
    Left err -> error err
    Right ts -> pure ts

parseTurtle :: String -> Either String [Triple]
parseTurtle src = do
  let ls = filter (not . null) . map trim $ lines src
  let (decls, rest) = span isDecl ls
  st <- foldl parseDecl (Right (PState Nothing M.empty)) decls
  let tripleText = unlines rest
  let statements = splitTopLevel '.' tripleText
  triples <- fmap concat (mapM (parseTripleStatement st) (filter (not . null) (map trim statements)))
  pure triples

isDecl :: String -> Bool
isDecl s = "@base" `L.isPrefixOf` s || "@prefix" `L.isPrefixOf` s

parseDecl :: Either String PState -> String -> Either String PState
parseDecl accE line = do
  acc <- accE
  case words line of
    ["@base", uri, "."] -> Right acc { stBase = Just (stripAngles uri) }
    ["@prefix", pfxColon, uri, "."] ->
      let pfx = reverse (drop 1 (reverse pfxColon))
      in Right acc { stPrefix = M.insert pfx (stripAngles uri) (stPrefix acc) }
    _ -> Left ("Bad declaration: " ++ line)

parseTripleStatement :: PState -> String -> Either String [Triple]
parseTripleStatement st line = do
  let body = trim line
  case parseSubject body of
    Nothing -> Left ("Bad triple line: " ++ line)
    Just (subjTxt, rest1) -> do
      subj <- parseUriLike st subjTxt
      parsePredGroups st subj rest1

parsePredGroups :: PState -> Node -> String -> Either String [Triple]
parsePredGroups st subj s = do
  let groups = splitTopLevel ';' s
  fmap concat $ mapM (parsePredGroup st subj) groups

parsePredGroup :: PState -> Node -> String -> Either String [Triple]
parsePredGroup st subj grp = do
  let ws = words grp
  case ws of
    [] -> Right []
    _  -> do
      let predTok = head ws
      pred <- parseUriLike st predTok
      let objText = trim (drop (length predTok) grp)
      objs <- mapM (parseObj st . trim) (splitTopLevel ',' objText)
      pure [Triple subj pred o | o <- objs]

parseObj :: PState -> String -> Either String Node
parseObj st s
  | not (null s) && head s == '"' && last s == '"' = Right (LitStr (init (tail s)))
  | not (null s) && all isDigit s = Right (LitInt (read s))
  | otherwise = parseUriLike st s

parseUriLike :: PState -> String -> Either String Node
parseUriLike st s
  | not (null s) && head s == '<' && last s == '>' = Right . URI $ expandAngle (stBase st) (init (tail s))
  | ':' `elem` s =
      let (pfx, _:local) = break (== ':') s
      in case M.lookup pfx (stPrefix st) of
           Just base -> Right (URI (base ++ local))
           Nothing   -> Left ("Unknown prefix: " ++ pfx)
  | otherwise = Left ("Bad URI-like token: " ++ s)

expandAngle :: Maybe String -> String -> String
expandAngle _ u | "http://" `L.isPrefixOf` u = u
expandAngle _ u | "https://" `L.isPrefixOf` u = u
expandAngle (Just b) u = b ++ u
expandAngle Nothing u = u

parseSubject :: String -> Maybe (String, String)
parseSubject s = case nextToken s of
  Nothing -> Nothing
  Just (tok, rest) -> Just (tok, trim rest)

nextToken :: String -> Maybe (String, String)
nextToken [] = Nothing
nextToken ('<':xs) =
  case span (/= '>') xs of
    (u, '>':rest) -> Just ('<':u ++ ">", rest)
    _             -> Nothing
nextToken ('"':xs) =
  case span (/= '"') xs of
    (v, '"':rest) -> Just ('"':v ++ "\"", rest)
    _             -> Nothing
nextToken xs =
  let (tok, rest) = break isSpace xs
  in if null tok then Nothing else Just (tok, rest)

splitTopLevel :: Char -> String -> [String]
splitTopLevel sep = go False False ""
  where
    go _ _ acc [] = [trim acc]
    go inAng inStr acc (c:cs)
      | c == '<' && not inStr = go True inStr (acc ++ [c]) cs
      | c == '>' && not inStr = go False inStr (acc ++ [c]) cs
      | c == '"' && not inAng = go inAng (not inStr) (acc ++ [c]) cs
      | c == sep && not inAng && not inStr = trim acc : go False False "" cs
      | otherwise = go inAng inStr (acc ++ [c]) cs

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

stripAngles :: String -> String
stripAngles = init . tail

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd f = reverse . dropWhile f . reverse