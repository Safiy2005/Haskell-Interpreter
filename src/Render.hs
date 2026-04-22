-- PRINT CANONICAL OUTPUT:
-- FULL URLS
-- ONE TRIPLE PER LINE
-- DEDUPLICATED
-- SORTED CORRECTLY

module Render
  ( renderGraph
  , renderTriple
  , renderNode
  ) where

import Data.List (groupBy, intercalate, sortOn)
import Syntax

renderNode :: Node -> String
renderNode (URI u)    = "<" ++ u ++ ">"
renderNode (LitStr s) = show s
renderNode (LitInt i) = show i

renderTriple :: Triple -> String
renderTriple (Triple s p o) =
  intercalate " " [renderNode s, renderNode p, renderNode o] ++ " ."

tripleKey :: Triple -> (String, String, String)
tripleKey (Triple s p o) = (renderNode s, renderNode p, renderNode o)

canon :: [Triple] -> [Triple]
canon = map head . groupBy same . sortOn tripleKey
  where
    same x y = tripleKey x == tripleKey y

renderGraph :: [Triple] -> String
renderGraph = unlines . map renderTriple . canon