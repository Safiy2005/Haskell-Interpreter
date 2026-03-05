module Types where

-- A resource is always a URI
type URI = String 

data Subject   = SubjURI URI 
               deriving (Show, Eq, Ord)

data Predicate = PredURI URI 
               deriving (Show, Eq, Ord)

-- Objects can be URIs, Strings, or Integers
data Object    = ObjURI URI 
               | ObjString String 
               | ObjInt Integer 
               deriving (Show, Eq, Ord)

-- A single edge in the graph
data Triple    = Triple Subject Predicate Object 
               deriving (Show, Eq, Ord)

-- A graph is just a collection of triples
type Graph     = [Triple]