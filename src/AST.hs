module AST where

import qualified Data.List as L

newtype Program = Program [Rule] deriving Show

-- e.g. ancestor(X,Y) :- parent(X,Y).
data Rule = Rule Rel [[Rel]] deriving Show

-- e.g. parent(X,Y).
{- this supports either a relation or a cut operator -}
data Rel = Rel String [Term] 
         | Cut deriving Show

{- this supports either an atom, variable or functor term -} 
data Term =
  Atom String
  | Var String
  | Func String [Term] deriving (Eq, Ord)

instance Show Term where
  show (Atom a) = a
  show (Var x) = x
  show (Func name terms) = name ++ "(" ++ L.intercalate ", " (map show terms) ++ ")"
