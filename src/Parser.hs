module Parser where

import Text.Parsec
import Lexer
import AST

import Debug.Trace 

type Parser = Parsec [(SourcePos, Token)] ()

-- converts function to parser
tokenP :: (Token -> Maybe a) -> Parser a
-- token :: Stream s Identity t	 
-- => (t -> String)	Token pretty-printing function.
-- -> (t -> SourcePos) Computes the position of a token.
-- -> (t -> Maybe a) Matching function for the token to parse.
-- -> Parsec s u a
-- fst (SourcePos, Token) returns the position of the token
-- test . snd (SourcePos, Token) returns the parsed token
tokenP test = token show fst (test . snd)

-- consume symbol and returns a void parser
symbol :: String -> Parser ()
symbol c = tokenP (\t -> case t of
  -- if token is TSym, return Just () if c matches whatever is passed
  TSym s -> if s == c then Just () else Nothing
  _ -> Nothing)

-- returns a functor Parser
functorP :: Parser (String, [Term]) -- functor and relation have the same parser
functorP = do
  term <- termP
  case term of 
    (Func name terms) -> trace ("functorP: " ++ show term) (return (name, terms))
    (Atom s) -> return (s, []) -- TODO: is this correct?
    (Var s) -> return(s, [])

-- returns a term Parser (which could be Var, Atom or Func)
termP :: Parser Term
termP = do
    -- set Parser for name/var
    name <- tokenP (\t -> case t of
                        (TName s) -> Just (Atom s)
                        (TVar s) -> Just (Var s)
                        _ -> Nothing) -- TODO: What is assigned to name if result is "Nothing"?
    -- maps parser result to Func if Atom is passed and Atom is function name
    case name of -- parser consumes name which can be of atom or functor
      -- Func a is partially applied function with a name but no terms
      -- if Terms separated by commas and brackets found, return Func
      -- if not, return Atom
      -- fmap <(Func a) . ... . ...> <termP>
      (Atom a) -> (fmap (Func a) . between (symbol "(") (symbol ")")
                 -- returns list of comma-separated terms 
                 . flip sepBy1 (symbol ",") $ termP) <|> return name
      _ -> return name

{- parse a relation or cut in body of clause -}
relP :: Parser Rel
-- consume "!" and return Cut OR relHeadP
relP = (symbol "!" *> return Cut) 
       <|> relHeadP

{- parse a relation in head of clause -}
relHeadP :: Parser Rel
-- uncurry: applies Rel to argument tuples
relHeadP = fmap (uncurry Rel) functorP

ruleP :: Parser Rule
ruleP = do
  head <- relHeadP
  rels <- 
    (between (symbol ":-") (symbol ".") (flip sepBy (symbol ",") . flip sepBy (symbol ";") $ relP))
      <|> (symbol "." *> return [[]])
  trace ("ruleP: " ++ show (Rule head rels)) (return (Rule head rels))

programP :: Parser Program
programP = fmap Program $ many ruleP

parseProgram :: String -> Either ParseError Program
parseProgram source = do
  tokens  <- parse (tokensL   <* eof) "" source
  parse (programP <* eof) "" tokens

parseRel :: String -> Either ParseError Rel
parseRel source = do
  tokens  <- parse (tokensL   <* eof) "" source
  parse (relHeadP <* (symbol ".") <* eof) "" tokens
