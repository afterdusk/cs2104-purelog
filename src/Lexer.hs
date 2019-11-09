module Lexer where

import Text.Parsec

-- TName: lowercase or quoted
-- TVar: uppercase or underscore
-- TSym: symbol e.g. :-, (, ), ., ...
data Token = TName String | TVar String | TSym String deriving Show

type Lexer = Parsec String ()

nameL :: Lexer Token
-- Tname fmap lowerName or quoted
nameL = TName <$> (lowerName <|> quoted)
  where
    -- box up (cons map lower map (many alphaNum or char '_'))
    lowerName = pure (:) <*> lower <*> (many (alphaNum <|> char '_'))
    -- match \' then discard it, match many alphaNum then match \' again and discard
    quoted = char '\'' *> many alphaNum <* char '\''

varL :: Lexer Token
-- TVar fmap upperName or underscores
varL = TVar <$> (upperName <|> string "_")
  where
    upperName = pure (:) <*> upper <*> (many (alphaNum <|> char '_'))

symL :: Lexer Token
-- TSym fmap ":-" or [string matches of "().;,!"]
symL = TSym <$> (string ":-" <|> (choice . map (string . (: [])) $ "().;,!"))

tokensL :: Lexer [(SourcePos, Token)]
-- between <open> <close> <p> parses open then p then close, returning p
-- between spaces spaces: partially applied function ready to parse something
-- in between spaces
-- flip <func> helps func take arguments in reverse
-- sepBy <p> <sep> parses 0 or more occurences of p separated by sep
tokensL = between spaces spaces . flip sepBy spaces $ 
  -- getPosition: Returns the current source position
  -- boxed up comma applied to getPosition applied to tokenP
  (pure (,) <*> getPosition <*> tokenP)
  where
    -- each token can be a name, var or sym
    tokenP = nameL <|> varL <|> symL