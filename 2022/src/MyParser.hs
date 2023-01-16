module MyParser where

import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P

bfProgram = P.many bfOp <|> bfLoop

bfOp :: Parser Char
bfOp = P.char '>' <|> P.char '<' <|> P.char '+' <|> P.char '-' <|> P.char '.' <|> P.char ','

bfLoop :: Parser String
bfLoop = P.between lbracket rbracket bfProgram

lbracket = P.char '['

rbracket = P.char ']'


main = P.parseTest bfProgram str
  where
    str = "++++++[>++++++++++++<-]>."
