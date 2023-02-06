module DayThirteen where

import Data.Char (digitToInt)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

data Packet
  = V Integer
  | P [Packet]
  deriving (Show)

parseValue = V <$> T.integer lexer

parsePacket = P <$> P.between open close (P.sepBy p (P.char ','))
  where
    open = T.symbol lexer "["
    close = T.symbol lexer "]"
    p = parseValue <|> parsePacket


lexer = T.makeTokenParser emptyDef

comparePacket :: Packet -> Packet -> Bool
comparePacket (V l) (V r) = l <= r
comparePacket (P l) (P r) = and $ zipWith comparePacket l r
comparePacket (P l) (V r) = comparePacket (P l) (P [V r])
comparePacket (V l) (P r) = comparePacket (P [V l]) (P r)

main = P.parseTest parsePacket
