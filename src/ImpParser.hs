{-# LANGUAGE RecordWildCards #-}
module ImpParser where

import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import qualified Data.Set as S
import qualified Data.Map as M

import Imp

T.TokenParser {..} = T.makeTokenParser L.haskellDef
    { T.reservedNames = [ "server", "process", "var" ] }

semicolon = reservedOp ";"

server = do
    reserved "server"
    name <- identifier

    braces $ do
        vars <- var `sepEndBy` semicolon
        transitions <- many transition

        return $ Server (M.fromList vars) transitions

var = (,)
    <$> (reserved "var" *> identifier)
    <*> (colon *> typ)

typ =
      Enum <$> braces (S.fromList <$> (identifier `sepBy1` comma))
  <|> Range <$> natural <*> (reservedOp ".." *> natural)

transition = do
    (message, pred) <- braces $ (,) <$> identifier <*>
        (reservedOp "|" *> predicate <|> pure (BoolLit True))
    reservedOp "->"
    assignments <- braces (assign `sepBy1` comma)
    return $ Transition message pred assignments

predicate = Cmp <$> expr <*> cmpOp <*> expr

assign = (,) <$> identifier <*> (reservedOp "=" *> expr)

cmpOp =
      pure Equal <* reservedOp "="
  <|> pure LessThan <* reservedOp "<"
  <|> pure GreaterThan <* reservedOp ">"

expr = Var <$> identifier <|> (Lit . Int) <$> natural

main = getContents >>= print . parse server ""
