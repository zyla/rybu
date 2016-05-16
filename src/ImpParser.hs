{-# LANGUAGE RecordWildCards #-}
module ImpParser where

import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))
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

predicate = buildExpressionParser
    [ [Infix (reservedOp "||" *> pure Or) AssocLeft]
    , [Infix (reservedOp "&&" *> pure And) AssocLeft]
    ] cmpExpr

cmpExpr = Cmp <$> expr <*> cmpOp <*> expr

assign = (,) <$> identifier <*> (reservedOp "=" *> expr)

cmpOp =
      pure Equal <* reservedOp "="
  <|> pure LessThan <* reservedOp "<"
  <|> pure GreaterThan <* reservedOp ">"

additiveOp =
      pure Plus <* reservedOp "+"
  <|> pure Minus <* reservedOp "-"

expr = buildExpressionParser table term <?> "expression"
table = [ [Infix (flip BinOp <$> additiveOp) AssocLeft] ]

term = Var <$> identifier <|> (Lit . Int) <$> natural

main = getContents >>= process

process file =
    case parse server "" file of
      Left err -> putStrLn $ "Syntax error: " ++ show err
      Right srv ->
        case compileTransitions srv of
          Left err -> putStrLn $ "Error: " ++ show err
          Right transitions -> mapM_ print transitions
