{-# LANGUAGE RecordWildCards #-}
module ImpParser where

import Control.Monad (forM_)
import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))
import qualified Data.Set as S
import qualified Data.Map as M

import Imp

T.TokenParser {..} = T.makeTokenParser L.haskellDef
    { T.reservedNames = [ "server", "process", "var", "loop", "match" ] }

semicolon = reservedOp ";"

server = do
    reserved "server"
    name <- identifier

    braces $ do
        vars <- var `sepEndBy` semicolon
        transitions <- many transition

        return $ Server name (M.fromList vars) transitions

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
    (maybeOutSignal, assignments) <- try (braces retvalOnly) <|>
        (braces $ do
            maybeOutSignal <- optionMaybe (try $ identifier <* semicolon)
            (,) maybeOutSignal <$> (assign `sepBy` comma))

    return $ Transition message pred maybeOutSignal assignments
  where
    retvalOnly = flip (,) [] . Just <$> identifier

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

model = Model
    <$> many server
    <*> many process

process =
  Process
    <$> (reserved "process" *> identifier <* parens (pure ()))
    <*> statement

statement =
      Block <$> braces (many statement)
  <|> Loop <$> (reserved "loop" *> statement)
  <|> (pure Skip <* reserved "skip" <* reservedOp ";")
  <|> Msg <$> (message <* semicolon)
  <|> Match <$> (reserved "match" *> message) <*> braces (many1 matchCase)

matchCase = (,)
    <$> identifier
    <*> (reservedOp "=>" *> statement)
  
message = Message <$> identifier <*> (reservedOp "." *> identifier <* parens (pure ()))


main = getContents >>= processModelFile

processModelFile file =
  case parse model "" file of

    Left err ->
      putStrLn $ "Syntax error: " ++ show err

    Right Model{model_servers=servers, model_procs=procs} -> do

      forM_ servers $ \srv -> do
        putStrLn $ "server " ++ server_name srv
        case compileTransitions srv of
          Left err -> putStrLn $ "Error: " ++ show err
          Right transitions -> mapM_ print transitions

      forM_ procs $ \proc -> do
        putStrLn $ "process " ++ show proc
