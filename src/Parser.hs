module Parser (
    module Parser
  , Text.Parsec.parse
) where

import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))

import AST

T.TokenParser {..} = T.makeTokenParser L.haskellDef
    { T.reservedNames = [ "server", "process", "var", "loop", "match" ] }

semicolon = reservedOp ";"

server = do
    reserved "server"
    name <- identifier

    braces $ do
        vars <- var `sepEndBy` semicolon
        transitions <- many transition

        return $ Server name vars transitions

var = (,)
    <$> (reserved "var" *> identifier)
    <*> (colon *> typ)

typ = simpleType >>= arraySuffix

arraySuffix t = 
      brackets ((ArrayE t <$> expr) >>= arraySuffix)
  <|> pure t

simpleType =
      EnumE <$> braces (identifier `sepBy1` comma)
  <|> parens typ
  <|> RangeE <$> term <*> (reservedOp ".." *> term)

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
table =
    [ [Postfix arrayIndexOrSlice]
    , [Infix (flip BinOp <$> additiveOp) AssocLeft]
    ]

term =
      Var <$> identifier
  <|> LitInt <$> natural
  <|> brackets (
        LitArrFill <$> try (expr <* semicolon) <*> expr
    <|> LitArr <$> (expr `sepBy` comma))
  <|> parens expr

arrayIndexOrSlice = brackets $ do
    index1 <- expr
    (reservedOp ".." *> do
        index2 <- expr
        pure (\arrayE -> ArraySlice arrayE index1 index2)
     ) <|> pure (\arrayE -> ArrayIndex arrayE index1)

model = Model
    <$> many server
    <*> many serverInstance
    <*> many process

serverInstance = ServerInstance
    <$> (reserved "var" *> identifier)
    <*> (reservedOp "=" *> identifier <* parens (pure ()))
    <*> (initialState <* semicolon)

initialState = braces (many ((,) <$> identifier <*> (reservedOp "=" *> constant)))

constant = Int <$> natural <|> Sym <$> identifier

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

parseModel = parse (model <* whiteSpace <* eof)
