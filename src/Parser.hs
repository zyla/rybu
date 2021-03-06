module Parser (
  -- * Main interface
    parseModel

  -- * Specific parsers
  , Parser
  , model
  , process
  , server
  , expr
  , predicate
  , typ
  , whiteSpace

  -- * Reexports for convenience
  , Text.Parsec.parse
) where

import Control.Monad
import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))

import AST

type Parser = Parsec String ()

whiteSpace :: Parser ()
T.TokenParser {..} = T.makeTokenParser L.haskellDef
    { T.reservedNames = [ "server", "process", "thread", "var", "loop", "match", "return", "yield" ] }

semicolon = reservedOp ";"

server :: Parser Server
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

typ :: Parser TypeExpr
typ = simpleType >>= arraySuffix

arraySuffix t = 
      brackets ((ArrayE t <$> expr) >>= arraySuffix)
  <|> pure t

simpleType =
      EnumE <$> braces (identifier `sepBy1` comma)
  <|> parens typ
  <|> RangeE <$> term <*> (reservedOp ".." *> term)

transition = do
    (message, pred) <- braces $ (,) <$> messageSig <*>
        (reservedOp "|" *> predicate <|> pure (BoolLit True))
    ndParams <- option [] $ reserved "for" *> parens (formalParam `sepBy` comma)
    reservedOp "->"
    (maybeOutSignal, assignments, yield) <-
        try (braces retvalOnly)
        <|> try (braces $ do
            maybeOutSignal <- optionMaybe (try $ identifier <* semicolon)
            assignments <- assign `sepBy` comma
            pure (maybeOutSignal, assignments, False)
        ) <|> try (braces $ do
          assignments <- many (assign <* semicolon)
          maybeOutSignal <- optionMaybe (reserved "return" *> atom <* semicolon)
          pure (maybeOutSignal, assignments, False)
        ) <|> try (braces $ do
          reserved "yield" <* semicolon  -- TODO(hator): make it show better error message when semicolon is missing
          pure (Nothing, [], True)
        )

    return $ if yield
        then Yield message pred
        else Transition message pred ndParams maybeOutSignal assignments
  where
    retvalOnly = do
        retVal <- identifier
        pure (Just retVal, [], False)

formalParam = (,) <$> identifier <*> (reservedOp ":" *> typ)

messageSig = MessageSig <$> identifier <*> option [] (parens $ formalParam `sepBy` comma)

predicate :: Parser Predicate
predicate = buildExpressionParser
    [ [Infix (reservedOp "&&" *> pure And) AssocLeft]
    , [Infix (reservedOp "||" *> pure Or) AssocLeft]
    ] cmpExpr

cmpExpr = Cmp <$> expr <*> cmpOp <*> expr

assign = (,) <$> lhs <*> (reservedOp "=" *> expr)

lhs = LHS <$> identifier <*> many (brackets expr)

cmpOp =
      pure Equal <* reservedOp "=="
  <|> pure Equal <* reservedOp "="
  <|> pure NotEqual <* reservedOp "!="
  <|> pure LessThan <* reservedOp "<"
  <|> pure GreaterThan <* reservedOp ">"
  <|> pure LessThanEqual <* reservedOp "<="
  <|> pure GreaterThanEqual <* reservedOp ">="

additiveOp =
      pure Plus <* reservedOp "+"
  <|> pure Minus <* reservedOp "-"

multiplicativeOp =
      pure Modulo <* reservedOp "%"

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"
table =
    [ [Postfix arrayIndexOrSlice]
    , [Infix (flip BinOp <$> multiplicativeOp) AssocLeft]
    , [Infix (flip BinOp <$> additiveOp) AssocLeft]
    ]

term =
      identifierTerm
  <|> LitInt <$> integer
  <|> LitSym <$> atom
  <|> brackets (
        LitArrFill <$> try (expr <* semicolon) <*> expr
    <|> LitArr <$> (expr `sepBy` comma))
  <|> parens expr

identifierTerm = do
  id <- identifier
  (do guard (id == "sum")
      expr <- parens expr
      pure (ArraySum expr)
   ) <|> pure (Var id)

atom = reservedOp ":" *> identifier

arrayIndexOrSlice = brackets $ do
    index1 <- expr
    (reservedOp ".." *> do
        index2 <- expr
        pure (\arrayE -> ArraySlice arrayE index1 index2)
     ) <|> pure (\arrayE -> ArrayIndex arrayE index1)

model :: Parser Model
model = Model
    <$> many globalConst
    <*> many server
    <*> many serverInstance
    <*> many process

globalConst = do
    reserved "const"
    (,) <$> identifier <*> (reservedOp "=" *> expr <* semicolon)
    

serverInstance = ServerInstance
    <$> (reserved "var" *> identifier)
    <*> (reservedOp "=" *> identifier <* parens (pure ()))
    <*> (initialState <* semicolon)

initialState = braces (((,) <$> identifier <*> (reservedOp "=" *> expr)) `sepBy` comma)

process :: Parser Process
process =
  Process
    <$> (threadKeyword *> identifier <* parens (pure ()))
    <*> statement

threadKeyword = reserved "process" <|> reserved "thread"

statement =
      Block <$> braces (many statement)
  <|> Loop <$> (reserved "loop" *> statement)
  <|> (pure Skip <* reserved "skip" <* reservedOp ";")
  <|> Msg <$> (message <* semicolon)
  <|> Match <$> (reserved "match" *> message) <*> braces (many1 matchCase)

matchCase = (,)
    <$> (optional colon *> identifier)
    <*> (reservedOp "=>" *> statement)
  
message = Message
    <$> identifier
    <*> (reservedOp "." *> identifier)
    <*> parens (expr `sepBy` comma)

-- | Parse a Rybu file.
parseModel = parse (whiteSpace *> model <* eof)
