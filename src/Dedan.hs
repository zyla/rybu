{-# LANGUAGE RecordWildCards #-}
module Dedan where

import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

type Name = String

data SystemDef = SystemDef
    { sd_defines :: [(Name, Integer)]
    , sd_servers :: [Server]
    , sd_serverVars :: [Var]
    , sd_agentVars :: [Var]
    , sd_init :: Init
    } deriving (Eq, Show)

data Server = Server
    { s_name :: Name
    , s_agents :: [Var]
    , s_servers :: [Var]
    , s_services :: [Var]
    , s_states :: [Var]
    , s_actions :: [Action]
    } deriving (Eq, Show)

data Agent = Agent Name deriving (Eq, Show)
data Init = Init deriving (Eq, Show)

data Number = Lit Integer | Symbol Name | Plus Number Number | Minus Number Number deriving (Eq, Show)
data Var = SimpleVar Name | Array Name Number deriving (Eq, Show)

data Repeater = Repeater Name Number Number deriving (Eq, Show)

data Action = Repeated Repeater Action | Action (Message, StateRef) (Message, StateRef) deriving (Eq, Show)

data Message = Message Var Var Var deriving (Eq, Show)
data StateRef = StateRef Var Var deriving (Eq, Show)

languageDef = L.haskellDef
  { T.reservedNames = [ "server", "servers", "agent", "agents", "actions", "services", "states", "init" ] }

T.TokenParser {..} = T.makeTokenParser languageDef

file = reserved "system" *> identifier *> reservedOp ";" *>
    (SystemDef <$> many define <*> many server <*> pure [] <*> pure [] <*> pure Init)

define =  reserved "#DEFINE" *> ((,) <$> identifier <*> natural)

server = do
    reserved "server"
    reservedOp ":"
    name <- identifier

    reservedOp "("
    reserved "agents"
    agents <- vars
    reservedOp ";"
    reserved "servers"
    servers <- vars
    reservedOp ")"
    reservedOp ","

    services <- varSet "services" <* optional (reservedOp ",")
    states <- varSet "states" <* optional (reservedOp ",")

    reserved "actions"
    actions <- braces $ action `sepBy` optional (reservedOp ",")

    return $ Server name agents servers services states actions

action =
      Repeated <$> repeater <*> action
  <|> Action <$> ms <*> (reservedOp "->" *> ms)

ms = braces $ (,)
    <$> (Message <$> var <*> dotVar <*> dotVar)
    <*> (reservedOp "," *> (StateRef <$> var <*> dotVar))

dotVar = reservedOp "." *> var

repeater = do
    reservedOp "<"
    name <- identifier
    reservedOp "="
    from <- number
    reservedOp ".."
    to <- number
    char '>'

    return $ Repeater name from to

varSet kw = reserved kw *> braces vars

vars = var `sepBy1` reservedOp ","

var = do
    name <- identifier
    option (SimpleVar name) (Array name <$> brackets number)

number = do
    num <- Lit <$> natural <|> Symbol <$> identifier
    Plus num <$> (reservedOp "+" *> number)
      <|> Minus num <$> (reservedOp "-" *> number)
      <|> pure num

main = getContents >>= print . parse file ""
