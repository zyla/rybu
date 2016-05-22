module AST where

type Symbol = String

-- Model

data Model = Model
    { model_servers :: [Server]
    , model_serverInstances :: [ServerInstance]
    , model_procs :: [Process]
    } deriving (Show)

-- Server

data Server = Server
    { server_name :: Symbol
    , server_vars :: [(Symbol, TypeExpr)]
    , server_transitions :: [Transition]
    } deriving (Show)

data Transition = Transition
    { t_name :: Symbol
    , t_pred :: Predicate
    , t_reply :: Maybe Symbol
    , t_update :: [(Symbol, Expr)]
    } deriving (Show)

data TypeExpr = EnumE [Symbol] | RangeE Expr Expr | ArrayE TypeExpr Expr deriving (Show)

data Type = Enum [Symbol] | Range Integer Integer | Array Type Integer deriving (Eq, Show)

data Predicate =
      Cmp Expr CmpOp Expr 
    | And Predicate Predicate 
    | Or Predicate Predicate 
    | BoolLit Bool
      deriving (Show)

data CmpOp =
      Equal
    | LessThan
    | GreaterThan
      deriving (Show)

data Expr =
      Var Symbol
    | LitInt Integer
    | LitArr [Expr] -- [1, 2, 3]
    | LitArrFill Expr Expr -- [5; 0]
    | ArrayIndex Expr Expr -- arr[3]
    | ArraySlice Expr Expr Expr -- arr[1..3]
    | BinOp Expr BinOp Expr -- 2 + 3
      deriving (Show)

data BinOp = Plus | Minus deriving (Show)

data Value = Sym Symbol | Int Integer | Arr [Value] deriving (Eq, Show)

data ServerInstance = ServerInstance
    { si_name :: Symbol
    , si_serverType :: Symbol
    , si_initialState :: [(Symbol, Value)]
    } deriving (Show)

-- Process

data Process = Process
    { process_name :: Symbol
    , process_stmt :: Statement
    } deriving (Show)

data Statement = Skip | Loop Statement | Block [Statement] | Msg Message | Match Message [(Symbol, Statement)]
    deriving (Show)

data Message = Message { message_server :: Symbol, message_msg :: Symbol } deriving (Show)
