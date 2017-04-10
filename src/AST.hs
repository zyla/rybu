module AST where

type Symbol = String

-- Model

data Model = Model
    { model_constants :: [(Symbol, Expr)]
    , model_servers :: [Server]
    , model_serverInstances :: [ServerInstance]
    , model_procs :: [Process]
    } deriving (Show)

-- Server
type ServerName = Symbol

data Server = Server
    { server_name :: ServerName
    , server_vars :: [(Symbol, TypeExpr)]
    , server_transitions :: [Transition]
    } deriving (Show)

data Transition = Transition
    { t_sig :: MessageSig
    , t_pred :: Predicate
    , t_ndParams :: [(Symbol, TypeExpr)]
    , t_reply :: Maybe Symbol
    , t_update :: [(LHS, Expr)]
    } deriving (Show)

data MessageSig = MessageSig
    { ms_name :: Symbol
    , ms_params :: [(Symbol, TypeExpr)]
    } deriving (Show)

data LHS = LHS
    { lhs_var :: Symbol
    , lhs_indexes :: [Expr]
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
    | NotEqual
    | LessThan
    | GreaterThan
    | LessThanEqual
    | GreaterThanEqual
      deriving (Show)

data Expr =
      Var Symbol
    | LitInt Integer
    | LitSym Symbol
    | LitArr [Expr] -- [1, 2, 3]
    | LitArrFill Expr Expr -- [5; 0]
    | ArrayIndex Expr Expr -- arr[3]
    | ArraySlice Expr Expr Expr -- arr[1..3]
    | BinOp Expr BinOp Expr -- 2 + 3
    | ArraySum Expr -- sum(arr)
    | ArrayCount Expr -- count(arr)
      deriving (Show)

data BinOp = Plus | Minus | Modulo deriving (Show)

data Value = Sym Symbol | Int Integer | Arr [Value] deriving (Eq, Show)

type ServerInstanceName = Symbol

data ServerInstance = ServerInstance
    { si_name :: ServerInstanceName
    , si_serverType :: ServerName
    , si_initialState :: [(Symbol, Expr)]
    } deriving (Show)

-- Process
type ProcessName = Symbol

data Process = Process
    { process_name :: ProcessName
    , process_stmt :: Statement
    } deriving (Show)

data Statement = Skip | Loop Statement | Block [Statement] | Msg Message | Match Message [(Symbol, Statement)]
    deriving (Show)

data Message = Message { message_server :: ServerName, message_msg :: Symbol, message_params :: [Expr] } deriving (Show)
