-- | Simplified model of the target language.
module Dedan where

type Symbol = String

data Server = Server
    { server_name :: Symbol
    , server_agents :: [Instance]
    , server_servers :: [Instance]
    , server_services :: [Symbol]
    , server_states :: [Symbol]
    , server_actions :: [Action]
    } deriving (Eq, Show)

data Instance = Instance
    { instance_name :: Symbol
    , instance_type :: Maybe Symbol
    } deriving (Eq, Show)

data Action = Action
    { action_actor :: Symbol
    , action_inState :: Symbol
    , action_inMessage :: Symbol
    , action_outServer :: Symbol
    , action_outMessage :: Symbol
    , action_outState :: Symbol
    } deriving (Eq, Show)

data Model = Model
    { model_servers :: [Server]
    , model_serverInstances :: [Instance]
    , model_actors :: [Symbol]
    , model_serverInits :: [ServerInit]
    , model_actorInits :: [ActorInit]
    }

data ServerInit = ServerInit
    { si_serverName :: Symbol
    , si_params :: [Symbol]
    , si_initialState :: Symbol
    } deriving (Eq, Show)

data ActorInit = ActorInit
    { ai_actorName :: Symbol
    , ai_serverName :: Symbol
    , ai_initialMessage :: Symbol
    } deriving (Eq, Show)
