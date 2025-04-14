{-# OPTIONS_GHC -Wno-orphans #-}

module TODO.API where

import Data.Aeson
import Data.Aeson.Types
import Data.Ix
import Data.Tagged
import Data.Text
import Data.Text qualified as Text
import Data.UUID
import Deriving.Aeson
import Servant.API
import Web.Cookie


type API = "api" :> NamedRoutes APIRoutes

data APIRoutes mode = APIRoutes
  { items
    :: mode
    :- "items"
    :> AuthProtect "csrf"
    :> NamedRoutes ItemsRoutes
  , tags
    :: mode
    :- "tags"
    :> AuthProtect "csrf"
    :> NamedRoutes TagsRoutes
  , auth
    :: mode
    :- "auth"
    :> Verb GET 302 '[JSON]
      (Headers
        [Header "Set-Cookie" SetCookie, Header "Location" String]
        NoContent)
  } deriving stock (Generic)

data ItemsRoutes mode = ItemsRoutes
  { listRoot
    :: mode
    :- Get '[JSON] [ItemBrief]
  , get
    :: mode
    :- Capture "item_id" ItemId
    :> Get '[JSON] Item
  , create
    :: mode
    :- ReqBody '[JSON] ItemInsert
    :> Post '[JSON] Item
  , update
    :: mode
    :- Capture "item_id" ItemId
    :> ReqBody '[JSON] ItemInsert
    :> Put '[JSON] Item
  , delete
    :: mode
    :- Capture "item_id" ItemId
    :> Delete '[JSON] NoContent
  , reparent
    :: mode
    :- Capture "item_id" ItemId
    :> "parent"
    :> ReqBody '[JSON] (Maybe ItemId)
    :> Patch '[JSON] Item
  } deriving stock (Generic)

data TagsRoutes mode = TagsRoutes
  { list
    :: mode
    :- Get '[JSON] [Tag]
  , create
    :: mode
    :- ReqBody '[JSON] TagInsert
    :> Post '[JSON] Tag
  , update
    :: mode
    :- Capture "tag_id" TagId
    :> ReqBody '[JSON] TagInsert
    :> Put '[JSON] Tag
  , delete
    :: mode
    :- Capture "tag_id" TagId
    :> Delete '[JSON] NoContent
  } deriving stock (Generic)

type APIJSONFlags = '[FieldLabelModifier CamelToSnake]
type APIJSON = CustomJSON APIJSONFlags
apiJSON :: Options
apiJSON = aesonOptions @APIJSONFlags

type ItemId = Tagged "item" UUID

data ItemBrief = ItemBrief
  { id :: ItemId
  , title :: Text
  , completed :: Bool
  } deriving stock (Show, Generic)
    deriving (FromJSON, ToJSON) via APIJSON ItemBrief

data Item = Item
  { id :: ItemId
  , parent :: Maybe ItemId
  , title :: Text
  , tags :: [TagId]
  , completed :: Bool
  , children :: [ItemId]
  } deriving stock (Show, Generic)
    deriving (FromJSON, ToJSON) via APIJSON Item

data ItemInsert = ItemInsert
  { title :: Text
  , tags :: [TagId]
  , completed :: Bool
  } deriving stock (Show, Generic)
    deriving (FromJSON, ToJSON) via APIJSON ItemInsert

type TagId = Tagged "tag" UUID

data Tag = Tag
  { id :: TagId
  , name :: Text
  , color :: Color
  } deriving stock (Show, Generic)
    deriving (FromJSON, ToJSON) via APIJSON Tag

data TagInsert = TagInsert
  { name :: Text
  , color :: Color
  } deriving stock (Show, Generic)
    deriving (FromJSON, ToJSON) via APIJSON TagInsert

newtype Color = Color Text
  deriving stock (Show)
  deriving newtype (ToJSON)

instance FromJSON Color where
  parseJSON = \case
    String str -> if
      | Text.length str == 6
      , Text.all (\c -> inRange ('0', '9') c || inRange ('a', 'f') c) str
      -> pure $ Color str
      | otherwise -> fail "Expected [0-9a-f]{6}"
    v -> typeMismatch "String" v

deriving newtype instance FromHttpApiData a => (FromHttpApiData (Tagged t a))
