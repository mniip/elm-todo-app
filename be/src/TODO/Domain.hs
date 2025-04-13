module TODO.Domain where

import Data.Tagged
import Data.Text
import Data.UUID


type ItemId = Tagged "item" UUID

data Item = Item
  { title :: Text
  , tags :: [TagId]
  , completed :: Bool
  }

type TagId = Tagged "tag" UUID

data Tag = Tag
  { name :: Text
  , color :: Color
  }

newtype Color = Color Text
