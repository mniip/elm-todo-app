module TODO.Store.InMemory.Type where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import TODO.Domain


data TaskStore = TaskStore
  { items :: !(Map ItemId Item)
  , itemParents :: !(Map ItemId ItemId)
  , itemChildren :: !(Map ItemId (NonEmpty ItemId))
  , tags :: !(Map TagId Tag)
  }
