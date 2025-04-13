module TODO.Store
  ( NoSuchItem(..)
  , ParentCycle(..)
  , NoSuchTag(..)
  , TagUsed(..)
  , MonadTaskStore(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import TODO.Domain


newtype NoSuchItem = NoSuchItem ItemId

newtype ParentCycle = ParentCycle (NonEmpty ItemId)

newtype NoSuchTag = NoSuchTag TagId

data TagUsed = TagUsed TagId ItemId

class Monad m => MonadTaskStore m where
  listRootItems :: m [(ItemId, Item)]
  getItem :: ItemId -> m (Either NoSuchItem Item)
  getItemParent :: ItemId -> m (Maybe ItemId)
  getItemChildren :: ItemId -> m [ItemId]
  insertItem :: Item -> m (Either NoSuchTag ItemId)
  updateItem :: ItemId -> Item -> m (Either (Either NoSuchItem NoSuchTag) ())
  deleteItem :: ItemId -> m (Either NoSuchItem ())
  reparentItem
    :: ItemId -> Maybe ItemId -> m (Either (Either NoSuchItem ParentCycle) ())
  listTags :: m [(TagId, Tag)]
  insertTag :: Tag -> m TagId
  updateTag :: TagId -> Tag -> m (Either NoSuchTag ())
  deleteTag :: TagId -> m (Either (Either NoSuchTag TagUsed) ())
