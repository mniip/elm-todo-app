module TODO.Handlers
  ( handlers
  ) where

import Data.Coerce
import Data.Functor
import Data.List.NonEmpty qualified as NonEmpty
import Servant.API
import Servant.Server
import TODO.API qualified as API
import TODO.Auth
import TODO.Domain
import TODO.Store


handlers :: MonadTaskStore m => ServerT API.API m
handlers = API.APIRoutes
  { items = \_ -> API.ItemsRoutes
    { listRoot = handleListRootItems
    , get = handleGetItem
    , create = handleCreateItem
    , update = handleUpdateItem
    , delete = handleDeleteItem
    , reparent = handleReparentItem
    }
  , tags = \_ -> API.TagsRoutes
    { list = handleListTags
    , create = handleCreateTag
    , update = handleUpdateTag
    , delete = handleDeleteTag
    }
  , auth = handleAuth
  }

handleListRootItems :: MonadTaskStore m => m [API.ItemBrief]
handleListRootItems = do
  items <- listRootItems
  pure $ items <&> convertItemBrief
  where
    convertItemBrief (i, item) = API.ItemBrief
      { id = i
      , title = item.title
      , completed = item.completed
      }

convertItem :: ItemId -> Item -> Maybe ItemId -> [ItemId] -> API.Item
convertItem i item parent children = API.Item
  { id = i
  , parent
  , title = item.title
  , tags = item.tags
  , completed = item.completed
  , children
  }

handleGetItem :: MonadTaskStore m => API.ItemId -> m API.Item
handleGetItem i = do
  getItem i >>= \case
    Left (NoSuchItem i') -> error $ "No item " <> show i'
    Right item -> do
      parent <- getItemParent i
      children <- getItemChildren i
      pure $ convertItem i item parent children

convertItemInsert :: API.ItemInsert -> Item
convertItemInsert API.ItemInsert{..} = Item{..}

handleCreateItem :: MonadTaskStore m => API.ItemInsert -> m API.Item
handleCreateItem ins = do
  insertItem item >>= \case
    Left (NoSuchTag t) -> error $ "No tag " <> show t
    Right i -> pure $ convertItem i item parent children
  where
    item = convertItemInsert ins
    parent = Nothing
    children = []

handleUpdateItem
  :: MonadTaskStore m => API.ItemId -> API.ItemInsert -> m API.Item
handleUpdateItem i ins = do
  updateItem i item >>= \case
    Left (Left (NoSuchItem i')) -> error $ "No item " <> show i'
    Left (Right (NoSuchTag t)) -> error $ "No tag " <> show t
    Right () -> do
      parent <- getItemParent i
      children <- getItemChildren i
      pure $ convertItem i item parent children
  where
    item = convertItemInsert ins

handleDeleteItem :: MonadTaskStore m => API.ItemId -> m NoContent
handleDeleteItem i = do
  void $ deleteItem i
  pure NoContent

handleReparentItem
  :: MonadTaskStore m => API.ItemId -> Maybe API.ItemId -> m API.Item
handleReparentItem i parent = do
  reparentItem i parent >>= \case
    Left (Left (NoSuchItem i')) -> error $ "No item " <> show i'
    Left (Right (ParentCycle cyc)) -> error
      $ "Parent cycle: " <> show (NonEmpty.toList cyc)
    Right () -> handleGetItem i

convertTag :: TagId -> Tag -> API.Tag
convertTag i tag = API.Tag
  { id = i
  , name = tag.name
  , color = coerce @Color @API.Color tag.color
  }

handleListTags :: MonadTaskStore m => m [API.Tag]
handleListTags = do
  tags <- listTags
  pure $ tags <&> uncurry convertTag

convertTagInsert :: API.TagInsert -> Tag
convertTagInsert ins = Tag
  { name = ins.name
  , color = coerce @API.Color @Color ins.color
  }

handleCreateTag :: MonadTaskStore m => API.TagInsert -> m API.Tag
handleCreateTag ins = do
  t <- insertTag tag
  pure $ convertTag t tag
  where
    tag = convertTagInsert ins

handleUpdateTag :: MonadTaskStore m => API.TagId -> API.TagInsert -> m API.Tag
handleUpdateTag t ins = do
  updateTag t tag >>= \case
    Left (NoSuchTag t') -> error $ "No tag " <> show t'
    Right () -> pure $ convertTag t tag
  where
    tag = convertTagInsert ins

handleDeleteTag :: MonadTaskStore m => API.TagId -> m NoContent
handleDeleteTag t = do
  void $ deleteTag t
  pure NoContent
