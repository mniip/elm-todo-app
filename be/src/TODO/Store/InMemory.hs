module TODO.Store.InMemory
  ( newInMemoryStore
  , runInMemoryStoreT
  ) where

import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.IORef
import Data.Kind
import Data.List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Tagged
import Data.UUID.V4 qualified as UUID
import TODO.Domain
import TODO.Store
import TODO.Store.InMemory.Type
import TODO.Store.InMemory.Type qualified as TaskStore (TaskStore(..))


newtype InMemoryStoreT (m :: Type -> Type) a
  = InMemoryStoreT (IORef TaskStore -> m a)
  deriving (Functor, Applicative, Monad, MonadIO)
    via (ReaderT (IORef TaskStore) m)

newInMemoryStore :: MonadIO m => m (IORef TaskStore)
newInMemoryStore = liftIO $ newIORef TaskStore
  { items = mempty
  , itemParents = mempty
  , itemChildren = mempty
  , tags = mempty
  }

runInMemoryStoreT :: IORef TaskStore -> InMemoryStoreT m a -> m a
runInMemoryStoreT ref (InMemoryStoreT k) = k ref

readTaskStore :: MonadIO m => InMemoryStoreT m TaskStore
readTaskStore = InMemoryStoreT \ref -> liftIO $ readIORef ref

modifyTaskStore
  :: MonadIO m => (TaskStore -> (TaskStore, a)) -> InMemoryStoreT m a
modifyTaskStore f = InMemoryStoreT \ref -> liftIO $ atomicModifyIORef' ref f

reparentChildren :: MonadIO m => ItemId -> InMemoryStoreT m ()
reparentChildren i = modifyTaskStore \store -> (,()) let
    !newParent = Map.lookup i store.itemParents
    !children = Map.lookup i store.itemChildren
  in store
    { itemParents = foldl'
      (flip $ Map.update $ const newParent)
      store.itemParents
      (Compose children)
    , itemChildren = store.itemChildren
      & case (newParent, children) of
        (Just parent, Just childrenNE) -> Map.insertWith (<>) parent childrenNE
        _ -> id
    }

instance MonadIO m => MonadTaskStore (InMemoryStoreT m) where
  listRootItems = readTaskStore <&> \store
    -> Map.toList (store.items `Map.difference` store.itemParents)
      <&> \(i, item) ->
        ( i
        , item
        )

  getItem i = readTaskStore <&> \TaskStore{ items }
    -> maybe (Left $ NoSuchItem i) Right $ Map.lookup i items

  getItemParent i = readTaskStore <&> \TaskStore{ itemParents }
    -> Map.lookup i itemParents

  getItemChildren i = readTaskStore <&> \TaskStore{ itemChildren }
    -> maybe [] NonEmpty.toList $ Map.lookup i itemChildren

  insertItem item = do
    i <- liftIO $ Tagged <$> UUID.nextRandom
    result <- modifyTaskStore \store -> if
      | Just t <- find (\t -> not $ Map.member t store.tags) item.tags
      -> (store, Left $ NoSuchTag t)
      | (!mOld, !items') <- Map.insertLookupWithKey
        (\_key _new old -> old) i item store.items
      -> (store { items = items' }, Right $ isJust mOld)
    case result of
      Left err -> pure $ Left err
      Right False -> pure $ Right i
      Right True -> insertItem item

  updateItem i item = modifyTaskStore \store -> if
    | Just t <- find (\t -> not $ Map.member t store.tags) item.tags
    -> (store, Left $ Right $ NoSuchTag t)
    | (Just _, !items') <- Map.insertLookupWithKey
      (\_key new _old -> new) i item store.items
    -> (store { items = items' }, Right ())
    | otherwise
    -> (store, Left $ Left $ NoSuchItem i)

  deleteItem i = do
    reparentChildren i
    modifyTaskStore \store -> if
      | (Just _, items') <- Map.updateLookupWithKey
        (\_ _ -> Nothing) i store.items
      , (mParent, itemParents') <- Map.updateLookupWithKey
        (\_ _ -> Nothing) i store.itemParents
      , itemChildren' <- store.itemChildren
        & Map.delete i
        & case mParent of
          Nothing -> id
          Just parent -> Map.update
            (NonEmpty.nonEmpty . delete i . NonEmpty.toList) parent
      ->
        ( store
          { items = items'
          , itemParents = itemParents'
          , itemChildren = itemChildren'
          }
        , Right ()
        )
      | otherwise
      -> (store, Left $ NoSuchItem i)

  reparentItem i mParent = modifyTaskStore \store -> if
    | not $ Map.member i store.items
    -> (store, Left $ Left $ NoSuchItem i)
    | Just parent <- mParent, not $ Map.member parent store.items
    -> (store, Left $ Left $ NoSuchItem parent)
    | Just parent <- mParent, Just cyc <- parentCycle store.itemParents parent []
    -> (store, Left $ Right cyc)
    | otherwise
    -> (, Right ()) store
      { itemParents = Map.alter (const mParent) i store.itemParents
      , itemChildren = store.itemChildren
        & case Map.lookup i store.itemParents of
          Nothing -> id
          Just oldParent -> Map.update
            (NonEmpty.nonEmpty . delete i . NonEmpty.toList) oldParent
        & case mParent of
          Nothing -> id
          Just parent -> Map.insertWith (<>) parent (NonEmpty.singleton i)
      }
    where
      parentCycle parents parent seen
        | i == parent = Just (ParentCycle $ parent NonEmpty.:| seen)
        | Just parent' <- Map.lookup parent parents
        = parentCycle parents parent' (parent:seen)
        | otherwise = Nothing

  listTags = readTaskStore <&> \TaskStore{ tags } -> Map.toList tags

  insertTag tag = do
    t <- liftIO $ Tagged <$> UUID.nextRandom
    result <- modifyTaskStore \store -> if
      | (!mOld, !tags') <- Map.insertLookupWithKey
        (\_key _new old -> old) t tag store.tags
      -> (store { TaskStore.tags = tags' }, isJust mOld)
    case result of
      False -> pure t
      True -> insertTag tag

  updateTag t tag = modifyTaskStore \store -> if
    | (Just _, !tags') <- Map.insertLookupWithKey
      (\_key new _old -> new) t tag store.tags
    -> (store { TaskStore.tags = tags' }, Right ())
    | otherwise
    -> (store, Left $ NoSuchTag t)

  deleteTag t = do
    modifyTaskStore \store -> if
      | Just (i, _) <- find (\(_, item) -> t `elem` item.tags)
        $ Map.toList store.items
      -> (store, Left $ Right $ TagUsed t i)
      | (Just _, tags') <- Map.updateLookupWithKey
        (\_ _ -> Nothing) t store.tags
      -> (store { TaskStore.tags = tags' }, Right ())
      | otherwise
      -> (store, Left $ Left $ NoSuchTag t)
