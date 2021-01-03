module Data.Cache.Vault
  ( Cache, newCache
  , Key, mintLabeledKey, mintUniqKey
  , insert, delete, reset, lookup
  )
where

import Control.Concurrent.STM (atomically)
import Data.IORef
import Data.Time
import Data.Typeable
import GHC.Exts
import GHC.Fingerprint
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified StmContainers.Map as M

type KeyRepr = T.Text

data CacheEntry
  = CacheEntry
  { ceValidUntil :: Maybe UTCTime
  , ceValue :: Any
  }

newtype Cache
  = Cache { _unCache :: M.Map KeyRepr CacheEntry }

newtype Key a
  = Key { unKey :: KeyRepr }
  deriving (Show, Eq)

keyCounter :: IORef Int
keyCounter =
  unsafePerformIO $ newIORef 0
{-# NOINLINE keyCounter #-}

-- | Mint a globally unique key
mintUniqKey :: IO (Key a)
mintUniqKey =
  atomicModifyIORef' keyCounter $ \ctr ->
  ( ctr + 1
  , Key $ "uniq/" <> T.pack (show ctr)
  )

-- | Mint a key with a label for a given type. Note that keys
-- with the same label but for different types are different.
mintLabeledKey :: forall a. Typeable a => T.Text -> Key a
mintLabeledKey label =
  Key $ "label/" <> label <> "/" <> typeSig
  where
    typeSig =
      let (Fingerprint x1 x2) =
            typeRepFingerprint (typeRep (Proxy :: Proxy (Proxy a)))
      in T.pack $ show x1 <> "." <> show x2

-- | Create a new cache container.
newCache :: IO Cache
newCache =
  Cache <$> M.newIO

-- | Insert a value into the cache with an optional expiry date.
insert :: Key a -> Maybe UTCTime -> a -> Cache -> IO ()
insert k t v (Cache ref) =
  atomically $ M.insert val key ref
  where
    val =
      CacheEntry
      { ceValidUntil = t
      , ceValue = unsafeCoerce v
      }
    key = unKey k

-- | Delete a value from the cache.
delete :: Key a -> Cache -> IO ()
delete k (Cache ref) =
  atomically $ M.delete (unKey k) ref

-- | Purge all values form the cache.
reset :: Cache -> IO ()
reset (Cache ref) =
  atomically $ M.reset ref

-- | Given the current time, lookup a key in the cache.
lookup :: UTCTime -> Key a -> Cache -> IO (Maybe a)
lookup now k (Cache ref) =
  do entry <-
       atomically $ M.lookup (unKey k) ref
     case entry of
       Nothing -> pure Nothing
       Just e ->
         case ceValidUntil e of
           Just validUntil | validUntil < now -> pure Nothing
           _ -> pure (Just $ unsafeCoerce (ceValue e))
