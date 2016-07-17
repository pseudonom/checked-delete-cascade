module Database.Persist.Delete.Indexed where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Indexed.Log
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist

data Delete a = Delete

class DeleteCascadeI record (pointedAtBy :: [*]) backend | record -> pointedAtBy, record -> backend where
  deleteCascadeI
    :: (MonadIO m, Delete record `ConsCollapse` i, (i `Covers` pointedAtBy) ~ 'True)
    => [Filter record] -> IndexedLogT (ReaderT backend m) i (Delete record ::: i) ()
