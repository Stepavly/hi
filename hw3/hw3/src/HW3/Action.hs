{-# LANGUAGE UndecidableInstances #-}
module HW3.Action
  (
    HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where
import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import Data.Sequence (fromList)
import Data.Set (Set, fromList, member)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import GHC.Exception (Exception)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (getStdGen, setStdGen, uniformR)

-- | Permissions for action execution.
data HiPermission =
    AllowRead -- ^ Enables 'read', 'cd', 'cwd' and 'echo' actions.
  | AllowWrite -- ^ Enables 'write' and 'mkdir' actions.
  | AllowTime -- ^ Enables 'now' action.
  deriving (Eq, Show, Ord)

-- | Permissions exception.
data PermissionException =
  PermissionRequired HiPermission
  deriving (Eq, Show)

instance Show PermissionException => Exception PermissionException

-- | Actions executor with some set of permissions.
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f (HIO runHIO') = HIO $ \perms -> do
    res <- runHIO' perms
    pure $ f res

instance Applicative HIO where
  pure a = HIO $ \_ -> pure a
  (HIO f) <*> (HIO a) = HIO $ \perms -> do
    f' <- f perms
    a' <- a perms
    pure $ f' a'

instance Monad HIO where
  (HIO m) >>= f = HIO $ \perms -> do
    a <- m perms
    runHIO (f a) perms

-- | Checks if desireable permission is owned, otherwise throws `HiPermission`.
checkPermissions :: Set HiPermission -> HiPermission -> IO ()
checkPermissions perms p =
  if member p perms
  then return ()
  else throwIO $ PermissionRequired p

-- | Action executer.
runWithPerms :: Set HiPermission -> HiAction -> IO HiValue
runWithPerms perms action =
  case action of
    HiActionRead path -> do
      checkPermissions perms AllowRead
      isFile <- doesFileExist path
      if isFile
      then do
        content <- BS.readFile path
        case decodeUtf8' content of
          Left _  -> return $ HiValueBytes content
          Right s -> return $ HiValueString s
      else do
        dirs <- listDirectory path
        return $ HiValueList $ Data.Sequence.fromList $ map (HiValueString . pack) dirs
    HiActionWrite path content -> do
      checkPermissions perms AllowWrite
      BS.writeFile path content
      return HiValueNull
    HiActionMkDir dir -> do
      checkPermissions perms AllowWrite
      createDirectory dir
      return HiValueNull
    HiActionChDir dir -> do
      checkPermissions perms AllowRead
      setCurrentDirectory dir
      return HiValueNull
    HiActionCwd -> do
      checkPermissions perms AllowRead
      HiValueString . Data.Text.pack <$> getCurrentDirectory
    HiActionNow -> do
      checkPermissions perms AllowTime
      HiValueTime <$> getCurrentTime
    HiActionRand l r -> do
      g <- getStdGen
      let (x, g') = uniformR (l, r) g
      setStdGen g'
      return $ HiValueNumber (fromIntegral x)
    HiActionEcho s -> do
      checkPermissions perms AllowWrite
      putStrLn (Data.Text.unpack s)
      return HiValueNull

instance HiMonad IO where
  runAction = runWithPerms (Data.Set.fromList [AllowRead, AllowWrite, AllowTime])

instance HiMonad HIO where
  runAction action = HIO $ flip runWithPerms action
