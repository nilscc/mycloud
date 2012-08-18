{-# LANGUAGE ViewPatterns #-}

module MyCloud.Files
  ( toFile, fromFile, fromFile'
  , storeFile, loadFile
  -- , blockSize, numberOfBlocks
  ) where

import Control.Monad.Reader
import System.IO
import Text.ProtocolBuffers

import qualified Data.Sequence        as S
import qualified Data.Foldable        as F
import qualified Data.ByteString.Lazy as BL

--import MyCloud.Internal.Types

import MyCloudProto.File
import MyCloudProto.File.Block

blockSize :: Int
blockSize = 512 * 1024

--numberOfBlocks :: File -> Int
--numberOfBlocks File { blocks = s } = S.length s

--------------------------------------------------------------------------------
-- Reading from/writing to real files

toFile :: MonadIO m => FilePath -> m File
toFile fp = File (fromString fp) `liftM` toBlocks fp

toBlocks :: MonadIO m => FilePath -> m (Seq Block)
toBlocks fp = liftIO $ do
  h <- openBinaryFile fp ReadMode
  hSetBinaryMode h True
  go h 0 S.empty
 where
  go h bid s = do
    bytes <- BL.hGetNonBlocking h blockSize
    if BL.null bytes
       then hClose h >> return s
       else go h (bid+1) $
            s S.|> Block { block_id    = bid
                         , block_bytes = bytes }

fromFile :: MonadIO m => File -> m ()
fromFile (File (toString -> fp) bs) = liftIO $
  withBinaryFile fp WriteMode $ \h -> do
    hSetBinaryMode h True
    F.forM_ bs $ \b -> do
      --putStrLn $ "Writing block #" ++ show (block_id b)
      BL.hPut h (block_bytes b)

fromFile' :: MonadIO m => FilePath -> File -> m ()
fromFile' fp f = fromFile f{ file_path = fromString fp }

--------------------------------------------------------------------------------
-- Loading/storing PB Files

storeFile :: MonadIO m => FilePath -> File -> m ()
storeFile fp f = liftIO $ do
  withBinaryFile fp WriteMode $ \h -> do
    hSetBinaryMode h True
    BL.hPut h (messagePut f)

loadFile :: MonadIO m => FilePath -> m (Maybe File)
loadFile fp = liftIO $ do
  h <- openBinaryFile fp ReadMode
  hSetBinaryMode h True
  bs <- BL.hGetContents h
  return $
    case messageGet bs of
         Right (msg,x) | BL.null x -> Just msg
         _                         -> Nothing
        
