module MyCloud.Connection where

import Codec.Crypto.SimpleAES
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent.MState
import Network
import System.IO
import Text.ProtocolBuffers

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import MyCloud.Internal.Types

import MyCloudProto.Helo
import MyCloudProto.Helo.Command

--------------------------------------------------------------------------------
-- Connection management

acceptConnections :: MyCloud ()
acceptConnections = do
  port_id <- MyCloud $ asks runOn
  s <- liftIO $ listenOn port_id
  forever $ liftIO (accept s) >>= handleConnection
 where
  handleConnection (h,_hn,_pn) = do
    sid <- addSession h
    -- check for encryption
    enc <- MyCloud $ asks encryption
    case enc of
         ForceAES _ -> do
           sendHelo sid ENABLE_ENCRYPTION Nothing
           enableEncryption sid
         _ -> return ()

addSession :: Handle -> MyCloud SessionID
addSession h = do
  MyCloud $ modifyM $ \st ->
    let clients     = connectedClients st
        sids        = M.keys $ connectedClients st
        sid         = SessionID $ maximum (map unSID sids) + 1
        client_info = ClientInfo { clientHandle   = h
                                 , usesEncryption = Nothing }
     in (sid, st { connectedClients = M.insert sid client_info clients })

getSession :: SessionID -> MyCloud (Maybe ClientInfo)
getSession sid = MyCloud $ gets (M.lookup sid . connectedClients)

enableEncryption :: SessionID -> MyCloud ()
enableEncryption sid = MyCloud $ do
  enc <- asks encryption
  let mkey = case enc of
                  NoEncryption -> error "Encryption not enabled."
                  _            -> Just $ keyFile enc
  modifyM_ $ \st -> st
    { connectedClients =
        M.update (\cs -> Just cs { usesEncryption = mkey })
                 sid
                 (connectedClients st)
    }

--------------------------------------------------------------------------------
-- Sending messages

sendMsg :: (ReflectDescriptor msg, Wire msg) => ClientInfo -> msg -> MyCloud ()
sendMsg ci msg = do
  sendBS =<< encryptMaybe (messagePut msg)
 where
  sendBS bs = liftIO $ BL.hPut (clientHandle ci) bs
  encryptMaybe bs
    | Just kfile <- usesEncryption ci = do
      k <- liftIO $ BS.readFile kfile
      liftIO $ encryptMsg CBC k bs
    | otherwise =
      return bs

sendHelo :: SessionID -> Command -> Maybe String -> MyCloud ()
sendHelo sid c ma = do
  mci <- getSession sid
  case mci of
       Just ci -> sendMsg ci helo
       _       -> return () -- TODO: give feedback
 where
  helo = Helo (Just (fromIntegral $ unSID sid)) c (fmap fromString ma)
