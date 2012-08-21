{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImplicitParams #-}

module MyCloud.Connection
  ( --acceptConnections
  ) where

import Codec.Crypto.SimpleAES
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent.MState
import Data.Maybe
import Network
import System.IO
import Text.ProtocolBuffers

import qualified Data.Map             as M
import qualified Data.Sequence        as S
import qualified Data.Foldable        as F
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy    (ByteString)

import MyCloud.Internal.Types

import MyCloudProto.Message
import MyCloudProto.Message.Command
import MyCloudProto.FileInfo

{-

--------------------------------------------------------------------------------
-- Connection management

acceptConnections :: MyCloud ()
acceptConnections = do
  port_id <- MyCloud $ asks runOn
  s <- liftIO $ listenOn port_id
  forever $ liftIO (accept s) >>= fork . handleConnection
 where
  fork (MyCloud f) = MyCloud $ forkM f
  handleConnection (h,_hn,_pn) = do
    sid <- addSession h
    let ?sid = sid -- implicit parameter
    -- check for encryption
    enc <- MyCloud $ asks encryption
    sendHelo
    case enc of
         ForceAES _ -> do
           sendEnableEncrypt
           _ <- enableEncryption
           return ()
         _ -> return ()
    sendListen
    forever handleIncoming

--------------------------------------------------------------------------------
-- Session management

-}

addSession :: Handle -> MyCloud SessionID
addSession h = do
  MyCloud $ modifyM $ \st ->
    let clients     = connectedClients st
        sids        = M.keys $ connectedClients st
        sid         = SessionID $ maximum (map unSID sids) + 1
        client_info = ClientInfo { clientHandle   = h
                                 , usesEncryption = Nothing }
                                 --, commandContext = Nothing }
     in (sid, st { connectedClients = M.insert sid client_info clients })

getSession :: (?sid :: SessionID) => MyCloud (Maybe ClientInfo)
getSession = MyCloud $ gets (M.lookup ?sid . connectedClients)

requireSession :: (?sid :: SessionID) => MyCloud ClientInfo
requireSession = fromJust `liftM` getSession

enableEncryption :: (?sid :: SessionID) => MyCloud Bool
enableEncryption = do
  enc <- MyCloud $ asks encryption
  case enc of
       NoEncryption -> return False
       _            -> do
         let kfile = keyFile enc
         MyCloud $ modifyM_ $ \st -> st
           { connectedClients =
               M.update (\cs -> Just cs { usesEncryption = Just kfile })
                        ?sid
                        (connectedClients st)
           }
         return True

encryptMaybe :: (?sid :: SessionID) => BL.ByteString -> MyCloud BL.ByteString
encryptMaybe bs = do
  mci <- getSession
  encryptMaybe' mci
 where
  encryptMaybe' mci
    | Just ci <- mci
    , Just kfile <- usesEncryption ci = do
      k <- liftIO $ BS.readFile kfile
      liftIO $ encryptMsg CBC k bs
    | otherwise =
      return bs

decryptMaybe :: (?sid :: SessionID) => BL.ByteString -> MyCloud BL.ByteString
decryptMaybe bs = do
  mci <- getSession
  decryptMaybe' mci
 where
  decryptMaybe' mci
    | Just ci <- mci
    , Just kfile <- usesEncryption ci = do
      k <- liftIO $ BS.readFile kfile
      return $ decryptMsg CBC k bs
    | otherwise =
      return bs

{-
setCommandContext :: (?sid :: SessionID) => Maybe Command -> MyCloud Bool -- ^ success?
setCommandContext mcmd = MyCloud $
  modifyM $ \cs ->
    let m   = connectedClients cs
        m'  = M.adjust (\c -> c { commandContext = mcmd }) ?sid m
        suc = M.member ?sid m
     in (suc, cs { connectedClients = m' })
-}

--------------------------------------------------------------------------------
-- Sending messages

sendMsg :: (ReflectDescriptor msg, Wire msg, ?sid :: SessionID) => msg -> MyCloud ()
sendMsg msg = do
  mci <- getSession
  sendBS mci =<< encryptMaybe (messagePut msg)
 where
  sendBS (Just ci) bs = liftIO $ BL.hPut (clientHandle ci) bs
  sendBS _ _ = error "Error in `sendMsg': Invalid session"

sendCmd :: (?sid :: SessionID) => Command -> [String] -> Maybe ByteString -> MyCloud ()
sendCmd c args mdata = sendMsg $
   Message (fromIntegral $ unSID ?sid) c (S.fromList $ map fromString args) mdata

sendHelo, sendListen, sendSuccess, sendEnableEncrypt
  :: (?sid :: SessionID) => MyCloud ()
sendHelo          = sendCmd HELO [] Nothing
sendListen        = sendCmd LISTEN [] Nothing
sendEnableEncrypt = sendCmd ENABLE_ENCRYPT [] Nothing
sendSuccess       = sendCmd SUCCESS [] Nothing

type Reason = String

sendFailure :: (?sid :: SessionID) => Reason -> MyCloud ()
sendFailure reason = sendCmd FAILURE [reason] Nothing

--------------------------------------------------------------------------------
-- Incoming messages

handleIncoming :: (?sid :: SessionID) => MyCloud ()
handleIncoming = do
  bs <- receiveBS
  --handleIncomingMessage
  undefined

{-

handleIncomingMessage :: (?sid :: SessionID) => BL.ByteString -> MyCloud ()
handleIncomingMessage bs

  | Just ENABLE_ENCRYPT <- command
  = do
    enabled <- enableEncryption
    if enabled
       then sendSuccess
       else sendFailure "Encryption not supported"

  | Just AUTH       <- command
  , Just [_usr,_pw] <- args
  = sendFailure "Authentication not supported"

  | Just SEND_FILE <- command
  = sendListen >> receiveFile

  | otherwise
  = sendFailure "Cannot deserialize message"

 where
  message = messageGet' bs
  command :: Maybe Command
  command = fmap cmd message
  args :: Maybe [String]
  args = fmap (seqToList . arguments) message
   where
    seqToList = F.foldr (\s l -> toString s : l) []

-}

--
-- Helper
--

receiveBS :: (?sid :: SessionID)
          => MyCloud BL.ByteString
receiveBS = do
  ci <- requireSession
  bs <- liftIO $ BL.hGetContents (clientHandle ci)
  decryptMaybe bs

{-
messageGet' :: (ReflectDescriptor msg, Wire msg) => BL.ByteString -> Maybe msg
messageGet' bs
  | Right (msg,bs') <- messageGet bs
  , BL.null bs' = Just msg
  | otherwise = Nothing
-}

--------------------------------------------------------------------------------
-- File transfer

{-

receiveFile :: (?sid :: SessionID) => MyCloud ()
receiveFile = receiveBS >>= handleFiletransferMessages

handleFiletransferMessages :: (?sid :: SessionID) => BL.ByteString -> MyCloud ()
handleFiletransferMessages bs

  | Just _f@File{} <- message
  = sendFailure "File transfer not implemented yet"

  | otherwise
  = sendFailure "Cannot deserialize message"

 where
  message = messageGet' bs
-}
