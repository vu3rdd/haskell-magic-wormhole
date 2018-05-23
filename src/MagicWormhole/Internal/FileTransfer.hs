{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Description : File transfer and simple text message protocol
--
-- Partial implementation of the [Magic Wormhole file transfer protocol](https://github.com/warner/magic-wormhole/blob/master/docs/file-transfer-protocol.md).
--
-- Once a connection has been made between peers (see 'MagicWormhole.withEncryptedConnection'),
-- you can send an 'Offer' to share a simple text message.
module MagicWormhole.Internal.FileTransfer
  ( Offer(..)
  ) where

import Protolude

import GHC.Generics
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , (.=)
  , object
  , withObject
  , withScientific
  , Value(..)
  )
import Data.Aeson.Types

import Data.Scientific
  ( coefficient
  )

import Network.Socket
  ( PortNumber
  , HostAddress
  , Socket( MkSocket )
  , socket
  , bind
  , socketPort
  , close
  , Family( AF_INET )
  , SocketType( Stream )
  , defaultProtocol
  , setSocketOption
  , SocketOption( ReuseAddr )
  , SockAddr( SockAddrInet )
  )

import Network.Info
  ( getNetworkInterfaces
  , NetworkInterface(..)
  , IPv4 (..)
  )

import System.Posix.Types (FileOffset)

-- | An offer made by a sender as part of the Magic Wormhole file transfer protocol.
--
-- Currently only supports sending simple text messages. A full version would
-- also support sending files and directories.
data Offer
  -- | A simple text message.
  = Message Text
  | File Text FileOffset
  deriving (Eq, Show)

instance ToJSON Offer where
  toJSON (Message text) = object [ "offer" .= object [ "message" .= text ] ]
  toJSON (File name size) = object [ "offer" .= object [ "file" .= object [ "filename" .= name, "filesize" .= (fromEnum size) ] ] ]

instance FromJSON Offer where
  parseJSON = withObject "Offer" $ \obj -> do
    offer <- obj .: "offer"
    asum [ Message <$> offer .: "message"
         , do
             offerObj <- obj .: "offer"
             fileObj <- offerObj .: "file"
             File <$> fileObj .: "filename" <*> (toEnum <$> (fileObj .: "filesize"))
         ]

data DirectTCPV1Hint = DirectTCPV1Hint { hostname :: HostAddress
                                       , port :: PortNumber
                                       , priority :: Int
                                       } deriving (Show, Eq, Generic)

instance ToJSON PortNumber where
  toJSON n = toJSON $ toInteger n

instance FromJSON PortNumber where
  parseJSON = withScientific "PortNumber" (return . fromInteger . coefficient)

instance ToJSON DirectTCPV1Hint where
  toJSON = genericToJSON defaultOptions { sumEncoding = TaggedObject { tagFieldName = "type"}, constructorTagModifier = camelTo2 '_' }

instance FromJSON DirectTCPV1Hint

data TorTCPV1Hint = TorTCPV1Hint { hostname :: HostAddress
                                 , port :: PortNumber
                                 , priority :: Int }

data RelayV1Hint = RelayV1Hint { hints :: (DirectTCPV1Hint, TorTCPV1Hint) }

allocateTcpPort :: IO PortNumber
allocateTcpPort = do
  s@(MkSocket fd fam stype _ _) <- socket AF_INET Stream defaultProtocol
  r <- setSocketOption s ReuseAddr 1
  _ <- bind s (SockAddrInet 0 0x0100007f) -- 127.0.0.1
  port <- socketPort s
  close s
  return port

build_direct_hints :: IO [DirectTCPV1Hint]
build_direct_hints = do
  portnum <- allocateTcpPort
  nwInterfaces <- getNetworkInterfaces
  let nonLoopbackInterfaces =
        filter (\nwInterface -> let (IPv4 addr4) = ipv4 nwInterface in addr4 /= 0x0100007f) nwInterfaces
  return $ map (\nwInterface ->
                  let (IPv4 addr4) = ipv4 nwInterface in
                  DirectTCPV1Hint { hostname = addr4
                                  , port = portnum
                                  , priority = 0 }) nonLoopbackInterfaces


-- TODO: not implemented yet
build_relay_hints :: IO [RelayV1Hint]
build_relay_hints = return []


