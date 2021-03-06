{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Messages sent to and from the Rendezvous server.
module MagicWormhole.Internal.Messages
  ( ClientMessage(..)
  , ServerMessage(..)
  , AppID(..)
  , MailboxMessage(..)
  , WelcomeMessage(..)
  , MessageID(..)
  , Side(..)
  , generateSide
  , Phase(..)
  , phaseName
  , Body(..)
  , Nameplate(..)
  , Mailbox(..)
  , Mood(..)
  ) where

import Protolude

import Control.Monad (fail)
import Crypto.Random (MonadRandom(..))
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(Object, String)
  , (.:)
  , (.:?)
  , (.=)
  , object
  )
import Data.Aeson.Types (Pair, typeMismatch)
import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base16))
import Numeric (readHex, showHex)


-- | A message received from the server.
--
-- Some open questions:
-- * general message stuff--how are we going to model this?
--   * outgoing messages include a randomly generated 'id' field, which is
--     returned by the server
--   * messages from the server include 'server_tx', a float timestamp recording
--     when the server received the message
--   * messages from the server that are direct responses include a 'server_rx'
--     timestamp--unclear what this means?
--   * do we want a separate Haskell type for each message type? e.g. PingMessage
--   * if we do that, how do associate request/response pairs? e.g. PingMessage &
--     PongMessage?
--   * do we want to (can we even?) structurally distinguish between messages that
--     make sense outside the scope of a binding (e.g. ping) and messages that only
--     make sense after a bind (e.g. allocate)
data ServerMessage
  = -- | Sent by the server on initial connection.
    Welcome WelcomeMessage
  | -- | Sent in response to "list"
    Nameplates
    {
      nameplates :: [Nameplate]
    }
  | -- | Sent in response to "allocate"
    Allocated
    {
      -- | The nameplate allocated to this connection (?).
      nameplate :: Nameplate
    }
  | -- | Sent in response to "claim"
    Claimed
    { -- | The mailbox claimed by this connection (?)
      mailbox :: Mailbox
    }
  | -- | Sent in response to "release"
    Released
  | -- | A message sent to the mailbox
    Message MailboxMessage
  | -- | Sent in response to "close"
    Closed
  | -- | Sent immediately after every message. Unused.
    Ack
  | -- | Sent in response to "pong"
    Pong Int
  | -- | Sent by the server when it receives something from the client that it does not understand.
    Error
    { -- | Message explaining what the problem is
      errorMessage :: Text
      -- | The message that caused the problem.
    , original :: ClientMessage
    }
  deriving (Eq, Show)

instance FromJSON ServerMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "welcome" -> do
        welcome <- v .: "welcome"
        Welcome <$> (WelcomeMessage <$> welcome .:? "motd" <*> welcome .:? "error")
      "nameplates" -> do
        ns <- v .: "nameplates"
        Nameplates <$> sequence [ Nameplate <$> n .: "id" | n <- ns ]
      "allocated" -> Allocated <$> v .: "nameplate"
      "claimed" -> Claimed <$> v .: "mailbox"
      "released" -> pure Released
      "message" -> Message <$> (MailboxMessage <$> v .: "side" <*> v .: "phase" <*> v .:? "id" <*> v .: "body")
      "closed" -> pure Closed
      "ack" -> pure Ack
      "pong" -> Pong <$> v .: "pong"
      "error" -> Error <$> v .: "error" <*> v .: "orig"
      _ -> fail $ "Unrecognized wormhole message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ServerMessage where
  toJSON (Welcome (WelcomeMessage motd' error')) =
    objectWithType "welcome"
    [ "welcome" .= object (catMaybes [ ("motd" .=) <$> motd'
                                     , ("error" .=) <$> error'
                                     ])
    ]
  toJSON (Nameplates nameplates') =
    objectWithType "nameplates" ["nameplates" .= [ object ["id" .= n] | n <- nameplates' ] ]
  toJSON (Allocated nameplate') =
    objectWithType "allocated" [ "nameplate" .= nameplate' ]
  toJSON (Claimed mailbox') =
    objectWithType "claimed" [ "mailbox" .= mailbox' ]
  toJSON Released = objectWithType "released" []
  toJSON (Message (MailboxMessage side' phase' id body')) =
    objectWithType "message"
    [ "phase" .= phase'
    , "side" .= side'
    , "body" .= body'
    , "id" .= id
    ]
  toJSON Closed = objectWithType "closed" []
  toJSON Ack = objectWithType "ack" []
  toJSON (Pong n) = objectWithType "pong" ["pong" .= n]
  toJSON (Error errorMsg orig) =
    objectWithType "error" [ "error" .= errorMsg
                           , "orig" .= orig
                           ]

-- | Create a JSON object with a "type" field.
--
-- Use this to construct objects for client and server messages.
objectWithType :: Text -> [Pair] -> Value
objectWithType typ pairs = object $ ("type" .= typ):pairs

-- | Identifier for a "nameplate".
--
-- A nameplate is a very short string that identifies one peer to another. Its
-- purpose is to allow peers to find each other without having to communicate
-- the 'Mailbox' identifier, which is generally too lengthy and cumbersome to
-- be easily shared between humans.
--
-- Typically, one peer will allocate a nameplate and then communicate it
-- out-of-band to the other peer.
newtype Nameplate = Nameplate Text deriving (Eq, Show, ToJSON, FromJSON)

-- | A phase in the peer-to-peer (aka "client") protocol.
--
-- Phases proceed in strict order: 'PakePhase', 'VersionPhase', then
-- many 'ApplicationPhase'.
data Phase
  = -- | Sent immediately on opening the mailbox.
    PakePhase
  | -- | Used to negotiate capabilities.
    VersionPhase
    -- | Reserved for application data. Messages with these phases will be
    -- delivered in numeric order.
  | ApplicationPhase Int
  deriving (Eq, Show)

-- | Get the name of the phase. Used to derive message keys.
phaseName :: Phase -> Text
phaseName PakePhase = "pake"
phaseName VersionPhase = "version"
phaseName (ApplicationPhase n) = show n
-- TODO: Add test to ensure this can be cleanly encoded & decoded to ASCII.

instance ToJSON Phase where
  toJSON = toJSON . phaseName

instance FromJSON Phase where
  parseJSON (String "pake") = pure PakePhase
  parseJSON (String "version") = pure VersionPhase
  parseJSON (String number) =
    let number' = toS number in
    case readMaybe number' of
      Just n -> pure (ApplicationPhase n)
      Nothing -> fail $ "Unrecognized phase: " <> number'
  parseJSON other = typeMismatch "Phase" other

-- | Identifier for a mailbox.
--
-- A mailbox is a shared access point between Magic Wormhole peers within the
-- same application (specified by 'AppID'). To get a mailbox, you must first
-- acquire a 'Nameplate' and then claim that nameplate for your side with
-- 'MagicWormhole.claim'.
--
-- A mailbox ID is defined in the
-- [spec](https://github.com/warner/magic-wormhole/blob/master/docs/server-protocol.md)
-- as a "large random string", but in practice is a 13 character, lower-case,
-- alpha-numeric string.
newtype Mailbox = Mailbox Text deriving (Eq, Show, ToJSON, FromJSON)

-- | The body of a Magic Wormhole message.
--
-- This can be any arbitrary bytestring that is sent to or received from a
-- wormhole peer.
newtype Body = Body ByteString deriving (Eq, Show)

instance ToJSON Body where
  toJSON (Body bytes) = toJSON (toS @ByteString @Text (convertToBase Base16 bytes))

instance FromJSON Body where
  parseJSON (String s) = either fail (pure . Body) (convertFromBase Base16 (toS @Text @ByteString s))
  parseJSON x = typeMismatch "Body" x

-- | A message sent from a rendezvous client to the server.
data ClientMessage
  = -- | Set the application ID and the "side" for the duration of the connection.
    Bind AppID Side
    -- | Get a list of all allocated nameplates.
  | List
    -- | Ask the server to allocate a nameplate
  | Allocate
    -- | Claim a nameplate.
  | Claim Nameplate
    -- | Release a claimed nameplate.
    --
    -- If no nameplate is provided, the server will attempt to release the
    -- nameplate claimed (via 'Claim') earlier in this connection.
  | Release (Maybe Nameplate)
    -- | Open a mailbox.
  | Open Mailbox
    -- | Send a message to an open mailbox. The message will be delivered to
    -- all connected clients that also have that mailbox open, including this
    -- one.
  | Add Phase Body
    -- | Close a mailbox. Since only one mailbox can be open at a time, if
    -- mailbox isn't specified, then close the open mailbox.
  | Close (Maybe Mailbox) (Maybe Mood)
    -- | Internal "ping". Response is 'Pong'. Used for testing.
  | Ping Int
  deriving (Eq, Show)

instance FromJSON ClientMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "bind" -> Bind <$> v .: "appid" <*> v .: "side"
      "list" -> pure List
      "allocate" -> pure Allocate
      "claim" -> Claim <$> v .: "nameplate"
      "release" -> Release <$> v .:? "nameplate"
      "open" -> Open <$> v .: "mailbox"
      "add" -> Add <$> v .: "phase" <*> v .: "body"
      "close" -> Close <$> v .:? "mailbox" <*> v .:? "mood"
      "ping" -> Ping <$> v .: "ping"
      _ -> fail $ "Unrecognized rendezvous client message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ClientMessage where
  toJSON (Bind appID side') =
    objectWithType "bind"  [ "appid" .= appID
                           , "side" .= side'
                           ]
  toJSON List = objectWithType "list" []
  toJSON Allocate = objectWithType "allocate" []
  toJSON (Claim nameplate') = objectWithType "claim" [ "nameplate" .= nameplate' ]
  toJSON (Release nameplate') =
    objectWithType "release" $ case nameplate' of
                                 Nothing -> []
                                 Just n -> ["nameplate" .= n]
  toJSON (Open mailbox') = objectWithType "open" [ "mailbox" .= mailbox' ]
  toJSON (Add phase' body') = objectWithType "add"
    [ "phase" .= phase'
    , "body" .= body'
    ]
  toJSON (Close mailbox' mood') =
    objectWithType "close" $ catMaybes [ ("mailbox" .=) <$> mailbox'
                                       , ("mood" .=) <$> mood'
                                       ]
  toJSON (Ping n) = objectWithType "ping" [ "ping" .= n]

-- | Short string to identify the application. Clients must use the same
-- application ID if they wish to communicate with each other.
--
-- Recommendation is to use "$DNSNAME/$APPNAME", e.g.
-- the Python `wormhole` command-line tool uses
-- @lothar.com\/wormhole\/text-or-file-xfer@.
newtype AppID = AppID Text deriving (Eq, Show, FromJSON, ToJSON)

-- | Short string used to differentiate between echoes of our own messages and
-- real messages from other clients.
--
-- TODO: This needs to be cleanly encoded to ASCII, so update the type or
-- provide a smart constructor.
newtype Side = Side Text deriving (Eq, Show, FromJSON, ToJSON)

-- | Generate a random 'Side'
generateSide :: MonadRandom randomly => randomly Side
generateSide = do
  randomBytes <- getRandomBytes 5
  pure . Side . toS @ByteString . convertToBase Base16 $ (randomBytes :: ByteString)

-- | How the client feels. Reported by the client to the server at the end of
-- a wormhole session.
data Mood
  = -- | The client had a great session with its peer.
    Happy
    -- | The client never saw its peer.
  | Lonely
    -- | The client saw a peer it could not trust.
  | Scary
    -- | The client encountered some problem.
  | Errory deriving (Eq, Show)

instance ToJSON Mood where
  toJSON Happy = "happy"
  toJSON Lonely = "lonely"
  toJSON Scary = "scary"
  toJSON Errory = "errory"

instance FromJSON Mood where
  parseJSON (String s) =
    case s of
      "happy" -> pure Happy
      "lonely" -> pure Lonely
      "scary" -> pure Scary
      "errory" -> pure Errory
      _ -> fail $ "Unrecognized mood: " <> toS s
  parseJSON unknown = typeMismatch "Mood" unknown

-- | Identifier sent with every client message that is included in the
-- matching server responses.
newtype MessageID = MessageID Int16 deriving (Eq, Show, Hashable)

instance ToJSON MessageID where
  toJSON (MessageID n) = toJSON $ showHex n ""

instance FromJSON MessageID where
  parseJSON (String s) =
    case readHex (toS s) of
      [(n, _)] -> pure (MessageID n)
      _ -> fail $ "Could not parse MessageID: " <> toS s
  parseJSON unknown = typeMismatch "MessageID" unknown

-- XXX: It's possible we want another type that represents an *unsent*
-- message, which will not have a side and will not have message ID. We
-- currently do this using (Phase, Body) tuples.
-- | A message sent to a mailbox.
data MailboxMessage
  = MailboxMessage
    {
      -- | Which side sent the message. Might be our side.
      side :: Side
    , -- | Which phase of the client protocol we are in.
      phase :: Phase
      -- | An identifier for the message. Unused.
      --
      -- According to the protocol docs, this should always be set, but the
      -- Python server will happily mirror an absent 'id' field as 'null'.
    , messageID :: Maybe MessageID
    , -- | The body of the message. To be interpreted by the client protocol.
      body :: Body
    } deriving (Eq, Show)

-- | Message received on initial connection to the server.
data WelcomeMessage
  = WelcomeMessage
    { -- | A message to be displayed to users when they connect to the server
      motd :: Maybe Text
      -- | If present, the server does not want the client to proceed. Here's the reason why.
    , welcomeErrorMessage :: Maybe Text
    } deriving (Eq, Show)
