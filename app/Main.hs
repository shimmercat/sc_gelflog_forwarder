{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric  #-}

module           Main where


import           System.IO
import           Data.Word                                               (Word16)
import           Data.Conduit
import qualified Control.Concurrent                                      as CC
import qualified Control.Exception                                       as E
import           Control.Concurrent.Chan
import qualified Data.Conduit.List                                       as DCL
import           Control.Lens
import           Control.Monad.IO.Class                                  (liftIO)
import qualified Data.ByteString                                         as B
import qualified Data.ByteString.Lazy                                    as LB
import qualified Data.ByteString.Builder                                 as Bu
import qualified Data.ByteString.Char8                                   as B8
import qualified Data.Serialize                                          as Ce
import           Data.Semigroup                                          ((<>))
import           Control.Applicative                                     ( (<|>), (<**>) )
import qualified Options.Applicative                                     as OA
import qualified Network.Socket                                          as NS
import qualified Network.Socket.ByteString                               as NSB
import qualified Database.Redis                                          as R
import qualified Data.Attoparsec.ByteString.Char8                        as ATO


import           Lib




-- | The main configuration data for the log forwarder... and from
--   there, forward all log messages.
data AppConfig = AppConfig {
    _redisConnectInfo_AC                 ::  R.ConnectInfo
  , _graylogHost_AC                      ::  NetworkAddressRepresentation
     } deriving Show


makeLenses ''AppConfig



appConfig :: OA.Parser AppConfig
appConfig = AppConfig <$>
    ( (representationToRedisConnectInfo . parseNetworkAddressRepresentation . B8.pack )  <$> OA.strOption (
         OA.long    "redis"
      <> OA.metavar "REDIS"
      <> OA.help    "Redis connection place"
        ))
    <*> ( (parseNetworkAddressRepresentation . B8.pack ) <$> OA.strOption (
         OA.long    "graylog"
      <> OA.metavar "GRAYLOG"
      <> OA.help    "Where graylog listens"
        ))


-- | Pumps messages out from Redis
subscribeLogPump :: R.ConnectInfo -> Source IO LogEntryWithWorker
subscribeLogPump conn_info =
  do
    ch <- liftIO $ do
        conn <-  R.connect conn_info
        let
            -- TODO: Add capability for specifying a namespace
            channel_aspect = "sc_log_" `mappend` "*"
            ps = R.psubscribe [channel_aspect]
        fetch_failed <- CC.newMVar False
        ch <- newChan

        -- Receive and queue the messages on an independen thread, so to
        -- not block the thread on UDP sends
        CC.forkIO $ E.onException (do
                R.runRedis conn . R.pubSub ps $ \ msg -> do
                    let
                        msgbs = case msg of
                            (R.Message _ m) -> m
                            (R.PMessage _ _ m) -> m
                        entry_wo = Ce.decode  msgbs :: Either String LogEntryWithWorker
                    case entry_wo of
                         Left _err -> do
                             -- Silently ignoring decoding errors
                             liftIO $ putStrLn "(log-decoding error ignored)"
                             return ()
                         Right wo -> do
                             writeChan ch wo
                    return mempty
            )
            (CC.modifyMVar_ fetch_failed $ \ _ -> return True )
        return ch

    -- Here, just read from the chan
    log_pump ch
  where
    log_pump :: Chan LogEntryWithWorker -> Source IO LogEntryWithWorker
    log_pump ch = do
        either_e_leww <- liftIO $ E.try (readChan ch)
        case either_e_leww :: Either E.BlockedIndefinitelyOnMVar LogEntryWithWorker  of
            Left e -> do
                liftIO $ hPutStr stderr "ConnectionToSourceOfMessagesWentDown"
                return ()
            Right leww -> do
                yield leww
                log_pump ch


-- | Pumps messages out as UDP datagrams
publishLogPump :: NS.Socket -> NS.SockAddr -> Sink B.ByteString IO ()
publishLogPump s addr = do
    maybe_data_to_send <- await
    case maybe_data_to_send of
        Nothing -> return ()
        Just datum -> do
           liftIO $ NSB.sendAllTo s datum addr
           publishLogPump s addr


translateMessage :: LogEntryWithWorker -> IO [B.ByteString]
translateMessage leww = do
     chm <- toChunkedUDPMessage leww
     return $ map LB.toStrict chm


-- | Does the actual work
transportMessages :: AppConfig -> IO ()
transportMessages app_config =
  do
    sock_addr <- representationToSocketAddress (app_config ^. graylogHost_AC )
    sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    (
        subscribeLogPump (app_config ^. redisConnectInfo_AC)
        =$=
        DCL.mapFoldableM translateMessage
        $$
        publishLogPump sock sock_addr )


main :: IO ()
main = transportMessages =<< OA.execParser opts
  where
    opts  = OA.info ( appConfig <**> OA.helper)
                 ( OA.fullDesc
                   <> OA.progDesc "Transports log messages from Redis to Graylog"
                   <> OA.header   "the-henrik: a program to copy ShimmerCat messages"
                 )
