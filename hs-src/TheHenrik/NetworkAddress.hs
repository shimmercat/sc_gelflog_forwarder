{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, DeriveGeneric  #-}
module TheHenrik.NetworkAddress (
                 NetworkAddressRepresentation                               (..)
               , parseNetworkAddressRepresentation
               , representationToSocketAddress
               , parseNetworkAddress
               , mustCreateSocket
               , representationToRedisConnectInfo
               , isZeroPort
               , socketAddressToFamily
       ) where


import           Control.Monad                                              (when)
import qualified Control.Exception                                          as E
import           GHC.Generics                                               (Generic)

import qualified Data.Serialize                                             as Cereal
import qualified Data.ByteString                                            as B
import           Data.ByteString.Char8                                      (
                                                                            unpack --,
                                                                            --pack
                                                                            )

import           Data.Word                                                  (Word8, Word16, Word32)
import           Data.Array                                                 ( (!) )
import qualified Data.Array                                                 as A

import qualified Text.Regex.TDFA.ByteString                                 as Rb
import qualified Text.Regex.Base.RegexLike                                  as Rl

-- import qualified Network.DNS.Lookup                                         as DL

import qualified Database.Redis                                             as Re
import qualified Network.Socket                                             as NS



-- import           Debug.Trace

-- | Used for textual representation of network addresses.
--   Also, sometimes a socket is inherited
--   and it should be used instead.
data NetworkAddressRepresentation =
     -- | An IpV4 address, always acceptable.
     IpV4_NAR !(Word8, Word8, Word8, Word8) !Word16
     -- | An unresolved host name, and a port. Always
     --   acceptable
   |  HostName_NAR !B.ByteString !Word16
   | UnixSocket_NAR !B.ByteString
   | SocketFD_NAR !Word16
     deriving (Eq,Generic, Ord)


instance Cereal.Serialize NetworkAddressRepresentation


mustCreateSocket :: NetworkAddressRepresentation -> Bool
mustCreateSocket (IpV4_NAR _ _) = True
mustCreateSocket (UnixSocket_NAR _) = True
mustCreateSocket (SocketFD_NAR _) = False



instance Show NetworkAddressRepresentation where
    show (IpV4_NAR (o1,o2,o3,o4) port) =
      (shows o1) . ('.':) . (shows o2). ('.':) . (shows o3) . ('.':)  . (shows o4) . (':':) . (shows port) $ ""

    show (HostName_NAR hn port) =
       "lookup(" `mappend` (show hn) `mappend` "):" `mappend` show port
    show (UnixSocket_NAR bs) =
      (showString "unix://") . (showString . unpack $ bs) $ ""
    show (SocketFD_NAR w16) =
      (showString "socket-fd://") . (showString . show $ w16) $ ""



-- | Used for some invalid email address I must trafique
isZeroPort :: NetworkAddressRepresentation -> Bool
isZeroPort (IpV4_NAR _ 0)  = True
isZeroPort _ = False


-- | Parses the following formats:
--
-- 127.0.0.1:4043
--
-- 4043 (localhost assumed)
--
-- lookup(some-host-name.look-at.me.com):3090
--
-- unix:///path/to/some/unix-socket
--
-- File descriptor to an inherited socket.
-- socket-fd://
parseNetworkAddressRepresentation :: B.ByteString -> NetworkAddressRepresentation
parseNetworkAddressRepresentation s =
  let
    subset (start,len) = B.take len . B.drop start $ s
    Right v1 = Rb.compile Rl.defaultCompOpt Rl.defaultExecOpt "^([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}):([0-9]+)$"
    Right v2 = Rb.compile Rl.defaultCompOpt Rl.defaultExecOpt "^([0-9]+)$"
    Right v2' = Rb.compile Rl.defaultCompOpt Rl.defaultExecOpt "^:([0-9]+)$"
    Right v3 = Rb.compile Rl.defaultCompOpt Rl.defaultExecOpt "^unix://(.+)$"
    Right v4 = Rb.compile Rl.defaultCompOpt Rl.defaultExecOpt "^socket-fd://([0-9]+)$"
    Right v5 = Rb.compile Rl.defaultCompOpt Rl.defaultExecOpt "^lookup\\(([a-z0-9-]+(\\.[a-z0-9-]+)*)\\):([0-9]+)"

    parse2 =
        case Rb.execute v2 s of
           Right (Just arr) ->
              let
                port :: Word16
                port = read . unpack . subset $ arr ! 1
              in IpV4_NAR (127,0,0,1) port

           _ -> parse2'

    parse2' =
        case Rb.execute v2' s of
           Right (Just arr) ->
              let
                port :: Word16
                port = read . unpack . subset $ arr ! 1
              in IpV4_NAR (127,0,0,1) port

           _ ->
              parse25

    parse25 =
        case Rb.execute v5 s of
           Right (Just arr) ->
              let
                (_first_group, last_group) = A.bounds arr
                port :: Word16
                port = read. unpack . subset $ arr ! last_group
                hostname :: B.ByteString
                hostname = subset $ arr ! 1
              in HostName_NAR hostname port

           -- Continues below, with the ifndef windows address parsing

           _ -> parse3

           _ -> error  $ "Network address  " ++ (unpack s) ++ " is at least win32invalid"

    parse3 =  case Rb.execute v3 s of
        Right (Just arr) ->
          let
            filename = subset $ arr ! 1
          in UnixSocket_NAR filename

        _ -> parse4

    parse4 = case Rb.execute v4 s of
        Right (Just arr) ->
          let
            socno = read . unpack . subset $ arr ! 1
          in
            SocketFD_NAR socno

        Left _ ->
          error $ "Unfortunate network address spec " ++ (unpack s)

        _ ->
          error $ "Unfortunate network address spec " ++ (unpack s)

  in {-# SCC parseNetAddress #-} case Rb.execute v1 s of
      Right (Just arr) ->
        let
            p1:: Word8
            p1 = read . unpack . subset $ arr ! 1
            p2:: Word8
            p2 = read . unpack . subset $ arr ! 2
            p3:: Word8
            p3 = read . unpack . subset $ arr ! 3
            p4:: Word8
            p4 = read . unpack . subset $ arr ! 4
            port:: Word16
            port = read . unpack . subset $ arr ! 5
          in IpV4_NAR (p1,p2,p3,p4) port

      _ ->
           parse2


representationToSocketAddress :: NetworkAddressRepresentation -> IO NS.SockAddr
representationToSocketAddress nar =
  let
       combineBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
       combineBytes o1 o2 o3 o4 = let
           oo1 :: Word32
           oo1 = fromIntegral o1
           oo2 :: Word32
           oo2 = fromIntegral o2
           oo3 :: Word32
           oo3 = fromIntegral o3
           oo4 :: Word32
           oo4 = fromIntegral o4
           in oo4 * 16777216 + oo3 * 65536 + oo2 * 256 + oo1
  in  case nar of
       IpV4_NAR (o1,o2,o3,o4) port  ->
           return $ NS.SockAddrInet (fromIntegral port) (combineBytes o1 o2 o3 o4)
       HostName_NAR hostname port -> do
           let
              hostname_str = unpack hostname
           either_addr_list  <- E.try $
               NS.getAddrInfo Nothing (Just hostname_str) Nothing

           case (either_addr_list :: Either IOError [NS.AddrInfo]) of

              Left _ -> error $ "Could not resolve address " ++ hostname_str
              Right addr_list -> do
                  when (length addr_list == 0 ) $ do
                      error $ "Could not resolve address " ++ hostname_str

                  let
                      addr_info0 : _ = addr_list
                  addr_info1  <- return $ addr_info0 {
                      NS.addrFamily = NS.AF_INET
                    , NS.addrSocketType = NS.Stream
                    , NS.addrAddress =
                        (\ (NS.SockAddrInet _ a) -> NS.SockAddrInet (fromIntegral port) a)
                            (NS.addrAddress addr_info0)
                      }
                  host_address <- return $ NS.addrAddress addr_info1
                  return host_address

       UnixSocket_NAR bs ->
           return $ NS.SockAddrUnix . unpack $ bs
       _ ->
           error "CantConvertThatToSocket"


socketAddressToFamily :: NS.SockAddr -> NS.Family
socketAddressToFamily (NS.SockAddrInet _ _) = NS.AF_INET
socketAddressToFamily (NS.SockAddrInet6 _ _ _ _) = NS.AF_INET6
socketAddressToFamily (NS.SockAddrUnix _) = NS.AF_UNIX
socketAddressToFamily (NS.SockAddrCan _) = error "SockAddrCan? What do you think you are doing?"


combineWord8ToStrIpAddr :: (Word8, Word8, Word8, Word8) -> String
combineWord8ToStrIpAddr (a,b,c,d) = (show a) ++ "." ++ (show b) ++ "." ++ (show c) ++ "." ++ (show d)


-- | All the steps into one
parseNetworkAddress :: B.ByteString ->  IO NS.SockAddr
parseNetworkAddress  = representationToSocketAddress .  parseNetworkAddressRepresentation


representationToRedisConnectInfo :: NetworkAddressRepresentation -> Re.ConnectInfo
representationToRedisConnectInfo _nar@(IpV4_NAR ip port) =  Re.defaultConnectInfo {
    Re.connectHost = combineWord8ToStrIpAddr ip,
    Re.connectPort = Re.PortNumber $ fromIntegral port
    }
representationToRedisConnectInfo (HostName_NAR hostname port) = Re.defaultConnectInfo    {
    Re.connectHost =unpack hostname,
    Re.connectPort = Re.PortNumber $  fromIntegral port
    }
representationToRedisConnectInfo (UnixSocket_NAR path) = Re.defaultConnectInfo {
    Re.connectHost = "",
    Re.connectPort = Re.UnixSocket $ unpack path
    }
representationToRedisConnectInfo _ = error "Can'tTranslateForRedis"
