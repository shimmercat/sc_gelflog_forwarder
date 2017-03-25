{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, ScopedTypeVariables  #-}

module TheHenrik.UDPPacker (
                 toChunkedUDPMessage
               , messageHeader   -- Exporting so that I can test it
               , computeMessageId
               ) where



import           Control.Lens
import qualified Control.Exception                                       as E
import           Control.Monad                                           (when,
                                                                          )
import           Control.Monad.IO.Class                                   (liftIO)
import           Control.Concurrent.Chan
import qualified Control.Concurrent                                      as CC(yield)
import qualified System.Random                                           as SR
import           System.IO
-- import           System.Console.ANSI
import           System.IO.Unsafe
import           Data.Ratio                                              ((%))
import qualified Data.ByteString                                         as B
import qualified Data.ByteString.Lazy                                    as LB
import qualified Data.ByteString.Builder                                 as Bu
import qualified Data.ByteString.Char8                                   as B8
import qualified Data.Vector                                             as DVec
import qualified Data.Serialize                                          as Ce
import           Data.Word
import           Data.List                                               (intersperse)
import qualified Data.Text                                               as T
import qualified Data.Text.Encoding                                      as DTE
import qualified Data.IntSet                                             as Ds
import           Data.Time.Clock
import           Data.Time.Clock.POSIX                                   (utcTimeToPOSIXSeconds)
import           Data.Time.Format
import           Data.Time.Calendar                                      (Day(..))
import           Data.IORef
import qualified Codec.Compression.GZip                                  as Gz
import           GHC.Generics
import           Data.FileEmbed

import qualified Data.Aeson.Types                                        as Ae
import qualified Data.Aeson                                              as Ae
import qualified Data.Yaml                                               as Y
import qualified Data.HashMap.Lazy                                       as HM
import qualified Crypto.Hash.BLAKE2.BLAKE2b                              as Blake

import           TheHenrik.LogEntry



messageCodes_BS :: B.ByteString
messageCodes_BS = $(embedFile "message_codes.yaml")


messageCodes_Table :: HM.HashMap Word16 T.Text
messageCodes_Table = let
   Just (Y.Array r) = (Y.decode messageCodes_BS) :: Maybe Ae.Value
   code2format = HM.fromList . DVec.toList . DVec.map (\ (Y.Object entry) ->
                                        ((
                                          let
                                              Ae.Number sci_code = entry HM.! "codeCI"
                                          in truncate sci_code
                                        ),
                                        ( let
                                             Ae.String t = entry HM.! "formatCI"
                                          in t
                                        ))
                                   ) $ r
 in code2format


shortMessage :: Word16 -> T.Text
shortMessage code = T.pack $ "M" ++ show code


fullMessage :: Word16 -> T.Text
fullMessage code = messageCodes_Table HM.! code


maybeRead :: Read a => String -> Maybe a
maybeRead s =
    case readsPrec 0 s of
      [(a,_)] -> Just a
      _ -> Nothing


additionalFields :: Word16 -> [Bu.Builder] -> [(T.Text, Ae.Value)]
additionalFields code builders = let
    field_name  i = T.pack $ "_M" ++ show code ++ "_" ++ show i
    encode_value :: B.ByteString -> Ae.Value
    encode_value s
       | (Just real_num::Maybe Double ) <- maybeRead . B8.unpack $ s
                             =  Ae.Number . realToFrac $ real_num
       | otherwise
                             =  Ae.String . DTE.decodeUtf8 $ s 


  in
    [
      (field_name i, encode_value  . LB.toStrict . Bu.toLazyByteString $ bu ) |
         (i, bu) <- zip [0..] builders
    ]

-- def modified_julian_time_to_unix_timestamp(mjd_seconds, mjd_microseconds):
--     s = 24*3600
--     tau = 2440587.5
--     sigma1 = 2400000.5
--     u = (mjd_seconds // s + sigma1 - tau) * s + mjd_seconds % s

--     return u + mjd_microseconds / 1000000.0



translateLogEntryToJSON :: LogEntryWithWorker -> Ae.Value
translateLogEntryToJSON leww = Ae.Object . HM.fromList $ [
    ("version", Ae.String "1.1"),
    ("host", Ae.String . DTE.decodeUtf8 $ leww ^. worker_LO),
    ("short_message", Ae.String . shortMessage $ message_code ),
    ("full_message", Ae.String . fullMessage $ message_code ),
    ("timestamp", Ae.Number . realToFrac . utcTimeToPOSIXSeconds $ leww ^. logEntry_LO . when_LE )
    ] ++ additionalFields message_code (leww ^. logEntry_LO . messagePieces_LE )
  where
    message_code = leww ^. logEntry_LO . messageCode_LE


-- | The name is not very accurate: this function may or may not create
--   a chunked version of the message., depending on the length of the message
toChunkedUDPMessage :: LogEntryWithWorker ->  IO [ LB.ByteString ]
toChunkedUDPMessage leww =
  let
    unchunked_message = gzipEncode . translateLogEntryToJSON $ leww
  in if LB.length unchunked_message < 8192
    then
        return [unchunked_message]
    else do
        splitMessageIntoChunks unchunked_message


gzipEncode :: Ae.Value -> LB.ByteString
gzipEncode  = Gz.compress . Ae.encode


-- | We need 12 bytes for the header.
chunkPayloadSize :: Num a => a
chunkPayloadSize = 8192 - 12


-- | But when a chunked version is needed, this function is in charge of splitting
--   the original message into chunks and adding a header.
--
-- It uses IO because it needs a unique identifier for the message, and this identifier
--  should not repeat across messages with similar contents. So we just pull out a value
--  from /dev/urandom
splitMessageIntoChunks :: LB.ByteString -> IO [LB.ByteString]
splitMessageIntoChunks message = do
    message_id <- computeMessageId message
    return [
       (messageHeader message_id chunk_no chunk_count) `mappend` chunk |
           (chunk_no, chunk) <- zip [0..] chunks
       ]
 where
    chunk_count = fromIntegral (LB.length message `div` chunkPayloadSize + 1)
    chunks =
        map fst $
        tail $
        takeWhile (\ (last, rest) -> not $ rest == "" && last == "" ) $
        iterate (\ (last, rest) -> LB.splitAt chunkPayloadSize rest) ("", message)


-- | Computes the message ID as a combination of a hash and a couple of random
--   numbers.
computeMessageId :: LB.ByteString -> IO B.ByteString
computeMessageId message = do
    let
        hash_part = Blake.hash 6 mempty (LB.toStrict message)
    random_part <- withBinaryFile "/dev/urandom" ReadMode $ \ hnd ->
        B.hGet hnd 2
    return (hash_part `mappend` random_part)


messageHeader :: B.ByteString -> Word8 -> Word8 -> LB.ByteString
messageHeader message_id chunk_no chunk_count =
    Ce.runPutLazy $ do
        Ce.putWord8 0x1e
        Ce.putWord8 0x0f
        Ce.putByteString message_id
        Ce.putWord8 chunk_no
        Ce.putWord8 chunk_count
