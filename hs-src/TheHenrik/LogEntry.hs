{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric  #-}

module TheHenrik.LogEntry (

                 LogEntryWithWorker                                      (..)
               , worker_LO
               , logEntry_LO

               , LogEntry                                                (..)
               , when_LE
               , severity_LE
               , messageCode_LE
               , messagePieces_LE
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
import qualified Data.IntSet                                             as Ds
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Calendar                                      (Day(..))
import           Data.IORef
import           GHC.Generics

import qualified Database.Redis                                          as DaR


-- | Arbitrary base date.
baseDate :: UTCTime
baseDate = UTCTime (ModifiedJulianDay 0) 0



data LogEntrySeverity =
    Debug_LES
    | Banner_LES    -- TODO: this should only be emitted when logging to interactive consoles and
                    --       in development mode
    | Character_LES
    | Boring_LES
    | Attention_LES
    | Info_LES
    | Error_LES
    deriving (Eq, Enum, Ord)



data LogEntry = LogEntry {
    _when_LE          :: ! UTCTime
  , _severity_LE      :: ! LogEntrySeverity
  , _messageCode_LE   :: ! Word16
  , _messagePieces_LE :: [Bu.Builder]
  }


makeLenses ''LogEntry

instance Ce.Serialize LogEntry where

    put entry =
      do
        let
            -- message = entry ^. message_LE
            dt = (entry ^. when_LE) `diffUTCTime` baseDate
            rat = toRational dt

            message_code = entry ^. messageCode_LE

            pieces = entry ^. messagePieces_LE

            seconds_from_base_date :: Word64
            (seconds_from_base_date, proper) = properFraction rat

            microseconds_from_second :: Word32
            microseconds_from_second = truncate (proper * 1000000)

            sev_code = fromIntegral . fromEnum $ entry ^. severity_LE :: Word8

            -- Create a message with intersperses ceros from the builder
            data_in_message =
                Bu.toLazyByteString . mconcat $ intersperse (Bu.word8 0) pieces

        -- Not serialized here, but in LogEntryWithOrigin: a further 8-bytes prefix that
        -- uniquely identifies the origin node

        -- 8 bytes with the number of seconds
        -- 4 bytes with the number of microseconds
        -- 1 byte with the severity code
        -- 2 bytes with the code of the message
        -- 8 bytes with the total length of the data (separated by zeros)
        Ce.putWord64le seconds_from_base_date
        Ce.putWord32le microseconds_from_second
        Ce.putWord8 sev_code
        Ce.putWord16le message_code
        Ce.putWord32le . fromIntegral . LB.length $ data_in_message

        -- Put the rest
        Ce.putLazyByteString data_in_message

    get =
      do
        seconds_from_base_date   <- Ce.getWord64le
        microseconds_from_second <- Ce.getWord32le
        sev_code <- Ce.getWord8
        message_code <- Ce.getWord16le
        length_data <- Ce.getWord32le

        -- Get the data in the message
        data_in_message <- Ce.getLazyByteString $ fromIntegral length_data
        let
            data_fragments = map Bu.lazyByteString $ LB.split 0 data_in_message

            rat = fromIntegral seconds_from_base_date % 1 +
                  fromIntegral microseconds_from_second % 1000000

            dt = fromRational rat :: NominalDiffTime
            when_happened = dt `addUTCTime` baseDate
            severity = toEnum . fromIntegral $ sev_code :: LogEntrySeverity

        return LogEntry {
            _when_LE = when_happened,
            _severity_LE = severity,
            _messageCode_LE = message_code,
            _messagePieces_LE =  data_fragments
            }


-- | This is the entry that we read from  Redis.
data LogEntryWithWorker = LogEntryWithWorker {
     -- | The worker name: exactly 8 bytes long.
      _worker_LO      :: B.ByteString
     -- | The rest of the log entry.
    , _logEntry_LO    :: LogEntry
    }
    deriving (Generic)

makeLenses ''LogEntryWithWorker


instance Ce.Serialize LogEntryWithWorker where

    put (LogEntryWithWorker worker entry) = do
        Ce.putByteString worker
        Ce.put entry

    get = do
        worker <- Ce.getBytes 8
        entry <- Ce.get

        return $ LogEntryWithWorker worker entry
