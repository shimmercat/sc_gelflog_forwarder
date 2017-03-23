{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric  #-}

module TheHenrik.UDPPacker (
                 toChunkedUDPMessage
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


import           TheHenrik.LogEntry


toChunkedUDPMessage :: LogEntryWithWorker ->  [ LB.ByteString ]
toChunkedUDPMessage = error "NIY:toChunkedUDPMessage"
