{-# LANGUAGE CPP #-}

-- |This module provides the serial port interface.
--
-- > import qualified Data.ByteString.Char8 as B
-- > import System.Hardware.Serialport
-- > let port = "COM3"          -- Windows
-- > let port = "/dev/ttyUSB0"  -- Linux
-- > withSerial port defaultSerialSettings{ commSpeed = CS2400 } $ \s -> do
-- >   send s $ B.pack "AT\r"
-- >   recv s 10 >>= print
--
-- Alternatively, use handles to perform IO:
--
-- > import System.IO
-- > import System.Hardware.Serialport
-- > let port = "COM3"           -- Windows
-- > let port = "/dev/ttyUSB0"   -- Linux
-- > hWithSerial port defaultSerialSettings $ \h -> do
-- >   hPutStr h "AT\r"
-- >   hGetLine h >>= print

module System.Hardware.Serialport (
  -- * Types
   CommSpeed(..)
  ,StopBits(..)
  ,Parity(..)
  ,FlowControl(..)
  ,SerialPort
  -- * Configure port
  -- | You don't need the get or set functions, they are used by openSerial
  ,SerialPortSettings(..)
  ,defaultSerialSettings
  ,setSerialSettings
  ,getSerialSettings
  -- * Serial methods
  -- ** Device
  ,hOpenSerial
  ,openSerial
  ,closeSerial
  ,withSerial
  ,hWithSerial
  -- ** Sending & receiving
  ,send
  ,recv
  ,flush
  -- ** Line control
  ,setDTR
  ,setRTS
  ) where

#if defined(mingw32_HOST_OS)
import System.Hardware.Serialport.Windows
#else
import System.Hardware.Serialport.Posix
#endif
import System.Hardware.Serialport.Types

import System.IO (Handle, hClose)
import qualified Control.Exception as Ex


-- |Safer device function, so you don't forget to close the device
withSerial :: FilePath -> SerialPortSettings -> ( SerialPort -> IO a ) -> IO a
withSerial dev settings = Ex.bracket (openSerial dev settings) closeSerial

-- |Like `withSerial` but using `Handle`
hWithSerial :: FilePath -> SerialPortSettings -> (Handle -> IO a) -> IO a
hWithSerial dev settings = Ex.bracket (hOpenSerial dev settings) hClose
