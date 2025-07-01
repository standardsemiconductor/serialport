import Control.Concurrent.Async
import Control.Monad
import System.Hardware.Serialport
import System.Hardware.Serialport.Cli
import System.IO

main :: IO ()
main = do
  opts <- serialPortCli
  hWithSerial (serialPortPath opts) (serialPortSettings opts) com

com :: Handle -> IO ()
com hndl = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  concurrently_ readUart writeUart
  where
    readUart  = forever $ putChar =<< hGetChar hndl
    writeUart = forever $ hPutChar hndl =<< getChar
