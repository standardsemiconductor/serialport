# Serialport

[![Haskell CI](https://github.com/standardsemiconductor/serialport/actions/workflows/haskell.yml/badge.svg)](https://github.com/standardsemiconductor/serialport/actions/workflows/haskell.yml)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

Cross platform (Linux, Windows and Mac OS) [serial port](https://en.wikipedia.org/wiki/Serial_port) interface.

## Sample Usage

```haskell
import System.IO
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B

let port = "COM3"         -- Windows
let port = "/dev/ttyUSB0" -- Linux
withSerial port defaultSerialSettings $ \s -> do
  send s $ B.pack "AT\r"
  recv s 10 >>= print
```

[Concurrently](https://hackage.haskell.org/package/async) read and write a serial port at 19200 [baud](https://learn.sparkfun.com/tutorials/serial-communication/rules-of-serial) using `hWithSerial`:
```haskell
import Control.Concurrent.Async ( concurrently_ )
import Control.Monad            ( forever )
import System.Hardware.Serialport
import System.IO

com :: String -> IO ()
com portPath = hWithSerial portPath serialPortSettings $ \hndl -> do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  concurrently_ (readUart hndl) (writeUart hndl)
    where
      readUart  hndl = forever $ putChar =<< hGetChar hndl
      writeUart hndl = forever $ hPutChar hndl =<< getChar

serialPortSettings :: SerialPortSettings
serialPortSettings = defaultSerialSettings{ commSpeed = CS19200 }
```

## Tests

### Setup
* [Arduino Leonardo](http://arduino.cc/en/Main/arduinoBoardLeonardo) + [Sparkfun FTDI breakout board](https://www.sparkfun.com/products/718).
* Connections: TX, RX and GND

### Prepare Arduino
* Upload arduino code using Arduino IDE or avrdude

### Prepare haskell test program
* Configure cabal to build the tests: `cabal configure --enable-tests`.
* Build: `cabal build`

### Running the tests
* Run the tests: `cabal test --test-options="/dev/ttyACM0 /dev/ttyUSB0"`

[hackage]:            <https://hackage.haskell.org/package/serialport>
[hackage-badge]:      <https://img.shields.io/hackage/v/serialport.svg?color=success>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/serialport.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=serialport>
