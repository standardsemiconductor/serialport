import Control.Concurrent.Async
import Control.Monad
import Data.Word
import Options.Applicative
import System.Hardware.Serialport
import System.IO

main :: IO ()
main = do
  opts <- cli
  hWithSerial (path opts) (settings opts) com

data Cli = Cli
  { path     :: FilePath
  , settings :: SerialPortSettings
  }

cli :: IO Cli
cli = customExecParser cliPrefs $ info (cliParser <**> helper) mempty

cliPrefs :: ParserPrefs
cliPrefs = prefs $ showHelpOnError <> showHelpOnEmpty


cliParser :: Parser Cli
cliParser = Cli <$> parsePath
                <*> (parseSettings <**> options)

parsePath :: Parser String
parsePath = strArgument $ mconcat
  [ help "serial port file path"
  , metavar "<tty-device>"
  ]

parseSettings :: Parser SerialPortSettings
parseSettings =
  SerialPortSettings <$> parseCommSpeed
                     <*> parseBitsPerWord
                     <*> parseStopBits
                     <*> parseParity
                     <*> parseFlowControl
                     <*> parseTimeout

options :: Parser (SerialPortSettings -> SerialPortSettings)
options = infoOption "" $ metavar "<options>"

parseCommSpeed :: Parser CommSpeed
parseCommSpeed = CS <$> parseBaud

parseBaud :: Parser Word32
parseBaud = option auto $ mconcat
  [ long "baudrate"
  , short 'b'
  , value 115200
  , showDefault
  , metavar "<bps>"
  , help "Baud rate"
  , hidden
  ]

parseBitsPerWord :: Parser Word8
parseBitsPerWord = option auto $ mconcat
  [ long "databits"
  , short 'd'
  , value $ bitsPerWord defaultSerialSettings
  , showDefault
  , metavar "7|8"
  , help "Data bits"
  , hidden
  ]

parseStopBits :: Parser StopBits
parseStopBits = option auto $ mconcat
  [ long "stopbits"
  , short 's'
  , value $ stopb defaultSerialSettings
  , showDefault
  , metavar "One|Two"
  , help "Stop bits"
  , hidden
  ]

parseParity :: Parser Parity
parseParity = option auto $ mconcat
  [ long "parity"
  , short 'p'
  , value $ parity defaultSerialSettings
  , showDefault
  , metavar "Even|Odd|NoParity"
  , help "Parity"
  , hidden
  ]

parseFlowControl :: Parser FlowControl
parseFlowControl = option auto $ mconcat
  [ long "flow"
  , short 'f'
  , value $ flowControl defaultSerialSettings
  , showDefault
  , metavar "Software|NoFlowControl"
  , help "Flow control"
  , hidden
  ]

parseTimeout :: Parser Int
parseTimeout = option auto $ mconcat
  [ long "timeout"
  , short 't'
  , value $ timeout defaultSerialSettings
  , showDefault
  , metavar "TIME"
  , help "Timeout in tenth-of-seconds"
  , hidden
  ]

com :: Handle -> IO ()
com hndl = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  concurrently_ readUart writeUart
  where
    readUart  = forever $ putChar =<< hGetChar hndl
    writeUart = forever $ hPutChar hndl =<< getChar
