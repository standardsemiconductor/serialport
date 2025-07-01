module System.Hardware.Serialport.Cli
  ( SerialPortCli(..)
  , serialPortCli
  , -- * Parsers
    serialPortCliParser
  , serialPortPathParser
  , serialPortSettingsParser
  , commSpeedParser
  , bitsPerWordParser
  , stopBitsParser
  , parityParser
  , flowControlParser
  , timeoutParser
  ) where

import Data.Word
import Options.Applicative
import System.Hardware.Serialport.Types

data SerialPortCli = SerialPortCli
  { serialPortPath     :: FilePath
  , serialPortSettings :: SerialPortSettings
  }

serialPortCli :: IO SerialPortCli
serialPortCli =
  customExecParser serialPortCliPrefs $
    info (serialPortCliParser <**> helper) mempty

serialPortCliPrefs :: ParserPrefs
serialPortCliPrefs = prefs $ showHelpOnError <> showHelpOnEmpty

serialPortCliParser :: Parser SerialPortCli
serialPortCliParser =
  SerialPortCli
    <$> serialPortPathParser
    <*> serialPortSettingsParser

serialPortPathParser :: Parser FilePath
serialPortPathParser = strArgument $ mconcat
  [ help "serial port file path"
  , metavar "<tty-device>"
  , completer $ bashCompleter "file"
  ]

serialPortSettingsParser :: Parser SerialPortSettings
serialPortSettingsParser =
  SerialPortSettings
    <$> commSpeedParser
    <*> bitsPerWordParser
    <*> stopBitsParser
    <*> parityParser
    <*> flowControlParser
    <*> timeoutParser

commSpeedParser :: Parser CommSpeed
commSpeedParser = option auto $ mconcat
  [ long "baudrate"
  , short 'b'
  , value CS115200
  , showDefault
  , metavar "<bps>"
  , help "Baud rate"
  ]

bitsPerWordParser :: Parser Word8
bitsPerWordParser = option auto $ mconcat
  [ long "databits"
  , short 'd'
  , value $ bitsPerWord defaultSerialSettings
  , showDefault
  , metavar "7|8"
  , help "Data bits"
  ]

stopBitsParser :: Parser StopBits
stopBitsParser = option auto $ mconcat
  [ long "stopbits"
  , short 's'
  , value $ stopb defaultSerialSettings
  , showDefault
  , metavar "One|Two"
  , help "Stop bits"
  ]

parityParser :: Parser Parity
parityParser = option auto $ mconcat
  [ long "parity"
  , short 'p'
  , value $ parity defaultSerialSettings
  , showDefault
  , metavar "Even|Odd|NoParity"
  , help "Parity"
  ]

flowControlParser :: Parser FlowControl
flowControlParser = option auto $ mconcat
  [ long "flow"
  , short 'f'
  , value $ flowControl defaultSerialSettings
  , showDefault
  , metavar "Software|NoFlowControl"
  , help "Flow control"
  ]

timeoutParser :: Parser Int
timeoutParser = option auto $ mconcat
  [ long "timeout"
  , short 't'
  , value $ timeout defaultSerialSettings
  , showDefault
  , metavar "TIME"
  , help "Timeout in tenth-of-seconds"
  ]
