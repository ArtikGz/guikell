module DefwDefinition where
import Utils

data DefwToken
  = DefwTitle String
  | DefwOn
      { caseName :: String,
        cases    :: String
      }
  | DefwBlock
      { blockName     :: String,
        blockCommands :: [DefwToken]
      }
  | DefwCommand
      { commandName :: String,
        commandData :: [DefwData]
      }
  | DefwWindow
      { windowCommands :: [DefwToken]
      }
  | DefwDraw
      { arguments :: [DefwArgument],
        commands  :: [DefwToken]
      }
  | DefwArgument (String, String)
  deriving (Show)

type DefwArgument = (String, String)

data DefwData
  = DefwAt (Int, Int)
  | DefwSized (Int, Int)
  | DefwAs String
  deriving (Show, Eq, Ord)

toDefwData :: String -> [Int] -> DefwData
toDefwData "sized" [x, y] = DefwSized (x, y) 
toDefwData "at" [x, y] = DefwAt (x, y)  