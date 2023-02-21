module DefwDefinition where

data DefwToken
  = DefwTitle String
  | DefwOn
      { caseName :: String,
        cases :: String
      }
  | DefwBlock
      { blockName :: String,
        blockCommands :: [DefwToken]
      }
  | DefwCommand
      { commandName :: String,
        commandArguments :: [(String, DefwDataType)],
        commandData :: [DefwData]
      }
  | DefwWindow
      { windowCommands :: [DefwToken]
      }
  deriving (Show)

data DefwData
  = DefwAt (Int, Int)
  | DefwSized (Int, Int)
  deriving (Show)

data DefwDataType
  = DefwInt Int
  | DefwStr String
  deriving (Show)