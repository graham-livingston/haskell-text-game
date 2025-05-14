module Function where

data Argument = 
    PasswordArg String
  | NoArg
  deriving (Show, Eq)

data FunctionName = 
    Su
  | Script
  deriving (Show, Eq)

parseArgument :: String -> Argument
parseArgument = PasswordArg
