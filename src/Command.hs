module Command where

import Item
import Direction
import Function
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type Conjunction = [Command]

data Command
  = Ls
  | Cd Direction
  | Take [ItemName]
  | Drop [ItemName]
  | Run ItemName [String]
  | Inventory
  | Exit
  deriving (Eq, Show)

-- Basic utilities
char :: Char -> Parser Char
char = P.char

string :: String -> Parser String
string = P.string

eof :: Parser ()
eof = P.eof

satisfy :: (Char -> Bool) -> Parser Char
satisfy = P.satisfy

spaces :: Parser ()
spaces = P.spaces *> pure ()

optionalSpace :: Parser ()
optionalSpace = P.spaces *> pure ()

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (P.try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . fmap P.try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . P.try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . P.try

-- Noun phrase parsing
itemNameP :: Parser ItemName
itemNameP = choice
    [ SuBinary <$ string "su"
    , GirlfriendSSHKey <$ string "girlfriend_ssh_key"
    , ReadmeTxt <$ string "readme.txt"
    , QuarterlyReport <$ string "quarterly_report.txt"
    , DiaryTxt <$ string "diary.txt"
    , TodoList <$ string "todo_list"
    , SecretTxt <$ string "secret.txt"
    , LoveLetter <$ string "love_letter.txt"
    , BashRC <$ string ".bashrc"
    , PermissionsManual <$ string "permissions_manual"
    , TerminalConfig <$ string "terminal_config"
    , PackageDeb <$ string "package.deb"
    , ScriptSh <$ string "script.sh"
    , EncryptedPhoto <$ string "encrypted_photo"
    , ReadmeMd <$ string "readme.md"
    , ManualPage <$ string "manual_page"
    ]

nounPhrase :: Parser [ItemName]
nounPhrase = itemNameP `sepBy1` (char ',' *> optionalSpace)

-- Command Parsers
lsP :: Parser Command
lsP = Ls <$ string "ls"

cdP :: Parser Command
cdP = do
    _ <- string "cd" <* spaces
    dir <- cdUpP <|> cdDirP
    return $ Cd dir
  where
    cdUpP = Up <$ string ".."
    cdDirP = do
        dirName <- P.many1 (P.noneOf " \n")
        case readRoomName dirName of
            Just room -> return $ Into room
            Nothing -> P.parserFail $ "Unknown directory: " ++ dirName

runP :: Parser Command
runP = do
    _ <- string "run" <* spaces
    itemName <- itemNameP
    args <- many (spaces *> many1 (P.noneOf " \n"))
    return $ Run itemName args

-- special su command parser
suP :: Parser Command
suP = do
    _ <- string "su" <* spaces
    args <- many (spaces *> many1 (P.noneOf " \n"))
    return $ Run SuBinary args

inventoryP :: Parser Command
inventoryP = Inventory <$ string "inventory"

takeP :: Parser Command
takeP = do
    _ <- string "take" <* spaces
    items <- nounPhrase
    return $ Take items

dropP :: Parser Command
dropP = do
    _ <- string "drop" <* spaces
    items <- nounPhrase
    return $ Drop items

exitP :: Parser Command
exitP = Exit <$ (P.try (string "exit") <|> string "quit")

-- Main command parser
commandP :: Parser Command
commandP = choice
    [ lsP
    , cdP
    , takeP
    , dropP
    , runP
    , inventoryP
    , exitP
    ]

-- Conjunction parser
conjunctionP :: Parser Conjunction
conjunctionP = do 
    commands <- commandP `sepBy1` (spaces *> string "and" *> spaces)
    eof
    return commands

-- convert string to RoomName
readRoomName :: String -> Maybe RoomName
readRoomName name = case name of
    "Documents" -> Just Documents
    "Downloads" -> Just Downloads
    "Desktop" -> Just Desktop
    "Projects" -> Just Projects
    "Games" -> Just Games
    "Work" -> Just Work
    "Personal" -> Just Personal
    "Programs" -> Just Programs
    "Media" -> Just Media
    "home" -> Just Home
    "user" -> Just UserHome
    "root" -> Just Root
    "sys" -> Just Sys
    "security" -> Just Security
    "keys" -> Just Keys
    "girlfriend" -> Just GirlfriendHome
    _ -> Nothing

parseInput :: String -> Maybe Conjunction
parseInput input = case P.parse conjunctionP "" input of
    Left _  -> Nothing
    Right c -> Just c
