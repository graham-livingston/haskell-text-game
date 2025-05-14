module GameState where

import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction
import Function
import Security

type GameMap = M.Map RoomName Room
type Error a = Either String a

data GameState = GameState {
    message :: Maybe String,
    gmap :: GameMap,
    universe :: Universe,
    player :: Player,
    permissions :: SecurityState
}

mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList [(rname room, room) | room <- rooms]

gameMap :: GameMap
gameMap = mkMap allRooms


-- looks up a Room in the GameMap
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp =
  case M.lookup rname mp of
    Just room -> room
    Nothing -> throw KeyError

-- looks up a Room in the GameState's GameMap
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)


setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rname room gmap =
    if M.member rname gmap
    then M.insert rname room gmap
    else gmap

currentInventory :: GameState -> [ItemName]
currentInventory st = inventory (player st)

inventoryWeight :: GameState -> Integer
inventoryWeight st =
    let inventoryItems = inventory (player st)
        itemWeights = map (\name -> size (getObject name st)) inventoryItems
    in sum itemWeights

nearbyObjects :: GameState -> [ItemName]
nearbyObjects st = objects (currentRoom st)

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck itemName st =
    if itemName `elem` currentInventory st
    then Left $ "You already have the " ++ show itemName ++ "."
    else Right st

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck itemName st =
    if itemName `elem` nearbyObjects st
    then Right st
    else Left $ "There is no " ++ show itemName ++ " here."

weightCheck :: ItemName -> GameState -> Error GameState
weightCheck itemName st =
    let
        item = getObject itemName st
        newWeight = size item + inventoryWeight st
        maxWeightLimit = maxSize (player st)
    in
    if newWeight > maxWeightLimit
    then Left $ "That's too much weight for you to carry."
    else Right st





anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck itemName st
  | itemName `elem` currentInventory st || itemName `elem` nearbyObjects st = Right st
  | otherwise = Left $ "What do you mean, drop the " ++ show itemName ++ "?"

inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck itemName st
  | itemName `elem` nearbyObjects st = Left $ "You aren't carrying the " ++ show itemName ++ "."
  | otherwise = Right st


setMessage :: String -> GameState -> GameState
setMessage msg st =
    st { message = if null msg then Nothing else Just msg }

currentRoom :: GameState -> Room
currentRoom st = getRoom (location (player st)) st

-- Navigation functions
move :: Direction -> GameState -> GameState
move Up st =
    let current = currentRoom st
        currentName = rname current

        maybeParent = case currentName of
            GirlfriendHome -> Just Home
            UserHome -> Just Home
            Desktop -> Just UserHome
            Documents -> Just UserHome
            Downloads -> Just UserHome
            Projects -> Just Documents
            Games -> Just Projects
            Work -> Just Projects
            Personal -> Just Projects
            Programs -> Just Downloads
            Media -> Just Downloads
            Security -> Just Sys
            Keys -> Just Security
            Home -> Just Root
            Sys -> Just Root
            Root -> Nothing  -- Can't go up from root
    in case maybeParent of
        Nothing -> st { message = Just "Already at root directory" }
        Just parentDir -> 
            case canAccessRoom (permissions st) (hasSSHKey st) parentDir of
                Left err -> st { message = Just err }
                Right _ -> st { 
                    player = Player.newLocation parentDir (player st),
                    message = Just $ "Changed directory to " ++ show parentDir
                }

move (Into target) st =
    let current = currentRoom st
        validExits = [dest | (Into r, dest) <- exits current]
    in if target `elem` validExits
        then case canAccessRoom (permissions st) (hasSSHKey st) target of
            Left err -> st { message = Just err }
            Right _ -> st {
                player = Player.newLocation target (player st),
                message = Just $ "Changed directory to " ++ show target
            }
        else st { message = Just $ "No such directory: " ++ show target }


-- Security check for room access
canAccessRoom :: SecurityState -> Bool -> RoomName -> Either String Bool
canAccessRoom sec hasSSHKey targetRoom
    | requiresRoot targetRoom = 
        if hasRootAccess sec
        then Right True
        else Left "Access denied: Root privileges required. Use 'su' with correct password."
    | isGirlfriendDir targetRoom = 
        if hasSSHKey
        then Right True
        else Left "Access denied: SSH key required to access /home/girlfriend"
    | otherwise = Right True


requiresRoot :: RoomName -> Bool
requiresRoot Root = True
requiresRoot Sys = True
requiresRoot Security = True
requiresRoot Keys = True
requiresRoot _ = False

isGirlfriendDir :: RoomName -> Bool
isGirlfriendDir GirlfriendHome = True
isGirlfriendDir _ = False

-- Item interaction
hasSSHKey :: GameState -> Bool
hasSSHKey st = GirlfriendSSHKey `elem` inventory (player st)


takeItem :: ItemName -> GameState -> GameState
takeItem itemName st = 
    case (alreadyHaveTakeCheck itemName st >>= inRoomTakeCheck itemName >>= weightCheck itemName) of
        Left err -> st { message = Just err }  
        Right updatedState ->
            let currentRm = currentRoom updatedState
                updatedRoom = Room.removeItem itemName currentRm
                updatedPlayer = Player.addItem itemName (player updatedState) (universe updatedState)
                updatedMap = setRoomMap (rname currentRm) updatedRoom (gmap updatedState)
                newMessage = Just $ "You take " ++ show itemName
            in case updatedPlayer of
                Left err -> st { message = Just err }
                Right newPlayer -> updatedState { 
                    gmap = updatedMap, 
                    player = newPlayer, 
                    message = newMessage 
                }

dropItem :: ItemName -> GameState -> GameState
dropItem itemName st = 
    case (anywhereDropCheck itemName st >>= inRoomDropCheck itemName) of
        Left err -> st { message = Just err }
        Right updatedState ->
            let currentRm = currentRoom updatedState
                updatedRoom = Room.addItem itemName currentRm
                updatedPlayer = Player.removeItem itemName (player updatedState) (universe updatedState)
                updatedMap = setRoomMap (rname currentRm) updatedRoom (gmap updatedState)
                newMessage = case message updatedState of 
                    Nothing -> Just $ "You drop " ++ show itemName
                    Just existingMsg -> Just $ existingMsg ++ "\nYou drop " ++ show itemName
            in updatedState { 
                gmap = updatedMap, 
                player = updatedPlayer, 
                message = newMessage 
            }

-- Program execution

runProgram :: ItemName -> [String] -> GameState -> GameState
runProgram prog args st = 
    if prog `notElem` inventory (player st)
    then st { message = Just $ "You don't have " ++ show prog }
    else 
        case prog of
            SuBinary -> 
                if length args /= 1 
                then st { message = Just "Usage: su [password]" }
                else case grantRootAccess (head args) (permissions st) of
                    Left err -> st { message = Just err }
                    Right newSecurity -> st { 
                        permissions = newSecurity,
                        message = Just "Root access granted. You can now access system directories."
                    }
            ScriptSh ->
                if not (null args)
                then st { message = Just "script.sh takes no arguments" }
                else st { message = Just "Hello, World!" }
            _ -> st { message = Just $ show prog ++ " is not executable" }




-- Room display
displayRoom :: GameState -> GameState
displayRoom st =
    let room = currentRoom st
        header = "Current directory: " ++ show (rname room)
        contents = if null (objects room) 
                  then "Directory is empty"
                  else "Contents:\n" ++ formatItems (objects room) st
        exits = "Subdirectories:\n" ++ unlines [show d | (Into d) <- map fst (Room.exits room)]
    in st { message = Just $ unlines [header, contents, exits] }

-- Helper function to format items with descriptions
formatItems :: [ItemName] -> GameState -> String
formatItems items st = 
    unlines $ map (formatItem st) items
    where
        formatItem :: GameState -> ItemName -> String
        formatItem state iname = 
            let item = getObject iname state
                name = show iname
                -- Split description into lines, indent each line, and rejoin
                description = unlines $ map ("    " ++) (lines $ desc item)
            in name ++ "\n" ++ description


-- Inventory display
displayInventory :: GameState -> GameState
displayInventory st =
    let p = player st
        inv = if null (inventory p)
              then "Empty"
              else unlines $ map (\i -> show i ++ " (" ++ 
                   show (size (getObject i st)) ++ " bytes)") (inventory p)
        status = getInventoryStatus p
    in st { message = Just $ status ++ "\n" ++ inv }

-- Helper functions
getObject :: ItemName -> GameState -> Item
getObject iname st = 
    case M.lookup iname (universe st) of
        Just obj -> obj
        Nothing -> throw KeyError

getSecurityStatus :: SecurityState -> String
getSecurityStatus sec =
    "Current privileges:\n" ++
    "Root access: " ++ show (hasRootAccess sec)


parseArgument :: String -> Argument
parseArgument = PasswordArg  -- treat all args as passwords 

data KeyError = KeyError deriving Show
instance Exception KeyError

-- Initialize game
initialState :: GameState
initialState = GameState {
    message = Nothing,
    gmap = gameMap,
    universe = univ,
    player = initialPlayer,
    permissions = initialSecurity
}
