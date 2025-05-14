module Player where

import Item
import Direction(RoomName(..))
import qualified Data.Map as M

data Player = Player {
    inventory :: [ItemName],
    inventorySize :: Integer,    -- Current size in bytes
    maxSize :: Integer,          -- Maximum inventory size (like disk quota)
    location :: RoomName
} deriving (Show, Eq)

-- Check if adding an item would exceed inventory size
canAddItem :: Item -> Player -> Universe -> Bool
canAddItem item player univ =
    inventorySize player + size item <= maxSize player

-- Add item to inventory if size permits
addItem :: ItemName -> Player -> Universe -> Either String Player
addItem itemName player univ =
    case M.lookup itemName univ of
        Nothing -> Left "Item does not exist"
        Just item ->
            if canAddItem item player univ
            then Right $ player { 
                inventory = itemName : inventory player,
                inventorySize = inventorySize player + size item
            }
            else Left "Not enough space in inventory"

-- Remove item from inventory
removeItem :: ItemName -> Player -> Universe -> Player
removeItem itemName player univ =
    case M.lookup itemName univ of
        Nothing -> player
        Just item -> player {
            inventory = filter (/= itemName) (inventory player),
            inventorySize = inventorySize player - size item
        }

-- Change player location
newLocation :: RoomName -> Player -> Player
newLocation r p = p { location = r }

-- Check if carrying any items
isCarryingAnything :: Player -> Bool
isCarryingAnything = not . null . inventory

-- Get formatted inventory status
getInventoryStatus :: Player -> String
getInventoryStatus player = 
    "Inventory (" ++ show (inventorySize player) ++ "/" ++ show (maxSize player) ++ " bytes)"

-- Initial player state
initialPlayer :: Player
initialPlayer = Player {
    inventory = [],
    inventorySize = 0,
    maxSize = 10024,
    location = UserHome
}
