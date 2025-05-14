module Room where

import Item
import Direction


type Exits = [(Direction, RoomName)]

data Room = Room {
    rname :: RoomName,
    exits :: Exits,
    objects :: [ItemName]
} deriving (Show, Eq)

root :: Room
root = Room {
    rname = Root,
    exits = [(Into Home, Home), (Into Sys, Sys)],
    objects = []
}

home :: Room
home = Room {
    rname = Home,
exits = [(Up, Root), (Into UserHome, UserHome), (Into GirlfriendHome, GirlfriendHome)],
    objects = []
}

-- (locked until SSH key obtained)
girlfriendHome :: Room
girlfriendHome = Room {
    rname = GirlfriendHome,
    exits = [(Up, Home)],
    objects = [SecretTxt, LoveLetter]
}

userHome :: Room
userHome = Room {
    rname = UserHome,
    exits = [(Up, Home), (Into Desktop, Desktop), (Into Documents, Documents), (Into Downloads, Downloads)],
    objects = [BashRC]
}

documents :: Room
documents = Room {
    rname = Documents,
    exits = [(Up, UserHome), (Into Projects, Projects)],
    objects = [TodoList]
}

projects :: Room
projects = Room {
    rname = Projects,
    exits = [(Up, Documents), (Into Games, Games), (Into Work, Work), (Into Personal, Personal)],
    objects = [ReadmeMd]
}

games :: Room
games = Room {
    rname = Games,
    exits = [(Up, Projects)],
    objects = [ScriptSh]
}

work :: Room
work = Room {
    rname = Work,
    exits = [(Up, Projects)],
    objects = [QuarterlyReport]
}

personal :: Room
personal = Room {
    rname = Personal,
    exits = [(Up, Projects)],
    objects = [DiaryTxt]
}

downloads :: Room
downloads = Room {
    rname = Downloads,
    exits = [(Up, UserHome), (Into Programs, Programs), (Into Media, Media)],
    objects = [PackageDeb]
}

programs :: Room
programs = Room {
    rname = Programs,
    exits = [(Up, Downloads)],
    objects = [SuBinary]
}

media :: Room
media = Room {
    rname = Media,
    exits = [(Up, Downloads)],
    objects = [EncryptedPhoto]
}

desktop :: Room
desktop = Room {
    rname = Desktop,
    exits = [(Up, UserHome)],
    objects = [TerminalConfig, ReadmeTxt]
}

sys :: Room
sys = Room {
    rname = Sys,
    exits = [(Up, Root), (Into Security, Security)],
    objects = [ManualPage]
}

security :: Room
security = Room {
    rname = Security,
    exits = [(Up, Sys), (Into Keys, Keys)],
    objects = [PermissionsManual]
}

keys :: Room
keys = Room {
    rname = Keys,
    exits = [(Up, Security)],
    objects = [GirlfriendSSHKey]
}

-- functions
allRooms :: [Room]
allRooms = [
    root, home, girlfriendHome, userHome,
    documents, projects, games, work, personal,
    downloads, programs, media, desktop,
    sys, security, keys
    ]

roomNames :: [RoomName]
roomNames = map rname allRooms

addItem :: ItemName -> Room -> Room
addItem itemName room = room { objects = itemName : objects room }

removeItem :: ItemName -> Room -> Room
removeItem itemName room = room { objects = filter (/= itemName) (objects room) }

hasObjects :: Room -> Bool
hasObjects room = not (null (objects room))
