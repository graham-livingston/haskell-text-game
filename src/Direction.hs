module Direction where

data RoomName
  = Root
  | Home
  | GirlfriendHome
  | UserHome
  | Documents
  | Projects
  | Games
  | Work
  | Personal
  | Downloads
  | Programs
  | Media
  | Desktop
  | Sys
  | Security
  | Keys
  deriving (Eq, Ord)

instance Show RoomName where
    show Root = "/"
    show Home = "/home"
    show GirlfriendHome = "/home/girlfriend"
    show UserHome = "/home/user"
    show Documents = "/home/user/Documents"
    show Projects = "/home/user/Documents/Projects"
    show Games = "/home/user/Documents/Projects/Games"
    show Work = "/home/user/Documents/Projects/Work"
    show Personal = "/home/user/Documents/Projects/Personal"
    show Downloads = "/home/user/Downloads"
    show Programs = "/home/user/Downloads/Programs"
    show Media = "/home/user/Downloads/Media"
    show Desktop = "/home/user/Desktop"
    show Sys = "/sys"
    show Security = "/sys/security"
    show Keys = "/sys/security/keys"

data Direction = 
    Up           -- cd ..  
  | Into RoomName -- cd directory_name
  deriving (Eq)

instance Show Direction where
    show Up = ".."
    show (Into dirname) = "cd " ++ show dirname

