module Security where

data SecurityState = SecurityState {
    hasRootAccess :: Bool,
    currentPassword :: Maybe String 
} deriving (Show, Eq)

rootPassword :: String
rootPassword = "sudo_master_123"

initialSecurity :: SecurityState
initialSecurity = SecurityState {
    hasRootAccess = False,
    currentPassword = Nothing
}

grantRootAccess :: String -> SecurityState -> Either String SecurityState
grantRootAccess password state =
    if password == rootPassword then 
        Right $ state { hasRootAccess = True, currentPassword = Just password }
    else 
        Left "Incorrect password. Root access denied."
