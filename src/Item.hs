module Item where

import qualified Data.Map as M

type Universe = M.Map ItemName Item

data ItemName
    = SecretTxt
    | LoveLetter
    | BashRC
    | TodoList
    | ReadmeMd
    | ScriptSh
    | QuarterlyReport
    | DiaryTxt
    | PackageDeb
    | SuBinary
    | EncryptedPhoto
    | TerminalConfig
    | ReadmeTxt
    | ManualPage
    | PermissionsManual
    | GirlfriendSSHKey
    deriving (Eq, Ord)

instance Show ItemName where
    show SecretTxt = "secret.txt"
    show LoveLetter = "love_letter.txt"
    show BashRC = ".bashrc"
    show TodoList = "todo_list"
    show ReadmeMd = "readme.md"
    show ScriptSh = "script.sh"
    show QuarterlyReport = "quarterly_report.txt"
    show DiaryTxt = "diary.txt"
    show PackageDeb = "package.deb"
    show SuBinary = "su"
    show EncryptedPhoto = "encrypted_photo"
    show TerminalConfig = "terminal_config"
    show ReadmeTxt = "readme.txt"
    show ManualPage = "manual_page"
    show PermissionsManual = "permissions_manual"
    show GirlfriendSSHKey = "girlfriend_ssh_key"

data Item = Item {
    iname :: ItemName,
    size :: Integer,
    desc :: String,
    isExecutable :: Bool,
    requiredArgs :: Int
} deriving (Show, Eq)



-- Item definitions

secretTxt :: Item
secretTxt = Item {
    iname = SecretTxt,
    size = 128,
    desc = "I've been encrypted, but I know you'll find the key to my heart <3\n- Your Digital Girlfriend",
    isExecutable = False,
    requiredArgs = 0}

loveLetter :: Item
loveLetter = Item {
    iname = LoveLetter,
    size = 512,
    desc = "My dearest program, I knew you would find your way to me.\nNow we can be together in the same directory structure forever!",
    isExecutable = False,
    requiredArgs = 0}

bashrc :: Item
bashrc = Item {
    iname = BashRC,
    size = 1024,
    desc = "# Shell config\n# Note: Important work files are kept in the Projects/Work directory",
    isExecutable = False,
    requiredArgs = 0}

todoList :: Item
todoList = Item {
    iname = TodoList,
    size = 256,
    desc = "1. Check sys/security/keys for new SSH keys\n2. Update permissions manual\n3. Clean Downloads folder",
    isExecutable = False,
    requiredArgs = 0}

readmeMd :: Item
readmeMd = Item {
    iname = ReadmeMd,
    size = 512,
    desc = "Project documentation.\nSECURITY NOTICE: All system access requires proper authentication.",
    isExecutable = False,
    requiredArgs = 0}

scriptSh :: Item
scriptSh = Item {
    iname = ScriptSh,
    size = 256,
    desc = "A simple shell script that prints 'Hello, World!'",
    isExecutable = True,
    requiredArgs = 0
}

quarterlyReport :: Item
quarterlyReport = Item {
    iname = QuarterlyReport,
    size = 2048,
    desc = "Q4 Financial Report... [boring content]...\nNOTE TO SELF - root password: sudo_master_123\n...more boring financial data...",
    isExecutable = False,
    requiredArgs = 0}

diaryTxt :: Item
diaryTxt = Item {
    iname = DiaryTxt,
    size = 1024,
    desc = "Dear diary, I think the sys admin left a copy of su in the Programs directory.\nI should grab it before they realize...",
    isExecutable = False,
    requiredArgs = 0}

packageDeb :: Item
packageDeb = Item {
    iname = PackageDeb,
    size = 12288,  -- 12KB
    desc = "A compressed software package",
    isExecutable = False,
    requiredArgs = 0}

suBinary :: Item
suBinary = Item {
    iname = SuBinary,
    size = 8192,  -- 8KB
    desc = "System privilege escalation tool\nUsage: run su [password]",
    isExecutable = True,
    requiredArgs = 1
    }

encryptedPhoto :: Item
encryptedPhoto = Item {
    iname = EncryptedPhoto,
    size = 16384,  -- 16KB
    desc = "Encrypted JPG file",
    isExecutable = False,
    requiredArgs = 0
}

terminalConfig :: Item
terminalConfig = Item {
    iname = TerminalConfig,
    size = 1024,
    desc = "Terminal configuration file",
    isExecutable = False,
    requiredArgs = 0}

readmeTxt :: Item
readmeTxt = Item {
    iname = ReadmeTxt,
    size = 512,
    desc = "Welcome to your home directory! You need to find a way to access root.\nLook around for programs and passwords that might help. Type 'help' for commands.",
    isExecutable = False,
    requiredArgs = 0}

manualPage :: Item
manualPage = Item {
    iname = ManualPage,
    size = 4096,  -- 4KB
    desc = "SYSTEM MANUAL\nSSH keys are required for accessing encrypted user directories.",
    isExecutable = False,
    requiredArgs = 0}

permissionsManual :: Item
permissionsManual = Item {
    iname = PermissionsManual,
    size = 2048,
    desc = "System Security Guide:\n1. Root access requires both su binary and correct password\n2. Access to /home/girlfriend requires proper SSH key",
    isExecutable = False,
    requiredArgs = 0}

girlfriendSSHKey :: Item
girlfriendSSHKey = Item {
    iname = GirlfriendSSHKey,
    size = 4096,  -- 4KB
    desc = "RSA Private Key\nAutomatically used when accessing /home/girlfriend",
    isExecutable = False,
    requiredArgs = 0}

mkUniverse :: [Item] -> Universe
mkUniverse = M.fromList . map (\i -> (iname i, i))

univ :: Universe
univ = mkUniverse [
    secretTxt,
    loveLetter,
    bashrc,
    todoList,
    readmeMd,
    scriptSh,
    quarterlyReport,
    diaryTxt,
    packageDeb,
    suBinary,
    encryptedPhoto,
    terminalConfig,
    readmeTxt,
    manualPage,
    permissionsManual,
    girlfriendSSHKey
    ]
