# UnixLove OS v1.0

## Overview
UnixLove OS is a text-based adventure game where you are a little program navigating through a Unix-like filesystem to find your girlfriend who has been carelessly encrypted. You'll need to explore directories, collect items, and gain the necessary permissions to access restricted areas.

## Basic Navigation
- Use `cd [directory]` to move into a directory
- Use `cd ..` to move up one directory level
- Use `ls` to list the contents of the current directory
- Use `inventory` to see what items you're carrying

## New Feature: Executable Items
Some items in the game are now executable using the `run` command. These items can perform special actions when executed with the correct arguments. For example:
- `su` - A binary that can grant root access when run with the correct password
- `script.sh` - A simple script that prints "Hello, World!"

To run an executable:
```
run [item_name] [arguments]
```

## How to Win (Solution)
Here's the sequence of commands to complete the game:

1. `cd Downloads and cd Programs and take su and run su sudo_master_123`
2. `cd .. and cd .. and cd .. and cd .. and cd sys and cd security and cd keys and drop su and take girlfriend_ssh_key and cd .. and cd .. and cd .. and cd home and cd girlfriend`

## File System Map
```
/
├── home/
│   ├── user/
│   │   ├── Desktop/
│   │   │   ├── terminal_config
│   │   │   └── readme.txt
│   │   ├── Documents/
│   │   │   ├── todo_list
│   │   │   └── Projects/
│   │   │       ├── Games/
│   │   │       │   └── script.sh
│   │   │       ├── Work/
│   │   │       │   └── quarterly_report.txt
│   │   │       └── Personal/
│   │   │           └── diary.txt
│   │   └── Downloads/
│   │       ├── package.deb
│   │       ├── Programs/
│   │       │   └── su
│   │       └── Media/
│   │           └── encrypted_photo
│   └── girlfriend/
│       ├── secret.txt
│       └── love_letter.txt
└── sys/
    └── security/
        ├── permissions_manual
        └── keys/
            └── girlfriend_ssh_key
```

## Available Commands
- `ls` - List contents of current directory
- `cd [directory]` - Change to specified directory
- `cd ..` - Move up one directory
- `take [item]` - Pick up an item
- `drop [item]` - Drop an item
- `inventory` - Show your inventory
- `run [item] [args]` - Execute an item with arguments
- `exit` or `quit` - Exit the game

Note: Multiple commands can be chained using "and" between them, e.g.:
`take su and cd .. and run su sudo_master_123`
