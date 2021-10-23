# What is this?

This is an echo-bot, which can work with Telegram and Vk.
Whenever a user of a messager sends a message to bot, it is
sent back to the user again.

The bot may not support some types of attachments for Vk,
if they appear in any message, a warning message is logged.
Currently, for Vk stickers, photos, videos, audios, documents,
wall posts and market units are supported.
For Telegram, all types of messages are supported, including
media groups. Also, you can roll a dice using /dice command;
the same effect is when you forward a dice message to bot.

Also, the two bots supports some shared commands:
+ `/help` shows help message;
+ `/set` tells the bot to show keyboard with five buttons, after pressing
one of those buttons every message will be repeated the respective amount of times.

# How it works?

## Main loop

The main logic scenario is the same for both messagers versions.
The first thing to do is to get updates from messager API server.
When updates are fetched, the bot starts to process them.
If the bot failed to get updates due to nonfatal error, it tries
to handle it (handlers are separate for two messagers implementations).

## Processing an update

The first thing to do is to define update type.
The update type can be one of these:
1. Request for help message
2. Request for keyboard, which buttons have a text label with a possible number of repeats (from 1 to 5);
3. Button pressed, which indicates to set the number of echo repeats for one user;
4. Other messages, that are simply sent back with the same information.

### Help message request
For this event, the text message with help message is sent away, which is included into the configuration file.

### Request for repeat-number keyboard
For this event, the JSON-serialized object with keyboard is sent to the user with the text message.

### Set repeat number
This event is usually caused by pressing a button with a corresponding repeat number, which causes
a callback update to be sent to the bot. The latter one parses callback payload and gets this number,
after what it is saved inside bot's internal state (currently, a map is used for this, but it's possible
to use database, too).

### Other messages
For messages that don't have any specific meaning the bot is trying to obtain an HTTP request,
which can be used to send the same message to the user. This is not possible only for a member of 
a media group in Telegram; all such messages are saved to the internal bot state and sent again
as a media groups after all other updates have been already processed.
Before sending a reply bot gets a number of repeats for the target user and replicates the HTTP request
the corresponding number of times.

## Implementation

### Type families

The main application logic is inside the `Execute.hs`.
How it works is totally written in the sections above.
Its main peculiarity is that for different messagers, though
logic is similar, but not exactly the same, and even types are not the same too.
Hence, all functions should be parameterized by a "messager", which can be done
in some way.
The type family (`BotClassTypes`) is defined in module `BotClass/ClassTypes.hs`.
For example when we need to get user from a received message, we can use something like
`getUserFromMessage :: (BotClassTypes s) :: Msg s -> User s`. As we go down to
the specific messager, we can add an instance of this type family for it. For instance,
Msg 'Vkontakte = VkMessage, User 'Vkontakte = VkUser, so we finally get something like
`getUserFromMessage :: VkMessage -> VkUser`.

For messager specific functions there are some typeclasses:
+ BotClassConfigurable for messager specific coniguration (see `Config.hs`);
+ BotClassUtility for utility functions (see `BotClass/Class.hs`);
+ BotClass for main functions (the same).

The latter class is responsible for the logic that is not common for both
messagers. Especially it is true for the functions `processMessage` and
`epilogue`, since their implementations are completely different for Vk and Telegram.
The first one has a type of 
`processMessage :: (Monad m) => Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))`.
For a given message this function tries to build a computation within a given monad,
that results in a HTTP request, used to send back the received message.
The possibility of returning `Nothing` is added for some cases when there is no
data to send immediately. The specific case is processing Telegram mediagroups,
when bot has to aggregate some messages and send them back as a one.
The `epilogue` function is responsible for post-processing actions:
`epilogue :: (Monad m) => Handle s m -> s -> [Upd s] -> RepSucc s -> m ()`.
For example, it can send mediagroups.

### Handle pattern

As it can be seen above, some functions can take a parameter of type `Handle s m`.
It serves two purposes. Firstly, it is used to carry some environment, i.e.
it contains help message, default number of repeats and so on. This is similar
to the Reader monad.
Its second purpose is to provide some effectful functions that use internal
state of the bot. For example, for both messagers it provides functions
that are working with (User, Repeat number) map (findUser, insertUser and so on).

The definition of handle can be found in `App/Handle.hs` module, `Handle s m`.
`s` parameter refers to a specific messager, `m` corresponds to a monad to execute
computations within. It also has a field named `specH` that keeps messager specific effectful functions.
The specific instances of it can be found in `App/Handle/Vkontakte.hs` and `App/Handle/Telegram.hs` modules.
The Telegram handle provides some functions working with updateID and mediaGroups, the Vk one -
with Vk timestamps and random ID's.

## Logging

Logging is also implemented as a handle, that is passed to all other functions.
Its type is `Priority -> Text -> IO ()`. For initial steps stderr
logger is used, and for the other part of the application the self-sufficient logger
is used. If any exception is raised inside it, it is handled inside it too.
If it becomes impossible to continue logging to file, the logger starts using
stderr handle.

## Auxiliary modules

* GenericPretty.hs is used for pretty logging;
* Config.hs is used for reading the configuration files;
* Stuff.hs includes some useful functions that have no application-logic sense only by themselves.

# How to get it working?

## Compile

First of all, you need to clone the repository with the bot sources and build it using stack.
Usually it is enough to just run `stack build`, all the libraries will be compiled automatically.
Eventually building can crash because of some external libraries that are not installed on your machine.
If you are using Linux, you can just install them yourself. For example, if you are using Ubuntu and
the pq library is missing, just install `libpq-dev` package using `# apt install libpq-dev`. Generally, for most cases if (libname) is
missing, the package lib(libname)-dev is what you need. There are some exceptions for this rule, for example, zlib.

## Obtain token for the bot
See `https://core.telegram.org/bots#6-botfather` for Telegram, `https://vk.com/dev/access_token` for Vk.

## Run with configuration file and command line options
Here the only thing to do is to get correct configuration file. Its format is discussed below.

## Tests

There are some pure logic tests done. They can be found in the `test/` directory.

# Configuration file
An example of configuration file follows:

```
general {
    help_message = "Hello. Available commands:\n-- /help - get help\n-- /set - change current number of messages repeats"
    repeat_question = "How many times would you like to repeat every reply?"
    default_repeat_num = 1
    timeout = 20
    help_command = "/help"
    set_rep_num_command = "/set"
}


telegram {
    initial_update_id = 0
    bot_url =  "telegram_token"
}


vkontakte {
    bot_url = "https://api.vk.com/method/" 
    access_token = "vk_access_token"
    group_id = 666666
    api_version = "5.124"
}

```
For correct work just replace values with correct ones.
If you need only telegram bot, you can omit Vk configuration, and vice versa.

# Command line options

The first command line options always should be a path to the configuration file.
The order of all subsequent parameters is arbitrary.

+ `--test-config` is used to test bot configuration. Data from config file will be get and then process will terminate
with success exitcode.
+ `--tl` or `--telegram` is used to indicate that Telegram bot will be used, `--vk` - for Vk bot.
One of these arguments is required, otherwise application will terminate with error.
+ `-l` is used to define logging settings. Currently logger can only log messages with
given priority or higher. For example, to log Warning, Error and Fatal messages use
`-l Warning`. The default is `-l Debug`, which logs all messages.
+ `--logpath <path>` is used to define the path to the log file.
+ `-h` or `--help` for help message.






