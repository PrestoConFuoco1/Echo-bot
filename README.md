# What is this?

This is an echo-bot, which can work with Telegram and Vk.
Whenever a user of a messager sends a message to bot, it is
sent back to the user again.

The bot may not support some types of attachments for Vk,
if they appear in any message, a warning message is logged.
Currently, for Vk stickers, photos, videos, audios, documents,
wall posts and market units are supported.
For Telegram, all types of messages are supported, including
media group of photos; currently, media groups containing any
other types of attachments will be sent back separately.

Also, the bot supports some commands:
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

### Set repeat number

### Other messages
For messages that don't have any specific meaning the bot is trying to obtain an HTTP request,
which can be used to send the same message to the user. This is not possible only for a member of 
a media group in Telegram; all such messages are saved to the internal server state and sent again
as a media groups after all other updates have been already processed.
Before sending a reply bot gets a number of repeats for the target user and replicates the HTTP request
the corresponding number of times.








