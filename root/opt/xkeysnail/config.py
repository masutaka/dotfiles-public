# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(1)

# [Global modemap] Change modifier keys as in xmodmap
define_modmap({
    Key.CAPSLOCK: Key.LEFT_CTRL,
    # Swap Alt and Win
    Key.LEFT_META: Key.LEFT_ALT,
    Key.LEFT_ALT: Key.LEFT_META,
})

# Keybindings for 1Password
define_keymap(re.compile("1Password"), {
    K("C-f"): K("SLASH"),   # Search
    K("Win-a"): K("C-a"),   # Select all
    K("Win-c"): K("C-c"),   # Copy
    K("Win-f"): K("SLASH"), # Search
    K("Win-v"): K("C-v"),   # Paste
    K("Win-x"): K("C-x"),   # Cut
}, "1Password")

# Keybindings for Web browser
define_keymap(re.compile("firefox|Google-chrome"), {
    K("Win-Alt-c"): K("C-Shift-c"),               # the Elements panel of Developer tools
    K("Win-Alt-i"): K("C-Shift-i"),               # Developer tools
    K("Win-Alt-j"): K("C-Shift-j"),               # the Console panel of Developer tools
    K("Win-Alt-u"): K("C-u"),                     # Show HTML source
    K("Win-KEY_0"): K("C-KEY_0"),                 # Reset size
    K("Win-LEFT_BRACE"): K("Alt-LEFT"),           # Win+[ to move previous page
    K("Win-MINUS"): K("C-MINUS"),                 # Zoom out
    K("Win-RIGHT_BRACE"): K("Alt-RIGHT"),         # Win+] to move next page
    K("Win-Shift-EQUAL"): K("C-Shift-EQUAL"),     # Zoom in
    K("Win-Shift-LEFT_BRACE"): K("C-PAGE_UP"),    # Win+{ to switch previous tab
    K("Win-Shift-RIGHT_BRACE"): K("C-PAGE_DOWN"), # Win+} to switch next tab
    K("Win-Shift-t"): K("C-Shift-t"),             # Reopen previously closed a tab
    K("Win-a"): K("C-a"),                         # Select all
    K("Win-c"): K("C-c"),                         # Copy
    K("Win-f"): K("C-f"),                         # Search page
    K("Win-g"): K("C-g"),                         # Search next
    K("Win-l"): K("C-l"),                         # Switch to the address bar
    K("Win-n"): K("C-n"),                         # New window
    K("Win-p"): K("C-p"),                         # Print page
    K("Win-r"): K("C-r"),                         # Reload
    K("Win-t"): K("C-t"),                         # New tab
    K("Win-v"): K("C-v"),                         # Paste
    K("Win-w"): K("C-w"),                         # Close tab
    K("Win-x"): K("C-x"),                         # Cut
}, "Web browser")

# Keybindings for Chrome
define_keymap(re.compile("Google-chrome"), {
    K("Win-Shift-n"): K("C-Shift-n"), # New secret window
}, "Web browser")

# Keybindings for Firefox
define_keymap(re.compile("firefox"), {
    K("Win-Shift-SPACE"): K("C-TAB"), # Cycles through tabs in recently used order
    K("Win-Shift-n"): K("C-Shift-p"), # New private window
}, "Firefox")

# Keybindings for qpdfview
define_keymap(re.compile("qpdfview"), {
    K("Win-Shift-LEFT_BRACE"): K("C-Shift-TAB"), # Win+{ to switch previous tab
    K("Win-Shift-RIGHT_BRACE"): K("C-TAB"),      # Win+} to switch next tab
    K("Win-w"): K("C-w"),                        # Close tab
}, "qpdfview")

# Keybindings for Slack
define_keymap(re.compile("Slack"), {
    K("Win-Alt-Shift-c"): K("C-Alt-Shift-c"), # Code block
    K("Win-LEFT_BRACE"): K("Alt-LEFT"),       # Win+[ to move previous page
    K("Win-RIGHT_BRACE"): K("Alt-RIGHT"),     # Win+] to move next page
    K("Win-Shift-KEY_7"): K("C-Shift-KEY_7"), # Ordered list
    K("Win-Shift-KEY_8"): K("C-Shift-KEY_8"), # Bulleted list
    K("Win-Shift-KEY_9"): K("C-Shift-KEY_9"), # Blockquote
    K("Win-Shift-c"): K("C-Shift-c"),         # Code
    K("Win-Shift-d"): K("C-Shift-d"),         # Show/hide sidebar
    K("Win-Shift-e"): K("C-Shift-e"),         # Directory
    K("Win-Shift-f"): K("C-Shift-f"),         # Toggle full screen
    K("Win-Shift-i"): K("C-Shift-i"),         # Channel info
    K("Win-Shift-k"): K("C-Shift-k"),         # All DMs
    K("Win-Shift-l"): K("C-Shift-l"),         # Browse channels
    K("Win-Shift-m"): K("C-Shift-m"),         # Mentions & reactions
    K("Win-Shift-s"): K("C-Shift-s"),         # Saved items
    K("Win-Shift-t"): K("C-Shift-t"),         # Threads
    K("Win-Shift-u"): K("C-Shift-u"),         # Link
    K("Win-Shift-x"): K("C-Shift-x"),         # Strikethrough
    K("Win-Shift-y"): K("C-Shift-y"),         # Set a status
    K("Win-a"): K("C-a"),                     # Select all
    K("Win-b"): K("C-b"),                     # Bold
    K("Win-c"): K("C-c"),                     # Copy
    K("Win-f"): K("C-f"),                     # Search current conversation
    K("Win-g"): K("C-g"),                     # Search
    K("Win-i"): K("C-i"),                     # Itaric
    K("Win-k"): K("C-k"),                     # Jump to a conversation
    K("Win-v"): K("C-v"),                     # Paste
    K("Win-x"): K("C-x"),                     # Cut
}, "Slack")

# Keybindings for Thunar (File Manager)
define_keymap(re.compile("Thunar"), {
    K("Win-LEFT_BRACE"): K("Alt-LEFT"),           # Win+[ to move previous page
    K("Win-RIGHT_BRACE"): K("Alt-RIGHT"),         # Win+] to move next page
    K("Win-Shift-LEFT_BRACE"): K("C-PAGE_UP"),    # Win+{ to switch previous tab
    K("Win-Shift-RIGHT_BRACE"): K("C-PAGE_DOWN"), # Win+} to switch next tab
    K("Win-down"): K("enter"),                    # Win + ↓ to go to the directory under the cursor
    K("Win-up"): K("Alt-up"),                     # Win + ↑ to go to the parent directory
    K("Win-w"): K("C-w"),                         # Close tab
    K("enter"): K("F2"),                          # Rename file
}, "Thunar (File Manager)")

# Keybindings for Xfce4-Desktop
define_keymap(re.compile("Xfdesktop"), {
    K("enter"): K("F2"), # Rename file
}, "Xfce4-Desktop")

# Keybindings for Xfce4-terminal
define_keymap(re.compile("Xfce4-terminal"), {
    K("C-c"): K("C-Shift-c"),
    K("C-v"): K("C-Shift-v"),
    K("Win-c"): K("C-Shift-c"),
    K("Win-v"): K("C-Shift-v"),
}, "Xfce4-terminal")

# Emacs-like keybindings in non-Emacs applications
define_keymap(lambda wm_class: wm_class not in ("Emacs", "Xfce4-terminal"), {
    K("C-COMMA"): K("C-left"),            # Backward word
    K("C-DOT"): K("C-right"),             # Forward word
    K("C-a"): K("home"),                  # Beginning of line
    K("C-b"): K("left"),                  # Left
    K("C-d"): K("delete"),                # Delete forward char
    K("C-e"): K("end"),                   # End of line
    K("C-f"): K("right"),                 # Right
    K("C-h"): K("backspace"),             # Delete backward char
    K("C-k"): [K("Shift-end"), K("C-x")], # Kill line
    K("C-m"): K("enter"),                 # Newline
    K("C-n"): K("down"),                  # Down
    K("C-p"): K("up"),                    # Up
}, "Emacs-like keys")
