#!/bin/bash
#
# Duplicate the current Finder tab: open a new tab on the folder this one
# shows.  cmd+T opens a tab at the "New Finder windows show" folder instead,
# and Finder has no duplicate command, menu item or setting.
#
# Karabiner runs this on left cmd + t in Finder.  It reads the front window's
# folder, presses cmd+T itself through System Events, waits for the new tab
# to come up, and points it at that folder.  Reading before pressing is what
# makes the order certain.
#
# The first version left the cmd+T to karabiner, after a 150ms no-op key that
# gave this script time to read the folder first.  That needed no
# Accessibility grant -- only Automation for Finder -- but the pause was
# noticeable, and anything much shorter races osascript's own 35-45ms start.
# Pressing the key from here costs an Accessibility grant for karabiner's
# helper instead, which is little to add for a program that already sees
# every key.  On a fresh machine the first press asks for it, and does
# nothing until it is given.
#
# key code 17 rather than keystroke "t": keystroke resolves the character
# through the current layout, which under russian has no t.
#
# A window with no folder behind it -- Recents, a search, a smart folder --
# has nothing to copy, and gets a plain new tab.

set -u

# Detach, as annotate_clipboard.sh does and for the same reason: karabiner
# kills a running shell_command when the next one starts, and this polls for
# up to a second.  Measured, the extra exec costs about a millisecond.
if [ "${FINDER_DUPLICATE_TAB_DETACHED-}" != 1 ]; then
    FINDER_DUPLICATE_TAB_DETACHED=1 /usr/bin/nohup "$0" "$@" >/dev/null 2>&1 &
    exit 0
fi

exec /usr/bin/osascript >/dev/null 2>&1 <<'EOF'
set t0 to missing value
tell application "Finder"
    if (count of Finder windows) > 0 then
        try
            set w0 to Finder window 1
            set id0 to id of w0
            set t0 to (target of w0) as alias
        end try
    end if
end tell

tell application "System Events" to key code 17 using command down

if t0 is missing value then return -- Recents, a search: plain new tab
set p0 to t0 as text
tell application "Finder"
    -- Whether AppleScript shows the new tab as a new front window or as the
    -- same window with a new target, either change marks it.  A tab that
    -- opened on this very folder already is what was wanted.
    repeat 50 times
        delay 0.02
        try
            set w to Finder window 1
            if id of w is not id0 then
                set target of w to t0
                return
            end if
            if ((target of w) as alias as text) is not p0 then
                set target of w to t0
                return
            end if
        on error
            -- a target with no folder, such as Recents: the new tab
            set target of Finder window 1 to t0
            return
        end try
    end repeat
end tell
EOF
