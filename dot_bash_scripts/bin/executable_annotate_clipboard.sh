#!/bin/bash
#
# Annotate the image on the clipboard, and put the annotated version back.
#
# macOS has both ends of this and not the middle.  shift+cmd+4 copies a region
# straight to the clipboard -- com.apple.screencapture's target, set in
# bootstrap_mac.sh -- and Markup draws on an image.  Nothing joins them: the
# floating thumbnail that opens Markup does not appear at all when the
# destination is the clipboard, and its Done button saves a file rather than
# copying.  So this spends a temp file to join the two, and leaves the capture
# itself to macOS.
#
# Preview rather than Markup proper, which is the nicer editor: Markup is
# reachable only from that thumbnail and from Quick Look, and neither is
# scriptable.  Preview has the same tools behind shift+cmd+A, and once shown
# the toolbar stays shown for the next document.
#
# Two ways out of Preview, both built in, and both land on the clipboard:
#   cmd+S        the save is watched for below, which copies the file
#   cmd+A cmd+C  Preview copies the rendered pixels itself, annotations and all
#
# With --wait, wait for a *new* image to arrive on the clipboard before
# opening anything.  That is for pairing with a synthesized shift+cmd+3 or 4
# in the same karabiner rule: the capture and this script come up together,
# and the editor opens once the shot has been taken.  Waiting for the
# clipboard to change, rather than clearing it first and waiting for anything
# at all, is what keeps a cancelled capture (escape) from costing whatever was
# on the clipboard already.
#
# karabiner has right option + 3 and 4 on the --wait form, the whole screen
# and a region, as shift+cmd+3 and 4 are.  Without the flag this
# annotates an image already on the clipboard, whatever put it there, and is
# left unbound -- right option + a, the obvious key, is wanted for Claude
# desktop -- so for now that mode is a thing to run by hand.

set -u

# Step out of karabiner's way before doing anything slow. Karabiner runs one
# shell_command at a time and forcibly terminates the running one when the
# next is invoked, so reaching for an app launcher would kill this mid-wait or
# mid-watch. Detached, karabiner's command finishes in milliseconds and has
# nothing left to kill. nohup for the same reason, and the variable is what
# stops the detached copy detaching again.
if [ "${ANNOTATE_CLIPBOARD_DETACHED-}" != 1 ]; then
    ANNOTATE_CLIPBOARD_DETACHED=1 /usr/bin/nohup "$0" "$@" >/dev/null 2>&1 &
    exit 0
fi

osa() { /usr/bin/osascript "$@" >/dev/null; }

notify() {
    /usr/bin/osascript -e "display notification \"$1\" with title \
\"annotate_clipboard\"" >/dev/null 2>&1
}

# The whole format-and-size list, so a second capture of the same region still
# reads as a change -- only identical pixels would not.
clipboard_state() { /usr/bin/osascript -e 'clipboard info' 2>/dev/null; }

has_image() { case "$(clipboard_state)" in *PNGf*) return 0;; esac; return 1; }

if [ "${1-}" = "--wait" ]; then
    before=$(clipboard_state)
    deadline=$(( $(/bin/date +%s) + 60 ))
    while :; do
        [ "$(/bin/date +%s)" -lt "$deadline" ] || exit 0   # cancelled
        now=$(clipboard_state)
        if [ "$now" != "$before" ] && has_image; then
            break
        fi
        /bin/sleep 0.3
    done
elif ! has_image; then
    notify "No image on the clipboard."
    exit 1
fi

tmp="${TMPDIR:-/tmp}"      # TMPDIR is per-user, and ends in a slash where
dir="${tmp%/}/annotate_clipboard"          # the fallback does not
/bin/mkdir -p "$dir" || exit 1
# Yesterday's are finished with.  Today's are not: Preview may still have one
# open, and it reads the file from disk every time it saves.
/usr/bin/find "$dir" -type f -mtime +1 -delete 2>/dev/null

png="$dir/$(/bin/date +%Y%m%d-%H%M%S).png"

if ! osa -e "set f to open for access (POSIX file \"$png\") \
with write permission" \
         -e 'set eof f to 0' \
         -e 'write (the clipboard as «class PNGf») to f' \
         -e 'close access f' 2>/dev/null; then
    /bin/rm -f "$png"
    notify "Could not read the clipboard as an image."
    exit 1
fi

/usr/bin/open -a Preview "$png" || exit 1

# Copy back on every save, by polling the mtime.  Nothing announces a save:
# asking Preview would be an Apple Events call, and the first one raises a TCC
# dialog that a karabiner shell command cannot answer.  Preview holds no
# descriptor on the file either -- it reads the document and closes it, so
# lsof shows nothing to watch.
#
# What is left is the process.  Preview quitting is the real end of the job,
# and two minutes is the backstop for the case it stays open on something
# else: that is the whole budget for choosing a region and drawing on it, and
# a missed auto-copy still leaves cmd+A cmd+C, which needs no watcher at all.
for _ in 1 2 3 4 5 6 7 8 9 10; do        # it was only just asked to open
    /usr/bin/pgrep -qx Preview && break
    /bin/sleep 0.5
done

stamp=$(/usr/bin/stat -f %m "$png")
deadline=$(( $(/bin/date +%s) + 120 ))
while [ "$(/bin/date +%s)" -lt "$deadline" ]; do
    /bin/sleep 1
    now=$(/usr/bin/stat -f %m "$png" 2>/dev/null) || now=$stamp
    [ -n "$now" ] || now=$stamp
    if [ "$now" != "$stamp" ]; then
        stamp=$now
        /bin/sleep 1    # let Preview finish writing before reading it back
        osa -e "set the clipboard to (read (POSIX file \"$png\") \
as «class PNGf»)" && notify "Annotated image copied."
    fi
    # After the save check, not before: save-then-quit inside one second must
    # still copy.
    /usr/bin/pgrep -qx Preview || break
done
