#!/bin/sh
# Draw my keybindings on a keyboard picture.
#
# Loads ~/.emacs.d/init.el in batch mode, paints the resulting global keymap
# onto ergoemacs-mode's SVG keyboard template, and rasterises it to PNG.
#
# Usage: ./draw-kbd.sh [layout]          (default layout: us)
# Env:   ERGOEMACS_SRC  checkout of ergoemacs-mode   (default ~/dev/ergoemacs-mode)
#        KBD_OUT        output directory             (default this directory)

set -eu

here=$(cd "$(dirname "$0")" && pwd)
: "${ERGOEMACS_SRC:=$HOME/dev/ergoemacs-mode}"
: "${KBD_OUT:=$here}"
layout=${1:-us}

export ERGOEMACS_SRC KBD_OUT
export KBD_LAYOUT="$layout"

if [ ! -f "$ERGOEMACS_SRC/kbd-ergo.svg" ]; then
    echo "draw-kbd: no kbd-ergo.svg under $ERGOEMACS_SRC" >&2
    echo "draw-kbd: clone https://github.com/ergoemacs/ergoemacs-mode or set ERGOEMACS_SRC" >&2
    exit 1
fi

# -Q keeps the batch Emacs from loading init.el twice; we load it explicitly so
# that a failure in it is visible instead of silently skipped.
emacs -Q --batch \
      -l "$HOME/.emacs.d/init.el" \
      -l "$here/draw-kbd.el" 2>&1 | grep -v '^Loading ' || true

svg="$KBD_OUT/$layout.svg"
png="$KBD_OUT/$layout.png"
[ -f "$svg" ] || { echo "draw-kbd: $svg was not produced" >&2; exit 1; }

# Rasterise with whatever is installed.  ImageMagick's `convert' is deliberately
# last: without its rsvg delegate it renders this file as garbage.
w=$(sed -n 's/.*[^-]width="\([0-9.]*\)".*/\1/p' "$svg" | head -1 | cut -d. -f1)
h=$(sed -n 's/.*[^-]height="\([0-9.]*\)".*/\1/p' "$svg" | head -1 | cut -d. -f1)
: "${w:=1178}" "${h:=613}"

if command -v inkscape >/dev/null 2>&1; then
    inkscape "$svg" -o "$png"
elif command -v rsvg-convert >/dev/null 2>&1; then
    rsvg-convert -o "$png" "$svg"
elif command -v chromium >/dev/null 2>&1 || command -v google-chrome >/dev/null 2>&1; then
    browser=$(command -v chromium || command -v google-chrome)
    "$browser" --headless --disable-gpu --hide-scrollbars \
               --default-background-color=FFFFFFFF \
               --window-size="$w,$h" --screenshot="$png" "file://$svg" 2>/dev/null
elif command -v convert >/dev/null 2>&1; then
    convert -density 150 -background white "$svg" "$png"
else
    echo "draw-kbd: wrote $svg (no SVG rasteriser found, skipping PNG)" >&2
    exit 0
fi

echo "draw-kbd: wrote $svg and $png"
