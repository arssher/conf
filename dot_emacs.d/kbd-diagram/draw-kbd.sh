#!/bin/sh
# Draw my keybindings.
#
# Writes <layout>.svg and <layout>.png: the keyboard with every modifier layer
# stacked in each key cell, and a section per prefix key for what no key can
# hold. Also <layout>_full.svg, the same with Print, Insert/Home/PgUp,
# Delete/End/PgDn and the arrows, which are wide and rarely interesting --
# whatever is bound on them is listed under the short picture anyway.
#
# Usage: ./draw-kbd.sh [--txt] [--ergo] [--scale N] [layout]   (default: us)
#   --txt      also write <layout>.txt, the same boards in box-drawing characters
#   --ergo     also draw ergoemacs-mode's own sheets, which look better and say
#              less: four layers per key and no boxes for the arrows
#   --scale N  how many PNG pixels per SVG unit (default 2). The SVG is vector
#              and unaffected; this only sets how crisp the PNG is.
#
# Env:   ERGOEMACS_SRC  checkout of ergoemacs-mode   (default ~/dev/ergoemacs-mode)
#        KBD_OUT        output directory             (default this directory)
#        KBD_INIT       init file to draw            (default ~/.emacs.d/init.el)
#        KBD_SCALE      same as --scale

set -eu

here=$(cd "$(dirname "$0")" && pwd)
: "${ERGOEMACS_SRC:=$HOME/dev/ergoemacs-mode}"
: "${KBD_OUT:=$here}"

ergo=
: "${KBD_SCALE:=2}"
scale=$KBD_SCALE
while [ $# -gt 0 ]; do
    case $1 in
        --txt)   export KBD_TXT=1;  shift ;;
        --ergo)  export KBD_ERGO=1; ergo=1; shift ;;
        --scale) scale=${2:-}; shift 2 ;;
        --*)     echo "draw-kbd: unknown option $1" >&2; exit 2 ;;
        *)       break ;;
    esac
done

case $scale in
    ''|*[!0-9]*) echo "draw-kbd: --scale wants a positive integer, got '$scale'" >&2
                 exit 2 ;;
    0)           echo "draw-kbd: --scale 0 makes no picture" >&2; exit 2 ;;
esac
dpi=$((96 * scale))
layout=${1:-us}

export ERGOEMACS_SRC KBD_OUT
export KBD_LAYOUT="$layout"

if [ ! -f "$ERGOEMACS_SRC/ergoemacs-layouts.el" ]; then
    echo "draw-kbd: no ergoemacs-layouts.el under $ERGOEMACS_SRC" >&2
    echo "draw-kbd: clone https://github.com/ergoemacs/ergoemacs-mode or set ERGOEMACS_SRC" >&2
    exit 1
fi

# -Q so that nothing but the init file under test is loaded. draw-kbd.el loads
# that file itself, after snapshotting the stock global map, so it can tell my
# bindings from the ones Emacs ships with.
emacs -Q --batch -l "$here/draw-kbd.el" 2>&1 | grep -v '^Loading ' || true

svg="$KBD_OUT/$layout.svg"
[ -f "$svg" ] || { echo "draw-kbd: $svg was not produced" >&2; exit 1; }

# Rasterise with whatever is installed.  ImageMagick's `convert' is deliberately
# last: without its rsvg delegate it renders these files as garbage.
rasterise() {
    _svg=$1
    _png=${_svg%.svg}.png
    # Size the headless window from the SVG root, so the shot has no scrollbar
    # and no dead margin.
    _w=$(sed -n 's/.*[^-]width="\([0-9.]*\)".*/\1/p' "$_svg" | head -1 | cut -d. -f1)
    _h=$(sed -n 's/.*[^-]height="\([0-9.]*\)".*/\1/p' "$_svg" | head -1 | cut -d. -f1)
    : "${_w:=1600}" "${_h:=1200}"

    # Each of these takes the scale differently: inkscape and convert as dots
    # per inch against the SVG's nominal 96, rsvg-convert as a zoom, and
    # chromium as a device pixel ratio over a window still sized in CSS pixels.
    if command -v inkscape >/dev/null 2>&1; then
        inkscape "$_svg" -d "$dpi" -o "$_png"
    elif command -v rsvg-convert >/dev/null 2>&1; then
        rsvg-convert -z "$scale" -o "$_png" "$_svg"
    elif command -v chromium >/dev/null 2>&1 || command -v google-chrome >/dev/null 2>&1; then
        _browser=$(command -v chromium || command -v google-chrome)
        "$_browser" --headless --disable-gpu --hide-scrollbars \
                    --force-device-scale-factor="$scale" \
                    --default-background-color=FFFFFFFF \
                    --window-size="$_w,$_h" --screenshot="$_png" "file://$_svg" 2>/dev/null
    elif command -v convert >/dev/null 2>&1; then
        convert -density "$dpi" -background white "$_svg" "$_png"
    else
        echo "draw-kbd: no SVG rasteriser found, leaving $_svg unconverted" >&2
        return 0
    fi
    echo "draw-kbd: wrote $_svg and $_png ($((_w * scale))x$((_h * scale)))"
}

for f in "$svg" "$KBD_OUT/${layout}_full.svg"; do
    [ -f "$f" ] && rasterise "$f"
done
if [ -n "$ergo" ]; then
    for f in "$KBD_OUT/$layout"-ergo*.svg; do
        [ -f "$f" ] && rasterise "$f"
    done
fi
