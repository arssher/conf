#!/bin/sh
# Draw my keybindings.
#
# Writes <layout>.txt: one keyboard whose every cell lists all the modifier
# layers, boards for the function keys and the navigation cluster, and a
# section per prefix key for what no board can hold. With --svg it also draws
# ergoemacs-mode's SVG sheets, which look better and say less.
#
# Usage: ./draw-kbd.sh [--svg] [layout]        (default layout: us)
# Env:   ERGOEMACS_SRC  checkout of ergoemacs-mode   (default ~/dev/ergoemacs-mode)
#        KBD_OUT        output directory             (default this directory)
#        KBD_INIT       init file to draw             (default ~/.emacs.d/init.el)

set -eu

here=$(cd "$(dirname "$0")" && pwd)
: "${ERGOEMACS_SRC:=$HOME/dev/ergoemacs-mode}"
: "${KBD_OUT:=$here}"

svg=
case ${1:-} in
    --svg) svg=1; shift ;;
esac
layout=${1:-us}

export ERGOEMACS_SRC KBD_OUT
export KBD_LAYOUT="$layout"
[ -n "$svg" ] && export KBD_SVG=1

if [ ! -f "$ERGOEMACS_SRC/kbd-ergo.svg" ]; then
    echo "draw-kbd: no kbd-ergo.svg under $ERGOEMACS_SRC" >&2
    echo "draw-kbd: clone https://github.com/ergoemacs/ergoemacs-mode or set ERGOEMACS_SRC" >&2
    exit 1
fi

# -Q so that nothing but the init file under test is loaded. draw-kbd.el loads
# that file itself, after snapshotting the stock global map, so it can tell my
# bindings from the ones Emacs ships with.
emacs -Q --batch -l "$here/draw-kbd.el" 2>&1 | grep -v '^Loading ' || true

txt="$KBD_OUT/$layout.txt"
[ -f "$txt" ] || { echo "draw-kbd: $txt was not produced" >&2; exit 1; }
echo "draw-kbd: wrote $txt"
[ -z "$svg" ] && exit 0

# Rasterise with whatever is installed.  ImageMagick's `convert' is deliberately
# last: without its rsvg delegate it renders these files as garbage.
rasterise() {
    _svg=$1
    _png=${_svg%.svg}.png
    # Size the headless window from the SVG root, so the shot has no scrollbar
    # and no dead margin.
    _w=$(sed -n 's/.*[^-]width="\([0-9.]*\)".*/\1/p' "$_svg" | head -1 | cut -d. -f1)
    _h=$(sed -n 's/.*[^-]height="\([0-9.]*\)".*/\1/p' "$_svg" | head -1 | cut -d. -f1)
    : "${_w:=1178}" "${_h:=613}"

    if command -v inkscape >/dev/null 2>&1; then
        inkscape "$_svg" -o "$_png"
    elif command -v rsvg-convert >/dev/null 2>&1; then
        rsvg-convert -o "$_png" "$_svg"
    elif command -v chromium >/dev/null 2>&1 || command -v google-chrome >/dev/null 2>&1; then
        _browser=$(command -v chromium || command -v google-chrome)
        "$_browser" --headless --disable-gpu --hide-scrollbars \
                    --default-background-color=FFFFFFFF \
                    --window-size="$_w,$_h" --screenshot="$_png" "file://$_svg" 2>/dev/null
    elif command -v convert >/dev/null 2>&1; then
        convert -density 150 -background white "$_svg" "$_png"
    else
        echo "draw-kbd: no SVG rasteriser found, leaving $_svg unconverted" >&2
        return 0
    fi
    echo "draw-kbd: wrote $_svg and $_png"
}

for f in "$KBD_OUT/$layout.svg" "$KBD_OUT/$layout"-*.svg; do
    [ -f "$f" ] && rasterise "$f"
done
