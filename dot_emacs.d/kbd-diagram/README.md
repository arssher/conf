# Keyboard diagram of my Emacs bindings

Draws the bindings from `~/.emacs.d/init.el` as one picture.

## Usage

```sh
./draw-kbd.sh                  # us layout -> us.svg + us.png, next to this README
./draw-kbd.sh dvorak           # any layout name from ergoemacs-layouts.el
./draw-kbd.sh --txt            # also us.txt, the same boards in box characters
./draw-kbd.sh --ergo           # also ergoemacs-mode's own sheets
```

| Variable        | Default                | Meaning                         |
|-----------------|------------------------|---------------------------------|
| `ERGOEMACS_SRC` | `~/dev/ergoemacs-mode` | checkout supplying the layouts  |
| `KBD_OUT`       | this directory         | where the output lands          |
| `KBD_INIT`      | `~/.emacs.d/init.el`   | the init file to draw           |

The checkout is the only prerequisite: `git clone
https://github.com/ergoemacs/ergoemacs-mode ~/dev/ergoemacs-mode`.  Nothing
needs to be byte-compiled or installed, and `ergoemacs-mode` is never turned
on — it is there for the layout vectors and the label tables, and turning it on
would replace the very bindings we are trying to draw.

## What comes out

`us.svg`, about 1560x1130, holding in order:

- a board for the function keys,
- the four rows of the main keyboard, every cell stacking all its layers,
- a board for the navigation cluster,
- the legend,
- a section per prefix key for what no board can hold,

and `us.png`, the same thing rasterised by whichever of inkscape,
rsvg-convert, headless chromium or ImageMagick `convert` is installed.

Each cell row is coloured by its modifier, following the ergoemacs picture:
blue Alt, red Alt+Shift, green Ctrl, magenta Ctrl+Shift, brown Ctrl+Alt.  A
greyed tag with nothing after it means nothing is bound; `λ` means an anonymous
command; `Prefix` means a prefix key, whose contents are in a section further
down.

The boards show the whole global map, Emacs's bindings included, because that
is what a reference chart is for.  The sections at the bottom are the opposite:
only what `init.el` itself added, since listing every stock `C-x` binding would
bury the handful that are mine.

## Three backends, one model

`draw-kbd--cell-data` resolves one key's layers into (TAG . LABEL) pairs and
records every key it looked up.  That is the whole model; each backend only
decides how to draw a cell and what to do with the leftovers.

| Function         | Flag     | Draws                                      |
|------------------|----------|--------------------------------------------|
| `draw-kbd-svg`   | default  | `<rect>` and `<text>`, in colour           |
| `draw-kbd-ascii` | `--txt`  | box-drawing characters, ~100x200           |
| `draw-kbd-ergo`  | `--ergo` | ergoemacs-mode's `kbd-ergo.svg` template   |

Adding a row to `draw-kbd-layers` grows every board in every backend at once.

## Tuning it

| Variable                      | Does                                        |
|-------------------------------|---------------------------------------------|
| `draw-kbd-layers`             | the rows inside each key cell               |
| `draw-kbd-nav-layers`         | the same, for the named keys                |
| `draw-kbd-fkey-layers`        | the same, for the function keys             |
| `draw-kbd-nav-keys`           | which named keys get a box                  |
| `draw-kbd-key-names`          | their cap legends                           |
| `draw-kbd-layer-names`        | what the legend calls each row              |
| `draw-kbd-labels`             | short names for commands ergoemacs-mode     |
|                               | has never heard of                          |
| `draw-kbd-svg-layer-colors`   | the colour of each row                      |
| `draw-kbd-svg-label-width`    | how much room a command name gets           |
| `draw-kbd-svg-font-size`      | and how big it is                           |

Labels come from `ergoemacs-function-short-names` first, then from the command
name with the usual prefixes stripped, truncated to the label width.  The cell
font is monospace on purpose: it makes truncating by character count honest,
which is what lets the cell width be computed rather than measured.

## Why not ergoemacs-mode's own picture

It is where this started — `--ergo` still draws it, and it is the picture at
the bottom of <https://ergoemacs.github.io/>.  It fills numbered placeholders
in an Inkscape SVG, `kbd-ergo.svg`, which has room for four layers per key and
no boxes at all for the arrows.  Of the 103 bindings my `init.el` adds to stock
Emacs it drew 65, and the rest needed a list beside the picture.  Drawing the
SVG directly has no such ceiling.

`--ergo` writes `us-ergo.svg` (Alt, Alt+Shift, Ctrl, Ctrl+Shift) and
`us-ergo-ctrl-meta.svg`, whose `draw-kbd-ergo-layers` re-points the Alt rows at
`control meta` and the Ctrl rows at `hyper`.  That trick works because the four
layers are only a convention: each slot reaches `ergoemacs-theme--svg-elt` as
(INDEX . MODIFIERS) and is resolved with `event-convert-list`, which takes any
modifiers.

## Why it is in the chezmoi repo but not applied

This is a tool that *reads* the applied `~/.emacs.d/init.el`; it is not config
that `~/.emacs.d` needs in order to work.  So `.chezmoiignore` lists
`.emacs.d/kbd-diagram`, and it is run from the source tree.  The generated
`*.txt`, `*.svg` and `*.png` are gitignored — regenerate them rather than
committing them.

## How it works

`draw-kbd.sh` runs `emacs -Q --batch -l draw-kbd.el`, and `draw-kbd.el` loads
`init.el` itself.  That order matters: it snapshots the stock global map before
`init.el` runs, so the sections can tell your bindings from Emacs's.  Each cell
row is then a `lookup-key` in the live global map, and the key it looked up is
recorded, so whatever is left over at the end is exactly what no board showed.

The layout vectors turn out to be a 4x15 grid already, rows padded with empty
strings, unshifted then shifted at +60, so the physical arrangement comes for
free and a renderer only has to place it.

## Things to know

- **Prefix sequences cannot go on a board.**  A prefix key shows as `Prefix`
  and its contents get a section.  This is not a limitation of the format: a
  two-key sequence simply is not a key.
- **`input-decode-map` is invisible here.**  The `C-i` → `H-i` trick in
  `init.el` is a translation, not a binding, so the board shows the raw keymap.
  It is also inside a `window-system` guard, which a batch Emacs never enters.
- `draw-kbd-inhibit-state-writes` detaches `savehist-autosave` and
  `recentf-save-list` from `kill-emacs-hook` before drawing, so the batch Emacs
  leaves `~/.emacs.d/savehist` and `~/.emacs.d/recentf` byte-identical.  It
  detaches the hooks by hand instead of turning the modes off, because
  `(recentf-mode -1)` itself calls `recentf-save-list`.

## Upstream quirks

Both only affect `--ergo`.

- `draw-kbd.el` defines `ergoemacs-M-O-binding`, which `ergoemacs-theme-engine.el`
  still reads although commit `dc2e1a6` dropped its `defvar`.  Without it the
  sheets die with *"Symbol's value as variable is void"*.
- ergoemacs-mode's per-prefix sheets (`full-p`) render empty.
  `ergoemacs-theme--svg-elt` does

      (or (lookup-key ergoemacs-override-keymap key)
          (lookup-key (current-global-map) key))

  and for a two-event sequence the first `lookup-key` returns the integer 1
  ("key sequence too long") rather than nil.  `or` takes that as a hit, the
  global map is never consulted, and the integer is blanked a line later.  A
  single-event key escapes it because a miss there returns nil.
