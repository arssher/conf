# Keyboard diagram of my Emacs bindings

Draws the bindings from `~/.emacs.d/init.el` as one picture.

## Usage

```sh
./draw-kbd.sh                  # us layout -> us.svg, us_full.svg and their PNGs
./draw-kbd.sh dvorak           # any layout name from ergoemacs-layouts.el
./draw-kbd.sh --txt            # also us.txt and us_full.txt, in box characters
./draw-kbd.sh --ergo           # also ergoemacs-mode's own sheets
./draw-kbd.sh --scale 4        # a crisper PNG (default 2)
```

| Variable        | Default                | Meaning                         |
|-----------------|------------------------|---------------------------------|
| `ERGOEMACS_SRC` | `~/dev/ergoemacs-mode` | checkout supplying the layouts  |
| `KBD_OUT`       | this directory         | where the output lands          |
| `KBD_INIT`      | `~/.emacs.d/init.el`   | the init file to draw           |
| `KBD_SCALE`     | `2`                    | PNG pixels per SVG unit         |

The checkout is the only prerequisite: `git clone
https://github.com/ergoemacs/ergoemacs-mode ~/dev/ergoemacs-mode`.  Nothing
needs to be byte-compiled or installed, and `ergoemacs-mode` is never turned
on — it is there for the layout vectors and the label tables, and turning it on
would replace the very bindings we are trying to draw.

## What comes out

Two pictures:

| File            | Keyboard                                              |
|-----------------|-------------------------------------------------------|
| `us.svg`        | the main block alone, about 2290x1415                 |
| `us_full.svg`   | plus Print, Ins/Home/PgUp, Del/End/PgDn, arrows; 2933x1281 |

The right-hand cluster is wide and rarely interesting, so it is off by default.
Nothing is lost by that: whatever is bound on those keys is listed under the
short keyboard instead, which is why `us.svg` is narrower but a little taller.

Each holds:

- the keyboard, laid out the way a real one is: function row along the top,
  Backspace at the end of the number row, Tab before `q`, Return after `'`,
  and the space bar below — every cell stacking all its layers,
- the legend,
- a section per prefix key for what no key can hold, in balanced columns
  rather than run down the page,

and a PNG of each, rasterised at `--scale` pixels per SVG unit — 2 by default,
so 4580x2830 and 5866x2562 — by whichever of inkscape, rsvg-convert, headless
chromium or ImageMagick `convert` is installed.  Each takes the scale
differently: inkscape and `convert` as dots per inch against the SVG's nominal
96, rsvg-convert as a zoom, chromium as a device pixel ratio over a window
still sized in CSS pixels.  The SVG is vector and unaffected; only the PNG has
a resolution at all.

The keys are drawn the way the ergoemacs picture draws them: gradient caps
with rounded corners and a dark border, each sitting on a grey slab nudged down
and right, all of them inside a case whose bounding box is whatever the rows
turned out to need.  The slab is a second flat rect rather than a blur filter —
that is upstream's trick, and it is the one thing every rasteriser draws the
same way.  The cap legend is proportional bold sans, like the heading; the rows
under it stay monospace, because that is what makes truncating by character
count honest.

Each cell row is coloured by its modifier, following the ergoemacs picture:
blue Alt, red Alt+Shift, green Ctrl, magenta Ctrl+Shift, brown Ctrl+Alt.  A
greyed tag with nothing after it means nothing is bound; `λ` means an anonymous
command; `Prefix` means a prefix key, whose contents are in a section further
down.

The keyboard shows the whole global map, Emacs's bindings included, because
that is what a reference chart is for.  The sections below it are the opposite:
only what `init.el` itself added, since listing every stock `C-x` binding would
bury the handful that are mine.

## Three backends, one model

`draw-kbd--cell-data` resolves one key's layers into (TAG . LABEL) pairs and
records every key it looked up.  That is the whole model; each backend only
decides how to draw a cell and what to do with the leftovers.

| Function         | Flag     | Draws                                      |
|------------------|----------|--------------------------------------------|
| `draw-kbd-svg`   | default  | `<rect>` and `<text>`, in colour           |
| `draw-kbd-ascii` | `--txt`  | box-drawing characters, us.txt + us_full.txt |
| `draw-kbd-ergo`  | `--ergo` | ergoemacs-mode's `kbd-ergo.svg` template   |

Adding a row to `draw-kbd-layers` grows every key in every backend at once, and
`draw-kbd--keyboard` is the one place that says where a key sits — rows of
segments, each pinned to a column in key widths, so moving a key or adding one
that is not drawn yet is a line there rather than a change to a renderer.  It
also takes the `full` flag that decides whether the right-hand cluster is
drawn, which is the whole difference between `us.svg` and `us_full.svg`.

## Tuning it

| Variable                      | Does                                        |
|-------------------------------|---------------------------------------------|
| `draw-kbd-layers`             | the rows inside each key cell               |
| `draw-kbd-nav-layers`         | the same, for the named keys                |
| `draw-kbd-key-names`          | their cap legends                           |
| `draw-kbd-layer-names`        | what the legend calls each row              |
| `draw-kbd-labels`             | short names for commands ergoemacs-mode     |
|                               | has never heard of                          |
| `draw-kbd-svg-layer-colors`   | the colour of each row                      |
| `draw-kbd-svg-gap-ratio`      | space between keys, per font size           |
| `draw-kbd-svg-stroke-ratio`   | key border weight, per font size            |
| `draw-kbd-svg-radius-ratio`   | its corner radius, per font size            |
| `draw-kbd-svg-shadow-ratio`   | how far its slab sticks out, per font size  |
| `draw-kbd-svg-key-stroke`     | the border's colour                         |
| `draw-kbd-svg-key-shadow`     | the slab's                                  |
| `draw-kbd-svg-key-fill`       | top and bottom of the gradient down a cap   |
| `draw-kbd-svg-case-fill`      | the same, for the case the keys sit in      |
| `draw-kbd-svg-case-stroke`    | its border                                  |
| `draw-kbd-svg-panel-fill`     | the fill behind a prefix section            |
| `draw-kbd-svg-panel-stroke`   | and its border                              |
| `draw-kbd-svg-head-font`      | the cap legend's face                       |
| `draw-kbd-svg-head-ratio`     | its size, per font size                     |
| `draw-kbd-svg-label-width`    | how much room a command name gets           |
| `draw-kbd-svg-line-ratio`     | line height, per font size                  |
| `draw-kbd-svg-font-size`      | how big everything is                       |
| `draw-kbd-ascii-label-width`  | the same room, in the `--txt` backend       |
| `draw-kbd-title`              | what the heading calls this                 |

`draw-kbd-svg-font-size` is the one knob for the size of the whole picture:
line height, cell padding, cell width, the gaps between keys, the border
weight, the row stagger, the heading, the legend spacing and the canvas are all
derived from it, so raising it scales the drawing rather than
making the text collide with the boxes.

Labels come from `ergoemacs-function-short-names` first, then from the command
name with the usual prefixes stripped, truncated to the label width.  The row
font is monospace on purpose: it makes truncating by character count honest,
which is what lets the cell width be computed rather than measured — there is
no way to measure a glyph in a batch Emacs.  The cap legend escapes that
because it is short, fixed in place and never truncated, which is why it can be
proportional.

## Why not ergoemacs-mode's own picture

It is where this started — `--ergo` still draws it, it is the picture at the
bottom of <https://ergoemacs.github.io/>, and `draw-kbd-svg` now looks like it
on purpose.  What it could not do was hold the content.  It fills numbered
placeholders in an Inkscape SVG, `kbd-ergo.svg`, which has room for four layers
per key and no boxes at all for the arrows.  Of the 103 bindings my `init.el` adds to stock
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
recorded, so whatever is left over at the end is exactly what the keyboard did
not show.  `draw-kbd--pack` then lays those sections out: it tries every column
count and keeps the shortest arrangement that fits the width, which is what
stops one long section setting the height of everything beside it.

The layout vectors turn out to be a 4x15 grid already, rows padded with empty
strings, unshifted then shifted at +60, so the physical arrangement comes for
free and a renderer only has to place it.

## Things to know

- **Prefix sequences cannot go on the keyboard.**  A prefix key shows as `Prefix`
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
