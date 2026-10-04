# Keyboard diagram of my Emacs bindings

Draws the bindings from `~/.emacs.d/init.el` as a keyboard, in one text file.

## Usage

```sh
./draw-kbd.sh            # us layout -> us.txt, next to this README
./draw-kbd.sh dvorak     # any layout name from ergoemacs-layouts.el
./draw-kbd.sh --svg      # also draw the ergoemacs SVG sheets
```

| Variable        | Default                | Meaning                         |
|-----------------|------------------------|---------------------------------|
| `ERGOEMACS_SRC` | `~/dev/ergoemacs-mode` | checkout supplying the layouts  |
| `KBD_OUT`       | this directory         | where the output lands          |
| `KBD_INIT`      | `~/.emacs.d/init.el`   | the init file to draw           |

The checkout is the only prerequisite: `git clone
https://github.com/ergoemacs/ergoemacs-mode ~/dev/ergoemacs-mode`.  Nothing
needs to be byte-compiled or installed.

## What comes out

`us.txt`, about 100 lines and 200 columns wide, holding in order:

- a board for the function keys,
- the four rows of the main keyboard, every cell listing all its layers,
- a board for the navigation cluster,
- a section per prefix key for what no board can hold,
- a count of what was drawn.

```
┌────────────┬────────────┬────────────┬────────────┐
│ e          │ r          │ t          │ y          │
│M  ⌫ word   │M  ⌦ word   │M  transpos…│M  yank pop │
│MS          │MS projecti…│MS          │MS          │
│C  → line   │C  rep      │C  transpose│C  ⌧ line   │
│CS          │CS          │CS          │CS          │
│CM end of d…│CM revert   │CM transpos…│CM          │
└────────────┴────────────┴────────────┴────────────┘
```

A blank means nothing is bound; `λ` means an anonymous command; `Prefix` means
a prefix key, whose contents are in a section further down.

The boards show the whole global map, Emacs's bindings included, because that
is what a reference chart is for.  The sections at the bottom are the opposite:
only what `init.el` itself added, since listing every stock `C-x` binding would
bury the handful that are mine.

## Tuning it

Everything worth changing is a defvar at the top of `draw-kbd.el`:

| Variable                       | Does                                        |
|--------------------------------|---------------------------------------------|
| `draw-kbd-ascii-layers`        | the rows inside each key cell               |
| `draw-kbd-ascii-nav-layers`    | the same, for the named keys                |
| `draw-kbd-ascii-fkey-layers`   | the same, for the function keys             |
| `draw-kbd-ascii-nav-keys`      | which named keys get a box                  |
| `draw-kbd-ascii-key-names`     | their cap legends                           |
| `draw-kbd-ascii-label-width`   | how much room a command name gets           |
| `draw-kbd-labels`              | short names for commands ergoemacs-mode     |
|                                | has never heard of                          |

Add a row to `draw-kbd-ascii-layers` and every board grows one.  Labels come
from `ergoemacs-function-short-names` first, then from the command name with
the usual prefixes stripped, truncated to `draw-kbd-ascii-label-width`.

## Why text and not the picture

ergoemacs-mode draws the keyboard picture at the bottom of
<https://ergoemacs.github.io/> by filling placeholders in an Inkscape SVG,
`kbd-ergo.svg`.  It is a nicer thing to look at, and `--svg` still produces it,
but it has room for four layers per key and no boxes at all for the arrows, so
it cannot show everything: of the 103 bindings my `init.el` adds to stock
Emacs, the sheets drew 65.  Text has no such ceiling, and it greps and diffs.

With `--svg` you get `us.svg` / `us.png` (Alt, Alt+Shift, Ctrl, Ctrl+Shift) and
`us-ctrl-meta.svg` / `.png`, whose `draw-kbd-extra-layers` re-points the Alt
rows at `control meta` and the Ctrl rows at `hyper`.  That trick works because
the four layers are only a convention: each slot reaches
`ergoemacs-theme--svg-elt` as (INDEX . MODIFIERS) and is resolved with
`event-convert-list`, which takes any modifiers.

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

`ergoemacs-mode` is loaded but **never turned on** — it is there for the layout
vectors and the label tables, and turning it on would replace the very
bindings we are trying to draw.

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

- `draw-kbd.el` defines `ergoemacs-M-O-binding`, which `ergoemacs-theme-engine.el`
  still reads although commit `dc2e1a6` dropped its `defvar`.  Without it the
  `--svg` path dies with *"Symbol's value as variable is void"*.
- ergoemacs-mode's own per-prefix sheets (`full-p`) render empty.
  `ergoemacs-theme--svg-elt` does

      (or (lookup-key ergoemacs-override-keymap key)
          (lookup-key (current-global-map) key))

  and for a two-event sequence the first `lookup-key` returns the integer 1
  ("key sequence too long") rather than nil.  `or` takes that as a hit, the
  global map is never consulted, and the integer is blanked a line later.  A
  single-event key escapes it because a miss there returns nil.
