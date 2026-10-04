# Keyboard diagram of my Emacs bindings

Paints the bindings from `~/.emacs.d/init.el` onto the SVG keyboard template
that ships with [ergoemacs-mode][], the same picture that appears at the bottom
of <https://ergoemacs.github.io/>.

[ergoemacs-mode]: https://github.com/ergoemacs/ergoemacs-mode

## Usage

```sh
./draw-kbd.sh            # us layout, next to this README
./draw-kbd.sh dvorak     # any layout name from ergoemacs-layouts.el
```

Two sheets come out of each run:

| File                   | Shows                                             |
|------------------------|---------------------------------------------------|
| `us.svg` / `.png`      | Alt, Alt+Shift, Ctrl, Ctrl+Shift                  |
| `us-ctrl-meta.svg`     | Ctrl+Alt, Ctrl+Alt+Shift, and Hyper if ever bound |
| `us-not-drawn.txt`     | every binding the two sheets leave out            |

The listing is the honest half of the picture.  A diagram you use as a
reference is dangerous when it silently omits things, so each run diffs the
global map against stock Emacs, drops what the sheets cover, and writes down
the rest:

```
  drawn       65
  not drawn   38

Under a prefix key (25)
  C-x 4                  my-split-root-window-below
  ...
No box for that key (13)
  C-<left>               shrink-window-horizontally
  ...
```

The third bucket it can print, "No slot for that modifier combination", is
empty as long as `draw-kbd-extra-layers` covers what you actually bind.

| Variable        | Default                | Meaning                         |
|-----------------|------------------------|---------------------------------|
| `ERGOEMACS_SRC` | `~/dev/ergoemacs-mode` | checkout supplying the template |
| `KBD_OUT`       | this directory         | where the output lands          |
| `KBD_INIT`      | `~/.emacs.d/init.el`   | the init file to draw           |

The checkout is the only prerequisite: `git clone
https://github.com/ergoemacs/ergoemacs-mode ~/dev/ergoemacs-mode`.  Nothing
needs to be byte-compiled or installed.

For the PNG the script uses the first of inkscape, rsvg-convert, headless
chromium, or ImageMagick `convert` that it finds.  `convert` is last on purpose:
without its rsvg delegate it renders this file as garbage.

## Why it is in the chezmoi repo but not applied

This is a tool that *reads* the applied `~/.emacs.d/init.el`; it is not config
that `~/.emacs.d` needs in order to work.  So `.chezmoiignore` lists
`.emacs.d/kbd-diagram`, and it is run from the source tree.  The generated
`*.svg` / `*.png` are gitignored — regenerate them rather than committing them.

## How it works

`draw-kbd.sh` runs `emacs -Q --batch -l draw-kbd.el`, and `draw-kbd.el` loads
`init.el` itself.  That order matters: it snapshots the stock global map before
`init.el` runs, so the listing can tell your bindings from the ones Emacs ships
with.  It then:

1. loads `ergoemacs-mode` but **never turns it on**.  `ergoemacs-theme--svg`
   looks each key up in `ergoemacs-override-keymap` and then falls back to
   `(current-global-map)` — with the mode off the first is empty, so what gets
   painted is exactly what `init.el` bound;
2. walks `kbd-ergo.svg`, whose `>M17<`, `>C77<`, `>T17<`, `>NF1<` … placeholders
   stand for "Alt+ this key", "Ctrl+Shift+ this key", the key's own character,
   and the function-key labels;
3. labels each binding from `ergoemacs-function-short-names`, with
   `draw-kbd-labels` prepended for commands ergoemacs-mode has never heard of;
4. writes into a throwaway temp directory, so no run can read a stale cache or
   leave `ergoemacs-extras/` behind, and copies the result here.

## Things to know

- **Labels are truncated to 10 characters.**  Long command names show up as
  `ggtags nex…`; give them an entry in `draw-kbd-labels` in `draw-kbd.el`.
- **The template has four layers per key.**  Which four is only a convention:
  each slot reaches `ergoemacs-theme--svg-elt` as (INDEX . MODIFIERS) and is
  resolved with `event-convert-list`, which accepts any modifiers.  That is how
  the second sheet works — `draw-kbd-extra-layers` re-points the Alt rows at
  `control meta` and the Ctrl rows at `hyper`.  Edit it to chase some other
  combination, or set it to nil for one sheet only.
- **Prefix sequences are still not drawn** on a sheet; they are listed in
  `*-not-drawn.txt` instead.  A prefix key shows only as "Prefix Key".  ergoemacs-mode has a
  `full-p` mode meant for exactly this, which emits `<theme>-<layout>-C-x.svg`
  and friends, but the sheets come out blank — see "Upstream quirks" below.
  Even working, it would only cover *modified* second keys (`C-x C-u`); plain
  ones like `C-x 4` have no slot, because there is no unmodified layer.
- **Keys remapped through `input-decode-map`** (the `C-i` → `H-i` trick in
  `init.el`) are drawn as the raw keymap has them, not as they are typed.
- `draw-kbd-inhibit-state-writes` detaches `savehist-autosave` and
  `recentf-save-list` from `kill-emacs-hook` before generating, so the batch
  Emacs leaves `~/.emacs.d/savehist` and `~/.emacs.d/recentf` byte-identical.
  It detaches the hooks by hand instead of turning the modes off, because
  `(recentf-mode -1)` itself calls `recentf-save-list`.
## Upstream quirks

- `draw-kbd.el` defines `ergoemacs-M-O-binding`, which `ergoemacs-theme-engine.el`
  still reads although commit `dc2e1a6` dropped its `defvar`.  Without it
  generation dies with *"Symbol's value as variable is void"*.
- The per-prefix sheets (`full-p`) render empty.  `ergoemacs-theme--svg-elt`
  does

      (or (lookup-key ergoemacs-override-keymap key)
          (lookup-key (current-global-map) key))

  and for a two-event sequence the first `lookup-key` returns the integer 1
  ("key sequence too long") rather than nil.  `or` takes that as a hit, the
  global map is never consulted, and the integer is blanked a line later.  A
  single-event key escapes it because a miss there returns nil.
