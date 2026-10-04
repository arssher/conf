# Keyboard diagram of my Emacs bindings

Paints the bindings from `~/.emacs.d/init.el` onto the SVG keyboard template
that ships with [ergoemacs-mode][], the same picture that appears at the bottom
of <https://ergoemacs.github.io/>.

[ergoemacs-mode]: https://github.com/ergoemacs/ergoemacs-mode

## Usage

```sh
./draw-kbd.sh            # us layout -> us.svg + us.png, next to this README
./draw-kbd.sh dvorak     # any layout name from ergoemacs-layouts.el
```

| Variable        | Default                | Meaning                        |
|-----------------|------------------------|--------------------------------|
| `ERGOEMACS_SRC` | `~/dev/ergoemacs-mode` | checkout supplying the template |
| `KBD_OUT`       | this directory         | where the output lands          |

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

`draw-kbd.sh` batch-loads `init.el`, then loads `draw-kbd.el`, which:

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
- **The template has four layers per key** — Alt, Alt+Shift, Ctrl, Ctrl+Shift.
  `C-M-` bindings have nowhere to go, and a prefix key shows only as
  "Prefix Key"; the keys under it are not drawn.
- **Keys remapped through `input-decode-map`** (the `C-i` → `H-i` trick in
  `init.el`) are drawn as the raw keymap has them, not as they are typed.
- `draw-kbd-inhibit-state-writes` detaches `savehist-autosave` and
  `recentf-save-list` from `kill-emacs-hook` before generating, so the batch
  Emacs leaves `~/.emacs.d/savehist` and `~/.emacs.d/recentf` byte-identical.
  It detaches the hooks by hand instead of turning the modes off, because
  `(recentf-mode -1)` itself calls `recentf-save-list`.
- `draw-kbd.el` defines `ergoemacs-M-O-binding`, which `ergoemacs-theme-engine.el`
  still reads although commit `dc2e1a6` dropped its `defvar`.  Without it
  generation dies with *"Symbol's value as variable is void"*.
