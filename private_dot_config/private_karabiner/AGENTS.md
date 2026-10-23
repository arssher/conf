# Checking karabiner.edn against karabiner.json

`karabiner.edn` is a hand-maintained mirror of `private_karabiner.json`,
written in [goku](https://github.com/yqrashawn/GokuRakuJoudo)'s EDN.  Nothing
compiles one from the other -- see readme.txt for why the migration was not
taken -- so the two drift apart the moment one is edited alone.

This is how to check them, without installing goku.  Repo-only: both this file
and the .edn are `.chezmoiignore`d, so neither reaches `$HOME`.

## Why not just `brew install`

The karabiner goku is `yqrashawn/goku/goku`, a third-party tap, and brew will
not load an untrusted tap's formula without `brew trust`, which lets that tap's
code run unprompted.  Worse, plain `brew install goku` silently installs
something else entirely -- brew core has an unrelated `goku`, an HTTP load
tester.  And goku writes `~/.config/karabiner/karabiner.json` with no flag to
redirect it, so a stray run clobbers the live config.

So: the pinned release binary, run under a sandbox, with `$HOME` pointed
somewhere harmless.

## Setup

Work in a scratch directory.  `$W` below is that directory.

```sh
mkdir -p "$W"/goku_dl && cd "$W"
curl -sL -o goku_dl/goku-arm.zip \
  https://github.com/yqrashawn/GokuRakuJoudo/releases/download/v0.8.0/goku-arm.zip
shasum -a 256 goku_dl/goku-arm.zip
# must equal the sha the formula pins:
# c912f151f01067eac4effe36c278e5b3a5274febdbfdbe58de5556ba36461008
unzip -q goku_dl/goku-arm.zip -d goku_bin      # -> goku_bin/goku/{goku,gokuw}
```

v0.8.0 is both the pinned and the latest release (Feb 2025).  The bundle is
ad-hoc signed and `codesign -v` fails on it after unzipping, which is expected
for a self-built GraalVM binary and another reason to keep it jailed.

goku shells out to `joker` to lint the EDN -- literally
`$SHELL -i -c "joker --lint <path>"`, an interactive shell, which is why the
run below sets `SHELL` and why `env -i` alone makes it die with
`Cannot run program ""`.  joker is in brew **core**, so it needs no tap:

```sh
brew install joker        # brew uninstall joker when done; nothing else uses it
```

## The sandbox

Deny-default does not work: on macOS 26 a profile with `(deny file-read*)`
kills the process before `main`, with SIGABRT, no stderr and nothing in the
log, and no allow-list of `/usr/lib`, `/System`, `/bin`, the dyld cache or the
Preboot cryptexes rescues it.  Deny-by-exception does work, and covers what
actually matters.  Write this as `$W/jail.sb`, with the two paths spelled out
in full -- `$HOME` must be the real one, `$W` your scratch directory:

```scheme
(version 1)
(allow default)

;; no talking to anything
(deny network*)

;; read nothing of the real home -- secrets, keys, the live config
(deny file-read* (subpath "/Users/YOU"))

;; write nowhere but the jail
(deny file-write*)
(allow file-write*
       (subpath "/path/to/W/jail")
       (literal "/dev/null") (literal "/dev/stdout") (literal "/dev/stderr"))
```

Verified boundaries, each probed rather than assumed: reads of `$HOME` and of
the live `karabiner.json` fail with `Operation not permitted`; writes outside
`jail/` fail the same way; HTTPS by hostname fails to resolve, and straight to
`1.1.1.1`, bypassing DNS, returns `http=000` where unsandboxed it is `301`.
Reads of `/usr`, `/System` and `/private/tmp` stay open -- this is "nothing of
yours, no network, no writes outside the jail", not a true capability box.
`sandbox-exec` is formally deprecated by Apple; Homebrew still relies on it.

## Running it

```sh
cd "$W"
mkdir -p jail/home/.config/karabiner
cp /path/to/chezmoi/private_dot_config/private_karabiner/karabiner.edn \
   jail/home/.config/karabiner.edn          # note: NOT inside karabiner/
cp ~/.config/karabiner/karabiner.json now.json

env -i HOME="$PWD/jail/home" PATH=/opt/homebrew/bin:/usr/bin:/bin \
       TERM=dumb SHELL=/bin/zsh \
  sandbox-exec -f jail.sb "$PWD/goku_bin/goku/goku" -d > out.json 2> out.err
```

`-d` is a dry run: it prints the compiled config to stdout and writes nothing.
Without it goku rewrites `$HOME/.config/karabiner/karabiner.json`, which inside
the jail is the throwaway copy -- the sandbox is the guarantee, the `HOME`
redirection only the convenience.  `goku -h` reports the paths it resolved, so
run that first if anything looks wrong.

Note goku's input path is `~/.config/karabiner.edn`, a level **above** the
`karabiner/` directory the file lives in here.

## Comparing

Three outcomes, and only the third is a problem:

- **identical** -- byte for byte.
- **equivalent** -- differs only in a way karabiner cannot observe.  Three
  known kinds: goku emits one `frontmost_application_unless` per
  `:applications` entry where the json merges the bundle ids into a single
  condition (karabiner ANDs conditions, so `unless(A or B)` is
  `unless(A) and unless(B)`); it orders a `to` event's `modifiers` list
  differently, and that list is a set; and it hoists `parameters` from the
  manipulator to the rule-set level.
- **different** -- a real behaviour change.

```sh
python3 - <<'PY'
import json
g=json.load(open('out.json'))['complex_modifications']['rules']
c=json.load(open('now.json'))['profiles'][0]['complex_modifications']['rules']
def k(m):
    d=json.loads(json.dumps(m)); x=d.pop('conditions',None)
    d.pop('parameters',None)
    if x: d['_c']=sorted((y['type'],b) for y in x for b in y['bundle_identifiers'])
    for t in d.get('to',[]):
        if 'modifiers' in t: t['modifiers']=sorted(t['modifiers'])
    return json.dumps(d,sort_keys=True)
e=q=bad=0
for G,C in zip(g,c):
    for a,b in zip(G['manipulators'],C['manipulators']):
        if a==b: e+=1
        elif k(a)==k(b): q+=1
        else: bad+=1; print("DIFFERS:\n  goku", json.dumps(a), "\n  live", json.dumps(b))
print(f"identical {e}, equivalent {q}, different {bad}")
PY
```

**Expected as of 2026-10-19: identical 27, equivalent 9, different 0** -- for the
13 rules and 36 manipulators of that date.

**Stale since 2026-10-23.** Two vscode rules were added by hand (14 rules, 38
manipulators now): the tab-switching rule gained vscode's bundle id through a
new `:tabapps` application set, and "move lines in vscode" is new. Nobody has
compiled the edn since, so the numbers above no longer describe it and a run
will report more of everything. Re-run the comparison and replace this note
with the counts it gives.

One thing to look at first if that run reports a difference: `:tabapps` exists
because an `if` condition cannot be split. goku emits one
`frontmost_application_if` per `:applications` keyword and karabiner ANDs
conditions, so `[:terminals :vscode]` would compile to a rule that can never
fire. The `:!emacs :!terminals` vectors elsewhere are safe only because
`unless(A) and unless(B)` is exactly `unless(A or B)`.

Anything reported as different is real drift -- either the .edn was not updated
alongside the .json, or a translation is wrong.

## Things goku does silently

One rule explains nearly all of it: **goku's name for a thing is never
karabiner's name for it**, and a key it does not recognise is dropped without a
word.  So a wrong spelling does not fail, it produces a rule that quietly does
nothing.  Each of these was found by compiling and diffing, never by reading:

- `{:key :h :modi {...}}`, not karabiner's `:key_code` and `:modifiers`.  Given
  the karabiner spelling goku emits `"from": {}` -- a rule matching nothing.
- `{:softf {:sleep {}}}`, not `{:software_function
  {:iokit_power_management_sleep_system {}}}`.  goku renames `:sleep` on the
  way out, as it renames `:dbc` to `cg_event_double_click`.  Given the long
  form it emits `"to": [{}]`, an empty action.
- `S` is `left_shift`, and there is no letter at all for karabiner's plain
  `shift`, which matches *either* key.  So `:!CSh` narrows a binding rather
  than translating it, and the shifted bindings use the long form instead.
  This one is not a naming slip but a gap in the shorthand.

`src/karabiner_configurator/tos.clj` upstream is the list of to-side keys goku
accepts, and `froms.clj` the from-side.  Worth opening before inventing a
spelling: its header comments map each short name to the karabiner JSON it
produces.

Treat a clean compile as evidence of very little.  The comparison is the test.

## Cleaning up

```sh
rm -rf "$W"/goku_dl "$W"/goku_bin "$W"/jail
brew uninstall joker
```
