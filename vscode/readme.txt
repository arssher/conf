Notes on vscode's config. Nothing in this directory is applied: the files
themselves live where chezmoi applies them from.

  keybindings.json, one per platform:
    linux  ~/.config/Code/User/keybindings.json
           <- private_dot_config/Code/User/keybindings.json
    macos  ~/Library/Application Support/Code/User/keybindings.json
           <- "private_Library/private_Application Support/private_Code/User/"

  settings.json, both platforms from one source:
    .chezmoitemplates/vscode-settings.json       the content
    linux  ~/.config/Code/User/settings.json
           <- private_dot_config/Code/User/settings.json.tmpl
    macos  ~/Library/Application Support/Code/User/settings.json
           <- "private_Library/.../private_Code/User/settings.json.tmpl"

Each keybindings file is .chezmoiignore'd on the other platform. The mac file
is the shorter of the two: karabiner turns the gestures into standard macOS
keys before vscode sees them, so the per-widget bindings the linux file needs
-- one alt+i for the suggest widget, one for lists, one for notebooks -- are
covered there by a real arrow key. Its header lists what karabiner delivers, so
what is missing from it is missing on purpose.

settings.json used to sit in this directory as a backup that nothing applied,
and fell behind the live file. It was moved to the linux path with the live
linux content as its starting point, and now feeds both platforms.

Unlike the keybindings, it is one source: the content lives in
.chezmoitemplates/vscode-settings.json and each platform's target is a one-line
template that includeTemplates it. chezmoi never applies .chezmoitemplates
itself, so it needs no .chezmoiignore line. Two keys differ and are behind an
os check in there:

  terminal.integrated.defaultProfile.linux   linux only. vscode's own key is
                                             platform-suffixed, so it would be
                                             inert on mac rather than wrong,
                                             but carrying it would just invite
                                             someone to "fix" it.
  python.defaultInterpreterPath              /bin/python3 on linux, which does
                                             not exist on macOS at all --
                                             Apple's is /usr/bin/python3 and
                                             .sh_env puts homebrew ahead of it,
                                             so mac gets
                                             /opt/homebrew/bin/python3.

Checked when it was wired up: the linux render is key-for-key the file it
replaced, and the two renders differ in exactly those two keys and nothing
else. Worth re-checking the same way after editing the template --
`chezmoi cat ~/.config/Code/User/settings.json` renders one without applying
it, and the os can be forced with
`chezmoi execute-template '{{ includeTemplate "vscode-settings.json" (dict "chezmoi" (dict "os" "linux")) }}'`.

The mac had no live settings.json at all when this was done, so nothing of its
own was overwritten. A machine that does have one wants comparing first.

One template trap: use {{- if }} and {{- end }}, which trim to the left. The
right-trimming {{ if -}} form eats the indentation of the line that follows the
block, and the result is valid JSON that looks mangled.


Settings Sync stays ON, with Settings and Keyboard Shortcuts taken out of it.

Those two files are chezmoi's now: sync writes keybindings.json and
settings.json and so does chezmoi, and with both on each file is whichever
wrote last. settingsSync.keybindingsPerPlatform (default true) keeps the
platforms' keybindings apart but does not stop sync owning the file. The rest
-- extensions above all, and snippets, tasks, ui state, profiles, prompts --
has no home here and keeps travelling by sync.

What sync covers is per machine and the dotfiles cannot carry it: it is
machine state in globalStorage, not a settings.json key, and there is no
command-line form of it either. So on every machine where sync is on, once:

  palette > "Settings Sync: Configure..." > untick Settings and Keyboard
  Shortcuts

The ticks are what IS synced -- untick to exclude, not the other way round.

Not `code --sync off`. It is no way to turn sync off for good: it overrides
sync for the vscode it launches and stores nothing -- that session logs
"[AutoSync] Disabled." while sync.enable stays true in state.vscdb, and the
next ordinary launch syncs again. Handed to an already running vscode it does
not even that, only opening a window there. Both seen on linux.

To check a machine, in globalStorage/state.vscdb:

  select key, value from ItemTable where key like 'sync.enable%'

sync.enable.settings and sync.enable.keybindings should both be 'false'. A
resource with no row there is synced: absent means the default, which is on.
A machine with sync.enable absent or false does not sync at all, which is
equally safe. userDataSync.log, in the newest session directory under
~/.config/Code/logs (mac: ~/Library/Application Support/Code/logs), shows which
resources each sync touched.
