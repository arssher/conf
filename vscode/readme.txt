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


Settings Sync is OFF, deliberately, on every machine.

It used to be how all of this travelled. It cannot stay on now: sync writes
keybindings.json and settings.json and so does chezmoi, and with both running
each file is whichever wrote last. settingsSync.keybindingsPerPlatform
(default true) keeps the platforms' keybindings apart but does not stop sync
owning the file.

Turning it off is per machine and is not something the dotfiles can carry: the
enablement is machine state in globalStorage, not a settings.json key. Both
bootstrap scripts have the line:

  code --sync off

That is the non-interactive form, and it only works with vscode not running:
handed to a running instance it just opens a window there, and sync carries
on. Seen on linux -- quit vscode first, or use the palette. The palette's "Settings Sync: Turn Off" also
offers to turn it off on all devices and delete the copy held in the cloud,
which the flag does not do -- worth using that one once if the cloud copy
should go too.

To check a machine: `select key from ItemTable where key like 'sync.%'` in
globalStorage/state.vscdb shows only store metadata (storeUrl, productQuality,
previous.store) and no sync.enable or account keys. A `sync` directory under
the User directory is left behind by a machine that once synced, so its
presence alone does not mean sync is on.
