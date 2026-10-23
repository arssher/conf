Notes on vscode's config. Nothing in this directory is applied: the files
themselves live where chezmoi applies them from.

  keybindings.json, one per platform:
    linux  ~/.config/Code/User/keybindings.json
           <- private_dot_config/Code/User/keybindings.json
    macos  ~/Library/Application Support/Code/User/keybindings.json
           <- "private_Library/private_Application Support/private_Code/User/"

  settings.json, linux only so far:
    linux  ~/.config/Code/User/settings.json
           <- private_dot_config/Code/User/settings.json

Each keybindings file is .chezmoiignore'd on the other platform. The mac file
is the shorter of the two: karabiner turns the gestures into standard macOS
keys before vscode sees them, so the per-widget bindings the linux file needs
-- one alt+i for the suggest widget, one for lists, one for notebooks -- are
covered there by a real arrow key. Its header lists what karabiner delivers, so
what is missing from it is missing on purpose.

settings.json used to sit in this directory as a backup that nothing applied,
and fell behind the live file. It was moved to the linux path above with the
live linux content, which is its starting point. The mac's settings are not
managed yet. When they are, the mac's live file wants comparing with this one
first; settings are mostly the same on both platforms, so the likely shape is
one source the mac path includes through a template, with the few that differ
behind an os check.


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
