Settings only, and a backup only: nothing here is applied.

Keybindings are not here any more. They are applied by chezmoi, one file per
platform, so that the ergoemacs bindings stay in step with the other places
readme.txt lists:

  linux  ~/.config/Code/User/keybindings.json
         <- private_dot_config/Code/User/keybindings.json
  macos  ~/Library/Application Support/Code/User/keybindings.json
         <- "private_Library/private_Application Support/private_Code/User/"

Each is .chezmoiignore'd on the other platform. The mac file is the shorter of
the two: karabiner turns the gestures into standard macOS keys before vscode
sees them, so the per-widget bindings the linux file needs -- one alt+i for the
suggest widget, one for lists, one for notebooks -- are covered there by a real
arrow key. Its header lists what karabiner delivers, so what is missing from it
is missing on purpose.


Settings Sync is OFF, deliberately, on every machine.

It used to be how all of this travelled, which is why this directory existed
only "for paranoia". It cannot stay on now: sync writes keybindings.json and so
does chezmoi, and with both running the file is whichever wrote last.
settingsSync.keybindingsPerPlatform (default true) keeps the platforms apart
but does not stop sync owning the file.

Turning it off is per machine and is not something the dotfiles can carry: the
enablement is machine state in globalStorage, not a settings.json key. Both
bootstrap scripts have the line:

  code --sync off

That is the non-interactive form. The palette's "Settings Sync: Turn Off" also
offers to turn it off on all devices and delete the copy held in the cloud,
which the flag does not do -- worth using that one once if the cloud copy
should go too.

To check a machine: there is no `sync` directory under the User directory, and
`select key from ItemTable where key like 'sync.%'` in globalStorage/state.vscdb
shows only store metadata (storeUrl, productQuality, previous.store) and no
enablement or account keys.


So settings.json, the file in here, is now the only copy and it travels by
nothing. It is a snapshot, not a source: if the settings on a machine matter,
re-add it here by hand, or manage it the way the keybindings are managed.
