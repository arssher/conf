Settings only, and for paranoia only: vscode normally syncs settings through
the GH account.

Keybindings are no longer here. They are applied by chezmoi, one file per
platform, because the two platforms need different files and because the
ergoemacs bindings have to be kept in step with the other places readme.txt
lists:

  linux  ~/.config/Code/User/keybindings.json
         <- private_dot_config/Code/User/keybindings.json
  macos  ~/Library/Application Support/Code/User/keybindings.json
         <- "Library/Application Support/Code/User/keybindings.json"

Each is .chezmoiignore'd on the other platform. The mac file is much the
shorter of the two: karabiner turns the gestures into standard macOS keys
before vscode sees them, so the per-widget bindings the linux file needs -- one
alt+i for the suggest widget, one for lists, one for notebooks -- are all
covered there by a real arrow key. Its header lists what karabiner delivers.

Settings Sync has to be kept away from keybindings.json, or it and chezmoi take
turns overwriting it.
