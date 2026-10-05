# A chezmoi source directory

Config files for $HOME, kept in [chezmoi](https://www.chezmoi.io).  Nothing
here is the live config: `chezmoi apply` copies it into place, and a file's
name in the source is not its name in $HOME.

**[readme.txt](readme.txt) is the documentation.**  Read it before changing
anything structural, and keep it current when you do.  It has the workflow, the
source naming rules, what `chezmoi init` does, the things that are easy to get
wrong, where the content that is *not* in this repo lives, and the notes on
bringing up a fresh machine.  It stays readme.txt -- do not rename it, and do
not fold it into this file.

## Working here

- Editing a file here changes nothing until `chezmoi apply`.  Show `chezmoi
  diff` first, so the change is reviewable, then run the apply yourself -- a
  change cannot be tested until it is applied.  Committing and pushing still
  wait to be asked for.
- When the user has edited the live file instead, `chezmoi re-add` brings it
  back into the source.  Don't copy it over by hand.
- Source names carry attributes: `dot_` for a leading dot, `private_` for 0700,
  `executable_` for +x, a `.tmpl` suffix for templating.  Nested paths need the
  prefix at every level, and a missing one makes chezmoi treat the file as
  internal and silently never apply it.
- `.chezmoiignore` patterns are TARGET paths -- where a file would land under
  $HOME, not where it sits here.  The file is itself always a template.
- chezmoi reads the filesystem, not git.  Anything in the source dir gets
  applied unless `.chezmoiignore` says otherwise, so repo-only content -- docs,
  tools, this file -- needs a line there.  Gitignored is not chezmoi-ignored.
- The repo is public (`github.com/arssher/conf`).  No secrets and no machine
  state; that lives outside the repo, under `$CONFPATH`.
- Commit subjects name the thing touched, then say what changed in lowercase:
  `.profile: guard the .cargo/env sourcing`.  Commit in the source dir as
  usual, only when asked.

Per-directory notes live beside the thing they describe:
[dot_emacs.d/kbd-diagram/CLAUDE.md](dot_emacs.d/kbd-diagram/CLAUDE.md).
