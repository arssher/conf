# Working with me

## Editing files

Prefer `apply_patch` for changing files, rather than `sed`, `python` heredocs
or other in-shell rewriting. Edits then arrive as reviewable diffs instead of
opaque shell commands.

The shell remains the right tool for the work it is actually better at: reading
and searching (`cat`, `grep`, `rg`), mechanical changes across many files,
`git`, and verification -- running the tests, parsers and checks that confirm
an edit did what it claimed.

## Web pages

When reading a web page directly fails for any reason (an HTTP 403 or 429, a
bot check, a page that only renders with JavaScript), open it in a real browser
with the browser tools instead of giving up or settling for search snippets.

## About me

The personal half of this lives in `~/.codex/about-me.md`: how I like to talk
and work, and the running record of what has been noticed. Read it at the start
of a session and treat it as part of these instructions -- nothing loads it for
you, unlike this file.

It is a symlink into a private stash, so on a fresh machine it may be absent.
That is fine: carry on without it, and don't recreate it.

## This file and its twin

Claude Code reads the same guidance from `~/.claude/CLAUDE.md`, which is a
separate copy rather than a link, because the tool names differ between the
two. Change one and consider whether the other wants the same change. Both are
applied from the dotfiles repo, this one from `private_dot_codex/AGENTS.md`.
