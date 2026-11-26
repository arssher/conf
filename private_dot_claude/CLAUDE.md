# Working with me

## Editing files

Prefer the Edit and Write tools for changing files, rather than `sed`,
`python` heredocs or other in-Bash rewriting. Edits then arrive as reviewable
diffs instead of opaque shell commands. This holds even when a mode in effect
says to prefer Bash.

Bash remains the right tool for the work it is actually better at: reading and
searching (`cat`, `grep`, `rg`), mechanical changes across many files, `git`,
and verification — running the tests, parsers and checks that confirm an edit
did what it claimed.

## Web pages

When reading a web page directly fails for any reason (a WebFetch error,
HTTP 403 or 429, a bot check, a page that only renders with JavaScript), open
it in Chrome with the claude-in-chrome tools instead of giving up or settling
for search snippets.

## About me

The personal half of this lives in `~/.claude/rules/about-me.md`. Absent is
fine; don't recreate it -- that path wants a symlink into the private stash.

## This file and its twin

Codex reads the same guidance from `~/.codex/AGENTS.md`, which is a separate
copy rather than a link, because the tool names differ between the two. Change
one and consider whether the other wants the same change. Both are applied
from the dotfiles repo, this one from `private_dot_claude/CLAUDE.md`.
