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
