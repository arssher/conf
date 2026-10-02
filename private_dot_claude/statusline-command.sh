#!/bin/bash
# Claude Code status line.
#
# Renders, left to right:
#   user@host  Model <context window size> | Git branch | Tokens (% used) | Directory
#
# Example:
#   ars@nonlibrem  Opus 5 1m | main | 492.5k tokens (49%) | ~/projects/foo
#
# Segments degrade independently: outside a git repository the branch reads
# "no git", and the window size and percentage are omitted entirely when the
# payload does not carry them — no empty brackets, no stray separators.
#
# Wiring — "statusLine" in ~/.claude/settings.json:
#   { "type": "command", "command": "bash ~/.claude/statusline-command.sh" }
#
# Claude Code runs this on every render and pipes a JSON payload on stdin.
# Read stdin exactly once: it is a stream, not a file, so a second `cat`
# comes back empty.
#
# Every field read below was confirmed against a live payload from Claude
# Code v2.1.284 — not guessed. The payload carries far more than is used
# here, including:
#   cost.total_cost_usd                          spend so far this session
#   rate_limits.five_hour.used_percentage        5-hour window, 0-100
#   rate_limits.seven_day.used_percentage        7-day window, 0-100
#   prompt_cache.hit_ratio / .warm               prompt cache health
#   context_window.remaining_percentage          inverse of used_percentage
#   effort.level, thinking.enabled, fast_mode    current run settings
#   session_name, session_id, transcript_path    session identity
# To see the whole shape, drop this line in after the read and open the file:
#   printf '%s' "$input" > /tmp/statusline-payload.json
input=$(cat)

# --- pull fields out of the payload -------------------------------------
# `// fallback` is jq's alternative operator: it kicks in when the field is
# missing OR null, so each of these degrades instead of printing "null".
model=$(jq -r '.model.display_name // "?"' <<<"$input")
cwd=$(jq -r '.workspace.current_dir // .cwd // empty' <<<"$input")

# Token count. Field confirmed against a live payload (v2.1.284).
tokens=$(jq -r '.context_window.total_input_tokens // 0' <<<"$input")

# Percentage of the context window in use. Confirmed against a live
# payload; arrives already rounded to an integer (e.g. 49).
pct=$(jq -r '.context_window.used_percentage // empty' <<<"$input")

# Total size of the context window, shown beside the model name.
size=$(jq -r '.context_window.context_window_size // empty' <<<"$input")

# --- chroot marker ------------------------------------------------------
# Debian convention: if the system is a chroot, /etc/debian_chroot holds its
# name, and the default Debian ~/.bashrc prefixes the shell prompt with
# "(name)" so you can tell you are not on the host. This block is copied
# verbatim from ~/.bashrc (lines 24-25) because the original status line was
# a copy of that prompt. On a normal install /etc/debian_chroot does not
# exist, so $debian_chroot stays empty and the prefix below expands to
# nothing — harmless, and it keeps the status line honest inside a chroot.
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# --- git branch ---------------------------------------------------------
# symbolic-ref gives the branch name; it fails on a detached HEAD, so fall
# back to a short commit hash. --no-optional-locks keeps this read-only so a
# status line firing every render cannot fight a concurrent git command.
# Outside a repository both fail and we print a placeholder.
branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short -q HEAD 2>/dev/null \
  || git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
[ -z "$branch" ] && branch="no git"

# --- format the token figure --------------------------------------------
# Above 1000, abbreviate: 465200 -> "465.2k". The `2>/dev/null` swallows the
# error bash raises if $tokens somehow is not a number.
if [ "$tokens" -ge 1000 ] 2>/dev/null; then
  tok=$(awk -v t="$tokens" 'BEGIN{printf "%.1fk", t/1000}')
else
  tok="$tokens"
fi

# Append the percentage only when the payload actually supplied one, so an
# older build (or a stub payload) shows "465.2k tokens" rather than "(%)".
# Abbreviate the window size: 1000000 -> "1m", 200000 -> "200k". %g drops
# trailing zeros, so 1500000 renders as "1.5m" rather than "1.500000m".
# Built as its own string (with a literal ESC byte) so it can be left out
# entirely when the payload carries no size, instead of printing a stray dot.
ctx=""
if [ -n "$size" ] && [ "$size" != "null" ]; then
  esc=$(printf '\033')
  short=$(awk -v s="$size" 'BEGIN{
    if (s >= 1000000) printf "%gm", s/1000000;
    else if (s >= 1000) printf "%gk", s/1000;
    else printf "%d", s }')
  ctx="${esc}[90m ${short}${esc}[0m"
fi

usage="$tok tokens"
if [ -n "$pct" ] && [ "$pct" != "null" ]; then
  usage=$(awk -v t="$tok" -v p="$pct" 'BEGIN{printf "%s tokens (%.0f%%)", t, p}')
fi

# --- render -------------------------------------------------------------
# ANSI colours: 01;32 green user@host, 36 cyan model, 33 yellow branch,
# 35 magenta usage, 01;34 blue path, 0m resets. ${debian_chroot:+(...)}
# expands to "(name)" only when the variable is non-empty.
printf '%s\033[01;32m%s@%s\033[0m \033[36m%s\033[0m%s | \033[33m%s\033[0m | \033[35m%s\033[0m | \033[01;34m%s\033[0m ' \
  "${debian_chroot:+($debian_chroot)}" "$(whoami)" "$(hostname -s)" "$model" "$ctx" "$branch" "$usage" "$cwd"
