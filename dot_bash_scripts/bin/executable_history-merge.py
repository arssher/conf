#!/usr/bin/env python3
"""Keep one shell history across bash and zsh, through ~/.persistent_history.

Three stores, three formats, none of which can be concatenated with another:

    ~/.persistent_history     [2026-10-07 20:20:00] | ls -la
    ~/.bash_eternal_history   #1791376094
                              ls -la
    ~/.zsh_history            : 1791376094:0;ls -la

~/.persistent_history is the common one: the other two are each a single
shell's, in that shell's own format, while this one is shared and so is the
sensible thing to carry between machines. bash also appends to it from
PROMPT_COMMAND, because HISTCONTROL=erasedups makes its own history file
forget when a repeated command ran before; zsh needs no such hook, nothing
trimming its file.

There is really one operation: read all three, take the union, write out
whichever ones were asked for. Both directions are therefore a merge rather
than a replace, which makes them idempotent and non-destructive -- running
either twice changes nothing, and neither can lose an entry that only one store
had.

    --to-common-log     gather: write ~/.persistent_history, so it holds
                        everything both shells have run.
    --from-common-log   scatter: write the two per-shell files, so each shell
                        can see the other's history.

Entries are keyed on (timestamp, command) and sorted by timestamp. What that
costs:

  * One second of resolution, because that is all ~/.persistent_history records.
    Two different commands run inside the same second collapse into one.
  * The elapsed-time field zsh has room for. It is written as 0, which is what
    SHARE_HISTORY produces anyway: zsh appends the line as it is typed, before
    the command has run, so the real duration is never recorded either.
  * Local time with no zone, again because that is the log's format. Entries
    written in another zone sort by their wall clock rather than their instant.
  * Newlines inside a command, when writing the log: they become "; ", which is
    what bash itself does by default (shopt lithist is off). zsh keeps them,
    using its own trailing-backslash continuation.

Reading is deliberately forgiving. History files collect junk -- half-written
lines, stray bytes from a crashed terminal -- which is why ph used `grep
--text`. Everything is read and written with surrogateescape, so bytes that are
not UTF-8 survive a round trip untouched. The two exceptions are NUL bytes,
dropped on reading, and ~/.zsh_history's own byte encoding; see _open_read and
_metafy.
"""

import argparse
import os
import re
import sys
import tempfile
from datetime import datetime

HOME = os.path.expanduser("~")
LOG = os.path.join(HOME, ".persistent_history")
BASH = os.path.join(HOME, ".bash_eternal_history")
ZSH = os.path.join(HOME, ".zsh_history")

LOG_FMT = "%Y-%m-%d %H:%M:%S"
LOG_RE = re.compile(r"^\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\] \| (.*)$")
BASH_TS_RE = re.compile(r"^#(\d{9,11})$")
ZSH_RE = re.compile(r"^: (\d+):(\d+);(.*)$")

# Entries whose timestamp cannot be recovered. They sort to the front, which is
# where they belong: a bash file written before HISTTIMEFORMAT was set has no
# timestamps at all, and that history is the oldest there is.
UNKNOWN = 0


# zsh metafies its history file: each byte from 0x83 to 0xa2 is written as 0x83
# followed by the byte XOR 0x20. The range is measured rather than taken from
# zsh's source -- zsh 5.9 was made to write every byte value from 1 to 255, and
# exactly these thirty-two came out escaped. UTF-8 is full of them: Cyrillic `у'
# is 0xd1 0x83. So a raw write is misread, zsh taking that 0x83 for an escape
# and garbling the byte after it, and a raw read hands back zsh's escapes as
# text, which is how commands typed in zsh used to reach the log.
META = b"\x83"
_METAFY_RE = re.compile(rb"[\x83-\xa2]")
_UNMETAFY_RE = re.compile(rb"\x83(.)", re.DOTALL)


def _metafy(data):
    return _METAFY_RE.sub(lambda m: META + bytes([m.group(0)[0] ^ 0x20]), data)


def _unmetafy(data):
    return _UNMETAFY_RE.sub(lambda m: bytes([m.group(1)[0] ^ 0x20]), data)


def _open_read(path, unmetafy=False):
    """Return the file's lines, or [] if it is not there.

    NUL bytes are dropped. No shell command can hold one, so in a history file
    they are only ever junk: a write cut short and zero-filled, as a crash
    leaves it. ~/.persistent_history has two such runs, each just ahead of an
    intact entry, and dropping them lets that entry parse as its own line
    again rather than as a continuation of the one above. Kept, they went out
    into ~/.zsh_history, and zsh stops reading its file at the first NUL.
    """
    if not os.path.exists(path):
        return []
    with open(path, "rb") as f:
        data = f.read().replace(b"\0", b"")
    if unmetafy:
        data = _unmetafy(data)
    return data.decode("utf-8", "surrogateescape").splitlines()


def _norm(cmd):
    """Flatten a command to one line, the invariant the whole merge rests on.

    Only zsh can hold a newline inside an entry. The log cannot -- it is one
    line per entry -- and bash does not either, since shopt lithist is off. So
    without this, a multi-line command read from zsh and the same command read
    back from the log are two different strings, dedup keeps both, and every
    backup-restore cycle adds another twin. Normalising at read time means
    entries are single-line everywhere and the merge is exactly idempotent.

    The cost is paid once, on the first merge, and only by multi-line entries
    already in ~/.zsh_history: they come out semicolon-joined, which is valid
    shell and is what bash would have recorded for them anyway.
    """
    return cmd.replace("\n", "; ")


def read_log(path=LOG):
    """[(epoch, command)] from the [date] | command format."""
    out = []
    for line in _open_read(path):
        m = LOG_RE.match(line)
        if m:
            stamp, cmd = m.groups()
            try:
                epoch = int(datetime.strptime(stamp, LOG_FMT).timestamp())
            except ValueError:
                epoch = UNKNOWN
            out.append([epoch, cmd])
        elif out:
            # No timestamp: a continuation of the command above. The log is
            # meant to be one line per entry, but .bashrc quotes the command
            # when it appends, so a multi-line command has always spilled.
            out[-1][1] += "\n" + line
        # else: junk before the first entry, dropped.
    return [(e, _norm(c)) for e, c in out]


def read_bash(path=BASH):
    """[(epoch, command)] from the #epoch / command pair format.

    An entry is everything from one timestamp to the next, which is how bash
    reads its own file once HISTTIMEFORMAT is set. Most are one line, but a
    heredoc is kept with its body, lines and all, under a single timestamp --
    read a line at a time, each line of the body became a timestampless
    "command" of its own. The line right after a timestamp belongs to that
    entry even when blank: that is an empty command, as the log has them.

    Lines before the first timestamp are separate entries without one. That is
    the shape of a file written before HISTTIMEFORMAT was set, and of what
    write_bash puts at the front, where timestampless entries sort.
    """
    out = []
    pending = None
    timestamped = False
    for line in _open_read(path):
        m = BASH_TS_RE.match(line)
        if m:
            pending = int(m.group(1))
            timestamped = True
            continue
        if pending is not None:
            out.append([pending, line])
            pending = None
        elif timestamped and out:
            out[-1][1] += "\n" + line
        elif line != "":
            out.append([UNKNOWN, line])
    return [(e, _norm(c)) for e, c in out]


# A command that really ends in a backslash -- `ls man/man7\', a stray one
# before Enter -- would read back as continued on the next line. zsh avoids
# that by writing one space after it and dropping one space from any line that
# ends in a backslash and spaces when it reads: `a\  ' comes back as `a\ ',
# while `plain ' keeps its space. Checked against zsh 5.9 both ways. Doing the
# same here keeps such entries from swallowing the one after them, a new
# glued-together twin on every run.
_PADDED_RE = re.compile(r"\\ +$")
_NEEDS_PAD_RE = re.compile(r"\\ *$")


def _unpad(cmd):
    return cmd[:-1] if _PADDED_RE.search(cmd) else cmd


def _pad(cmd):
    return cmd + " " if _NEEDS_PAD_RE.search(cmd) else cmd


def read_zsh(path=ZSH):
    """[(epoch, command)] from the extended-history format.

    A command containing newlines is stored with a backslash at the end of each
    line but the last, so those have to be rejoined before anything else.
    """
    out = []
    pending_epoch = None
    pending_cmd = None
    for line in _open_read(path, unmetafy=True):
        if pending_cmd is not None:
            # Continuation of a multi-line command.
            pending_cmd = pending_cmd[:-1] + "\n" + line
            if not line.endswith("\\"):
                out.append((pending_epoch, _norm(_unpad(pending_cmd))))
                pending_epoch = pending_cmd = None
            continue
        m = ZSH_RE.match(line)
        if m:
            epoch, _elapsed, cmd = m.groups()
            if cmd.endswith("\\"):
                pending_epoch, pending_cmd = int(epoch), cmd
            else:
                out.append((int(epoch), _norm(_unpad(cmd))))
        elif line != "":
            # A line with no ": epoch:elapsed;" prefix. zsh writes these when
            # EXTENDED_HISTORY is off, so take the command and give up on when.
            out.append((UNKNOWN, _norm(line)))
    if pending_cmd is not None:
        out.append((pending_epoch, _norm(pending_cmd)))
    return out


def merge(*groups):
    """Union of the groups, deduplicated on (epoch, command), oldest first.

    Dedup cannot key on the command alone: a command repeats legitimately, and
    when it was last run is the interesting part. Sorting is stable, so entries
    sharing a second keep the order they were read in.
    """
    seen = set()
    out = []
    for group in groups:
        for entry in group:
            if entry not in seen:
                seen.add(entry)
                out.append(entry)
    out.sort(key=lambda e: e[0])
    return out


def _write_atomic(path, text, mode=0o600, metafy=False):
    """Write via a temporary file in the same directory, then rename.

    History files are worth not truncating halfway: the rename is atomic, so an
    interrupted run leaves the old file intact.
    """
    data = text.encode("utf-8", "surrogateescape")
    if metafy:
        data = _metafy(data)
    d = os.path.dirname(path) or "."
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".history-merge.")
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
        os.chmod(tmp, mode)
        os.replace(tmp, path)
    except BaseException:
        if os.path.exists(tmp):
            os.unlink(tmp)
        raise


def write_log(entries, path=LOG):
    lines = []
    for epoch, cmd in entries:
        stamp = datetime.fromtimestamp(epoch).strftime(LOG_FMT)
        lines.append("[%s] | %s" % (stamp, cmd.replace("\n", "; ")))
    _write_atomic(path, "".join(l + "\n" for l in lines))


def write_bash(entries, path=BASH):
    """#epoch / command pairs.

    Two things the format cannot carry, both of which would otherwise come back
    wrong on the next read:

      * A newline inside a command. bash's file is one line per entry unless
        shopt lithist is set, and it is not, so the newline becomes "; " --
        which is what bash would have written itself. Left raw, the second line
        would be read back as a separate, timestampless entry.
      * An epoch of 0, meaning the timestamp was never recorded. "#0" is not a
        plausible timestamp and BASH_TS_RE will not take it, so the line is
        simply left out and the entry stays timestampless, as it was.
    """
    lines = []
    for epoch, cmd in entries:
        if epoch != UNKNOWN:
            lines.append("#%d" % epoch)
        lines.append(cmd.replace("\n", "; "))
    _write_atomic(path, "".join(l + "\n" for l in lines))


def write_zsh(entries, path=ZSH):
    """: epoch:0;command lines, metafied as zsh writes them.

    Entries without a timestamp are left out. zsh has no way to say so: it
    reads an epoch of 0 as missing, stamps the entry with the time it was
    loaded, and saves it back that way -- so it would come round on the next
    gather as a fresh entry dated today. The log and bash's file keep them.
    """
    lines = []
    for epoch, cmd in entries:
        if epoch == UNKNOWN:
            continue
        # zsh's own continuation: backslash at the end of every line but the
        # last, which is how it wrote the entry in the first place.
        lines.append(": %d:0;%s" % (epoch, _pad(cmd).replace("\n", "\\\n")))
    _write_atomic(path, "".join(l + "\n" for l in lines), metafy=True)


def main():
    p = argparse.ArgumentParser(
        description="Merge bash and zsh history through ~/.persistent_history.",
        epilog="Given neither direction, nothing is written: it reads the "
               "three files and reports what a merge would hold.",
    )
    p.add_argument("--to-common-log", action="store_true",
                   help="gather: write ~/.persistent_history")
    p.add_argument("--from-common-log", action="store_true",
                   help="scatter: write ~/.bash_eternal_history "
                        "and ~/.zsh_history")
    p.add_argument("--extra-log", action="append", metavar="PATH", default=[],
                   help="another file in ~/.persistent_history's format to "
                        "read into the merge; never written to. Repeatable. "
                        "This is how a copy kept elsewhere -- a backup, "
                        "another machine's -- gets folded in without either "
                        "side replacing the other")
    p.add_argument("-n", "--dry-run", action="store_true",
                   help="report what would happen, write nothing")
    args = p.parse_args()

    log, bash, zsh = read_log(), read_bash(), read_zsh()
    extras = [(p, read_log(p)) for p in args.extra_log]
    merged = merge(log, *[e for _, e in extras], bash, zsh)

    print("read   %7d  %s" % (len(log), LOG))
    for path, entries in extras:
        print("read   %7d  %s" % (len(entries), path))
    print("read   %7d  %s" % (len(bash), BASH))
    print("read   %7d  %s" % (len(zsh), ZSH))
    print("merged %7d  entries" % len(merged))

    if not (args.to_common_log or args.from_common_log):
        print("nothing asked for; pass --to-common-log or --from-common-log",
              file=sys.stderr)
        return 0

    targets = []
    if args.to_common_log:
        targets.append((LOG, write_log))
    if args.from_common_log:
        targets += [(BASH, write_bash), (ZSH, write_zsh)]

    for path, writer in targets:
        if args.dry_run:
            print("would write    %s" % path)
        else:
            writer(merged, path)
            print("wrote  %7d  %s" % (len(merged), path))

    if args.from_common_log and not args.dry_run:
        print("\nAlready-open shells keep their own in-memory history and will "
              "append it\nlater, which cannot undo this: zsh appends rather "
              "than replaces unless\nAPPEND_HISTORY is unset. With "
              "SHARE_HISTORY they pick these entries up\non their next "
              "command; `fc -R' forces it sooner.", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
