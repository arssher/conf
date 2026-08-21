#!/usr/bin/env python3
"""Propagate synced lyrics (.lrc) from original songs to their derivative versions.

Assumes originals are kept under one root (--src) and alternate versions of the
same songs -- instrumental/minus, vocals-only, remix, etc. -- under another root
(--dst). The derivatives usually have their metadata stripped and their file
name is the original name plus a suffix like "[vocals]" or "[remix]".

For each audio file under --src that has a sibling ".lrc", this script finds
every matching file under --dst and copies the ".lrc" next to it (named after
the destination file). A destination file matches when BOTH hold:

  * name:   the dst file's stem starts with the src file's stem, followed by a
            non-alphanumeric boundary (so "Song" matches "Song [vocals]" but
            not "Songbird"), or the stems are identical.
  * length: the decoded audio lengths differ by no more than the tolerance
            (default 50 ms; see --tolerance-ms). A small tolerance is used
            rather than an exact match because converting between formats
            (e.g. a FLAC original re-encoded to an AAC/m4a or Opus version)
            pads or trims the stream by a few milliseconds, so derivatives of
            the same source rarely report a bit-identical length.

Dry-run by default; pass --apply to actually copy. An existing dst ".lrc" is
left alone unless --force is given.

Dependency: mutagen. On first run, if mutagen is not importable, a local
virtualenv is created next to this script (in a .venv-lyrics directory
alongside it), mutagen is installed into it, and the script re-executes
itself using that interpreter.
"""

import argparse
import os
import shutil
import subprocess
import sys
import venv
from pathlib import Path

VENV_DIR = Path(__file__).resolve().parent / ".venv-lyrics"

AUDIO_EXTS = {".mp3", ".flac", ".m4a", ".mp4", ".ogg"}

# Characters after the matched prefix that count as a "suffix boundary" are any
# non-alphanumeric character; this is checked via str.isalnum() at match time.


def _venv_python(venv_dir: Path) -> Path:
    return venv_dir / "bin" / "python"


def ensure_mutagen() -> None:
    """Import mutagen, bootstrapping a local venv on first run if needed.

    If a bootstrap is performed, this function re-executes the script with the
    venv's interpreter and does not return.
    """
    try:
        import mutagen  # noqa: F401
        return
    except ImportError:
        pass

    # Guard against a re-exec loop: if we already relaunched into the venv and
    # mutagen still is not importable, fail loudly rather than spin.
    if os.environ.get("_PROPAGATE_LRC_BOOTSTRAPPED"):
        sys.stderr.write(
            "error: mutagen still unavailable after venv bootstrap.\n"
            f"       Try manually: {_venv_python(VENV_DIR)} -m pip install mutagen\n"
        )
        sys.exit(1)

    py = _venv_python(VENV_DIR)
    if not py.exists():
        sys.stderr.write(
            "mutagen not found -- setting up a local virtualenv (first run only)...\n"
            f"  venv: {VENV_DIR}\n"
        )
        try:
            venv.create(VENV_DIR, with_pip=True)
        except Exception as exc:  # pragma: no cover - environment dependent
            sys.stderr.write(f"error: could not create virtualenv: {exc}\n")
            sys.exit(1)

        result = subprocess.run(
            [str(py), "-m", "pip", "install", "--quiet", "mutagen"],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            sys.stderr.write("error: failed to install mutagen into the virtualenv.\n")
            sys.stderr.write(result.stdout)
            sys.stderr.write(result.stderr)
            sys.stderr.write(
                f"\nYou can retry manually: {py} -m pip install mutagen\n"
            )
            sys.exit(1)
        sys.stderr.write("  done.\n")

    # Re-execute this script using the venv's interpreter.
    env = dict(os.environ)
    env["_PROPAGATE_LRC_BOOTSTRAPPED"] = "1"
    os.execve(str(py), [str(py), os.path.abspath(__file__), *sys.argv[1:]], env)


# Ensure the dependency is importable (bootstrapping a venv and re-execing if
# needed) before importing it at module scope. After this call mutagen is
# guaranteed available, so the rest of the module can use plain top-level imports.
ensure_mutagen()

import mutagen  # noqa: E402


def find_audio_files(root: Path, recursive: bool):
    """Yield audio files under root whose extension is in AUDIO_EXTS.

    Walks recursively when recursive is True, otherwise only the top level.
    """
    if recursive:
        it = (p for p in root.rglob("*") if p.is_file())
    else:
        it = (p for p in root.iterdir() if p.is_file())
    for p in it:
        if p.suffix.lower() in AUDIO_EXTS:
            yield p


def audio_length_ms(audio: Path):
    """Return the decoded audio length rounded to the millisecond, or None.

    None means the file could not be read or reports no stream info.
    """
    try:
        obj = mutagen.File(audio)
    except Exception:
        return None
    if obj is None or obj.info is None:
        return None
    length = getattr(obj.info, "length", None)
    if length is None:
        return None
    return round(length, 3)


def valid_lrc(audio: Path):
    """Return the sibling .lrc path if it exists and is non-empty, else None."""
    lrc = audio.with_suffix(".lrc")
    try:
        if lrc.is_file() and lrc.stat().st_size > 0:
            return lrc
    except OSError:
        pass
    return None


def stem_matches(src_stem: str, dst_stem: str) -> bool:
    """True if dst_stem is src_stem plus an optional non-alphanumeric suffix."""
    if not dst_stem.startswith(src_stem):
        return False
    if len(dst_stem) == len(src_stem):
        return True
    return not dst_stem[len(src_stem)].isalnum()


def build_dst_index(dst_root: Path, recursive: bool, verbose: bool):
    """Return a list of (path, stem, length_ms) for readable dst audio files."""
    index = []
    for audio in find_audio_files(dst_root, recursive):
        length = audio_length_ms(audio)
        if length is None:
            if verbose:
                print(f"SKIP-DST  {audio}  (unreadable / no length)")
            continue
        index.append((audio, audio.stem, length))
    return index


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=f"Dependency virtualenv (created on first run): {VENV_DIR}",
    )
    parser.add_argument("--src", type=Path, required=True, help="root of original songs")
    parser.add_argument("--dst", type=Path, required=True, help="root of derivative versions")
    parser.add_argument(
        "--apply",
        action="store_true",
        help="actually copy .lrc files (default is a dry run)",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="overwrite an existing .lrc at the destination",
    )
    parser.add_argument(
        "--tolerance-ms",
        type=int,
        default=50,
        help="max length difference to consider two files the same song "
        "(default 50 ms; absorbs format-conversion drift)",
    )
    parser.add_argument(
        "--no-recursive",
        dest="recursive",
        action="store_false",
        help="only scan the top level of --src and --dst",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="also report skipped files"
    )
    args = parser.parse_args()

    for label, path in (("--src", args.src), ("--dst", args.dst)):
        if not path.is_dir():
            sys.stderr.write(f"error: {label} is not a directory: {path}\n")
            return 2

    tolerance = args.tolerance_ms / 1000.0

    dst_index = build_dst_index(args.dst, args.recursive, args.verbose)

    n_src_with_lrc = 0
    n_matched_src = 0
    n_copied = 0
    n_skip_exists = 0
    n_no_match = 0

    for audio in find_audio_files(args.src, args.recursive):
        lrc = valid_lrc(audio)
        if lrc is None:
            if args.verbose:
                print(f"SKIP-SRC  {audio}  (no valid .lrc)")
            continue
        n_src_with_lrc += 1

        src_len = audio_length_ms(audio)
        if src_len is None:
            if args.verbose:
                print(f"SKIP-SRC  {audio}  (unreadable / no length)")
            n_no_match += 1
            continue

        matches = [
            entry
            for entry in dst_index
            if abs(entry[2] - src_len) <= tolerance
            and stem_matches(audio.stem, entry[1])
        ]
        if not matches:
            if args.verbose:
                print(f"NOMATCH   {audio}")
            n_no_match += 1
            continue
        n_matched_src += 1

        for dst_audio, _stem, _length in matches:
            dst_lrc = dst_audio.with_suffix(".lrc")

            if dst_lrc.resolve() == lrc.resolve():
                continue  # src and dst point at the same .lrc; nothing to do

            if dst_lrc.exists() and not args.force:
                n_skip_exists += 1
                if args.verbose:
                    print(f"SKIP      {dst_lrc}  (already exists; use --force)")
                continue

            if args.apply:
                try:
                    shutil.copyfile(lrc, dst_lrc)
                    n_copied += 1
                    print(f"COPIED    {lrc}  ->  {dst_lrc}")
                except OSError as exc:
                    sys.stderr.write(f"error: failed to copy to {dst_lrc}: {exc}\n")
            else:
                n_copied += 1
                print(f"COPY      {lrc}  ->  {dst_lrc}")

    print()
    if args.apply:
        print(
            f"Summary: {n_src_with_lrc} src files with .lrc, "
            f"{n_matched_src} matched a version, {n_copied} .lrc copied, "
            f"{n_skip_exists} skipped (already present), "
            f"{n_no_match} with no match."
        )
    else:
        print(
            f"Summary: {n_src_with_lrc} src files with .lrc, "
            f"{n_matched_src} matched a version, {n_copied} .lrc to copy, "
            f"{n_skip_exists} skipped (already present), "
            f"{n_no_match} with no match "
            f"(dry run -- re-run with --apply to copy)."
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
