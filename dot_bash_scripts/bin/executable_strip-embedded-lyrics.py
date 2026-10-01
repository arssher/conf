#!/usr/bin/env python3
"""Strip embedded lyrics from music files when a synced .lrc file already exists.

For a given directory (recursive by default), each supported audio file is
paired with a sibling ".lrc" of the same basename in the same folder. If that
.lrc exists and is non-empty, and the audio file carries embedded lyrics tags,
those tags are removed -- the .lrc is treated as the authoritative copy.

Supported formats and the lyrics fields removed:
  MP3 / ID3        USLT and SYLT frames (all languages/descriptors)
  FLAC, OGG        Vorbis keys LYRICS, UNSYNCEDLYRICS, SYNCEDLYRICS
  M4A / MP4        the (c)lyr atom

Dry-run by default; pass --apply to actually modify files.

Dependency: mutagen. On first run, if mutagen is not importable, a local
virtualenv is created next to this script (in a .venv-lyrics directory
alongside it), mutagen is installed into it, and the script re-executes
itself using that interpreter.
"""

import argparse
import os
import subprocess
import sys
import venv
from pathlib import Path

VENV_DIR = Path(__file__).resolve().parent / ".venv-lyrics"

AUDIO_EXTS = {".mp3", ".flac", ".m4a", ".mp4", ".ogg"}

# Vorbis-comment keys (FLAC/OGG) that hold lyrics, compared case-insensitively.
VORBIS_LYRIC_KEYS = {"lyrics", "unsyncedlyrics", "syncedlyrics"}


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
    if os.environ.get("_STRIP_LYRICS_BOOTSTRAPPED"):
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
    env["_STRIP_LYRICS_BOOTSTRAPPED"] = "1"
    os.execve(str(py), [str(py), os.path.abspath(__file__), *sys.argv[1:]], env)


# Ensure the dependency is importable (bootstrapping a venv and re-execing if
# needed) before importing it at module scope. After this call mutagen is
# guaranteed available, so the rest of the module can use plain top-level imports.
ensure_mutagen()

import mutagen  # noqa: E402
from mutagen.id3 import ID3FileType  # noqa: E402


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


def has_valid_lrc(audio: Path) -> bool:
    """True if a non-empty sibling .lrc with the same basename exists."""
    lrc = audio.with_suffix(".lrc")
    try:
        return lrc.is_file() and lrc.stat().st_size > 0
    except OSError:
        return False


def detect_lyrics(audio):
    """Return (audio_obj, list_of_removable_field_labels) for a mutagen file.

    The audio object is loaded once and returned so the caller can mutate and
    save it without re-reading. Returns (None, []) if the file cannot be read.
    """
    try:
        obj = mutagen.File(audio)
    except Exception:
        return None, []
    if obj is None or obj.tags is None:
        return obj, []

    labels = []
    tags = obj.tags

    # MP3 / ID3: USLT / SYLT frames, keyed as e.g. "USLT::eng" or "USLT:desc:eng".
    if isinstance(obj, ID3FileType) or hasattr(tags, "getall"):
        try:
            for key in list(tags.keys()):
                if key.startswith("USLT") or key.startswith("SYLT"):
                    labels.append(key.split(":")[0])
        except AttributeError:
            pass
    else:
        # Vorbis comments (FLAC/OGG) and MP4 atoms behave like dict-of-lists.
        for key in list(tags.keys()):
            kl = key.lower()
            if kl in VORBIS_LYRIC_KEYS:
                labels.append(key)
            elif key == "\xa9lyr":  # MP4 lyrics atom
                labels.append("(c)lyr")

    # De-duplicate while preserving order.
    seen = set()
    uniq = [l for l in labels if not (l in seen or seen.add(l))]
    return obj, uniq


def strip_lyrics(obj) -> None:
    """Remove all lyrics fields from an already-loaded mutagen object."""
    tags = obj.tags
    if tags is None:
        return

    if isinstance(obj, ID3FileType) or hasattr(tags, "getall"):
        try:
            for key in list(tags.keys()):
                if key.startswith("USLT") or key.startswith("SYLT"):
                    del tags[key]
            obj.save()
            return
        except AttributeError:
            pass

    for key in list(tags.keys()):
        if key.lower() in VORBIS_LYRIC_KEYS or key == "\xa9lyr":
            del tags[key]
    obj.save()


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=f"Dependency virtualenv (created on first run): {VENV_DIR}",
    )
    parser.add_argument("directory", type=Path, help="directory to scan")
    parser.add_argument(
        "--apply",
        action="store_true",
        help="actually modify files (default is a dry run)",
    )
    parser.add_argument(
        "--no-recursive",
        dest="recursive",
        action="store_false",
        help="only scan the top-level directory",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="also report skipped files"
    )
    args = parser.parse_args()

    root = args.directory
    if not root.is_dir():
        sys.stderr.write(f"error: not a directory: {root}\n")
        return 2

    n_with_lrc = 0
    n_with_lyrics = 0
    n_stripped = 0
    n_skipped = 0

    for audio in find_audio_files(root, args.recursive):
        if not has_valid_lrc(audio):
            n_skipped += 1
            if args.verbose:
                print(f"SKIP   {audio}  (no valid .lrc)")
            continue
        n_with_lrc += 1

        obj, fields = detect_lyrics(audio)
        if obj is None:
            n_skipped += 1
            if args.verbose:
                print(f"SKIP   {audio}  (unreadable)")
            continue
        if not fields:
            n_skipped += 1
            if args.verbose:
                print(f"SKIP   {audio}  (no embedded lyrics)")
            continue
        n_with_lyrics += 1

        fieldstr = ", ".join(fields)
        if args.apply:
            try:
                strip_lyrics(obj)
                n_stripped += 1
                print(f"STRIPPED  {audio}  ({fieldstr})")
            except Exception as exc:
                sys.stderr.write(f"error: failed to strip {audio}: {exc}\n")
        else:
            print(f"STRIP     {audio}  ({fieldstr})")

    print()
    if args.apply:
        print(
            f"Summary: {n_with_lrc} files with .lrc, "
            f"{n_with_lyrics} had embedded lyrics, {n_stripped} stripped, "
            f"{n_skipped} skipped."
        )
    else:
        print(
            f"Summary: {n_with_lrc} files with .lrc, "
            f"{n_with_lyrics} had embedded lyrics, {n_skipped} skipped "
            f"(dry run -- re-run with --apply to strip)."
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
