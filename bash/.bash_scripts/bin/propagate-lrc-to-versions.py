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

With --copy-metadata, the common text tags (title, artist, album, albumartist,
track/disc number, date, genre) and the cover art are also copied from each
original onto its matched versions, overwriting whatever is there -- handy for
derivatives whose metadata was stripped. Metadata is copied for every matched
version, including originals that have no ".lrc". A version whose tags already
match the original is left untouched (reported as unchanged) so its file -- and
mtime -- are not needlessly rewritten. Lyrics tags are never copied this way;
the ".lrc" remains the sole home for synced lyrics.

Dependency: mutagen. On first run, if mutagen is not importable, a local
virtualenv is created next to this script (in a .venv-lyrics directory
alongside it), mutagen is installed into it, and the script re-executes
itself using that interpreter.
"""

import argparse
import base64
import os
import shutil
import subprocess
import sys
import venv
from collections import namedtuple
from pathlib import Path

VENV_DIR = Path(__file__).resolve().parent / ".venv-lyrics"

AUDIO_EXTS = {".mp3", ".flac", ".m4a", ".mp4", ".ogg"}

# Common text tags copied across formats via mutagen's uniform "easy" interface.
COMMON_TAG_KEYS = [
    "title",
    "artist",
    "album",
    "albumartist",
    "tracknumber",
    "discnumber",
    "date",
    "genre",
]

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
from mutagen.flac import FLAC, Picture  # noqa: E402
from mutagen.id3 import APIC, ID3, ID3NoHeaderError  # noqa: E402
from mutagen.mp4 import MP4, MP4Cover  # noqa: E402
from mutagen.oggvorbis import OggVorbis  # noqa: E402


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


Cover = namedtuple("Cover", "mime data type desc")


def read_common_tags(path: Path):
    """Return {key: [values]} of the common text tags present on path."""
    obj = mutagen.File(path, easy=True)
    tags = {}
    if obj is not None and obj.tags is not None:
        for key in COMMON_TAG_KEYS:
            value = obj.tags.get(key)
            if value:
                tags[key] = value
    return tags


def write_common_tags(path: Path, tags) -> None:
    """Overwrite the common text tags on path with the given {key: [values]}."""
    obj = mutagen.File(path, easy=True)
    if obj.tags is None:
        obj.add_tags()
    for key, value in tags.items():
        try:
            obj[key] = value
        except Exception:
            pass  # key not supported by this format's easy interface
    obj.save()


def _pick_front(items, type_of):
    """Prefer a front-cover (type 3) picture, else the first available."""
    for item in items:
        if type_of(item) == 3:
            return item
    return items[0]


def read_cover(path: Path):
    """Return a Cover for path's embedded art, or None if there is none."""
    ext = path.suffix.lower()
    try:
        if ext == ".mp3":
            try:
                tags = ID3(path)
            except ID3NoHeaderError:
                return None
            apics = tags.getall("APIC")
            if not apics:
                return None
            a = _pick_front(apics, lambda x: int(x.type))
            return Cover(a.mime, a.data, int(a.type), a.desc)
        if ext == ".flac":
            pics = FLAC(path).pictures
            if not pics:
                return None
            p = _pick_front(pics, lambda x: int(x.type))
            return Cover(p.mime, p.data, int(p.type), p.desc)
        if ext in (".m4a", ".mp4"):
            tags = MP4(path).tags
            covr = tags.get("covr") if tags else None
            if not covr:
                return None
            c = covr[0]
            mime = "image/png" if c.imageformat == MP4Cover.FORMAT_PNG else "image/jpeg"
            return Cover(mime, bytes(c), 3, "")
        if ext == ".ogg":
            tags = OggVorbis(path).tags
            blocks = tags.get("metadata_block_picture") if tags else None
            if not blocks:
                return None
            p = Picture(base64.b64decode(blocks[0]))
            return Cover(p.mime, p.data, int(p.type), p.desc)
    except Exception:
        return None
    return None


def write_cover(path: Path, cover: "Cover") -> None:
    """Overwrite path's embedded cover art with the given Cover."""
    ext = path.suffix.lower()
    desc = cover.desc or ""
    if ext == ".mp3":
        obj = mutagen.File(path)
        if obj.tags is None:
            obj.add_tags()
        obj.tags.delall("APIC")
        obj.tags.add(
            APIC(encoding=3, mime=cover.mime, type=cover.type, desc=desc, data=cover.data)
        )
        obj.save()
    elif ext == ".flac":
        obj = FLAC(path)
        obj.clear_pictures()
        pic = Picture()
        pic.type = cover.type
        pic.mime = cover.mime
        pic.desc = desc
        pic.data = cover.data
        obj.add_picture(pic)
        obj.save()
    elif ext in (".m4a", ".mp4"):
        obj = MP4(path)
        if obj.tags is None:
            obj.add_tags()
        fmt = MP4Cover.FORMAT_PNG if cover.mime.endswith("png") else MP4Cover.FORMAT_JPEG
        obj.tags["covr"] = [MP4Cover(cover.data, imageformat=fmt)]
        obj.save()
    elif ext == ".ogg":
        obj = OggVorbis(path)
        pic = Picture()
        pic.type = cover.type
        pic.mime = cover.mime
        pic.desc = desc
        pic.data = cover.data
        obj["metadata_block_picture"] = [base64.b64encode(pic.write()).decode("ascii")]
        obj.save()


def metadata_diff(src_tags, src_cover, dst_path: Path):
    """Return the labels of fields that would change if dst were overwritten.

    Compares the source's values against the destination's current ones. Only
    the keys the source actually provides (and the cover, if the source has one)
    are considered, mirroring what the overwrite would touch.
    """
    changed = []
    dst_tags = read_common_tags(dst_path)
    for key in COMMON_TAG_KEYS:
        if src_tags.get(key) and src_tags.get(key) != dst_tags.get(key):
            changed.append(key)
    if src_cover is not None:
        dst_cover = read_cover(dst_path)
        if dst_cover is None or dst_cover.data != src_cover.data:
            changed.append("cover")
    return changed


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
        "--copy-metadata",
        action="store_true",
        help="also copy common text tags and cover art from the original to "
        "each matched version (overwriting them); processes matches even when "
        "the original has no .lrc",
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
    n_meta_written = 0
    n_meta_same = 0

    for audio in find_audio_files(args.src, args.recursive):
        lrc = valid_lrc(audio)
        if lrc is not None:
            n_src_with_lrc += 1
        elif not args.copy_metadata:
            # Nothing to do for this src: no lyrics to copy and metadata copy off.
            if args.verbose:
                print(f"SKIP-SRC  {audio}  (no valid .lrc)")
            continue

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

        # Read the source's metadata once, reused for every matched version.
        if args.copy_metadata:
            src_tags = read_common_tags(audio)
            src_cover = read_cover(audio)
        else:
            src_tags, src_cover = {}, None

        for dst_audio, _stem, _length in matches:
            # --- lyrics ---
            if lrc is not None:
                dst_lrc = dst_audio.with_suffix(".lrc")
                if dst_lrc.resolve() == lrc.resolve():
                    pass  # src and dst point at the same .lrc; nothing to do
                elif dst_lrc.exists() and not args.force:
                    n_skip_exists += 1
                    if args.verbose:
                        print(f"SKIP      {dst_lrc}  (already exists; use --force)")
                elif args.apply:
                    try:
                        shutil.copyfile(lrc, dst_lrc)
                        n_copied += 1
                        print(f"COPIED    {lrc}  ->  {dst_lrc}")
                    except OSError as exc:
                        sys.stderr.write(f"error: failed to copy to {dst_lrc}: {exc}\n")
                else:
                    n_copied += 1
                    print(f"COPY      {lrc}  ->  {dst_lrc}")

            # --- metadata ---
            if args.copy_metadata and (src_tags or src_cover):
                changed = metadata_diff(src_tags, src_cover, dst_audio)
                if not changed:
                    n_meta_same += 1
                    if args.verbose:
                        print(f"META-SAME {dst_audio}  (no change)")
                elif args.apply:
                    try:
                        if any(c != "cover" for c in changed):
                            write_common_tags(dst_audio, src_tags)
                        if "cover" in changed:
                            write_cover(dst_audio, src_cover)
                        n_meta_written += 1
                        print(f"META      {dst_audio}  (changed: {', '.join(changed)})")
                    except Exception as exc:
                        sys.stderr.write(
                            f"error: failed to copy metadata to {dst_audio}: {exc}\n"
                        )
                else:
                    n_meta_written += 1
                    print(f"META      {dst_audio}  (changed: {', '.join(changed)})")

    print()
    verb = "copied" if args.apply else "to copy"
    parts = [
        f"{n_src_with_lrc} src files with .lrc",
        f"{n_matched_src} matched a version",
        f"{n_copied} .lrc {verb}",
        f"{n_skip_exists} skipped (already present)",
    ]
    if args.copy_metadata:
        meta_verb = "changed" if args.apply else "to change"
        parts.append(f"{n_meta_written} metadata {meta_verb}")
        parts.append(f"{n_meta_same} metadata unchanged")
    parts.append(f"{n_no_match} with no match")
    summary = "Summary: " + ", ".join(parts) + "."
    if not args.apply:
        summary = summary[:-1] + " (dry run -- re-run with --apply)."
    print(summary)
    return 0


if __name__ == "__main__":
    sys.exit(main())
