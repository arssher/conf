#! /usr/bin/env python3

"""
Convert plain text domains list to Amnezia JSON

Usage:
    python3 list2amnezia.py my_list.txt

Input file CAN contains:
    - 1 hostname per line
    - 1 IP or subnet per line
    - Comments started with # (to be ignored)
    - Empty lines (to be ignored)

Requires python>=3.6
"""

import json
import sys
from pathlib import Path

INDENT=4

def main(input_file: str) -> None:
    inp_p = Path(input_file).absolute()
    with inp_p.open('r') as f:
        result = []
        for line in f.readlines():
            val = line.strip()
            if not val:
                continue
            if val.startswith('#'):
                comm = val.lstrip("#").lstrip()
                print(f'Comment: `{comm}`')
                continue
            # Current AmntziaVPN==4.8.14.5 expors IPs as hostnames
            result.append({'hostname': val, 'ip': ''})


    out_p = inp_p.with_suffix('.json')
    with Path(out_p).open('w') as f:
        json.dump(result, fp=f, indent=INDENT)

    print(f'{inp_p.name} -> {out_p.name}: OK')


if __name__ == '__main__':
    if len(sys.argv) != 2:
        dosa = '\nExpected exactly 1 input file as an argument\n'
        print(dosa, file=sys.stderr)
        sys.exit(1)
    main(sys.argv[1])
