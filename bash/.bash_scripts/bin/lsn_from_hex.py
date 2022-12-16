#!/usr/bin/env python

import sys

def lsn_from_hex(lsn_hex: str) -> int:
    """ Convert lsn from hex notation to int. """
    l, r = lsn_hex.split('/')
    return (int(l, 16) << 32) + int(r, 16)

hexed = sys.argv[1]
print(lsn_from_hex(hexed))
