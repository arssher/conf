#!/usr/bin/env python

import sys

def lsn_to_hex(num: int) -> str:
    """ Convert lsn from int to standard hex notation. """
    return "{:X}/{:X}".format(num >> 32, num & 0xffffffff)

lsn = int(sys.argv[1])
print(lsn_to_hex(lsn))
