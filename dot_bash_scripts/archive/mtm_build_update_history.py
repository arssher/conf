#!/usr/bin/env python3

# Makes sure that set of committed xacts is the same and updates history is
# identic everywhere

import re
import sys

nodes = [1, 2, 3]
# we assume keys are [0; numkeys)
numkeys = 1000

# build set of committed gids
def committed_gids(node_id):
    res = set()

    # text logs
    committed_txfinish_re = re.compile(r"TXFINISH: (MTM-.*) committed")
    with open("logs" + str(node_id)) as f:
        for line in f:
            m = committed_txfinish_re.search(line)
            if m is not None:
                gid = m.group(1)
                res.add(gid)

    # wal
    with open("xtx" + str(node_id)) as f:
        for line in f:
            res.add(line.rstrip())

    return res

def updates_history(node_id, committed_gids):
    histories = [[] for i in range(numkeys)]
    pid2gid = {}

    update_local_re = re.compile(r"Updated key (\d+) locally, gid (MTM-\d+-\d+)")
    update_recv_re = re.compile(r"\[(\d+)\]: LOG:  Updated key (\d+) in receiver")
    prepare_re = re.compile(r"\[(\d+)\]: LOG:  Got PREPARE in receiver, gid (MTM-\d+-\d+)")
    with open("logs" + str(node_id)) as f:
        for line in f:
            m = update_local_re.search(line)
            if m is not None:
                key = int(m.group(1))
                gid = m.group(2)
                if gid not in committed_gids:
                    continue
                histories[key].append(gid)
                continue

            m = prepare_re.search(line)
            if m is not None:
                pid = int(m.group(1))
                gid = m.group(2)
                if gid not in committed_gids:
                    continue
                print("got prepare gid {}, pid {}".format(gid, pid))
                pid2gid[pid] = gid
                continue

            m = update_recv_re.search(line)
            # remember that this receiver updated this key, we learn gid on PREPARE
            if m is not None:
                pid = int(m.group(1))
                key = int(m.group(2))
                if pid not in pid2gid:
                    continue
                histories[key].append(gid)
                continue

    return histories



if __name__ == '__main__':
    c_gids = committed_gids(1)
    for node_id in nodes[1:]:
        c_gids2 = committed_gids(node_id)
        if c_gids != c_gids2:
            diff = c_gids ^ c_gids2
            print("Set of committed xacts on nodes 1 and {} is not equal. Diff (also in xacts.diff):".format(node_id))
            with open("xacts.diff", 'w') as f:
                for gid in diff:
                    print(gid)
                    f.write(gid + "\n")
            sys.exit(1)


    # and now check
    histories1 = updates_history(1, c_gids)
    for node in nodes[1:]:
        histories2 = updates_history(node, c_gids)
        for key in range(0, numkeys):
            hist1 = histories1[key]
            hist2 = histories2[key]
            if len(hist1) != len(hist2):
                print("Node {} has more or less updates of key {} than node {}".format(1, key, node_id))
                print("Node 1 updates: {}".format(hist1))
                print("Node {} updates: {}".format(node, hist2))
                sys.exit(1)

            prev_gid = ""
            for i in range(len(hist1)):
                if hist1[i] != hist2[i]:
                    print("Node {} updates key {} with {} {} gids, but node {} with {} {} gids".format(
                        1, key, prev_gid, hist1[i], node_id, prev_gid, hist2[i]))
                    sys.exit(1)
                prev_gid = hist1[i]
