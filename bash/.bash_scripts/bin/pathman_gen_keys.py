#!/usr/bin/env python3

# Get all <table_name>_partnum tables and generate to stdout numbers such
# that get_hash_part_idx(hashint4(aid), totalparts) = one of partnum.

import sys
import os
import random
from subprocess import check_output

if len(sys.argv) < 3:
   sys.stderr.write(format("Usage: {0} <num_of_keys> <table_name> [psql_opts]\n",
                os.path.basename(__file__)))
   sys.exit(1)

num_of_keys = int(sys.argv[1])
sys.stderr.write("num of keys: {}\n".format(num_of_keys))
table_name = sys.argv[2]
psql_opts = sys.argv[3]

total_parts = int(check_output(
   "psql -qtA {0} -c \"select count(*) from pathman_partition_list where parent = '{1}'::regclass\"".format(psql_opts, table_name), shell=True))
if total_parts == 0:
   sys.stderr.write("Seems like {0} is not partitioned\n".format(table_name))
   sys.exit(1)
sys.stderr.write("total parts: {0}\n".format(total_parts))

part_ids_str = check_output(
   """
   psql -qtA {0} -c "select substring(relname, 'pgbench\_accounts_(\d+)$')
     from pg_class where relname ~ 'pgbench\_accounts_\d+$';"
   """.format(psql_opts, table_name), shell=True)
part_ids = [int(x) for x in part_ids_str.decode("ascii").splitlines()]
sys.stderr.write("part ids are {}\n".format(part_ids))

key_count = 0
while key_count != num_of_keys:
   potential_key = random.randint(1, 2147483647)
   target_part = int(check_output(
      """
      psql -qtA {} -c "select get_hash_part_idx(hashint4({}), {})"
      """.format(psql_opts, potential_key, total_parts), shell=True))
   if target_part in part_ids:
      print(potential_key)
      key_count += 1
