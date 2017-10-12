#!/usr/bin/env python

'''
Print dns address of nth ec2 node, nodes are sorted by public ip.
'''

import os
import socket
import argparse

from pprint import pprint
from boto import ec2

def main(idx, u):
    conn = ec2.connect_to_region(
        'eu-central-1',
        aws_access_key_id=os.environ['AWS_ACCESS_KEY_ID'],
        aws_secret_access_key=os.environ['AWS_SECRET_ACCESS_KEY'])

    reservations = conn.get_all_instances()
    instances = [i for r in reservations for i in r.instances if i.state != 'terminated']
    instances.sort(key = lambda i: socket.inet_aton(i.ip_address))

    dns = instances[idx].public_dns_name
    if u:
        print('ubuntu@{}'.format(dns))
    else:
        print(dns)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", help="index of node", type=int, default=0)
    parser.add_argument('-u', action='store_true',
                        help="add ubuntu@ to output", default=True)
    args = parser.parse_args()
    main(args.i, args.u)
