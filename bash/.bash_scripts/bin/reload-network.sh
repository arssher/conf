#!/bin/bash

set -e

# turn off and on all NM's connections
sudo nmcli nm enable false && sleep 1 && sudo nmcli nm enable true 

# bonus: that's how you connect to wifi from cmd using network manager:
#sudo /usr/lib/2013.com.canonical.certification:checkbox/bin/create_connection wifi "host7314(netorn.ru)" -S wpa -K "89057218970"
# this script can be found in package plainbox-provider-checkbox.
# See http://askubuntu.com/questions/16584/how-to-connect-and-disconnect-to-a-network-manually-in-terminal for more details
