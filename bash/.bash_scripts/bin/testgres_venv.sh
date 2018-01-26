#!/bin/bash

rm -rf ~/venv/testgres
python3 -m venv ~/venv/testgres
source ~/venv/testgres/bin/activate
cd ~/postgres/testgres
python setup.py install
