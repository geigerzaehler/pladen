#!/bin/bash

PATH=~/bin:$PATH
APP_DIR=~/www/pladen

cd "$APP_DIR"
git fetch origin
git reset --hard origin/master
make server && sudo /sbin/restart pladen
