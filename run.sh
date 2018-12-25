#!/bin/bash
open -a XQuartz
appname=$(fd --max-depth 2 --type d --exec echo "{/}" \; ".*.app" /Applications | sed -e 's/\.app$//' | dmenu -i -l 10 -p Launch -nf "#ebdbb2" -nb "#333333" -sf "#333333" -sb "#ebdbb2")
if [ -n "$appname" ]; then
  open -a "$appname"
fi
