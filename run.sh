#!/bin/bash
# Necessary tools for this script:
# * fd: https://github.com/sharkdp/fd
# * kies: https://github.com/JodusNodus/kies
kiesflags='-fn "Iosevka" -fs 18 -nf "ebdbb2" -sf "fbf1c7" -nb "282828" -sb "333333"'
# kiesflags='-fn "Iosevka" -fs 18 -nf "f8f8f2" -sf "bd93f9" -nb "282a36" -sb "44475a"'
appname=$( \
    fd --max-depth 2 --type d --exec echo "{/}" \; ".*.app" /Applications \
  | sed -e 's/\.app$//' \
  | kies -p "Open " -l 15 -pd 5 -w 50 $kiesflags \
)
if [ -n "$appname" ]; then
  open -a "$appname"
fi
