#!/usr/bin/env bash

kiesflags='-fn "Iosevka" -fs 16 -nf "ebdbb2" -sf "fbf1c7" -nb "282828" -sb "333333" -pd 5 -l 15 -w 50'

prefix=${PASSWORD_STORE_DIR-~/.password-store}
password_files=$( ls -1 $prefix )
password_files=$( printf '%s\n' $password_files | sed 's/\.gpg$//g' )
password=$( printf '%s\n' $password_files | kies -p "Password: " $kiesflags )
[[ -n $password ]] || exit
pass show -c "$password" 2>/dev/null
