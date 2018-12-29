# vim:shiftwidth=2 foldmethod=marker
fpath+=~/.zfunc

autoload -Uz compinit
compinit

HISTSIZE=1000
SAVEHIST=1000
HYPHEN_INSENSITIVE="true"

# zprezto {{{
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
# }}}

# fzf {{{
source ~/.nix-profile/share/fzf/key-bindings.zsh
# }}}

# nnn {{{
export NNN_USE_EDITOR=1
# }}}

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'

# Theme {{{
#  0 black
#  1 red
#  2 green
#  3 yellow
#  4 blue
#  5 magenta
#  6 cyan
#  7 white
#  8 bright black
#  9 bright red
# 10 bright green
# 11 bright yellow
# 12 bright blue
# 13 bright magenta
# 14 bright cyan
# 15 bright white

setopt prompt_subst

local ret_status="%(?:%F{2}:%F{1})"
PS1="%F{4}%c%F{7} %B${ret_status}::%b%F{15} "

generate_rps1 () {
  git symbolic-ref HEAD &>/dev/null
  if [ $? -ne 0 ]; then
    return
  fi
  git branch &>/dev/null
  if [ $? = 0 ]; then
    local GIT_PROMPT=""
    GIT_PROMPT=$GIT_PROMPT"[%B%F{4}"

    local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
    local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"

    # Branch
    GIT_PROMPT=$GIT_PROMPT"${$(git symbolic-ref HEAD)#refs/heads/}"

    # Ahead
    if [ "$NUM_AHEAD" -gt 0 ]; then
      GIT_PROMPT=$GIT_PROMPT" %F{2}$NUM_AHEAD+"
    fi

    # Behind
    if [ "$NUM_BEHIND" -gt 0 ]; then
      GIT_PROMPT=$GIT_PROMPT" %F{1}$NUM_AHEAD-"
    fi

    GIT_PROMPT=$GIT_PROMPT"%F{7}%b]"
    echo "$GIT_PROMPT"
  fi
}
RPS1='$(generate_rps1)'

# grep colours
export GREP_COLORS='1;32'
export GREP_COLOR=$GREP_COLORS

# less colours
export LESS_TERMCAP_mb=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_md=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 1) # red
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

# fzf colours
export FZF_DEFAULT_OPTS='
  --color fg:7,bg:0,hl:5
  --color fg+:7,bg+:0,hl+:5
  --color pointer:4,prompt:2,spinner:2,marker:2
'
# }}}

# Python {{{
alias setpython3path="unset PYTHONPATH && export PYTHONPATH=/usr/local/lib/python3.6/site-packages/"
alias setpython2path="unset PYTHONPATH && export PYTHONPATH=/usr/local/lib/python2.7/site-packages/"
unset PYTHONPATH
# setpython3path
# }}}

# Lang {{{
export LC_ALL="nl_NL.UTF-8"
export LANG="nl_NL.UTF-8"
# }}}

# Editors {{{
export EDITOR=nvim
export VISUAL=nvim
export PAGER=less
# }}}

# Aliases {{{
alias ga="git add"
alias gc="git commit --verbose"

alias md="mkdir -p"
alias vi="nvim"
# alias vim="nvim"
alias scrot="screencapture -t png"
alias watch1="watch -t -d -n 1"
alias :q="exit"
# alias ls="exa -F"
alias ls="ls -G"
alias ll="ls -lh"
alias la="ll -a"

alias vimrc="$EDITOR ~/.vim/vimrc"
# }}}

# Functions {{{
ip () {
  echo "Public ip: $(curl -4fNs ipinfo.io/ip)"
  echo "Local ip: $(ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')"
}
pdf () {
  # Open a pdf with mupdf.
  /Applications/mupdf.app/Contents/MacOS/mupdf -I $@ &!
}
copy () {
  # Copy text to the clipboard.
  echo "$@" | pbcopy
}
ed () {
  # Change ed's prompt to ":".
  command ed -p":" "$@"
}
# }}}
