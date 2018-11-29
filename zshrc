# vim: shiftwidth=2
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
# }}}

# Python {{{
alias setpython3path="unset PYTHONPATH && export PYTHONPATH=/usr/local/lib/python3.6/site-packages/"
alias setpython2path="unset PYTHONPATH && export PYTHONPATH=/usr/local/lib/python2.7/site-packages/"
unset PYTHONPATH
# setpython3path
# }}}

# Lang {{{
export LC_ALL="en_GB.UTF-8"
export LANG=en_GB.UTF-8
# }}}

# Editors {{{
export EDITOR=nvim
export VISUAL=nvim
# }}}

# Aliases {{{
alias ga="git add"
alias md="mkdir -p"
alias vi="nvim"
# alias vim="nvim"
alias scrot="screencapture -t png"
alias cr="cargo run"
alias cb="cargo build"
alias fuck="sudo !!"
alias watch1="watch -t -d -n 1"
alias love="open -na love"
alias :q="exit"
alias ls="exa -F"
alias ll="ls -lh"
alias la="ll -a"
alias rm="safe-rm"
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
code () {
  # Open VS Code manually, i.e. without their shell script that requires python 2.
  open -a Visual\ Studio\ Code "$@"
}
ed () {
  # Change ed's prompt to ": ".
  command ed -p":" "$@" ;
}
# }}}

# if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
