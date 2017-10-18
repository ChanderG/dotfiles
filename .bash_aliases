alias c="clear"
alias x="exit"
alias xt="xterm & sleep .8s && transset -a 0.4"

export EDITOR="vim"

# descend into the subdirectories below
function d {
  cd $(find . -type d 2>/dev/null | dmenu -p '>' -l 10)
}

# ascend to the directories directly above you
# has aa known flaw: if 2 anscestors have the same name, defaults to the one higher on the tree
function a {
  path=$(pwd | tr '/' ' ')
  dest=$(for i in $path; do echo $i; done | dmenu -p '<')
  cd "$(echo $(pwd) | awk -F"/$dest" '{print $1}')/$dest"
}

# related to git
alias gl="git log --oneline"

# related to quilt
alias q="quilt"
# tab completion for q too
complete -F _quilt_completion $_quilt_complete_opt q
alias qv='vi $(q files | dmenu -l 10)'
alias qe='q edit $(q files | dmenu -l 10)'
alias qs='q series'
alias qr='q refresh'
alias qa='q add $(find . | dmenu -l 10)'

# grep utils
alias vo="dmenu -p 'vo' -l 25 | awk -F: '{print \"+\"\$2,\$1}' | xargs sh -c 'vim "\$@" < /dev/tty' vim"

# general settings
if [ -f ~/.shell_options ]; then
  source ~/.shell_options
fi

# source local settings
if [ -f ~/.local_aliases ]; then
  source ~/.local_aliases
fi
