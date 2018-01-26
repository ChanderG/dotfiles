alias c="clear"
alias x="exit"
alias xt="xterm & sleep .8s && transset -a 0.4"

export EDITOR="vim"

# better management of large ouput commands
# log interactive -> only filter by terms
alias li="tee lastcommand.log | egrep -C3 -n "
# simply open the full output
alias lo="vi lastcommand.log"

# related to git
alias gl="git log --oneline"

# xmessage output
alias xm='xmessage -file - -center -default okay -center -fg "#eeeeee" -bg "#003366"'

# related to quilt
alias q="quilt"
# tab completion for q too
complete -F _quilt_completion $_quilt_complete_opt q
alias qv='vi $(q files | dmenu -l 10)'
alias qe='vi $(q files | dmenu -l 10); q refresh'
alias qs='q series'
alias qr='q remove $(q files | dmenu -l 10)'
alias qa='q add $(find . | dmenu -l 10)'

# related to emacs/orgmode
alias omtodos="emacs -batch -l ~/.emacs -eval '(org-batch-agenda \"t\")' | tail -n+3"

# general settings
if [ -f ~/.shell_options ]; then
  source ~/.shell_options
fi

# source local settings
if [ -f ~/.local_aliases ]; then
  source ~/.local_aliases
fi
