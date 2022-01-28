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

# related to syncing files

alias sync_blog_dryrun="aws s3 sync --dryrun ./blog s3://www.chandergovind.org/blog/ --acl public-read --size-only"
alias sync_blog="aws s3 sync ./blog s3://www.chandergovind.org/blog/ --acl public-read --size-only"
alias sync_index="aws s3 cp ./index.html s3://www.chandergovind.org/index.html --acl public-read"
alias sync_boty="aws s3 cp ./boty.html s3://www.chandergovind.org/boty.html --acl public-read"
alias sync_readings="aws s3 cp ./readings.html s3://www.chandergovind.org/readings.html --acl public-read"
alias sync_quotes="aws s3 cp ./quotes.html s3://www.chandergovind.org/quotes.html --acl public-read"

alias sync_writings="aws s3 cp ./Writings.zip s3://writings.chandergovind.org/"

# general settings
if [ -f ~/.shell_options ]; then
  source ~/.shell_options
fi

# source local settings
if [ -f ~/.local_aliases ]; then
  source ~/.local_aliases
fi
