# aliases.zsh: Sets up aliases which make working at the command line easier.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Looking around, and moving about.
alias ls="ls --color=tty"
alias l="ls -lhB --color=tty"

alias ...=../..

# Some application shortcuts.
alias b="sudo -E bauerbill"
alias g="grep -EiRn --color=tty"
alias m="mplayer"
alias o="okular"

# The many forms of zmv.
alias zmv="zmv -wM"
alias zcp="zmv -wC"
alias zln="zmv -wL"

# Start a Vim Server, I'm told it's useful.
alias vim='vim --servername $(date +"%Y-%m-%D/%H:%M:%S")'

# Not exactly an alias, but a workaround for completion's sake.
which hub > /dev/null; (( 1 - $? )) && function git() { hub "$@" }
