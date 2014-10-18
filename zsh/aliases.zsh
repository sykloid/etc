# aliases.zsh: Sets up aliases which make working at the command line easier.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Looking around, and moving about.
alias ls="ls --color=tty"
alias l="ls -lhB --color=tty"

alias ...=../..

# Some application shortcuts.
alias g="grep -EiRn --color=tty"
alias m="mpv"
alias o="okular"

# The many forms of zmv.
alias zmv="zmv -wM"
alias zcp="zmv -wC"
alias zln="zmv -wL"

# Not exactly an alias, but a workaround for completion's sake.
which hub &> /dev/null; (( 1 - $? )) && function git() { hub "$@" }
