# aliases.zsh: Sets up aliases which make working at the command line easier.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Looking around, and moving about.
alias ls="ls --color=tty"
alias l="ls -lhB -N --color=tty"

alias ...=../..

# Some application shortcuts.
alias e="emacsclient -nw -c"
alias g="grep -EiRn --color=tty"
alias m="mpv"
alias o="qpdfview"

# The many forms of zmv.
alias zmv="zmv -wM"
alias zcp="zmv -wC"
alias zln="zmv -wL"

# Not exactly an alias, but a workaround for completion's sake.
which hub &> /dev/null; (( 1 - $? )) && function git() { hub "$@" }

# Also not exactly an alias, but it's pretty much aliasing every command that doesn't exist.
if [[ -f /usr/share/doc/pkgfile/command-not-found.zsh ]]; then
   source /usr/share/doc/pkgfile/command-not-found.zsh
fi
