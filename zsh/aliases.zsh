# aliases.zsh: Sets up aliases which make working at the command line easier.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Looking around, and moving about.
alias ls="ls --color=tty"
alias l="ls -lhB --color=tty"

alias ...=../..

# Some application shortcuts.
alias g="grep -EiRn --color=tty"
alias m="mplayer"
alias o="okular"
alias u="uzbl"
alias z="zathura"

# The many forms of zmv.
alias zmv="zmv -wM"
alias zcp="zmv -wC"
alias zln="zmv -wL"
