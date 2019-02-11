# environment.zsh: Sets up a working shell environment.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Various Paths
typeset -xU PATH path=(~/bin ~/.cabal/bin ~/.opam/system/bin $path)

typeset -xU FPATH fpath=($Z/functions $fpath)

# Find out how many colors the terminal is capable of putting out.
# Color-related settings _must_ use this if they don't want to blow up on less
# endowed terminals.
C=$(tput colors)

# Java Options
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=javax.swing.plaf.metal.MetalLookAndFeel'

# OCaml Options
export OCAML_TOPLEVEL_PATH=~/.opam/system/lib/toplevel
export CAML_LD_LIBRARY_PATH=~/.opam/system/lib/stublibs:/usr/lib/ocaml/stublibs

# Important applications.
export EDITOR="emacsclient -nw"
export BROWSER=chromium

# History Settings
export SAVEHIST=2000
export HISTSIZE=2000
export HISTFILE=~/.zsh_history

# Zsh Reporting
export REPORTTIME=5

# EC2
export EC2_PRIVATE_KEY=~/.ec2/pk-x509-$(hostname)-$(whoami).pem
export EC2_CERT=~/.ec2/cert-x509-$(hostname)-$(whoami).pem

# Allow GTK applications to make use of X11 dead keys, including they multi-key.
export GTK_IM_MODULE="xim"

# Compensate for non-existent configuration settings for QT5 applications.
export QT_QPA_PLATFORMTHEME="qt5ct"

typeset -x SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh
