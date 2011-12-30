# environment.zsh: Sets up a working shell environment.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Various Paths
typeset -U path
path=(~/bin ~/lib/python{2.6,3.1}/bin /opt/android-sdk/platform-tools ~/.cabal/bin $path /usr/bin/vendor_perl /usr/local/bin)
export PATH

typeset -U fpath
fpath=($Z/functions $fpath)

# Find out how many colors the terminal is capable of putting out.
# Color-related settings _must_ use this if they don't want to blow up on less
# endowed terminals.
C=$(tput colors)

# Python per-user site-packages.
export PYTHONUSERBASE=~

# Python Virtualenvwrapper initialization
export WORKON_HOME=~/.virtualenvs

# Important applications.
export EDITOR=vim
export BROWSER=google-chrome

# History Settings
export SAVEHIST=2000
export HISTSIZE=2000
export HISTFILE=~/.zsh_history

# Zsh Reporting
export REPORTTIME=5

# EC2
export EC2_PRIVATE_KEY=~/.ec2/pk-x509-$(hostname)-$(logname).pem
export EC2_CERT=~/.ec2/cert-x509-$(hostname)-$(logname).pem
