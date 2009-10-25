# environment.zsh: Sets up a working shell environment.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Various Paths
typeset -U path
path=(~/bin ~/lib/python{2.6,3.1}/bin $path /usr/local/bin)
export PATH

# Proxy Settings.
export http_proxy=127.0.0.1:5865
export ftp_proxy=127.0.0.1:5865
export https_proxy=127.0.0.1:5865

# Find out how many colors the terminal is capable of putting out.
# Color-related settings _must_ use this if they don't want to blow up on less
# endowed terminals.
C=$(tput colors)

# Python per-user site-packages.
export PYTHONUSERBASE=~
