# .zshrc: Configuration for the Z-Shell.
# P.C. Shyamshankar <sykora@lucentbeing.com>

# Where everything is.
Z=~/etc/zsh

# Set up a working environment.
source $Z/environment.zsh

# Set up some aliases
source $Z/aliases.zsh

# Set some options.
source $Z/options.zsh

# Define some functions.
source $Z/functions.zsh

# Set up the Z line editor.
source $Z/zle.zsh

# Set the prompt.
if (( C == 256 )); then
    source $Z/prompt_256.zsh
else
    source $Z/prompt.zsh
fi

# Set up some colors for directory listings.
if (( C == 256 )); then
    source $Z/ls_colors_256.zsh
fi

# Initialize the completion system.
source $Z/completion.zsh
