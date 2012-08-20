# functions.zsh: Custom functions, and function invocations.
# P.C. Shyamshankar <sykora@lucentbeing.com>

if (( C == 256 )); then
    autoload spectrum && spectrum # Set up 256 color support.
fi

# Autoload some useful utilities.
autoload -Uz zmv
autoload -Uz zargs
autoload -Uz vcs_info

case $TERM in
    *xterm*|*rxvt*|*screen*)
        # Special function precmd, executed before displaying each prompt.
        function precmd() {
            # Set the terminal title to the current working directory.
            print -Pn "\e]0;%n@%m:%~\a"

            # Get the current git branch into the prompt.
            vcs_info
        }

        # Special function preexec, executed before running each command.
        function preexec () {
            # Set the terminal title to the curently running command.
            print -Pn "\e]2;[${2:q}]\a"
        }
esac
