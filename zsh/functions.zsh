case $TERM in
    *xterm*|*rxvt*)
        # Special function precmd, executed before displaying each prompt.
        function precmd() {
            # Set the terminal title to the current working directory.
            print -Pn "\e]0;%~: %n@%m\a"
        }

        # Special function preexec, executed before running each command.
        function preexec () {
            # Set the terminal title to the currently executing command.
            print -Pn "\e]0;$1 (%~): %n@%m\a"
        }
esac
