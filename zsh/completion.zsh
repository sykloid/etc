# completion.zsh: Directives for the Z-Shell completion system.
# P.C. Shyamshankar <sykora@lucentbeing.com>

autoload -Uz compinit && compinit

zstyle ':completion:*' list-colors "${LS_COLORS}" # Complete with same colors as ls.
zstyle ':completion:*' max-errors 2 # Be lenient to 2 errors.
zstyle ':completion:*' completer _expand _complete _correct _approximate # Completion modifiers.
zstyle ':completion:*' use-cache true # Use a completion cache.
zstyle ':completion:*' ignore-parents pwd # Ignore the current directory in completions.
