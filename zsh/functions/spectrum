#! /bin/zsh
# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
#
# Spectrum accepts an optional argument, indicating the number of colors the terminal actually
# supports. This allows it to gracefully degrade, so that you don't have to write more than version
# of the same thing. By default, this argument is assumed to be 256, which maintains backwards
# compatibility.
#
# TODO: Degrade gracefully through approximation?

# We define three associative arrays, for effects, foreground colors and background colors.
typeset -Ag FX FG BG

FX=(
    reset     "[00m"
    bold      "[01m" no-bold      "[22m"
    italic    "[03m" no-italic    "[23m"
    underline "[04m" no-underline "[24m"
    blink     "[05m" no-blink     "[25m"
    reverse   "[07m" no-reverse   "[27m"
)

local SUPPORT

# Optionally handle impoverished terminals.
if (( $# == 0 )); then
    SUPPORT=256
else
    SUPPORT=$1
fi

# Fill the color maps.
for color in {000..$SUPPORT}; do
    FG[$color]="[38;5;${color}m"
    BG[$color]="[48;5;${color}m"
done
