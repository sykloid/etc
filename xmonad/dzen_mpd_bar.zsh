#! /bin/zsh

while true; do
    mpc_output=$(mpc --format "'%title%', by %artist%, from '%album%'")
    if (( $(print $mpc_output | wc -l)  == 1 )); then
        print "Stopped ^fg(#AFAFAF)|"
    else
        mpc_line=$(print $mpc_output | head -1)
        print $mpc_line "^fg(#AFAFAF)|"
    fi
    sleep 5
done
