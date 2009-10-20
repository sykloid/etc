#! /bin/zsh

print $(date +"%A, %B %d, %Y : %H:%M")
sleep $(( 60 - $(date +"%S") ))

while true; do
    print $(date +"%A, %B %d, %Y : %H:%M")
    sleep 60
done
