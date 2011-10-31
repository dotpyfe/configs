#!/bin/zsh

# Configuration
DATE_FORMAT='%A, %l:%M %p '

# Main loop interval in seconds
INTERVAL=1

# function calling interval in seconds
DATEIVAL=1

fdate() {
    date +$DATE_FORMAT
}

DATECOUNTER=$DATEIVAL;

while true; do
    if [ $DATECOUNTER -ge $DATEIVAL ]; then
        PDATE=$(fdate)
        DATECOUNTER=0
    fi

    print "^fg(white)${PDATE}^fg()"

    DATECOUNTER=$((DATECOUNTER+1))

    sleep $INTERVAL
done

