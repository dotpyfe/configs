#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn && trayer
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH=420
HEIGHT=12
X_POS=946
Y_POS=754

#Colors and font
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
DZEN_BG="#020202"
DZEN_BG2="#101010"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
CRIT="#99cc66"
CRIT2="#9c0b42"
BAR_FG="#0148d4"
BAR_BG="#363636"
COLOR_SEP=$DZEN_FG2

#Options
ICONPATH="/home/nnoell/.icons/xbm_icons/subtle/"
INTERVAL=5
WIFISIGNAL=0

textBox() {
	echo -n "^fg("$3")^i("$ICONPATH"boxleft.xbm)^bg("$3")^fg("$2")"$1"^bg()^fg("$3")^i("$ICONPATH"boxright.xbm)^fg()"
}

printDiskInfo() {
	RFSP=$(df -h / | tail -1 | awk '{ print $5 }' | tr -d '%')
	HFSP=$(df -h /home | tail -1 | awk '{ print $5 }' | tr -d '%')
	textBox "ROOT ^fg($BAR_FG)"${RFSP}"% ^fg($DZEN_FG2)HOME ^fg($BAR_FG)${HFSP}%" ${DZEN_FG2} ${DZEN_BG2}
	return
}

printBattery() {
	BatPresent=$(acpi -b | wc -l)
	ACPresent=$(acpi -a | grep -c on-line)
	if [[ $BatPresent == "0" ]]; then
		textBox "^fg($DZEN_FG2)AC ^fg($BAR_FG)on ^fg($DZEN_FG2)BAT ^fg($CRIT)off" ${DZEN_FG2} ${DZEN_BG2}
		return
	else
		RPERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
		if [[ $ACPresent == "1" ]]; then
			textBox "BATTERY $RPERC%" ${DZEN_BG} ${CRIT}
		else
			textBox "BATTERY $RPERC%" "" ${CRIT2}
		fi
	fi
	return
}

printBrightnessInfo() {
	BRIFILE=$(cat /sys/class/backlight/acpi_video0/actual_brightness)
	textBox "^fg($DZEN_FG2)BRIGNESS ^fg($BAR_FG)${BRIFILE}" ${DZEN_FG2} ${DZEN_BG2}
	return
}

printWifiInfo() {
	WIFIDOWN=$(wicd-cli --wireless -d | wc -l)
	WIFISIGNAL=0
	if [[ $WIFIDOWN -ne "1" ]]; then
		WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
		textBox "WIFI ^fg($CRIT)$WIFISIGNAL%" ${DZEN_FG2} ${DZEN_BG2}
	else
		textBox "WIFI ^fg($CRIT2)N/A " ${DZEN_FG2} ${DZEN_BG2}
	fi
	return
}

printBottomBar() {
	while true; do
		printDiskInfo
		printBrightnessInfo
		printWifiInfo
		printBattery
		echo
		sleep $INTERVAL
	done
	return
}

#Print all and pipe into dzen2
printBottomBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
