#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

urxvtd -q -f -o
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

# exec gnome-session
# exec startkde
# ...or the Window Manager of your choice

# normal XFCE4
# exec ck-launch-session startxfce4

# add me some awesome fonts!
xset +fp /usr/share/fonts/local
xset fp rehash

##########
# Xmonad #
##########
xsetroot -cursor_name left_ptr
# load resources
xrdb -merge .Xdefaults
# set up icon tray
#trayer --edge top --align right --SetDockType true --SetPartialStrut true\
#  --expand true --width 10 --transparent true --tint 0x191970 --height 20 &
# set bg color
#xsetroot -solid midnightblue
# set desktop background
# apps
xscreensaver -no-splash &
xcompmgr -cCfF -D5 -o0 -r0 &
hsetroot -full /home/maciek/Downloads/wallpaper-514915.jpg

if [ -x /usr/bin/nm-applet ] ; then
  nm-applet --sm-disable &
fi

if [ -x /usr/bin/gnome-power-manager ] ; then
  sleep 3
  gnome-power-manager &
fi

exec ck-launch-session xmonad
