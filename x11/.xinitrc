# xrandr --output DVI-I-3 --primary
# xrandr --output DVI-I-0 --right-of DVI-I-3 --rotate left
# xrandr --output DVI-I-0 --right-of DVI-I-3
# xrandr --output DVI-I-0 --off

# Workaround for Nvidia shit: chrome/games/videos appearing on all workspaces
# See https://bbs.archlinux.org/viewtopic.php?pid=1384701#p1384701
# https://bbs.archlinux.org/viewtopic.php?id=175709
# Also see /etc/X11/xorg.conf.d/70-nvidia-composite-fix.conf
# compton -CGb
# xcompmgr -n &
# setxbmap colemak
# xmodmap xmodmap.colemak

source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh
# Handled by systemctl --user enable pulseaudio
# /usr/bin/start-pulseaudio-x11
# hsetroot -solid '#000000'

# Add terminus/etc to X11:
xset +fp /usr/share/fonts/local
xset +fp /home/josh/.local/share/fonts
xset fp rehash

xrdb $HOME/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap -option terminate:ctrl_alt_bksp

# Replace LON,LAT below with appropriate coordinates.
#redshift -l LON,LAT -t 6500:4500 -m vidmode &

# The Razer Naga Epic is way too fast by default. It also needs to be reset
# every time you change from wired to wireless and vice-versa.
xset m 1 1
# xinput set-prop "pointer:Razer Razer Naga Epic" "Device Accel Constant Deceleration" 1.5
xinput set-prop pointer:Razer\ Razer\ Naga\ Epic 'libinput Accel Speed' 0


# No beep
xset -b

xset -dpms
xset s off
xset r rate 250 50

# Cache keys
eval `keychain --eval -q id_rsa`

# Sync PRIMARY with clipboard
# This has spiked cpu usage while running eve
autocutsel -s PRIMARY -fork
#autocutsel -fork

mpd
xscreensaver &
exec xmonad
