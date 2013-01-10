xrdb $HOME/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap -option terminate:ctrl_alt_bksp

# Replace LON,LAT below with appropriate coordinates.
#redshift -l LON,LAT -t 6500:4500 -m vidmode &

# The Razer Naga Epic is way too fast by default. It also needs to be reset
# every time you change from wired to wireless and vice-versa.
xset m 1 1
xinput set-prop "pointer:Razer Razer Naga Epic" "Device Accel Constant Deceleration" 1

# Cache keys
eval `keychain --eval -q id_dsa`

# Sync PRIMARY with clipboard
autocutsel -s PRIMARY -buttonup -fork &

mpd
exec xmonad
