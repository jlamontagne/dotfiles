xrdb $HOME/.Xresources
xsetroot -cursor_name left_ptr

# The Razer Naga Epic is way too fast by default. It also needs to be reset
# every time you change from wired to wireless and vice-versa.
xset m 1 1
xinput set-prop "pointer:Razer Razer Naga Epic" "Device Accel Constant Deceleration" 2

# Replace LON,LAT below with appropriate coordinates.
#redshift -l LON,LAT -t 6500:4500 -m vidmode &

# Cache keys
eval `keychain --eval -q id_dsa`

setxkbmap -option terminate:ctrl_alt_bksp

# Sync PRIMARY with clipboard
autocutsel -s PRIMARY -buttonup -fork &

exec xmonad
