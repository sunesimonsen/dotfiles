# Source: https://wiki.archlinux.org/index.php/GNOME_Keyring
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xsetroot -solid "#333333"

setxkbmap us altgr-intl -option ctrl:nocaps

if [ -a ~/.bashrc ]; then source ~/.bashrc; fi

exec i3
