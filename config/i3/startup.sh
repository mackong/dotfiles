function touchpad_config() {
    x=`xinput list | grep -i Touchpad | cut -d'=' -f 2 | awk '{print $1}'`
    p1=`xinput list-props $x | grep 'Natural Scrolling Enabled (' | cut -d'(' -f 2 | cut -d')' -f 1`
    p2=`xinput list-props $x | grep 'Tapping Enabled (' | cut -d'(' -f 2 | cut -d')' -f 1`
    xinput --set-prop $x $p1 1
    xinput --set-prop $x $p2 1
}

touchpad_config

xset s off
xset -dpms

autorandr -l hdmi
i3-msg 'workspace 1'

feh --bg-scale /home/mackong/.local/share/wallpapers/wallpaper.png

fcitx5 -d

/usr/lib/polkit-kde-authentication-agent-1 &
