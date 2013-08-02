# touchpad tweaking (synaptics)
# see also /etc/X11/xorg.conf.d/10-synaptics.conf
synclient TapButton1=1
synclient TapButton2=2              # this way 2 fingers = middle button and 3 fingers = right button
synclient TapButton3=3
synclient VertEdgeScroll=1
synclient HorizEdgeScroll=1
synclient VertTwoFingerScroll=1
synclient HorizTwoFingerScroll=1
synclient PalmDetect=1
synclient PalmMinWidth=10
synclient PalmMinZ=200

# set my keyboard to br
setxkbmap br

xbindkeys &
