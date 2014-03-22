#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess
from i3pystatus import Status

status = Status(standalone=True)
default_color="#ffffff"
secondary_color="881010"

# Clock
# 2014-03-02 17:05 KW09
status.register("clock",
                format="%Y-%m-%d %H:%M KW%V",)

# ALSA
status.register("alsa",
                format="♪ {volume}",
                color=default_color,
                color_muted=secondary_color,
)

# Backlight
status.register("backlight",
                format="L: {brightness}%",
                color=default_color,)


# Shows the average load of the last minute and the last 5 minutes
# (the default value for format is used)
# status.register("load")

# Shows your CPU temperature, if you have a Intel CPU
# status.register("temp",
#                     format="{temp:.0f}°C",)

# Battery
status.register("battery",
                battery_ident="BAT1",
                format="{status} [{percentage_design:.1f}%]", # {remaining:%E%hh:%Mm}",
                alert=True,
                alert_percentage=15,
                status={
                    "DIS": "↓D",
                    "CHR": "↑C",
                    "FULL": "=",
                },)

# Displays whether a DHCP client is running
# status.register("runwatch",
#                     name="DHCP",
#                     path="/var/run/dhclient*.pid",)

# Shows the address and up/down state of eth0. If it is up the address is shown in
# green (the default value of color_up) and the CIDR-address is shown
# (i.e. 10.10.10.42/24).
# If it's down just the interface name (eth0) will be displayed in red
# (defaults of format_down and color_down)
#
# Note: the network module requires PyPI package netifaces-py3
# status.register("network",
#                     interface="eth0",
#                     format_up="{v4cidr}",)

# Has all the options of the normal network and adds some wireless specific things
# like quality and network names.
#
# Note: requires both netifaces-py3 and basiciw
status.register("network",
                interface="wlp2s0",
                color_up=default_color,
#                format_up="{v4cidr}",
                format_up="{v4}",
                format_down="",)

status.register("wireless",
                interface="wlp2s0",
                color_up=default_color,
                format_up="W: {essid} {quality:.0f}%",
                format_down="W: down",)

# status.register("network",
#                 interface="enp1s0",
#                 color_up=default_color,
#                 format_up="E: {v4cidr}",
#                 format_down="",)

# Shows pulseaudio default sink volume
#
# Note: requires libpulseaudio from PyPI
# status.register("pulseaudio",
#                     format="♪{volume}",)

# Memory
status.register("mem",
#                format="{used_mem:.0f} MiB ({percent_used_mem}%)",
                format="{used_mem:.0f} MiB",
                color=default_color,
                warn_color=default_color,
                alert_color=default_color,
)


# Shows disk usage of /
# Format:
# 42/128G [86G]
status.register("disk",
                path="/home",
#                format="{used}/{total}G [{avail}G]",)
                format="{avail:.0f}G @ /home",)


# Shows mpd status
# Format:
# Cloud connected▶Reroute to Remain
# status.register("mpd",
#                     format="{title}{status}{album}",
#                     status={
#                                 "pause": "▷",
#                                 "play": "▶",
#                                 "stop": "◾",
#                             },)

status.run()
