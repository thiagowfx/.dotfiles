#!/bin/bash
# Public Domain
# (someone claimed the next lines would be useful for…
#  people. So here goes: © 2012 Stefan Breunig
#  stefan+measure-net-speed@mathphys.fsk.uni-heidelberg.de)

if [ -z "$BLOCK_INSTANCE" ]
  then echo 'please give $BLOCK_INSTANCE'
  exit 1
fi

if ! [ -e "/sys/class/net/${BLOCK_INSTANCE}/operstate" ] || ! [ "`cat /sys/class/net/${BLOCK_INSTANCE}/operstate`" = "up" ]
  then echo " "
  exit 0
fi

# path to store the old results in
path="/dev/shm/measure-net-speed-$BLOCK_INSTANCE"

# grabbing data for each adapter. 
read rx < "/sys/class/net/${BLOCK_INSTANCE}/statistics/rx_bytes"
read tx < "/sys/class/net/${BLOCK_INSTANCE}/statistics/tx_bytes"

# get time
time=$(date +%s)

# write current data if file does not exist. Do not exit, this will cause
# problems if this file is sourced instead of executed as another process.
if ! [[ -f "${path}" ]]; then
  echo "${time} ${rx} ${tx}" > "${path}"
  chmod 0666 "${path}"
fi

# read previous state and update data storage
read old < "${path}"
echo "${time} ${rx} ${tx}" > "${path}"

# parse old data and calc time passed
old=(${old//;/ })
time_diff=$(( $time - ${old[0]} ))

# sanity check: has a positive amount of time passed
if [[ "${time_diff}" -gt 0 ]]; then
  # calc bytes transferred, and their rate in byte/s
  rx_diff=$(( $rx - ${old[1]} ))
  tx_diff=$(( $tx - ${old[2]} ))
  rx_rate=$(( $rx_diff / $time_diff ))
  tx_rate=$(( $tx_diff / $time_diff ))

  # shift by 10 bytes to get KiB/s. If the value is larger than
  # 1024^2 = 1048576, then display MiB/s instead

  # incoming
  rx_kib=$(( $rx_rate >> 10 ))
  if [[ "$rx_rate" -gt 1048576 ]]; then
    printf '%s M↓' "`echo "scale=1; $rx_kib / 1024" | bc`"
  else
    echo -n "${rx_kib} K↓"
  fi

  echo -n "  "

  # outgoing
  tx_kib=$(( $tx_rate >> 10 ))
  if [[ "$tx_rate" -gt 1048576 ]]; then
    printf '%s M↑' "`echo "scale=1; $tx_kib / 1024" | bc`"
  else
    echo -n "${tx_kib} K↑"
  fi
else
  echo -n " ? "
fi
