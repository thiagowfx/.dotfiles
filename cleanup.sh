#!/bin/sh
# Maintenance script, intended to be run periodically (for example, with cron).

msg() {
  echo "--> $*..."
}

msg "Running paccache."
paccache -r

msg "Running fstrim on the SSD"
sudo fstrim -v /

# msg "Removing orphan packages."
# sudo pacman -Rns $(pacman -Qdtq)
