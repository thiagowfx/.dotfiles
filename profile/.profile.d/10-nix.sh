# https://nixos.wiki/wiki/Locales
# Troubleshooting when using nix on non-NixOS linux distributions
[[ "$(uname -s)" == "Linux" ]] && export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive"

# https://ariya.io/2020/05/nix-package-manager-on-ubuntu-or-debian
src_files "$HOME/.nix-profile/etc/profile.d/nix.sh"
