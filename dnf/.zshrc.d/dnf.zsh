# Local DNF cache for `container run`. For local development only; CI must pull fresh packages.
# https://docs.ase.apple.com/docs/base-images/guides/local-dnf-cache/
if (( $+commands[container] )); then
  [[ -d "$HOME/.cache/dnf" ]] || { mkdir -p "$HOME/.cache/dnf" && chmod 777 "$HOME/.cache/dnf"; }
  alias container-cached='container run -it --rm \
    -v "$HOME/.cache/dnf:/var/cache/dnf" \
    -v "$HOME/.config/dnf/dnf.conf:/etc/dnf/dnf.conf:ro"'
fi
