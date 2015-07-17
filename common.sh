# colors
source_if_exists "$HOME/.base16-atelierlakeside.dark.sh"

# termite
source_if_exists "/etc/profile.d/vte.sh"

if [[ $TERM == xterm-termite ]]; then
      . /etc/profile.d/vte.sh
        __vte_prompt_command
fi

# Templates are functions/snippets for general use.
# They are aliased preceded with "t-".
#
# The first argument should be a function defined internally, preceded with _.
#
# A program can be optionally specified as second argument; if it is not found
# in the PATH, then the template won't be defined.
#
# Similarly, you can specify a file as third argument. If the file is not found,
# then the template won't be defined either.
#
# Arguments:
#  $1: template name
#  $2: a program (e.g. vim or /usr/bin/vim)
#  $3: a file (e.g. /boot/grub/grub.cfg)
#
addtemplate() {
  [[ "x$2" != "x" ]] && ! command -v "$2" &>/dev/null && return
  [[ "x$3" != "x" ]] && [ ! -e "$3" ] && return
  alias "t-$1"="_$1"
}

for f in "$HOME"/.alias.d/*
do
	source $f
done
