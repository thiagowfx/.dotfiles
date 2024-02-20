# Inspiration from https://frantic.im/cdtmp/ and grml-zsh-config's cdt
# Usage: cdtmp [foo]
cdtmp() {
	builtin cd $(mktemp -d "/tmp/$USER-${1:+$1-}XXXXXX")
	builtin pwd
}
