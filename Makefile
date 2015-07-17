all: upgrade

upgrade:
	git pull --recurse-submodules
	make -C i3blocks

install:
	rcup -v

uninstall:
	rcdn -v

clean:
	make -C i3blocks clean

.PHONY: all upgrade install uninstall clean
