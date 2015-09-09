all:
	git pull --recurse-submodules

install:
	rcup -v

uninstall:
	rcdn -v

clean:
	make -C i3blocks clean

i3blocks:
	make -C i3blocks

.PHONY: all install uninstall clean
