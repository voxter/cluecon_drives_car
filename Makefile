
all: server

server:
	$(MAKE) -C cluecon_drives_car

clean:
	$(MAKE) -C cluecon_drives_car clean

distclean:
	$(MAKE) -C cluecon_drives_car distclean