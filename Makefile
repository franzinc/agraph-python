
DISTDIR = agraph-$(VERSION)-client-python

TARNAME = $(DISTDIR).tar.gz

FILES = LICENSE franz stress tutorial windows-support

PATH := /usr/local/python26/bin:$(PATH)

default: dist

dist: FORCE
ifndef VERSION
	@echo VERSION is not set.
	@echo 1
endif
	rm -fr DIST
	mkdir -p DIST/$(DISTDIR)
	for f in $(FILES); do cp -r $$f DIST/$(DISTDIR); done
	tar -c -h -z --owner=root --group=root -f DIST/$(TARNAME) -C DIST $(DISTDIR)
ifdef DESTDIR
	cp -p DIST/$(TARNAME) $(DESTDIR)
endif

prepush: FORCE
	@echo Using port $(AGRAPH_PORT)
	nosetests

FORCE:
