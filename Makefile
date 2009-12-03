
VERSION := $(shell fgrep __version__ src2/franz/__init__.py | sed -e 's/^.*"\(.*\)".*/\1/')

DISTDIR = agraph-client-python-$(VERSION)

TARNAME = $(DISTDIR).tar.gz

FILES = src2 stress tutorial windows-support

default: dist

dist: FORCE
	rm -fr DIST
	mkdir -p DIST/$(DISTDIR)
	for f in $(FILES); do cp -r $$f DIST/$(DISTDIR); done
	tar -c -h -z --owner=root --group=root -f DIST/$(TARNAME) -C DIST $(DISTDIR)
ifdef DESTDIR
	cp -p DIST/$(TARNAME) $(DESTDIR)
endif

FORCE:
