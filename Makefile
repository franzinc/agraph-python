
VERSION = 1.0m1a
SERVER_VERSION = 4.0m1a

DISTDIR = agraph-$(SERVER_VERSION)-client-python-$(VERSION)

TARNAME = $(DISTDIR).tar.gz

FILES = src2 stress tutorial windows-support

default: dist

dist: FORCE
	rm -fr DIST
	mkdir -p DIST/$(DISTDIR)
	for f in $(FILES); do cp -r $$f DIST/$(DISTDIR); done
	tar -c -h -z --owner=root --group=root -f DIST/$(TARNAME) -C DIST $(DISTDIR)

FORCE:
