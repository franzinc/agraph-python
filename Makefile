
VERSION = 1.0m1

DISTDIR = agraph-4.0m1-client-python-$(VERSION)

TARNAME = $(DISTDIR).tar.gz

FILES = src2 stress tutorial windows-support

default: dist

dist: FORCE
	rm -f *.tar.gz
	rm -fr $(DISTDIR)
	mkdir -p $(DISTDIR)
	for f in $(FILES); do cp -r $$f $(DISTDIR); done
	tar -c -h -z --owner=root --group=root -f $(TARNAME) $(DISTDIR)

FORCE:
