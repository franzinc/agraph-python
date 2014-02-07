
DISTDIR = agraph-$(VERSION)-client-python

TARNAME = $(DISTDIR).tar.gz

FILES = LICENSE src2 stress tutorial windows-support

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

checkPort: FORCE
ifndef AGRAPH_PORT
	echo "AGRAPH_PORT not set"
	exit 1
endif
	@echo Using port $(AGRAPH_PORT)

prepush: prepush2 prepush3

prepush2: checkPort
	cd src2; python `which nosetests`

prepush3: checkPort
	cd src3; python3 -m nose

events: checkPort
	cd stress/events ; python ./events -s 10m -l 8 -q 8

events3: checkPort
	cd stress3/events ; python3 ./events -s 10m -l 8 -q 8

tags: FORCE
	etags `find . -name '*.py'`

FORCE:
