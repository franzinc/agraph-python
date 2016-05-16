
DISTDIR = agraph-$(VERSION)-client-python

TARNAME = $(DISTDIR).tar.gz

FILES = LICENSE MANIFEST.in requirements.txt requirements2.txt setup.py src stress tutorial windows-support

PATH := /usr/local/python26/bin:/opt/rh/rh-python34/root/usr/bin:$(PATH)

# TOXENV will have current tox installed.
TOXENVDIR := toxenv
# Testdir is used by external tests and benchmarks
ENVDIR := env

TOX := $(TOXENVDIR)/bin/tox

# Some hosts have only 2.6, some only 2.7...
PYTHON2_PATH := $(shell { command -v python2.7 || command -v python2.6; } 2>/dev/null)
PYTHON2 := $(shell basename "$(PYTHON2_PATH)")

ifeq ($(PYTHON2),python2.7)
    PY2 := py27
else
    PY2 := py26
endif

default: dist

dist: FORCE
ifndef VERSION
	@echo VERSION is not set.
	@echo 1
endif
	rm -fr DIST
	mkdir -p DIST/$(DISTDIR)
	for f in $(FILES); do cp -r $$f DIST/$(DISTDIR); done
	echo $(VERSION) > DIST/VERSION
	echo $(VERSION) > DIST/$(DISTDIR)/VERSION
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

$(TOXENVDIR):
	virtualenv --no-site-packages $(TOXENVDIR)
	. ./$(TOXENVDIR)/bin/activate && python -m pip install -U setuptools wheel pip tox

$(ENVDIR): $(TOXENVDIR)
	$(TOX) -e $(PY2)-env

test-env: $(ENVDIR)

prepush: prepush2 prepush3

prepush2: checkPort $(TOXENVDIR)
	$(TOX) -e $(PY2)-test

prepush3: checkPort $(TOXENVDIR)
	$(TOX) -e py34-test

events: checkPort $(TOXENVDIR)
	$(TOX) -e $(PY2)-events

events3: checkPort $(TOXENVDIR)
	$(TOX) -e py34-events

tags: FORCE
	etags `find . -name '*.py'`

FORCE:
