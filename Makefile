
DISTDIR = agraph-$(VERSION)-client-python

TARNAME = $(DISTDIR).tar.gz

FILES = LICENSE MANIFEST.in README.rst requirements.txt requirements2.txt setup.py src stress tutorial windows-support

PATH := /usr/local/python26/bin:/opt/rh/rh-python34/root/usr/bin:$(PATH)

# Important for building pycurl
export PYCURL_SSL_LIBRARY=nss

# Prebuilt wheels location
WHEELHOUSE ?= file:///net/san1/disk1/wheelhouse/

# Use prebuilt packages ONLY by default
export AG_PIP_OPTS = --use-wheel --find-links=$(WHEELHOUSE)

ifndef USE_PYPI
AG_PIP_OPTS += --no-index
endif

# TOXENV will have current tox installed.
TOXENVDIR := toxenv
# This dir is used by external tests and benchmarks
ENVDIR := env
ENVDIR3 := env3

TOX := $(TOXENVDIR)/bin/tox

# Some hosts have only 2.6, some only 2.7...
VERSION_SCRIPT := import sys; print('py%d%d' % (sys.version_info[0], sys.version_info[1]))
PY2 := $(shell python2 -c "$(VERSION_SCRIPT)")
PY3 := $(shell python3 -c "$(VERSION_SCRIPT)")

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
	echo $(VERSION) > DIST/$(DISTDIR)/src/franz/VERSION
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

$(ENVDIR3): $(TOXENVDIR)
	$(TOX) -e $(PY3)-env

test-env: $(ENVDIR)

wheelhouse: $(ENVDIR) $(ENVDIR2)
	$(ENVDIR)/bin/pip wheel -rrequirements.txt -rrequirements2.txt -w wheelhouse
	$(ENVDIR3)/bin/pip wheel -rrequirements.txt -w wheelhouse

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
