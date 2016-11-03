DISTDIR = agraph-$(VERSION)-client-python

TARNAME = $(DISTDIR).tar.gz

FILES = LICENSE MANIFEST.in README.rst requirements.txt requirements2.txt setup.py src stress tutorial

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

# Do not recreate virtualenvs unless necessary
TOX_RECREATE :=
TOX := $(TOXENVDIR)/bin/tox

# List of virtual environments created during build (not including .tox ones).
# stress/env is created by the events test.
ENVS := $(ENVDIR) $(ENVDIR3) $(TOXENVDIR) stress/env

# Some hosts have only 2.6, some only 2.7...
VERSION_SCRIPT := import sys; print('py%d%d' % (sys.version_info[0], sys.version_info[1]))
PY2 := $(shell python2 -c "$(VERSION_SCRIPT)")
PY3 := $(shell python3 -c "$(VERSION_SCRIPT)")

default: dist

dist: FORCE
ifndef VERSION
	@echo VERSION is not set.
	@exit 1
endif
	./check-version.sh "$(VERSION)"
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

$(TOXENVDIR):
	virtualenv --no-site-packages $(TOXENVDIR)
	. ./$(TOXENVDIR)/bin/activate && python -m pip install -U ${AG_PIP_OPTS} setuptools wheel pip tox

$(ENVDIR): $(TOXENVDIR) .venv
	$(TOX) $(TOX_RECREATE) -e $(PY2)-env

$(ENVDIR3): $(TOXENVDIR) .venv
	$(TOX) $(TOX_RECREATE) -e $(PY3)-env

test-env: $(ENVDIR)

wheelhouse: $(ENVDIR) $(ENVDIR3)
	$(ENVDIR)/bin/pip wheel -rrequirements.txt -rrequirements2.txt -w wheelhouse
	$(ENVDIR3)/bin/pip wheel -rrequirements.txt -w wheelhouse

prepush: prepush2 prepush3

prepush2: checkPort $(TOXENVDIR) .venv
	$(TOX) $(TOX_RECREATE) -e $(PY2)-test

prepush3: checkPort $(TOXENVDIR) .venv
	$(TOX) $(TOX_RECREATE) -e $(PY3)-test

events: checkPort $(TOXENVDIR) .venv
	$(TOX) $(TOX_RECREATE) -e $(PY2)-events

events3: checkPort $(TOXENVDIR) .venv
	$(TOX) $(TOX_RECREATE) -e $(PY3)-events

tags: FORCE
	etags `find . -name '*.py'`

clean-envs:
	rm -rf .tox $(ENVS)

# If any of these files change rebuild the virtual environments.
.venv: setup.py requirements.txt requirements2.txt tox.ini
	$(eval TOX_RECREATE := --recreate)
	touch .venv

FORCE:
