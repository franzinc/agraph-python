# Project name, must match that provided in setup.py
DISTDIR = agraph-$(VERSION)-client-python

TARNAME = $(DISTDIR).tar.gz

FILES = LICENSE MANIFEST.in README.rst requirements.txt requirements2.txt setup.py src stress tutorial

PATH := /usr/local/python26/bin:/opt/rh/rh-python34/root/usr/bin:$(PATH)

YEAR := $(shell date +%Y)

# Sed regex used to locate the line containing copyright year in LICENSE
COPYRIGHT_REGEX := Copyright (c) 2006-[0-9]* Franz Inc.
# Expected/correct value of that line.
COPYRIGHT_NOW := Copyright (c) 2006-$(YEAR) Franz Inc.

# Important for building pycurl
export PYCURL_SSL_LIBRARY=nss

# Used to download packages, the default is https://pypi.python.org/simple
PIP_INDEX ?= https://san1.franz.com:8443/repository/pypi-group/simple
# If the index is not available over HTTPS users need to pass --trusted-host
PIP_EXTRA_OPTS ?=

# PyPI server used for uploads.
PYPI_REPO_URL ?= https://pypi.python.org/pypi
# GPG key used to sign releases
PYPI_GPG_KEY ?= support@franz.com
# User credentials for PyPI
PYPI_USER ?= franz_inc

# Twine options
ifdef PYPI_REPO
    # Use a name defined in .pypirc
    TWINE_ARGS = -r $(PYPI_REPO)
else
    # Use a raw URL.
    TWINE_ARGS = -r $(PYPI_REPO_URL) --repository-url $(PYPI_REPO_URL) -u $(PYPI_USER)
endif

export AG_PIP_OPTS = --use-wheel --index-url=$(PIP_INDEX) --cert=$(abspath nexus.ca.crt) $(PIP_EXTRA_OPTS)

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
ENVS := $(ENVDIR) $(ENVDIR3) $(TOXENVDIR) stress/env disttest

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
	python ./check-version.py "$(VERSION)"
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

# This environment might initially get an ancient version of pip
# that does not support --trusted-host. So we use a script that 
# checks if that is the case and filters the arguments if necessary.
# Note that pip will be updated and all other environments will get
# a more reasonable version.
$(TOXENVDIR): Makefile
	rm -rf $(TOXENVDIR)
	virtualenv --no-site-packages $(TOXENVDIR)
	. ./$(TOXENVDIR)/bin/activate && pip install -U $$(python pip-hack.py ${AG_PIP_OPTS}) setuptools wheel pip tox twine

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

# This does not use Tox, since the idea is to check if 'pip install'
# will work correctly at the target machine.
disttest: dist FORCE
	# Always recreate the environment from scratch
	rm -rf disttest
	# Use toxenv's virtualenv so we get a recent enough pip
	$(TOXENVDIR)/bin/virtualenv -p python2 --no-site-packages disttest
	# Update to the very latest
	disttest/bin/pip install -U ${AG_PIP_OPTS} setuptools wheel pip
	# Install from the release tarball
	# Make sure pycurl compiles
	PYCURL_SSL_LIBRARY=nss disttest/bin/pip install $(AG_PIP_OPTS) DIST/$(TARNAME)

tutorial: checkPort disttest
	cd tutorial && AGRAPH_PORT=$(AGRAPH_PORT) ../disttest/bin/python runner.py

wheel: $(ENVDIR) dist
	mkdir -p DIST
	rm -f DIST/*.whl
	$(ENVDIR)/bin/pip wheel -e . -w DIST --build-option --universal --no-deps

register: $(TOXENVDIR) wheel
	$(TOXENVDIR)/bin/twine register $(TWINE_ARGS) DIST/*.whl

sign: wheel
	rm -f DIST/*.asc
ifdef AG_GPG_PASSPHRASE
	gpg -u $(PYPI_GPG_KEY) --batch --passphrase "$(AG_GPG_PASSPHRASE)" --detach-sign -a DIST/*.whl
else ifdef AG_GPG_PASSPHRASE_FILE
	gpg -u $(PYPI_GPG_KEY) --batch --passphrase-file "$(AG_GPG_PASSPHRASE_FILE)" --detach-sign -a DIST/*.whl
else
	echo -n "Enter GPG passphrase for $(PYPI_GPG_KEY) to sign the package: " && \
        stty -echo && \
        gpg -u $(PYPI_GPG_KEY) --batch --passphrase-fd 0 \
            --detach-sign -a DIST/*.whl ; \
        RET=$? ; stty echo ; echo ''; \
        exit ${RET}
endif

publish: $(TOXENVDIR) wheel sign
	$(TOXENVDIR)/bin/twine upload $(TWINE_ARGS) DIST/*.whl DIST/*.asc 

tags: FORCE
	etags `find . -name '*.py'`

clean-envs: FORCE
	rm -rf .tox $(ENVS)

fix-copyrights: FORCE
	sed -i'' -e "s/$(COPYRIGHT_REGEX)/$(COPYRIGHT_NOW)/i" LICENSE 
	find src -name '*.py' -print0 | xargs -0 python fix-header.py

# If any of these files change rebuild the virtual environments.
.venv: setup.py requirements.txt requirements2.txt tox.ini Makefile
	$(eval TOX_RECREATE := --recreate)
	touch .venv

FORCE:
