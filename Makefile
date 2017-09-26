VERSION = $(shell python2 -c 'execfile("src/franz/__init__.py"); print __version__')

DISTDIR = agraph-python-$(VERSION)

# Names of distribution files under DIST

# Source distribution (for PyPI and FTP distribution)
SDIST = agraph-python-$(VERSION).tar.gz

# Binary distribution (for PyPI).
WHEEL = agraph_python-$(VERSION)-py2.py3-none-any.whl

PATH := /usr/local/python26/bin:/opt/rh/rh-python34/root/usr/bin:$(PATH)

YEAR := $(shell date +%Y)

# Sed regex used to locate the line containing copyright year in LICENSE
COPYRIGHT_REGEX := Copyright (c) 2006-[0-9]* Franz Inc.
# Expected/correct value of that line.
COPYRIGHT_NOW := Copyright (c) 2006-$(YEAR) Franz Inc.

# Important for building pycurl
export PYCURL_SSL_LIBRARY=nss

# SSL tests have to be enabled explicitly. The reason is that people running
# the test suite without this makefile likely do not have access to AG
# sources and setting up the server with the right certificate and SSL auth
# is quite complex.
export AG_RUN_SSL_TEST=y

# Prevent virtualenv from downloading stuff from PyPI
export VIRTUALENV_NO_DOWNLOAD=y

# Used to download packages, the default is https://pypi.python.org/simple
PIP_INDEX ?= https://san1.franz.com:8443/repository/pypi-group/simple
# If the index is not available over HTTPS users need to pass --trusted-host
# --no-cache-dir is another option that can be added here.
PIP_EXTRA_OPTS ?=

# PyPI server used for uploads.
PYPI_REPO_URL ?= https://upload.pypi.org/legacy/
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

TOX := $(TOXENVDIR)/bin/tox

# List of virtual environments created during build (not including .tox ones).
# stress/env is created by the events test.
ENVS := $(ENVDIR) $(ENVDIR3) $(TOXENVDIR) stress/env disttest

# Some hosts have only 2.6, some only 2.7...
VERSION_SCRIPT := import sys; print('py%d%d' % (sys.version_info[0], sys.version_info[1]))
PY2 := $(shell python2 -c "$(VERSION_SCRIPT)")
PY3 := $(shell python3 -c "$(VERSION_SCRIPT)")

# Note: GPG_PASS_OPTS will only be set in the appropriate target,
# since a prompt might be required to get the passphrase.
GPG_SIGN=gpg -u $(PYPI_GPG_KEY) --batch $(GPG_PASS_OPTS) --detach-sign -a

# Prompt used when reading the passpharse from stdin:
GPG_PROMPT=Enter GPG passphrase for $(PYPI_GPG_KEY) to sign the package:

# Check if it is safe to use the curses-based gpg-agent prompt
# Note that the condition is also true if TERM is empty or not defined.
ifeq ($(TERM),$(filter $(TERM),emacs dumb))
    AG_NO_GPG_AGENT=y
endif

default: dist

prepare-release: FORCE
# Make sure we have a dev version.
	python version.py verify-dev
# Strip '.dev' from the version
	python version.py undev
# Check again.
	python version.py verify-not-dev
# Commit the result
	git add src/franz/__init__.py
	git commit -m "Release `python version.py`"
	git tag -f -m "Release `python version.py`" \
	  -a "release_v`python version.py`"

post-release: FORCE
# We should be in a release version
	python version.py verify-not-dev
# Increment the version and add '.dev'
	python version.py next
# Commit the result
	git add src/franz/__init__.py
	git commit -m "Next dev version: `python version.py`"
# Push (directly, skipping gerrit review).
	git push origin HEAD

checkPort: FORCE
ifndef AGRAPH_PORT
	echo "AGRAPH_PORT not set"
	exit 1
endif
	@echo Using port $(AGRAPH_PORT)

$(TOXENVDIR): Makefile .venv
	rm -rf $(TOXENVDIR)
	virtualenv --no-site-packages $(TOXENVDIR)
	. ./$(TOXENVDIR)/bin/activate && pip install -U ${AG_PIP_OPTS} setuptools wheel pip tox twine

$(ENVDIR): $(TOXENVDIR) .venv
	$(TOX) -e $(PY2)-env

$(ENVDIR3): $(TOXENVDIR) .venv
	$(TOX) -e $(PY3)-env

test-env: $(ENVDIR)

wheelhouse: $(ENVDIR) $(ENVDIR3)
	$(ENVDIR)/bin/pip wheel -rrequirements.txt -rrequirements2.txt -w wheelhouse
	$(ENVDIR3)/bin/pip wheel -rrequirements.txt -w wheelhouse

prepush: prepush2 prepush3

prepush2: checkPort $(TOXENVDIR) .venv
	$(TOX) -e $(PY2)-test
	AG_FORCE_REQUESTS_BACKEND=y $(TOX) -e $(PY2)-test

prepush3: checkPort $(TOXENVDIR) .venv
	$(TOX) -e $(PY3)-test
	AG_FORCE_REQUESTS_BACKEND=y $(TOX) -e $(PY3)-test

events: checkPort $(TOXENVDIR) .venv
	$(TOX) -e $(PY2)-events

events3: checkPort $(TOXENVDIR) .venv
	$(TOX) -e $(PY3)-events

# This does not use Tox, since the idea is to check if 'pip install'
# will work correctly at the target machine.
disttest: wheel $(TOXENVDIR) FORCE
        # Always recreate the environment from scratch
	rm -rf disttest
        # Use toxenv's virtualenv so we get a recent enough pip
	$(TOXENVDIR)/bin/virtualenv -p python2 --no-site-packages disttest
        # Update to the very latest
	disttest/bin/pip install -U ${AG_PIP_OPTS} setuptools wheel pip
        # Install from the release tarball
        # Make sure pycurl compiles
	PYCURL_SSL_LIBRARY=nss disttest/bin/pip install $(AG_PIP_OPTS) DIST/$(SDIST)

tutorial: checkPort disttest
	cd tutorial && AGRAPH_PORT=$(AGRAPH_PORT) ../disttest/bin/python runner.py

wheel: $(ENVDIR)
	mkdir -p DIST
	rm -f DIST/$(WHEEL) DIST/$(SDIST)
	$(ENVDIR)/bin/pip wheel -e . -w DIST --build-option --universal --no-deps
        # Also build a source dist
	$(ENVDIR)/bin/python setup.py sdist -d DIST # --owner=root --group=root 

register: $(TOXENVDIR) wheel
	$(TOXENVDIR)/bin/twine register $(TWINE_ARGS) DIST/$(WHEEL)

sign: wheel
	 rm -f DIST/$(WHEEL).asc DIST/$(SDIST).asc
ifdef AG_GPG_PASSPHRASE
        # Read passphrase from a variable.
        # Note that this is insecure since the passphrase will appear
        # on command line of gpg
	$(eval GPG_PASS_OPTS := --passphrase "$(AG_GPG_PASSPHRASE)")
else ifdef AG_GPG_PASSPHRASE_FILE
	$(eval GPG_PASS_OPTS := --passphrase-file "$(AG_GPG_PASSPHRASE_FILE)")
else ifdef AG_NO_GPG_AGENT
        # Prompt manually to avoid gpg-agent.
        # This is as insecure as using AG_GPG_PASSPHRASE
	$(eval PASS=$(shell read -s -r -p '$(GPG_PROMPT)' PASS && echo $${PASS}))
	$(eval GPG_PASS_OPTS := --passphrase "$(PASS)")
else
        # Just rely on gpg-agent
	$(eval GPG_PASS_OPTS := )
endif
	@$(GPG_SIGN) DIST/$(WHEEL)
	@$(GPG_SIGN) DIST/$(SDIST)

publish: $(TOXENVDIR) wheel sign
	python version.py verify-not-dev
	cp DIST/$(SDIST) CHANGES.rst /fi/ftp/pub/agraph/python-client/
	$(TOXENVDIR)/bin/twine upload $(TWINE_ARGS) DIST/$(WHEEL) DIST/$(WHEEL).asc DIST/$(SDIST) DIST/$(SDIST).asc
	./conda-upload.sh

tags: FORCE
	etags `find . -name '*.py'`

clean-envs: FORCE
	rm -rf .tox $(ENVS)

fix-copyrights: FORCE
	sed -i'' -e "s/$(COPYRIGHT_REGEX)/$(COPYRIGHT_NOW)/i" LICENSE
	find src -name '*.py' -print0 | xargs -0 python2 fix-header.py

# If any of these files change rebuild the virtual environments.
.venv: setup.py requirements.txt requirements2.txt tox.ini Makefile
	rm -rf $(ENVS) .tox
	touch .venv

FORCE:
