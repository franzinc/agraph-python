VERSION = $(shell python2 -c 'execfile("src/franz/__init__.py"); print __version__')

DISTDIR = agraph-python-$(VERSION)

# Names of distribution files under DIST

# Source distribution (for PyPI and FTP distribution)
SDIST = agraph-python-$(VERSION).tar.gz

# Binary distribution (for PyPI).
WHEEL = agraph_python-$(VERSION)-py2.py3-none-any.whl

# Package repositories on SAN1
NEXUS_PYPI = https://san1.franz.com:8443/repository/pypi-group/simple
# NEXUS_CONDA = https://san1.franz.com:8443/repository/anaconda-proxy/anaconda
NEXUS_CONDA = https://san1.franz.com:8443/repository/conda-forge-proxy


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

# Used to download packages:
#  - Inside Franz we want to use Nexus on SAN1
#  - If that is not available use the default PyPI index / Anaconda channel
#  - In both cases one can override the choice by setting PIP_INDEX
#    or CONDA_OPTS.
ifeq ($(shell domainname),franz.com)
    USE_NEXUS ?= y
endif

ifeq ($(USE_NEXUS),y)
    # Nexus repository on SAN1
    PIP_INDEX ?= $(NEXUS_PYPI)

    # Certificate verification for Nexus fails on some boxes
    export REQUESTS_CA_BUNDLE=$(abspath nexus.ca.crt)
    PIP_CERT = --cert=$(abspath nexus.ca.crt)

    # Use the Nexus proxy, disable default channels.
    CONDA_OPTS ?= -c $(NEXUS_CONDA) -k --override-channels
else
    # Global PyPI index
    PIP_INDEX ?= https://pypi.python.org/simple
    # Do not set CONDA_OPTS - use defaults or ~/.condarc
endif


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

export AG_PIP_OPTS = --index-url=$(PIP_INDEX) $(PIP_CERT)  $(PIP_EXTRA_OPTS)

# TOXENV will have current tox installed.
TOXENVDIR := toxenv
# This dir is used by external tests and benchmarks
ENVDIR := env
ENVDIR3 := env3

TOX := $(TOXENVDIR)/bin/tox

# List of virtual environments created during build (not including .tox ones).
# stress/env is created by the events test.
ENVS := $(ENVDIR) $(ENVDIR3) $(TOXENVDIR) stress/env disttest

# Note: GPG_PASS_OPTS will only be set in the appropriate target,
# since a prompt might be required to get the passphrase.
GPG_SIGN=gpg --batch $(GPG_PASS_OPTS) --detach-sign -a

default: wheel

# Python installation - we use conda to create an environment
# for each Python version we want to test on.
# Conda installers and packages needed to create the environments
# are stored on the SAN, but can be downloaded directly from
# the internet if needed.

# Versions we want to test on
PYTHONS=2.7 3.5 3.6
PYTHONS2=$(filter 2.%,$(PYTHONS))
PYTHONS3=$(filter 3.%,$(PYTHONS))

# Use this to install all interpreters
all-pythons: $(addprefix py,$(PYTHONS))

# To install a single interpreter, call 'make py<VERSION>'.
$(foreach V,$(PYTHONS),$(eval py$(V): pythons/.python$(V)-timestamp))
# Use $(PY<V>) in dependencies.
$(foreach V,$(PYTHONS),$(eval PY$(V)=pythons/.python$(V)-timestamp))
.PHONY: $(addprefix py,$(PYTHONS))

# Put all binary directories on the path, so tox can find them
# Note: $(eval) is a hack that allows us to get a literal space.
export PATH := $(subst $(eval) ,:,$(patsubst %,${CURDIR}/pythons/%/bin,$(PYTHONS))):$(PATH)

CONDA3_BIN=miniconda3/bin
CONDA3=$(CONDA3_BIN)/conda

$(CONDA3): conda-install.sh
	bash ./conda-install.sh 3

pythons/.python%-timestamp: $(CONDA3)
	rm -rf pythons/$*
	$(CONDA3) create $(CONDA_OPTS) -qym -p pythons/$* python=$* virtualenv=15.1.0
	touch pythons/.python$*-timestamp

# End Python installation

# PYTHONPATH can cause problems and it is useless here because
# we use our own Python interpreters and virtualenvs anyway.
ifdef PYTHONPATH
  $(warning Unsetting PYTHONPATH)
  unexport PYTHONPATH
endif

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
# Push and submit via gerrit
	# FIXME: This never works.  Gerrit doesn't like two commits
	# which modify the same file.
	git push gerrit 'HEAD:refs/for/master%submit'

checkPort: FORCE
ifndef AGRAPH_PORT
	echo "AGRAPH_PORT not set"
	exit 1
endif
	@echo Using port $(AGRAPH_PORT)

TOXDEP=$(TOXENVDIR)/.timestamp
$(TOXENVDIR): $(TOXDEP)

$(TOXENVDIR)/.timestamp: $(PY3.6) toxenv-requirements.txt
	rm -rf $(TOXENVDIR)
	$(CONDA3) create -p $(TOXENVDIR) --clone pythons/3.6
        # --no-deps is important to avoid overwriting virtualenv
	. ./$(CONDA3_BIN)/activate $(abspath $(TOXENVDIR)) && pip install -U ${AG_PIP_OPTS} --no-deps -r toxenv-requirements.txt
	touch $(TOXENVDIR)/.timestamp

$(ENVDIR)/.timestamp: $(TOXDEP) $(PY2.7) requirements.txt tox.ini
	rm -rf $(ENVDIR)
	$(TOX) -e py27-env
	touch $(ENVDIR)/.timestamp
$(ENVDIR): $(ENVDIR)/.timestamp

$(ENVDIR3)/.timestamp: $(TOXDEP) $(PY3.6) requirements.txt tox.ini
	rm -rf $(ENVDIR3)
	$(TOX) -e py36-env
	touch $(ENVDIR3)/.timestamp
$(ENVDIR3): $(ENVDIR3)/.timestamp

test-env: $(ENVDIR)

.PHONY: $(TOXENVDIR) $(ENVDIR) $(ENVDIR3) test-env

wheelhouse: $(ENVDIR) $(ENVDIR3)
	$(ENVDIR)/bin/pip wheel -rrequirements.txt -w wheelhouse
	$(ENVDIR3)/bin/pip wheel -rrequirements.txt -w wheelhouse

prepush: prepush2 prepush3

prepush2: checkPort $(TOXDEP) $(addprefix py,$(PYTHONS2)) .venv
	$(eval RUN=$(TOX) $(patsubst %, -e py%-test,$(subst .,,$(PYTHONS2))))
	$(RUN)
	AG_FORCE_REQUESTS_BACKEND=y $(RUN)

prepush3: checkPort $(TOXDEP) $(addprefix py,$(PYTHONS3)) .venv
	$(eval RUN=$(TOX) $(patsubst %, -e py%-test,$(subst .,,$(PYTHONS3))))
	$(RUN)
	AG_FORCE_REQUESTS_BACKEND=y $(RUN)

events: checkPort $(TOXDEP) py$(lastword $(PYTHONS2)) .venv
	$(TOX) $(patsubst %,-e py%-events,$(lastword $(subst .,,$(PYTHONS2))))

events3: checkPort $(TOXDEP) py$(lastword $(PYTHONS3)) .venv
	$(TOX) $(patsubst %,-e py%-events,$(lastword $(subst .,,$(PYTHONS3))))

# This does not use Tox, since the idea is to check if 'pip install'
# will work correctly without Tox.
disttest/.timestamp: $(TOXDEP) .venv
	rm -rf disttest
        # Use toxenv's virtualenv so we get a recent enough pip	
	$(TOXENVDIR)/bin/virtualenv -p python2 --no-site-packages disttest
        # Update pip and setuptools
	disttest/bin/pip install -U $(AG_PIP_OPTS) -r toxenv-requirements.txt
        # We need sphinx to run the doctests
	disttest/bin/pip install $(AG_PIP_OPTS) -rdocs-requirements.txt
        # Remember creation time, to detect changes
	touch disttest/.timestamp

disttest: wheel disttest/.timestamp
        # Install from the release tarball
        # Make sure pycurl compiles
	PYCURL_SSL_LIBRARY=nss disttest/bin/pip install $(AG_PIP_OPTS) -U DIST/$(SDIST)

.PHONY: disttest

# runs the examples from the tutorial and compares the actual output
# to whatever the tutorial claims should be printed.
# To run just a single example do 'EXAMPLE=example7 make tutorial'.
tutorial: checkPort disttest
	cd docs && AGRAPH_USER=test AGRAPH_PASSWORD=xyzzy AGRAPH_PORT=$(AGRAPH_PORT) ../disttest/bin/sphinx-build -b doctest src build/doctest

docs: $(TOXDEP) .venv FORCE
	$(TOX) -e doc

jupyter: $(TOXDEP) .venv FORCE
	$(TOX) -e jupyter
	rm -rf jupyter
	mkdir -p jupyter
	for f in docs/build/jupyter/tutorial/*.ipynb ; do \
          python fix_notebook.py < $$f > jupyter/$$(basename $$f) ; \
        done
	cp -r docs/src/images jupyter/
	cp -r docs/data jupyter/

publish-jupyter: jupyter
	tar czf agraph-jupyter.tar.gz jupyter/
	mv agraph-jupyter.tar.gz /fi/ftp/pub/agraph/python-client

wheel: $(ENVDIR)/.timestamp FORCE
	mkdir -p DIST
	rm -f DIST/$(WHEEL) DIST/$(SDIST)
	$(ENVDIR)/bin/pip wheel -e . -w DIST --build-option --universal --no-deps
        # Also build a source dist
	$(ENVDIR)/bin/python setup.py sdist -d DIST # --owner=root --group=root 

.PHONY: wheel

register: $(TOXDEP) wheel
	$(TOXENVDIR)/bin/twine register $(TWINE_ARGS) DIST/$(WHEEL)

sign: wheel
	$(eval GPG_PASS_OPTS=$(shell ./gpg-opts.sh "$(PYPI_GPG_KEY)"))
	rm -f DIST/$(WHEEL).asc DIST/$(SDIST).asc
	@$(GPG_SIGN) DIST/$(WHEEL)
	@$(GPG_SIGN) DIST/$(SDIST)

publish: $(TOXDEP) wheel sign
	python version.py verify-not-dev
	cp DIST/$(SDIST) CHANGES.rst /fi/ftp/pub/agraph/python-client/
	# Do not use the special nexus.ca.crt CA bundle when performing the
	# uploads to PyPi and Conda.  It will result in SSL server
	# certificate validation errors.
	REQUESTS_CA_BUNDLE= $(TOXENVDIR)/bin/twine upload --skip-existing $(TWINE_ARGS) DIST/$(WHEEL) DIST/$(WHEEL).asc DIST/$(SDIST) DIST/$(SDIST).asc
	REQUESTS_CA_BUNDLE= ./conda-upload.sh

tags: FORCE
	etags `find . -name '*.py'`

clean-envs: FORCE
	rm -rf .tox $(ENVS)

fix-copyrights: FORCE
	sed -i'' -e "s/$(COPYRIGHT_REGEX)/$(COPYRIGHT_NOW)/i" LICENSE
	find src -name '*.py' -print0 | xargs -0 python2 fix-header.py

# If any of these files change rebuild the virtual environments.
.venv: setup.py requirements.txt docs-requirements.txt tox.ini $(TOXDEP)
	rm -rf .tox
	touch .venv

clean: clean-envs
	rm -rf DIST pythons miniconda2 miniconda3 report.xml build .venv \
            src/agraph_python.egg-info/ docs/build docs/source/_gen/ \
            .cache/
	find . -name \*.pyc -delete
	find . -path '*/__pycache__*' -delete

FORCE:
