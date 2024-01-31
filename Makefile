# Conda scripts require Bash
SHELL = /bin/bash

CONDA_CHANNEL=conda-forge
CONDA_OPTS ?= --channel $(CONDA_CHANNEL) --insecure

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

# PYTHONPATH can cause problems and it is useless here because
# we use our own Python interpreters and virtualenvs anyway.
ifdef PYTHONPATH
  $(warning Unsetting PYTHONPATH)
  unexport PYTHONPATH
endif

checkPort: FORCE
ifndef AGRAPH_PORT
	echo "AGRAPH_PORT not set"
	exit 1
endif
	@echo Using port $(AGRAPH_PORT)

# $(PYTHON) is only used for installing Hatch, it shall NOT be used for testing or anything else
# $(PYTHON_DIST_PYTHON_VERSION) MAY be considered to be upgraded, only if the current version is end-of-life
# A list of actively supported Python versions can be found here: https://endoflife.date/python
# If you are upgrading it, please be careful with the following:
# - does it support glic>=2.17? Current yes.
# - what version of OpenSSL does it use? Currently OpenSSL 3.0.12
# - See here for more details: https://gregoryszorc.com/docs/python-build-standalone/main/
PYTHON_DIST_TIMESTAMP=20240107
PYTHON_DIST_PYTHON_VERSION=3.8.18
PYTHON_DIST_MACHINE_ARCH=x86_64-unknown-linux-gnu
PYTHON=python/bin/python3

$(PYTHON):
	@mkdir -p python
	@curl -L https://github.com/indygreg/python-build-standalone/releases/download/$(PYTHON_DIST_TIMESTAMP)/cpython-$(PYTHON_DIST_PYTHON_VERSION)+$(PYTHON_DIST_TIMESTAMP)-$(PYTHON_DIST_MACHINE_ARCH)-install_only.tar.gz | tar xz

HATCH_VERSION=1.9.3
HATCH=python/bin/hatch
export HATCH_INTERACTIVE=false
export HATCH_DATA_DIR=$(shell pwd)/.hatch/data
export HATCH_CACHE_DIR=$(shell pwd)/.hatch/cache
export HATCH_BUILD_CLEAN=true

$(HATCH): $(PYTHON)
	$(PYTHON) -m pip install --quiet --no-cache hatch==$(HATCH_VERSION)
	mkdir -p .hatc/data .hatch/cache

.DEFAULT: build
.PHONY: prepush check-style fix-style events3 tutorial docs jupyter publish-jupyter publish tags FORCE

build: $(HATCH)
	$(HATCH) build -t wheel -t sdist

prepush: $(HATCH) checkPort check-style
	$(HATCH) run test:run

check-style: $(HATCH)
	$(HATCH) run style:check || (echo "Run 'make fix-style' to fix formatting" && exit 1)

fix-style: $(HATCH)
	$(HATCH) run style:fix

# TODO: document or clean up this target
events3: checkPort $(TOXDEP) py$(lastword $(PYTHONS3)) .venv
	$(TOX) $(patsubst %,-e py%-events,$(lastword $(subst .,,$(PYTHONS3))))

# runs the examples from the tutorial and compares the actual output
# to whatever the tutorial claims should be printed.
# To run just a single example do 'EXAMPLE=example7 make tutorial'.
tutorial: $(HATCH) checkPort
	@$(HATCH) run docs:test

docs: $(HATCH) FORCE
	$(HATCH) run docs:build-html

jupyter: $(HATCH) FORCE
	$(HATCH) run docs:build-jupyter
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

publish: $(HATCH) build
	python version.py verify-not-dev
	cp dist/agraph-python-$(shell $(HATCH) version).tar.gz CHANGES.rst /fi/ftp/pub/agraph/python-client/
	# Upload both wheel and sdist to PyPI
	# The authentication is done through a project-based API Token: https://pypi.org/help/#apitoken
	# The API Token can be found in 1Password > pypi.python.org (franz_inc)
	$(HATCH) publish
	./conda-upload.sh

tags: FORCE
	etags `find . -name '*.py'`

fix-copyrights: FORCE
	sed -i'' -e "s/$(COPYRIGHT_REGEX)/$(COPYRIGHT_NOW)/i" LICENSE
	find src -name '*.py' -print0 | xargs -0 python fix-header.py

clean:
	rm -rf python .hatch dist miniconda3 report.xml build .venv \
		src/agraph_python.egg-info/ docs/build docs/source/_gen/ \
        .*cache/
	find . -name \*.pyc -delete
	find . -path '*/__pycache__*' -delete

FORCE:
