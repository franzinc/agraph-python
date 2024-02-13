# Conda scripts require Bash
SHELL = /bin/bash

CONDA_CHANNEL=conda-forge
CONDA_OPTS ?= --channel $(CONDA_CHANNEL) --insecure

YEAR := $(shell date +%Y)

# Sed regex used to locate the line containing copyright year in LICENSE
COPYRIGHT_REGEX := Copyright (c) 2006-[0-9]* Franz Inc.
# Expected/correct value of that line.
COPYRIGHT_NOW := Copyright (c) 2006-$(YEAR) Franz Inc.

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
	mkdir -p .hatch/data .hatch/cache

.DEFAULT: build
.PHONY: prepush check-style fix-style events3 tutorial docs jupyter publish-jupyter publish tags FORCE

build: build-wheel build-sdist build-bdist
	@ls -lh dist/

build-wheel: $(HATCH)
	@echo "Building wheel ..."
	$(HATCH) build -t wheel

build-sdist: $(HATCH)
	@echo "Building sdist ..."
	$(HATCH) build -t sdist

build-bdist: $(HATCH)
	@mkdir -p dist
	@echo "Building (conda) bdist ..."
	@docker run --rm \
		-v $(shell pwd):/build/agraph-python \
		-v $(shell pwd)/recipe:/build/recipe \
		--env AGRAPH_PYTHON_VERSION="$(shell $(HATCH) version)" \
		docker.io/condaforge/miniforge3 bash -c "\
set -eu; \
rm -rf /build/recipe/output; \
mkdir -p /build/recipe/output; \
conda config --set anaconda_upload no; \
mamba install --yes --channel conda-forge conda-build conda-verify; \
conda build /build/recipe --output-folder /build/recipe/output"
	@cp recipe/output/noarch/agraph-python-$(shell $(HATCH) version)-py_0.tar.bz2 dist/

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

verify-not-dev:
	python version.py verify-not-dev

publish: verify-not-dev publish-pypi publish-anaconda
	cp dist/agraph_python-$(shell $(HATCH) version).tar.gz CHANGES.rst /fi/ftp/pub/agraph/python-client/
	@echo "CHANGES.rst and sdist haven been uploaded to the internal FTP server."

publish-pypi: build-wheel build-sdist
	@echo "Uploading both wheel and sdist to PyPI ..."
	@echo "The authentication is done through a project-based API Token: https://pypi.org/help/#apitoken"
	@echo "The API Token can be found in franzinc.1password.com > Python Dev > pypi.python.org (franz_inc)"
ifndef PYPI_API_TOKEN
	$(error PYPI_API_TOKEN is not set)
endif
	@$(HATCH) publish --user "__token__" --auth "$(PYPI_API_TOKEN)"

publish-anaconda:
	@echo "Uploading bdist to Anaconda - franzinc channel ..."
	@echo "The authentication is done through the ANACONDA_USERNAME and ANACONDA_PASSWORD environment variables."
	@echo "The ANACONDA_USERNAME and ANACONDA_PASSWORD information should belong to someone with access to the franzinc channel."
ifndef ANACONDA_USERNAME
	$(error ANACONDA_USERNAME is not set)
endif
ifndef ANACONDA_PASSWORD
	$(error ANACONDA_PASSWORD is not set)
endif
	@docker run --rm \
		-v $(shell pwd)/dist:/dist \
		--env ANACONDA_USERNAME="$(ANACONDA_USERNAME)" \
		--env ANACONDA_PASSWORD="$(ANACONDA_PASSWORD)" \
		docker.io/condaforge/miniforge3 bash -c "\
set -eu; \
mamba install --yes anaconda-client; \
anaconda login --username \"$${ANACONDA_USERNAME}\" --password \"$${ANACONDA_PASSWORD}\"; \
anaconda upload --channel franzinc --user franzinc --label main /dist/agraph-python-$(shell $(HATCH) version)-py_0.tar.bz2"

tags: FORCE
	etags `find . -name '*.py'`

fix-copyrights: FORCE
	sed -i'' -e "s/$(COPYRIGHT_REGEX)/$(COPYRIGHT_NOW)/i" LICENSE
	find src -name '*.py' -print0 | xargs -0 python fix-header.py

clean:
	rm -rf python .hatch recipe/output dist miniconda3 report.xml build .venv \
		src/agraph_python.egg-info/ docs/build docs/source/_gen/ \
        .*cache/
	find . -name \*.pyc -delete
	find . -path '*/__pycache__*' -delete

FORCE:
