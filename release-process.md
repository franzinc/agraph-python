AllegroGraph Python client release process
==========================================

# Introduction

Python client is released through three separate channels:

   - _sdist_ is published on franz.com.
   - _Wheel_ and _sdist_ are uploaded to PyPI.
   - _bdist_ is uploaded to anaconda.org.
   
# Preparing for a New Release

1. **Increment the Version Number**: Determine whether the update constitutes a
   major or minor change, and update it accordingly in `src/franz/__init__.py`.
2. **Update the CHANGES.rst File**: Document the specifics of the change in the
   CHANGES.rst file to keep track of modifications.
3. **Submit a Gerrit Change for Testing**: Initiate testing by submitting a
   change request through Gerrit.
4. **Proceed After Gerrit Change Merge**: Once the Gerrit change is approved and
   merged, move forward to the publishing phase (see next section).

# Building and publishing releases

Run `make publish` to publish PyPI and the Anaconda packages. This requires
proper credentials, as described in the next section (also in _Makefile_).
Meanwhile, `docker` or `podman` is also required during the publishing process.

If everything was successfuly published, ask Kevin to update the AllegroGraph
documentation website.

# Credentials

The following credentials are required by `make publish`

   - The project-based API Token for publishing to PyPI
   - Anaconda account belonging to the franzinc organization. This can be your
     personal ananconda account as long as you've been added to the franzinc
     organization by a current member of that organization.

To configure the PyPI API token:

   - it can be found in franzinc.1password.com > Python Dev > pypi.python.org (franz_inc)
   - can be passed by an environment variable `PYPI_API_TOKEN`.

To configure Anaconda credentials:

   - Create an account on anaconda.org
   - Send a request to someone to add you to the 'franzinc' organization.
   - Your username and password should be passed as environment variables
     `ANACONDA_USERNAME` and `ANACONDA_PASSWORD`.
