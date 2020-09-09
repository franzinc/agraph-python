AllegroGraph Python client release process
==========================================

# Introduction

Python client is released through three separate channels:

   - A tar archive is published on franz.com.
   - A package is uploaded to PyPI.
   - another package is uploaded to anaconda.org.

The first of these happens as a part of the general AG release process.
Other two are triggered by using the `publish` makefile target.

# Version numbers

It is necessary to ensure that each release of the Python client has
a distinct version number. To this end we've adopted the following
policy:

   - Version numbers consist of the AG version number
     padded to four segments and a single segment 
     client release number.
   - During development the version number carries the 
     '.dev' suffix.
   - Before a release is made this suffix must be removed.
     This can be achieved by running `make prepare-release`.
   - After the release we increment the client release 
     segment and add '.dev' back. `make post-release` does this.

# Building and publishing releases

   - Run `make prepare-release`. This will strip .dev from the version
     number, commit, and tag the commit.  Nothing is pushed at this
     point.

   - Run `make publish` to publish PyPI and conda packages. This
     requires proper credentials, as described in the next section.
     This step might be skipped during rc and test releases.

   - Run `make post-release`. This will increment the version number
     (and change it back to a .dev version), commit, and submit the
     outstanding commits via gerrit.  This step must be performed
     before any other changes to the agraph-python module are
     committed.   Note that when you commit and push this change
     to gerrit with a %submit suffix the submit will fail
     because you've uploaded two commits and asked to submit the
     second one.  Maybe this worked at one time but it doesn't now.
     However if you look at gerrit you'll see the two commits
     as open gerrit changes.  You'll need to use the gerrit UI to manually
     submit each commit in turn.

     After the changes are merged, ask Kevin to update the
     AllegroGraph documentation website.

# Credentials

The following credentials are required by `make publish`

   - password for the franz_inc user on PyPI 
   - support@franz.com's GPG key    This will be on your gpg keyring
     protected by a passphase you choose.
   - anaconda account belonging to the franzinc organization.  This can
     be your personal ananconda account as long as you've been added
     to the franzinc organization by a current member of that organization.

PyPI credentials can be passed in two ways: 

   - By configuring a section in ~/.pypirc and setting PYPI_REPO to
   - its name.  If PYPI_REPO is not set you'll be prompted for the
     password for 'franz_inc' at the default PyPI server.

The GPG passphrase can be passed in following ways: 
   
   - through an env variable: AG_GPG_PASSPHRASE 
   - In a file pointed to by $AG_GPG_PASSPHRASE_FILE 
   - If neither variable is set an interactive prompt will be presented.

To configure conda credentials:
  
   - Create an account on anaconda.org 
   - Yell at someone to add you to the 'franzinc' organization. 
   - Just call the script, enter your credentials and the sources 
     will be packaged and uploaded to anaconda.org.

It is not currently possible to pass anaconda password through 
an environment variable. However, invoking 

   /net/san1/disk1/conda3/bin/anaconda login

performs a persistent login (storing data in your home directory)
and after doing that `make publish` will no longer cause a 
password prompt to be displayed.
