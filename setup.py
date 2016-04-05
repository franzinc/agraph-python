#!/usr/bin/env python

###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from setuptools import setup
import sys
import subprocess

version = subprocess.Popen('../agraph/lisp/build-tools/agversion.sh', 
                           stdout=subprocess.PIPE).communicate()[0]

install_requires = [
    'future>=0.15.2,<1.0',
    'pycurl>=7.21.5,<8.0']

if sys.version_info[0] == 2:  # Python 2
    install_requires += ['python-cjson>=1.1.0,<2.0']
else:  # Python 3
    version = version.decode()

setup_requires = ['nose>=1.3.7,<2.0']

tests_require = ['unittest2>=1.1.0,<2.0']


setup(name='agraph-python',
      version=version,
      description='AllegroGraph python client',
      author='Franz Inc.',
      url='http://http://franz.com/agraph/support/documentation/current/python-tutorial/python-API.html',
      packages=['franz'],
      package_dir={'': 'src'},
      install_requires=install_requires,
      setup_requires=setup_requires,
      tests_require=tests_require)
