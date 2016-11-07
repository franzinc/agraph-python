#!/usr/bin/env python

###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from setuptools import find_packages, setup
import sys
import subprocess

# Necessary when running from sources...
sys.path.append('./src')
import franz

version = franz.__version__

with open('README.rst', 'rb') as f:
    long_description = f.read().decode('utf-8')

install_requires = [
    'future>=0.15.2,<1.0',
    'pycurl>=7.21.5,<8.0']

if sys.version_info[0] == 2:  # Python 2
    install_requires += ['python-cjson>=1.1.0,<2.0']

setup_requires = ['nose>=1.3.4,<2.0', 'pytest>=3.0,<4.0', 'pytest-timeout>=1.0,<2.0']

tests_require = ['unittest2>=1.1.0,<2.0']


setup(name='agraph-python',
      version=version,
      description='AllegroGraph Python client',
      long_description=long_description,
      author='Franz Inc.',
      url='http://franz.com/agraph/support/documentation/current/python-tutorial/python-API.html',
      license='EPL',
      packages=find_packages('src'),
      package_dir={'': 'src'},
      package_data={'franz': ['VERSION']},
      install_requires=install_requires,
      setup_requires=setup_requires,
      tests_require=tests_require,
      classifiers=[
          'Development Status :: 5 - Production/Stable',
          'Intended Audience :: Developers',
          'License :: OSI Approved',
          'Operating System :: OS Independent',
          'Programming Language :: Python',
          'Programming Language :: Python :: 2.6',
          'Programming Language :: Python :: 2.7',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: 3.3',
          'Programming Language :: Python :: 3.4',
          'Programming Language :: Python :: 3.5',
          'Topic :: Database',
          'Topic :: Software Development :: Libraries',
          'Topic :: Software Development :: Libraries :: Python Modules',
      ])
