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
    'iso8601>=0.1.12,<1.0.0',
    'six>=1.10.0',
    'future>=0.15.2,<1.0',
    'requests>=2.11.1,<3.0']

tests_require = ['nose>=1.3.7,<2.0', 'pytest>=3.0,<4.0', 'pytest-timeout>=1.0,<2.0']

setup(name='agraph-python',
      version=version,
      description='AllegroGraph Python client',
      long_description=long_description,
      author='Franz Inc.',
      author_email='support@franz.com',
      url='http://franz.com/agraph/support/documentation/current/python-tutorial/python-API.html',
      license='EPL',
      packages=find_packages('src'),
      package_dir={'': 'src'},
      package_data={'franz': ['VERSION']},
      install_requires=install_requires,
      tests_require=tests_require,
      # To use these, install like this: pip install agraph-python-client[curl]
      extras_require={
          'simplejson': ['simplejson>=2.0.9'],
          'curl': ['pycurl>=7.19.0,<8.0']
      },
      classifiers=[
          'Development Status :: 5 - Production/Stable',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: MIT License',
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
