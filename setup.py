from distribute_setup import use_setuptools
use_setuptools()

import re
from setuptools import setup, find_packages

def parse_requirements(file_name):
    requirements = []
    for line in open(file_name, 'r').read().split('\n'):
        if re.match(r'(\s*#)|(\s*$)', line):
            continue
        if re.match(r'\s*-e\s+', line):
            # TODO support version numbers
            requirements.append(re.sub(r'\s*-e\s+.*#egg=(.*)$', r'\1', line))
        else:
            requirements.append(line)

    return requirements

def parse_dependency_links(file_name):
    dependency_links = []
    for line in open(file_name, 'r').read().split('\n'):
        if re.match(r'\s*-e\s+', line):
            dependency_links.append(re.sub(r'\s*-e\s+', '', line))

    return dependency_links


from franz import __version__


setup(
    name="franz",
    version=__version__,
    packages=find_packages(),
    author="franzinc",
    author_email="franzinc",
    description="", #  TODO
    long_description="", #  TODO
    url="https://dmr@github.com/nwebs/agraph-python.git",
    # Build dependencies (for setup.py)
    setup_requires=[
                    'setuptools_git>=0.3.4', # needed for install_package_data with GIT
                   ],
    # Dependencies
    install_requires=parse_requirements('requirements.txt'),
    dependency_links=parse_dependency_links('requirements.txt'),
    include_package_data=True,
    zip_safe=True,
)

