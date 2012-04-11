from franz import __version__
from setuptools import setup, find_packages
import re

setup(
    name="franz",
    version=__version__,
    packages=find_packages(),
    author="franzinc",
    author_email="franzinc",
    description="", #  TODO
    long_description="", #  TODO
    url="https://dmr@github.com/nwebs/agraph-python.git",
    setup_requires=[
                    'setuptools_git>=0.3.4', # needed for install_package_data with GIT
                   ],
    install_requires=[],
    dependency_links=[],
    include_package_data=True,
    zip_safe=True,
)
