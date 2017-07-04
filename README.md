# Introduction #

Welcome to PSyclone. PSyclone is a code generation system that generates
appropriate code for the PSyKAl code structure developed in the GungHo project.

Please see psyclone.pdf in this directory for more information. If you would
prefer to build the documentation, please see the README file in the "doc"
directory.

# Installation #

If you are reading this then you have presumably not already installed
PSyclone from the Python Package Index (https://pypi.python.org/pypi).
That being so, you can install this copy of PSyclone by doing:

    $ python setup.py install

or, if you have pip:

    $ pip install .

The latter attempts to perform a system-wide install. For a user-local
installation simply add the --user flag:

    $ pip install --user .

There is also a simple installer (contributions/install) which creates a
stand-alone tree containing the functional components. To make use of the
result add the python package directories to PYTHONPATH and consider adding
the "bin" directory to PATH.

# Structure #

Path | Description
---- | -----------
bin/ | top-level driver scripts for PSyclone and the Kernel stub generator
changelog      	    | Information on changes between releases
contributions/ 	    | Unsupported additional material
doc/           	    | Documentation source using sphinx
examples/      	    | Simple examples
psyclone.pdf   	    | Generated documentation
README.md      	    | This file
README.gource  	    | Information on how to generate a gource video from the repository
README.uml     	    | Information on how to create UML class diagrams from the source using pyreverse
src/psyclone   	    | The python source code
src/psyclone/tests/ | Unit and functional tests using pytest

# Status #

[![Build Status](https://travis-ci.org/stfc/PSyclone.svg?branch=master)](https://travis-ci.org/stfc/PSyclone)

[![Coverage Status](https://coveralls.io/repos/github/stfc/PSyclone/badge.svg?branch=master)](https://coveralls.io/github/stfc/PSyclone?branch=master)


