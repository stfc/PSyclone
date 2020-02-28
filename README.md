# Introduction #

Welcome to PSyclone. PSyclone is a code generation system that generates
appropriate code for the PSyKAl code structure developed in the GungHo project.

Please see [psyclone.pdf](psyclone.pdf) in this directory (or on
[ReadTheDocs](http://psyclone.readthedocs.io)) for
more information. If you would prefer to build the documentation,
please see the [README](doc/README.md) file in the "doc" directory.

# Installation #

If you are reading this then you have presumably not already installed
PSyclone from the Python Package Index (https://pypi.python.org/pypi).
That being so, you can install this copy of PSyclone by doing:

    $ python setup.py install

or, if you do not have root access:

    $ python setup.py install --prefix /my/install/path

Alternatively, if you have pip:

    $ pip install .

For a user-local installation simply add the --user flag:

    $ pip install --user .

This installs the PSyclone modules in
~/.local/lib/pythonX.Y/site-packages (where X.Y is the version of
Python that you are using) and the 'psyclone' script in
~/.local/bin. Depending on your linux distribution, you may need to
add the latter location to your PATH.

# Structure #

Path                | Description
------------------- | -----------
bin/                | top-level driver scripts for PSyclone and the Kernel stub generator
changelog      	    | Information on changes between releases
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

[![codecov](https://codecov.io/gh/stfc/PSyclone/branch/master/graph/badge.svg)](https://codecov.io/gh/stfc/PSyclone)

