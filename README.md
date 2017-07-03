# Introduction #

Welcome to PSyclone. PSyclone is a code generation system that generates
appropriate code for the PSyKAl code structure developed in the GungHo project.

Please see psyclone.pdf in this directory for more information. If you would
prefer to build the documentation, please see the README file in the "doc"
directory.

There is a simple installer (contributions/install) which creates a
stand-alone tree containing the functional components. To make use of the
result add the python package directories to PYTHONPATH and consider adding
the "bin" directory to PATH.

# Structure #

Path | Description
---- | -----------
bin/ | top-level driver scripts for PSyclone and the Kernel stub generator
changelog      	    | information on changes between releases
contributions/ 	    | Unsupported additional material
doc/           	    | documentation source using sphinx
examples/      	    | simple examples
psyclone.pdf   	    | generated documentation
README.md      	    | this file
README.gource  	    | information on how to generate a gource video from the repository
README.uml     	    | information on how to create UML class diagrams from the source using pyreverse
src/psyclone   	    | the python source code
src/psyclone/tests/ | unit and functional tests using pytest

# Status #

[![Build Status](https://travis-ci.org/stfc/PSyclone.svg?branch=master)](https://travis-ci.org/stfc/PSyclone)

[![Coverage Status](https://coveralls.io/repos/github/stfc/PSyclone/badge.svg?branch=master)](https://coveralls.io/github/stfc/PSyclone?branch=master)


