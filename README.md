# Introduction #

Welcome to PSyclone. PSyclone is a code generation and transformation
system that generates appropriate code for the PSyKAl code structure
developed in the GungHo project. It is also capable of working with
existing Fortran code.

Please see [psyclone.pdf](psyclone.pdf) in this directory (or on
[ReadTheDocs](http://psyclone.readthedocs.io)) for
more information. If you would prefer to build the documentation,
please see the [README](doc/README.md) file in the "doc" directory.

# Try it on Binder #

Some of the examples are available as Jupyter notebooks. These may
be launched using Binder from the links below. (Note that the first time
this is done, Binder has to construct a Container and install the necessary
software. This can  take several minutes. You can track its progress
by clicking the 'show' link next to the 'Build logs' heading.)

 * [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=examples%2Fnemo%2Feg4%2Fcopy_stencil.ipynb) Uses PSyclone's NEMO API to process some simple Fortran code, display the resulting PSyIR and then re-generate Fortran.

 * [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=examples%2Fgocean%2Feg1%2Fopenmp.ipynb) Uses PSyclone's GOcean API to process example code that conforms to the PSyKAl separation of concerns. Transformations are applied in order to fuse various loops before parallelising the result with OpenMP.
 
 * [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=examples%2Fgocean%2Feg1%2Fdag.ipynb) demonstrates the generation of a DAG for the PSy layer of the previous example.

# Tutorial #

The PSyclone tutorial may be found in the tutorial directory. Since part of
this takes the form of a series of Jupyter notebooks, it too may be launched on
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=tutorial%2Fnotebooks%2Fintroduction.ipynb).

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
bin/                | Top-level driver scripts for PSyclone and the PSyclone kernel tool
changelog      	    | Information on changes between releases
doc/           	    | Documentation source using Sphinx
examples/      	    | Simple examples
psyclone.pdf   	    | Generated documentation
README.md      	    | This file
README.gource  	    | Information on how to generate a gource video from the repository
README.uml     	    | Information on how to create UML class diagrams from the source using pyreverse
src/psyclone   	    | The Python source code
src/psyclone/tests/ | Unit and functional tests using pytest
tutorial/notebooks  | Tutorial using Jupyter notebooks
tutorial/practicals | Hands-on exercises using a local installation of PSyclone

# Status #

![Build Status](https://github.com/stfc/PSyclone/workflows/PSyclone%20tests%20and%20examples/badge.svg)

[![codecov](https://codecov.io/gh/stfc/PSyclone/branch/master/graph/badge.svg)](https://codecov.io/gh/stfc/PSyclone)

