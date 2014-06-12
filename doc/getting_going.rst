Getting Going
=============

Download
--------

PSyclone is available for download from the GungHo repository.

``svn co https://puma.nerc.ac.uk/svn/GungHo_svn/PSyclone/trunk PSyclone``

Dependencies
------------

PSyclone is written in python so needs python to be installed on the
target machine. PSyclone has been tested under python 2.6.5 and 2.7.3.

PSyclone immediately relies on two external libraries, f2py and
pyparsing. f2py also requires numpy.

f2py
^^^^

PSyclone requires version 3 of f2py, a library designed to allow
fortran to be called from python (see
http://code.google.com/p/f2py/wiki/F2PYDevelopment for more
information). PSyclone makes use of the fortran parser (fparser)
contained within. The source code of f2py (revision 88) is provided
with PSyclone in the sub-directory ``f2py_88``.

f2py uses the numpy distutils package to install. In version 1.6.1 of
distutils (currently the default in Ubuntu) distutils supports
interactive setup. In this case to install f2py using gfortran and gcc
(for example) you can perform the following (where cgcc, fgfortran, 1
and 2 are interactive commands to setup.py)
::
    > cd f2py_88
    > sudo ./setup.py
    cgcc
    fgfortran
    1
    > sudo ./setup.py
    cgcc
    fgfortran
    2

For later versions of distutils (1.8.0 has been tested) where the
interactive setup has been disabled you can perform the following
(using g95 and gcc in this case)
::
    > cd f2py_88
    > sudo ./setup.py build -fcompiler=g95 -ccompiler=gcc
    > sudo ./setup.py install
 
For more information about possible build options you can use the
available help
::
    > ./setup.py --help
    > ./setup.py build --help
    > ./setup.py build --help-fcompiler

In particular, if you do not have root access then the python 
modules can be installed in your user account by specifying 
--user to the install command
::
    > ./setup.py install --user

This causes the software to be installed under ${HOME}/.local/

pyparsing
^^^^^^^^^

PSyclone requires pyparsing, a library designed to allow parsers to be be
built in Python. PSyclone uses pyparsing to parse fortran regular
expressions as f2py does not fully parse these, (see
http://pyparsing.wikispaces.com for more information).

PSyclone has been tested with pyparsing version 1.5.2 which is a relatively
old version but is currently the version available in the Ubuntu
software center.

You can test if pyparsing is already installed on your machine by
typing ``import pyparsing`` from the python command line. If pyparsing
is installed, this command will complete succesfully. If pyparsing is
installed you can check its version by typing
``pyparsing.__version__`` after succesfully importing it. Versions
higher than 1.5.2 should work but have not been tested.

If pyparsing is not installed on your system you can install it from
within Ubuntu using the software center (search for the
"python-pyparsing" module in the software center and install). If you
do not run Ubuntu you could follow the instructions here
http://pyparsing.wikispaces.com/Download+and+Installation.

Run
---

The generator.py script can be used to generate the required PSy code as well as the modified algorithm code.
::
    > python ./generator.py 
    usage: generator.py [-h] [-oalg OALG] [-opsy OPSY] filename
    generator.py: error: too few arguments

A working example is provided in the example directory. To create the PSy code and modify the algorithm code using the script do the following
::
    > cd example
    > python ../generator.py -oalg integrate_one_alg.F90 -opsy integrate_one_psy.F90 integrate_one.F90

To generate, build and run the example use the Makefile
::
    > cd example
    > make
    > ./integrate_one_generated 
    1.0000000000000000     
