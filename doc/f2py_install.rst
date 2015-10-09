.. _sec_f2py_install:

f2py installation
^^^^^^^^^^^^^^^^^

PSyclone requires version 3 of f2py, a library designed to allow
fortran to be called from python (see
http://code.google.com/p/f2py/wiki/F2PYDevelopment for more
information). PSyclone makes use of the fortran parser (fparser)
contained within.

The source code of f2py (revision 93) is provided with PSyclone in the
sub-directory ``f2py_93``. If you would prefer to install f2py rather
than simply use it as is (see the previous section) then the rest of
this section explains how to do this.

f2py uses the numpy distutils package to install. In version 1.6.1 of
distutils (currently the default in Ubuntu) distutils supports
interactive setup. In this case to install f2py using gfortran and gcc
(for example) you can perform the following (where "cgcc", "fgfortran", "1"
and "2" are interactive commands to setup.py)
::

    > cd f2py_93
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
(using g95 and gcc in this case):
::

    > cd f2py_93
    > sudo ./setup.py build -fcompiler=g95 -ccompiler=gcc
    > sudo ./setup.py install
 
For more information about possible build options you can use the
available help:
::

    > ./setup.py --help
    > ./setup.py build --help
    > ./setup.py build --help-fcompiler

In particular, if you do not have root access then the python 
modules can be installed in your user account by specifying 
--user to the install command:
::

    > ./setup.py install --user

This causes the software to be installed under ${HOME}/.local/
