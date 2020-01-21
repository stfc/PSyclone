.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2020, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab

.. _getting-going:

Getting Going
=============

Download
--------

The following instructions are intended for a PSyclone user who wants
to work with a released version of the code. If you are a developer or
wish to test a specific branch of PSyclone from the GitHub repository
please see :ref:`dev-installation` in the
`Developer Guide <https://psyclone-dev.readthedocs.io/>`_.

PSyclone is available on the Python Package Index (pypi.org) and is
hosted on GitHub:

``https://github.com/stfc/PSyclone``

The latest release is |release| and the latest stable version is on
the master branch.

PSyclone can be installed using pip:

``> pip install psyclone``

or downloaded from github - either see |release| in the ``releases`` tab
on the PSyclone page or download and extract the latest release of
PSyclone directly, e.g.

.. parsed-literal::
   > wget https://github.com/stfc/PSyclone/archive/\ |release|\ .tar.gz
   > tar zxf \ |release|\ .tar.gz
   > ls
   PSyclone-\ |release|\ 
   
Hereon the location where you download or clone PSyclone (including the
PSyclone directory itself) will be referred to as ``<PSYCLONEHOME>``.

Dependencies
------------

PSyclone is written in Python so needs Python to be installed on the
target machine. PSyclone has been tested under Python 2.7 and 3.6.

.. warning:: Release 1.6.0 of PSyclone requires version 0.0.7 of
             fparser and will fail on more recent versions. However
             simply installing this version using pip will install a
             later version of fparser. The suggested solution is to
             use release 1.6.1 which fixes this problem and is otherwise
             identical to 1.6.0. However, if you want to use release
             1.6.0 you need to ensure that you have fparser 0.0.7. This
             can be achieved by downgrading an existing fparser
             installation or by installing version 0.0.7 of fparser
             *before* installing PSyclone.

PSyclone immediately relies on four external Python packages; ``six``,
``configparser``, ``fparser`` and ``pyparsing``. The easiest way to
satisfy the Python dependencies is to use the Python Package Index
(pypi.org) and ``pip``. See https://packaging.python.org/installing/
for more information.

If everything is working correctly then using pip to install PSyclone:

``> pip install psyclone``

will automatically install the Python dependencies (``fparser`` and
``pyparsing``).


In addition to the mandatory dependencies just described, PSyclone
also has optional dependencies on both ``graphviz`` and ``termcolor``.
PSyclone can use graphviz to produce a visualisation of a schedule's
dependency graph. If this is desired then the Python package
``graphviz`` (for the Python bindings) as well as the graphviz package
itself must be installed. If the ``graphviz`` package is not available
then the associated PSyclone routines will return silently and no
visualisations will be produced. The Python package ``termcolor`` is
used for pretty-printing a schedule in terminals that support coloured
text. If the package is not available then the schedule is simply
printed in plain text without colour highlighting.


System-specific set-up
^^^^^^^^^^^^^^^^^^^^^^

:ref:`system_specific_setup` instructions are available for Ubuntu 14.04.2 and
OpenSUSE 42.2.

fparser
^^^^^^^

The fparser package (https://github.com/stfc/fparser) is a Fortran
parser originally developed as a part of the f2py project.

The minimum version of fparser required by PSyclone is currently 0.0.9
but we strongly recommend you install the latest version to reduce the
chance of encountering problems when parsing existing algorithm or
kernel code. (Note that for older versions of PSyclone up to and
including 1.5.1 you must use version 0.0.6 of fparser.)

fparser is available from the Python Package
Index and thus may be installed using ``pip``
(https://packaging.python.org/installing/#requirements-for-installing-packages):
::

   > pip install fparser

If you do not have sufficient permissions to perform a system-wide install
then you can instruct pip to do a user-local install:
::

   > pip install --user fparser

Should you wish to remove fparser then simply do:
::

   > pip uninstall fparser

If you have already installed fparser and want to upgrade to the
latest version simply do:
::

   > pip install fparser --upgrade


pyparsing
^^^^^^^^^

PSyclone requires pyparsing, a library designed to allow parsers to be be
built in Python. PSyclone uses pyparsing to parse fortran regular
expressions as fparser does not fully parse these, (see
http://pyparsing.wikispaces.com for more information).

PSyclone has been tested with pyparsing versions 1.5.2, 2.0.1 and 2.2.0.

You can test whether pyparsing is already installed on your machine by
typing ``import pyparsing`` from the python command line. If pyparsing
is installed, this command will complete succesfully. If pyparsing is
installed you can check its version by typing
``pyparsing.__version__`` after succesfully importing it.

If pyparsing is not installed on your system then it may be installed
from the Python Package Index using ``pip``:
::

   > pip install pyparsing

Should you wish to, uninstalling is simply performed by doing:
::

   > pip uninstall pyparsing

If you do not have sufficient privileges for a system-wide install then
you can instruct pip to do a user-local install:
::

   > pip install --user pyparsing

Alternatively, you could follow the instructions here
http://pyparsing.wikispaces.com/Download+and+Installation.

graphviz
^^^^^^^^

The data dependencies of a PSyIR schedule determine the validity of
changes to this schedule.
PSyclone supports the visualisation of these dependencies as
a graph using graphviz. This visualisation is not needed to use
PSyclone.

If the Python bindings to graphviz are not installed on your system
then it may be installed from the Python Package Index using ``pip``:
::

   > sudo pip install graphviz

Should you wish to, uninstalling is simply performed by doing:
::

   > sudo pip uninstall graphviz

If you do not have sufficient privileges for a system-wide install then
you can instruct pip to do a user-local install:
::

   > pip install --user graphviz

If graphviz itself is not installed on your system and your system
supports the ``apt`` package manager then see below, otherwise please
refer to the download and install instructions which are available
here http://www.graphviz.org/Download..php.

If your system supports the ``apt`` package manager then it can be
installed and removed in the following way:
::

   > sudo apt install graphviz
   > sudo apt remove graphviz

termcolor
^^^^^^^^^

By default, the ``view()`` method available on any PSyIR (PSyclone
Internal Representation) object prints a plain-text representation
to standard-out. However, if the ``termcolor`` package is available
then PSyclone uses this to add colour highlighting to the output text.

Installation (and uninstallation) of this package can be done via
``pip`` in exactly the same way as for graphviz, as described above.


.. _getting_going_env:

Environment
-----------

In order to use PSyclone (including running the test suite and
building documentation) you will need to install it. The simplest way to
do this is to use pip with the supplied ``setup.py`` file::

   > cd <PSYCLONEHOME>
   > pip install .

By default pip will attempt a system-wide install. If you wish to do
a user-local install instead then supply the ``--user`` flag::
   
   > pip install --user .

This installs the PSyclone modules in
``~/.local/lib/pythonX.Y/site-packages`` (where X.Y is the version of
Python that you are using) and the 'psyclone' script in
``~/.local/bin``. Depending on your linux distribution, you may need to
add the latter location to your $PATH. The examples are installed in
``~/.local/share/psyclone/examples``.

If for some reason you'd rather not use pip then you can run the setup
manually::

   > python setup.py install

or, if you don't have root access::

   > python setup.py install --user

or,
::

   > python setup.py install --prefix /my/install/path

If using the latter method then it will be necessary to take
further action to ensure PSyclone can find the
configuration file installed as a part of this process - see below.

.. _getting-going-configuration:

Configuration
-------------

Various aspects of PSyclone are configured through a configuration
file, ``psyclone.cfg``. The default version of this file is installed
to ``<python-base-prefix>/shared/psyclone/`` during the installation
process. If a system-wide installation is being performed then this
will be something like ``/usr/share/psyclone/``. If a user-local
installation is performed (``--user`` flag to ``pip install``) then
the location will be something like ``~/.local/share/psyclone/``.

.. warning::

   if PSyclone is installed to a non-standard location (e.g. by
   specifying the ``--prefix=/some/path`` option to ``pip install``)
   then PSyclone will not be able to find the configuration file at
   execution time. There are two solutions to this: 1. copy the
   configuration file to a location where PSyclone will find it (see
   :ref:`configuration`) or 2. set the ``PSYCLONE_CONFIG`` environment
   variable to the full-path to the configuration file, e.g.::

   > export PSYCLONE_CONFIG=/some/path/PSyclone/config/psyclone.cfg

.. warning::

   when installing in 'editable' mode (``-e`` flag to pip), pip does
   *not* install the configuration file. You will have to take one of
   the two actions described above.

See :ref:`configuration` for details of the settings contained within
the config file.

Test
----

PSyclone contains an extensive test suite, but this test suite is not
part of a standard installation. If you want to run the full test 
suite, you need to install PSyclone from source, see 
:ref:`dev-installation` in the
`Developer Guide <https://psyclone-dev.readthedocs.io/>`_.

.. _getting-going-run:

Run
---

You are now ready to try running PSyclone on the examples. One way of
doing this is to use the ``psyclone`` driver script. Assuming it is
on your PATH:
::

   > psyclone
   usage: psyclone [-h] [-oalg OALG] [-opsy OPSY] [-okern OKERN] [-api API]
                   [-s SCRIPT] [-d DIRECTORY] [-I INCLUDE] [-l] [-dm] [-nodm]
                   [--kernel-renaming {multiple,single}]
		   [--profile {invokes,kernels}] [--config CONFIG] [-v]
                   filename
   psyclone: error: too few arguments

As indicated above, the psyclone script takes the name of the
Fortran source file containing the algorithm specification (in terms
of calls to invoke()). It parses this, finds the necessary kernel
source files and produces two Fortran files. The first contains the
PSy, middle layer and the second a re-write of the algorithm code to
use that layer. These files are named according to the user-supplied
arguments (options -oalg and -opsy). If those arguments are not
supplied then the script writes the generated/re-written Fortran to
the terminal. For details of the other command-line arguments please
see the :ref:`psyclone_script` Section.

Examples are provided in the examples directory of the PSyclone git
repository - if you have cloned the repository then ``EGS_HOME`` in
what follows is the root ``PSyclone`` directory. Alternatively, if you
have installed PSyclone using pip then they may be found in the
``share/psyclone`` directory under your Python installation. In this
case you should copy them to some convenient location (hereafter
called ``EGS_HOME``) before attempting to carry out the following
instructions.

There are 3 subdirectories (``lfric``, ``gocean`` and ``nemo``)
corresponding to different APIs that are supported by PSyclone. Note,
the lfric directory corresponds to the dynamo0.1 and dynamo0.3
APIs. In this case we are going to use one of the LFRic examples::

   > cd <EGS_HOME>/examples/lfric/eg1
   > psyclone -api dynamo0.1 \
   > -oalg dynamo_alg.f90 -opsy dynamo_psy.f90 dynamo.F90

You should see two new files created called dynamo_alg.f90 (containing
the re-written algorithm layer) and dynamo_psy.f90 (containing the
generated PSy- or middle-layer). Since this is an LFRic example the
Fortran source code has dependencies on the LFRic system and
therefore cannot be compiled stand-alone.

You can also use the ``runme.py`` example to see the interactive
API in action. This script contains::

   from psyclone.parse.algorithm import parse
   from psyclone.psyGen import PSyFactory
   
   # This example uses version 0.1 of the Dynamo API
   api = "dynamo0.1"
   
   # Parse the file containing the algorithm specification and
   # return the Abstract Syntax Tree and invokeInfo objects
   ast, invokeInfo = parse("dynamo.F90", api=api)
   
   # Create the PSy-layer object using the invokeInfo
   psy = PSyFactory(api).create(invokeInfo)
   # Generate the Fortran code for the PSy layer
   print(psy.gen)
   
   # List the invokes that the PSy layer has
   print(psy.invokes.names)
   
   # Examine the 'schedule' (e.g. loop structure) that each
   # invoke has
   schedule = psy.invokes.get('invoke_0_v3_kernel_type').schedule
   schedule.view()
    
   schedule = psy.invokes.get('invoke_1_v3_solver_kernel_type').schedule
   schedule.view()

It can be run non-interactively as follows::

   > cd <EGS_HOME>/example/lfric/eg1
   > python runme.py

However, to understand this example in more depth it is instructive to
cut-and-paste from the ``runme.py`` file into your own, interactive python
session::

   > cd <EGS_HOME>/example/lfric/eg1
   > python

In addition to the ``runme.py`` script, there is also
``runme_openmp.py`` which illustrates how one applies an OpenMP
transform to a loop schedule within the PSy layer. The initial part of
this script is the same as that of runme.py (above) and is therefore
omitted here::

   # List the various invokes that the PSy layer contains
   print(psy.invokes.names)

   # Get the loop schedule associated with one of these
   # invokes
   schedule = psy.invokes.get('invoke_v3_kernel_type').schedule
   schedule.view()

   # Get the list of possible loop transformations
   from psyclone.psyGen import TransInfo
   t = TransInfo()
   print(t.list)

   # Create an OpenMPLoop-transformation object
   ol = t.get_trans_name('OMPLoopTrans')

   # Apply it to the loop schedule of the selected invoke
   new_schedule, memento = ol.apply(schedule.children[0])
   new_schedule.view()

   # Replace the original loop schedule of the selected invoke
   # with the new, transformed schedule 
   psy.invokes.get('invoke_v3_kernel_type')._schedule = new_schedule
   # Generate the Fortran code for the new PSy layer
   print(psy.gen)
