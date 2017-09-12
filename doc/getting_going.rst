.. _getting-going:

Getting Going
=============

Download
--------

PSyclone is available on github.

``https://github.com/stfc/PSyclone``

The latest release is |release| and the latest stable version is on
the master branch.

PSyclone releases can be downloaded (see |release| in the ``releases`` tab
on the github website) or you can download and extract the latest release of
PSyclone directly, e.g.

.. parsed-literal::
   > wget https://github.com/stfc/PSyclone/archive/\ |release|\ .tar.gz
   > gunzip \ |release|\ .tar.gz
   > tar xf \ |release|\ .tar
   > rm \ |release|\ .tar
   > ls
   PSyclone-\ |release|\ 
   

Alternatively PSyclone can be cloned:

``> git clone https://github.com/stfc/PSyclone.git``

By default you will have access to the master branch if you clone. To
change to the latest release then subsequently do the following

.. parsed-literal::
    > git checkout tags/\ |release|\ 

Hereon the location where you download or clone PSyclone (including the
PSyclone directory itself) will be referred to as <PSYCLONEHOME>

Dependencies
------------

PSyclone is written in Python so needs Python to be installed on the
target machine. PSyclone has been tested under Python 2.6.5 and 2.7.3.

PSyclone immediately relies on two external Python packages;
``fparser`` and ``pyparsing``. In addition, ``fparser`` requires
``numpy``. In order to run the test suite ``py.test`` is required. The
easiest way to satisfy the Python dependencies is to use the Python
Package Index (pypi.org) and ``pip``. See
https://packaging.python.org/installing/ for more information.
Note that some Linux distributions install pip only for python 3 by
default. In this case it is necessary to install pip for python 2. For
example in openSUSE 42.2:
::
   > zypper install python-pip

and then use pip2.7 instead of pip.


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

The minimum version of fparser required by PSyclone is currently 0.0.2
but we strongly recommend you install the latest version to reduce the
chance of encountering problems when parsing existing algorithm or
kernel code.

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

(See :ref:`install_fparser` for more details.)

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

The data dependencies of a PSy-layer schedule (see Section
:ref:`psy-layer-schedule`) determine the validity of changes to a
schedule. PSyclone supports the visualisation of these dependencies as
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

By default, the ``view()`` method of a ``schedule`` object (representing
the schedule of a PSy-layer routine) prints a plain-text representation
to standard-out. However, if the ``termcolor`` package is available
then PSyclone uses this to add colour highlighting to the output text.

Installation (and uninstallation) of this package can be done via
``pip`` in exactly the same way as for graphviz, as described above.


py.test
^^^^^^^

The PSyclone test suite uses py.test. This is not needed to use
PSyclone but is useful to check whether PSyclone is working correctly
on your system. You can test whether it is already installed by simply
typing ``py.test`` at a shell prompt. If it is present you will get
output that begins with
::

    ======================== test session starts ==================

If you do not have it then py.test can again be installed using
``pip`` or from here http://pytest.org/latest/ (or specifically here
http://pytest.org/latest/getting-started.html).

Environment
-----------

In order to use PSyclone (including running the test suite and
building documentation) you will need to install it. The simplest way to
do this is to use pip with the supplied ``setup.py`` file:
::

   > cd <PSYCLONEHOME>
   > pip install .

By default pip will attempt a system-wide install. If you wish to do
a user-local install instead then supply the ``--user`` flag:
::
   
   > pip install --user .

This installs the PSyclone modules in
~/.local/lib/pythonX.Y/site-packages (where X.Y is the version of
Python that you are using) and the 'psyclone' script in
~/.local/bin. Depending on your linux distribution, you may need to
add the latter location to your $PATH.

If for some reason you'd rather not use pip then you can run the setup
manually:
::

   > python setup.py install

or, if you don't have root access:
::

   > python setup.py install --prefix /my/install/path


Test
----

Once you have the necessary dependencies installed and your
environment configured, you can check that things are working by using
the PSyclone test suite. These tests are not required and can be
skipped if preferred:
::

   > cd <PSYCLONEHOME>/src/psyclone/tests
   > py.test

If everything is working as expected then you should see output similar to:
::

   ============================= test session starts ==============================
   platform linux2 -- Python 2.6.5 -- py-1.4.29 -- pytest-2.7.2
   rootdir: /home/rupert/proj/GungHoSVN/PSyclone_r3373_scripts/src/tests, inifile: 
   collected 175 items 

   alggen_test.py .......xxxxxxxxxxx.
   dynamo0p1_transformations_test.py .
   dynamo0p3_test.py .....................................x
   generator_test.py ...................
   ghproto_transformations_test.py x
   gocean0p1_transformations_test.py .......
   gocean1p0_test.py ....
   gocean1p0_transformations_test.py ......................x........
   parser_test.py ..........
   psyGen_test.py ..............................

   =================== 160 passed, 15 xfailed in 13.59 seconds ====================

.. _getting-going-run:

Run
---

You are now ready to try running PSyclone on the examples. One way of
doing this is to use the ``psyclone`` driver script. Assuming it is
on your PATH:
::

   > psyclone
   usage: psyclone [-h] [-oalg OALG] [-opsy OPSY] [-api API] [-s SCRIPT]
                   [-d DIRECTORY] [-l] [-dm] [-nodm]
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
the terminal.

Examples are provided in the examples directory. There are 3
subdirectories (dynamo, gocean and gunghoproto) corresponding to different
API's that are supported by PSyclone. In this case we are going to use
one of the dynamo examples
::

   > cd <PSYCLONEHOME>/examples/dynamo/eg1
   > psyclone -api dynamo0.1 \
   > -oalg dynamo_alg.f90 -opsy dynamo_psy.f90 dynamo.F90

You should see two new files created called dynamo_alg.f90 (containing
the re-written algorithm layer) and dynamo_psy.f90 (containing the
generated PSy- or middle-layer). Since this is a dynamo example the
Fortran source code has dependencies on the dynamo system and
therefore cannot be compiled stand-alone.

You can also use the runme.py example to see the interactive
API in action. This script contains:
::

   from psyclone.parse import parse
   from psyclone.psyGen import PSyFactory
   
   # This example uses version 0.1 of the Dynamo API
   api="dynamo0.1"
   
   # Parse the file containing the algorithm specification and
   # return the Abstract Syntax Tree and invokeInfo objects
   ast,invokeInfo=parse("dynamo.F90",api=api)
   
   # Create the PSy-layer object using the invokeInfo
   psy=PSyFactory(api).create(invokeInfo)
   # Generate the Fortran code for the PSy layer
   print psy.gen
   
   # List the invokes that the PSy layer has
   print psy.invokes.names
   
   # Examine the 'schedule' (e.g. loop structure) that each
   # invoke has
   schedule=psy.invokes.get('invoke_0_v3_kernel_type').schedule
   schedule.view()
    
   schedule=psy.invokes.get('invoke_1_v3_solver_kernel_type').schedule
   schedule.view()

It can be run non-interactively as follows:
::

   > cd <PSYCLONEHOME>/example/dynamo/eg1
   > python runme.py

However, to understand this example in more depth it is instructive to
cut-and-paste from the runme.py file into your own, interactive python
session:
::

   > cd <PSYCLONEHOME>/example/dynamo/eg1
   > python

In addition to the runme.py script, there is also runme_openmp.py which
illustrates how one applies an OpenMP transform to a loop schedule
within the PSy layer. The initial part of this script is the same as that 
of runme.py (above) and is therefore omitted here:
::

   # List the various invokes that the PSy layer contains
   print psy.invokes.names

   # Get the loop schedule associated with one of these
   # invokes
   schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
   schedule.view()

   # Get the list of possible loop transformations
   from psyclone.psyGen import TransInfo
   t=TransInfo()
   print t.list

   # Create an OpenMPLoop-transformation object
   ol=t.get_trans_name('OMPLoopTrans')

   # Apply it to the loop schedule of the selected invoke
   new_schedule,memento=ol.apply(schedule.children[0])
   new_schedule.view()

   # Replace the original loop schedule of the selected invoke
   # with the new, transformed schedule 
   psy.invokes.get('invoke_v3_kernel_type')._schedule=new_schedule
   # Generate the Fortran code for the new PSy layer
   print psy.gen
