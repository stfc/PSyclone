.. _getting-going:

Getting Going
=============

Download
--------

PSyclone is available on github.

``https://github.com/stfc/PSyclone``

The latest release is 1.3.3 and the latest stable version is on the master branch.

PSyclone releases can be downloaded (see ``1.3.3`` in the releases tab
on the website) or you can download and extract the latest release of
PSyclone directly
::

   > wget https://github.com/stfc/PSyclone/archive/1.3.3.tar.gz
   > gunzip 1.3.3.tar.gz
   > tar xf 1.3.3.tar
   > rm 1.3.3.tar
   > ls
   PSyclone-1.3.3
   

Alternatively PSyclone can be cloned:

``git clone https://github.com/stfc/PSyclone.git``

By default you will have access to the master branch if you clone. To
change to the latest release then subsequently do the following

``git checkout tags/1.3.3``

Hereon the location where you download or clone PSyclone (including the
PSyclone directory itself) will be refered to as <PSYCLONEHOME>

Dependencies
------------

PSyclone is written in Python so needs Python to be installed on the
target machine. PSyclone has been tested under Python 2.6.5 and 2.7.3.

PSyclone immediately relies on two external Python packages; fparser
and pyparsing. In addition, fparser requires numpy. To run the test
suite you will require py.test. The easiest way to satisfy these
dependencies is to use the Python Package Index (pypi.org) and
``pip``. See https://packaging.python.org/installing/ for more
information.

System-specific set-up
^^^^^^^^^^^^^^^^^^^^^^

System-specific set-up instructions are available for the following systems

* Ubuntu 14.03.3 :ref:`Ubuntu14.03.3`

fparser
^^^^^^^

The fparser package (https://github.com/stfc/fparser) is a Fortran
parser originally developed as a part of the f2py project. PSyclone
requires version >= 0.0.2. It is available from the Python Package
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

(See :ref:`install_fparser` for more details.)

pyparsing
^^^^^^^^^

PSyclone requires pyparsing, a library designed to allow parsers to be be
built in Python. PSyclone uses pyparsing to parse fortran regular
expressions as fparser does not fully parse these, (see
http://pyparsing.wikispaces.com for more information).

PSyclone has been tested with pyparsing versions 1.5.2 and 2.0.1.

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
building documentation) you will need to tell Python where to find the
PSyclone source:
::

    > export PYTHONPATH=<PSYCLONEHOME>/src:${PYTHONPATH}

Test
----

Once you have the necessary dependencies installed and your
environment configured, you can check that things are working by using
the PSyclone test suite. These tests are not required and can be
skipped if preferred:
::

    > cd <PSYCLONEHOME>/src/tests
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
doing this is to use the generator.py script:
::

    > cd <PSYCLONEHOME>/src
    > python ./generator.py 
    usage: generator.py [-h] [-oalg OALG] [-opsy OPSY] [-api API] [-s SCRIPT]
                        [-d DIRECTORY] [-l]
                        filename
    generator.py: error: too few arguments

As indicated above, the generator.py script takes the name of the
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
    > python ../../../src/generator.py -api dynamo0.1 \
    > -oalg dynamo_alg.f90 -opsy dynamo_psy.f90 dynamo.F90

You should see two new files created called dynamo_alg.f90 (containing
the re-written algorithm layer) and dynamo_psy.f90 (containing the
generated PSy- or middle-layer). Since this is a dynamo example the
Fortran source code has dependencies on the dynamo system and
therefore cannot be compiled stand-alone.

You can also use the runme.py example to see the interactive
API in action. This script contains:
::

    from parse import parse
    from psyGen import PSyFactory
    
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
    from psyGen import TransInfo
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
