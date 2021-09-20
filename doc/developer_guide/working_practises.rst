.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
.. Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

Working With PSyclone from GitHub
#################################

A PSyclone developer will, by definition, be working with the GitHub
PSyclone repository_ rather than
installing a released version from pypi (using e.g. ``pip install
psyclone``).  This section describes the general set-up necessary when
using PSyclone in this way. It also describes some of the development
practises of the PSyclone project.

.. _repository: https://github.com/stfc/PSyclone

More detailed instructions for the Ubuntu and OpenSUSE Linux
distributions may be found in the :ref:`system_specific_dev_setup`
Section.

.. _dev-installation:

Installation
============

Although PSyclone releases always work with a released version of
fparser, the same is not always true of other versions (e.g. the HEAD
of the master branch). For those versions of PSyclone requiring
fparser functionality that is not yet in a release, we use the git
submodule feature such that the PSyclone repository always has a link
to the correct version of fparser. In order to obtain this version
the PSyclone repository must be cloned with the ``--recursive`` flag::
  
   > git clone --recursive https://github.com/stfc/PSyclone.git

Alternatively, if you already have a local clone of the PSyclone github
repository then doing::

  > cd <PSYCLONEHOME>
  > git submodule init
  > git submodule update --init

will fetch the fparser submodule. Failure to do this will mean that
for example the ``<PSYCLONEHOME>/external/fparser`` directory will be
empty.

Note that after cloning the repository from GitHub, the local copy
will be on the master branch. If you are working with some other
branch then this must be checked out by doing::

  > cd <PSYCLONEHOME>
  > git checkout <BRANCH_NAME>

Once the above steps have been performed, the
``<PSYCLONEHOME>/external/fparser`` directory will contain the correct
version of the fparser code. This can then be installed using ``pip``::

  > cd <PSYCLONEHOME>/external/fparser
  > pip install --user .

Once you have the correct version of fparser installed you are ready to
install PSyclone itself. Again, the simplest way of doing this is to use
``pip``::

  > cd <PSYCLONEHOME>
  > pip install --user -e .

where ``-e`` requests an 'editable' installation so that changes to
the PSyclone source are immediately reflected in the installed
package.  (For alternatives to using pip please see the
:ref:`user_guide:getting-going` section.)

.. _test_suite:

Test Suite
==========

The PSyclone test suite is integral to the development process and all
new code must be covered (i.e. executed) by one or more tests. As
described in :ref:`user_guide:getting-going`, the test suite is
written for use with ``pytest``.

Tests should be run from the ``<PSYCLONEHOME>/src/psyclone/tests`` 
directory, from which all tests in subdirectories 
will be automatically found and started. If only a subset of all tests
need to be run, ``pytest`` can be invoked from the corresponding
subdirectory or with that subdirectory or filename as an argument.

Fixtures
--------

Various pytest fixtures
(https://docs.pytest.org/en/latest/fixture.html) are provided as part
of the PSyclone test suite. These are implemented in
``<PSYCLONEHOME>/src/psyclone/tests/conftest.py`` and are
automatically discovered by pytest.

Those fixtures available for use when implementing tests are (in
alphabetical order):

.. tabularcolumns:: |l|L|

================ ==============================================================
Fixture name   	 Description
================ ==============================================================
annexed        	 Supplies a test with the various possible values of the LFRic
                 `annexed_dofs` option.
dist_mem       	 Supplies a test with the various possible values of the
                 `distributed-memory` option (only applicable to the LFRic API
                 currently).
fortran_reader   Provides a Fortran PSyIR front-end object to convert Fortran
                 code snippets into PSyIR.
fortran_writer   Provides a Fortran PSyIR back-end object to convert PSyIR
                 trees into Fortran code.
have_graphviz  	 True if the Python bindings to the graphviz package (used when
                 generating DAG visualisations) are available. Does *not* check
                 that the underlying graphviz library is installed.
kernel_outputdir Sets the output directory used by PSyclone for transformed
                 kernels to be `tmpdir` (a built-in pytest fixture) and then
                 returns `tmpdir`. Any test that directly or indirectly causes
                 kernels to be transformed needs to use this fixture in order
                 to avoid having unwanted files created within the git working
                 tree.
parser           Creates an fparser2 parser for the Fortran2008 standard. This
                 is an expensive operation so this fixture is only run once
                 per test session.
================ ==============================================================

In addition, there are two fixtures that are automatically run (just
once) whenever a test session is begun. The first of these,
``setup_psyclone_config``, ensures that the PSyclone configuration
file used when running the test suite is the one distributed with
PSyclone and not any locally-modified version.  The second,
``infra_compile``, sets-up the ``tests.utilities.Compile`` class with
any compilation-testing flags (see :ref:`compilation_testing`)
provided to the pytest command line. It also ensures that (if
compilation testing is enabled) the LFRic-stub and GOcean infrastructure
libraries are compiled prior to any tests running.


.. _test_coverage:

Coverage
--------

The easiest and most user-friendly way of checking the coverage of any
new code is to use CodeCov (https://codecov.io/gh/stfc/PSyclone) which
is integrated with GitHub. Coverage for Pull Requests is automatically
reported and will appear as a comment on the Pull Request. This
comment is then automatically updated whenever new code is pushed to
the associated branch.

For checking test coverage on your local machine you will need to install
the ``cov`` plugin (``pip install pytest-cov``). You can then
request various types of coverage report when running the test suite. e.g.
to ask for a terminal report of missed lines for the ``dynamo0p3`` module
you would do::

  > cd <PSYCLONEHOME>
  > pytest --cov-report term-missing --cov psyclone.dynamo0p3

Note that you specify the python module name, and not the file name.
This will produce output along the lines of::
  
  ----------- coverage: platform linux, python 3.5.4-final-0 -----------
  Name                        Stmts   Miss  Cover   Missing
  ---------------------------------------------------------
  src/psyclone/dynamo0p3.py    2540     23    99%   558, 593, 777, 2731, 2972, 3865, 4132-4133, 4135-4136, 4139-4140, 4143-4144, 4149-4151, 4255, 4270, 4488, 5026, 6540, 6658, 6768

showing the line numbers which are not covered. By using ``--cov`` more than once
you can report on more than one file. You can also request
only selected tests to be run by specifying the file names on the command line.
Additionally html output can be created by adding the option ``--cov-report html``::

  > cd <PSYCLONEHOME>/src/psyclone/tests
  > pytest --cov-report term-missing --cov-report html --cov psyclone.dynamo0p3 ./dynamo0p3_basis_test.py ./parse_test.py

The html output can be viewed with a browser at ``file:///.../tests/htmlcov/index.html``
and it highlights all source lines in red that are not covered by at least one test.

.. _parallel_execution:

Parallel execution
------------------

The size of the test suite is such that running all of it in serial
can take many minutes, especially if you have requested a coverage
report. It is therefore very helpful to run it in parallel and pytest
provides support for this via the ``xdist`` plugin (``pip install
pytest-xdist``). Once you have this plugin, the test suite may be run
in parallel simply by providing the number of cores to use via the
``-n`` flag::

  > cd <PSYCLONEHOME>
  > pytest -n 4

Running the test suite in parallel also changes the order in which
tests are run which can reveal any problems resulting from tests not
being sufficiently isolated from one another.

Gotchas
-------
The test utility pytest will only discover files that either start
or end with "test". The PSyclone convention is to have all files ending
with "_test.py", e.g. ``constants_test.py``. A name using "tests"
(plural) will not be automatically discovered or executed by pytest!

Note that pytest will not complain if two tests (within a module) have
the same name - it will just silently ignore one of them! The best way
of checking for this is to run pylint on any modified test modules.
(This needs to be done anyway as one of the requirements of the
:ref:`code-review` is that all new code be pylint-clean.)

.. note::
    You can use ``pytest --collect-only``
    to check the names of the files and tests that would be executed,
    without actually executing the tests.


.. _compilation_testing:

Compilation testing
-------------------

The test suite provides support for testing that the code generated by
PSyclone is valid Fortran. This is performed by writing the generated
code to file and then invoking a Fortran compiler. This testing is not
performed by default since it requires a Fortran compiler and
significantly increases the time taken to run the test suite.

The Gnu Fortran compiler (gfortran) is used by default. If you wish to
use a different compiler and/or supply specific flags then these are
specified by further command-line flags::

  > pytest --compile --f90=ifort --f90flags="-O3"

If you want to test OpenCL code created by PSyclone, you must use the command line
option --compileopencl (which can be used together with --compile,
and --f90 and --f90flags), e.g.::

  > pytest --compileopencl --f90=<opencl-compiler> --f90flags="<opencl-specific flags>"


Infrastructure libraries
++++++++++++++++++++++++
Since the code generated by PSyclone for the GOcean and LFRic domains makes
calls to an infrastructure library, compilation tests must have access to
compiler specific .mod files. For LFRic, a stub implementation of the required
functions from the LFRic infrastructure is included in
``tests/test_files/dynamo0p3/infrastructure``. When compilation tests
are requested, the stub files are automatically compiled to create the required
.mod files. 

For the gocean1.0 domain a complete copy of the dl_esm_inf library is included 
as a submodule in ``<PSYCLONEHOME>/external/dl_esm_inf``. Before running tests
with compilation, make sure this submodule is up-to-date (see
:ref:`dev-installation`). The test process will compile dl_esm_inf
automatically, and all PSyclone gocean1.0 compilation tests will reference
these files.

If you  run the tests in parallel (see :ref:`parallel_execution` section) each
process will compile its own version of the wrapper files and infrastructure
library to avoid race conditions. This happens only once per process in each
test session.

Other Dependencies
++++++++++++++++++
Occasionally the code that is to be compiled as part of a test may depend
upon some piece of code that is not a Kernel or part of one of the supported
infrastructure libraries. In order to support this, the ``code_compiles``
method of ``psyclone.tests.utilities.Compile`` allows the user to supply a
list of additional files upon which kernels depend:

.. automethod:: psyclone.tests.utilities.Compile.code_compiles

These files must be located in the same directory as the kernels.

Continuous Integration
======================

The PSyclone project uses GitHub Actions
(https://psyclone.readthedocs.io/en/stable/examples.html#examples)
for continuous integration. GitHub triggers an action whenever there
is a push to the repository. The work performed by the action is
configured by the ``PSyclone/.github/workflows/python-package.yml``
file.

Currently there are three main checks performed, in order of increasing
computational cost (so that we 'fail fast'):

 1. All links within all MarkDown files are checked;

 2. All of the examples are tested (for Python versions 2.7, 3.5 and 3.8)
    using the ``Makefile`` in the ``examples`` directory. No compilation is
    performed; only the ``transform`` (performs the PSyclone transformations)
    and ``notebook`` (runs the various Jupyter notebooks) targets are used.

 3. The full test suite is run for Python versions 2.7, 3.5 and 3.8 but without
    the compilation checks.

Since we try to be good 'open-source citizens' we do not do any compilation
testing using GitHub as that would use a lot more compute time. Instead, it
is the responsibility of the developer and code reviewer to run these checks
locally (see :ref:`compilation_testing`).

By default, the GitHub Actions configuration uses ``pip`` to install
the dependencies required by PSyclone before running the test
suite. This works well when PSyclone only depends upon released
versions of other packages. However, PSyclone relies heavily upon
fparser which is also under development. Occasionally it may be that a
given branch of PSyclone requires a version of fparser that is not yet
released. As described in :ref:`dev-installation`, PSyclone has
fparser as a git submodule. In order to configure GitHub Actions to
use that version of fparser instead of a release, the
``python-package.yml`` file must be edited and the line executing
``pip install external/fparser`` must be uncommented.

Note that this functionality is only for development purposes. Any
release of PSyclone must work with a released version of fparser
and therefore the line described above must be commented out again
before making a release.

A single run of the test suite on GitHub Actions uses
approximately 20 minutes of CPU time and we run the test suite on three
different versions of Python. Therefore, it is good practise to avoid
triggering the tests unnecessarily (e.g. when we know that a certain commit
won't pass). This may be achieved by including the "[skip ci]" tag (without
the quotes) in the associated commit message.

Performance
===========

Exceptions
----------

PSyclone exceptions are designed to provide useful information to the
user. When there are problems transforming the PSyIR it can be useful
to use one of the backends to provide the code causing problems in an
easily readable form.

However, transformation exceptions can also be usefully used to only
apply a transformation to valid parts of a tree. For example:

.. code::

   for node in nodes:
       try:
           transform(node)
       except TransformationError:
           pass

If a transformation is called many times in the way described above the
exception string generated by the transformation error can cause
PSyclone to run very slowly - particularly if the exception makes use
of one of the backends.

The solution to this problem is to use the ``LazyString`` utility
class (see ``psyclone/errors.py``). This utility takes a function that
returns a string and only executes the function if the ``str`` method
is called for the class. This will not be the case for the above code
as the exception string is not used.

This approach is currently used in the ``CreateNemoKernelTrans``
transformation and internally in the ``TransformationError`` exception
(so that this transformation does not accidentally cause the string to
be evaluated).

If a transformation is used in the way described above and PSyclone
subsequently runs more slowly it is recommended that the ``LazyString``
class is used. It could be mandated that all transformation exceptions
use this approach but so far this problem has only been found in one
use case so it has been decided to modify the code as and when
required.

.. _code-review:

Code Review
===========

Before a branch can be merged to master it must pass code review. The
guidelines for performing a review (i.e. what is expected from the
developer) are available on the GitHub PSyclone wiki pages:
https://github.com/stfc/PSyclone/wiki.
