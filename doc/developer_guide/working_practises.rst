.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
  > git submodule update --init --recursive

will fetch the fparser submodule. Failure to do this will mean that
for example the ``<PSYCLONEHOME>/external/fparser`` directory will be
empty. The ``--recursive`` option is necessary because dl_esm_inf uses
submodules as well.

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
f2008_parser     Creates an fparser2 parser for the Fortran2008 standard. This
                 is only done once per test session.
have_graphviz  	 True if the Python bindings to the graphviz package (used when
                 generating DAG visualisations) are available. Does *not* check
                 that the underlying graphviz library is installed.
kernel_outputdir Sets the output directory used by PSyclone for transformed
                 kernels to be `tmpdir` (a built-in pytest fixture) and then
                 returns `tmpdir`. Any test that directly or indirectly causes
                 kernels to be transformed needs to use this fixture in order
                 to avoid having unwanted files created within the git working
                 tree.
parser           Creates an fparser2 parser for the Fortran2003 standard. This
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

Note that pytest will not complain if two tests (within a module) have
the same name - it will just silently ignore one of them! The best way
of checking for this is to run pylint on any modified test modules.
(This needs to be done anyway as one of the requirements of the
:ref:`code-review` is that all new code be pylint-clean.)

.. _compilation_testing:

Compilation testing
-------------------

The test suite provides support for testing that the code generated by
PSyclone is valid Fortran. This is performed by writing the generated
code to file and then invoking a Fortran compiler. This testing is not
performed by default since it requires a Fortran compiler and
significantly increases the time taken to run the test suite.

.. note:: Compilaton testing is currently only supported for the
          "dynamo0.3" and "gocean1.0" APIs.


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
Since the code generated by PSyclone makes calls to an infrastructure
library, compilation tests must have access to compiler specific
.mod files. For dynamo0.3 a stub implementation of the required functions
from the LFRic infrastructure is included in 
``tests/test_files/dynamo0p3/infrastructure``. When compilation tests
are requested, the stub files are automatically compiled to create the required
.mod files. 

For the gocean1.0 API a complete copy of the dl_esm_inf library is included 
as a submodule in ``<PSYCLONEHOME>/external/dl_esm_inf``. Before running tests
with compilation,
make sure this submodule is up-to-date (see :ref:`dev-installation`). The test
process will compile dl_esm_inf automatically, and all PSyclone
gocean1.0 compilation tests will reference these files.

If you  run the tests in parallel (see :ref:`parallel_execution` section) each
process will compile its own version of the wrapper files and infrastructure
library to avoid race conditions. This happens only once per process in each
test session.

Other Dependencies
++++++++++++++++++
Occasionally the code that is to be compiled as part of a test may depend
upon some piece of code that is not a Kernel or part of one of the supported
infrastructure libraries. On order to support this, the ``code_compiles``
method of ``psyclone.tests.utilities.Compile`` allows the user to supply a
list of additional files upon which kernels depend:

.. automethod:: psyclone.tests.utilities.Compile.code_compiles

These files must be located in the same directory as the kernels.

Continuous Integration
======================

The PSyclone project uses Travis (https://travis-ci.org/stfc/PSyclone)
for continuous integration. GitHub triggers Travis to execute the test
suite whenever there is a push to the repository. The work performed
by Travis is configured by the ``.travis.yml`` file in the root
directory of the repository. Currently Travis is configured to run the
test suite for both Python 2.7 and 3.6 but does not do the extra
compilation checks (since that would considerably increase the execution
time of the test suite).

Travis also runs all of the examples using the ``check_examples``
bash script in the ``examples`` directory. Although this script has the
option to do some limited compilation of generated code (via the
``--compile`` flag) this is currently not enabled on Travis. Note that
a bash script is possibly not the best choice for implementing this
functionality and it might be better handled by a Makefile (issue #713).
That would also allow support to be added for running the examples for
a specific domain rather than for all of them.

By default, the Travis configuration uses ``pip`` to install the
dependencies required by PSyclone before running the test suite. This
works well when PSyclone only depends upon released versions of other
packages. However, PSyclone relies heavily upon fparser which is also
under development. Occasionally it may be that a given branch of
PSyclone requires a version of fparser that is not yet released. As
described in :ref:`dev-installation`, PSyclone has fparser as a git
submodule. In order to configure Travis to use that version of fparser
instead of a release, the ``.travis.yml`` file must be edited and the
line executing the "install_optional.sh" script (in the
``before_install`` section) must be edited to pass in the
"fparser_submodule" argument::

    - ./bin/install_optional.sh fparser_submodule

Note that this functionality is only for development purposes. Any
release of PSyclone must work with a released version of fparser
and therefore the "fparser_submodule" argument must be removed
before making a release.

Given that a run of the test-suite on Travis uses approximately 45
minutes of CPU time, it is good practise to avoid triggering it
unnecessarily (e.g. if you know that a certain commit won't
pass). This can be achieved by appending "[skip ci]" (without the
quotes) to the end of the associated git commit message.

.. _code-review:

Code Review
===========

Before a branch can be merged to master it must pass code review. The
guidelines for performing a review (i.e. what is expected from the
developer) are available on the GitHub PSyclone wiki pages:
https://github.com/stfc/PSyclone/wiki.
