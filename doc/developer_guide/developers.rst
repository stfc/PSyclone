.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019, Science and Technology Facilities Council.
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

.. _developers-guide:

Developers' guide
*****************

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
------------------------
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


Continuous Integration
======================

The PSyclone project uses Travis (https://travis-ci.org/stfc/PSyclone)
for continuous integration. GitHub triggers Travis to execute the test
suite whenever there is a push to the repository. The work performed
by Travis is configured by the ``.travis.yml`` file in the root
directory of the repository. Currently Travis is configured to run the
test suite for both Python 2.7 and 3.6. It also runs all of the examples
using the ``check_examples`` script in the ``examples`` directory.

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

The PSyclone Internal Representation (PSyIR)
############################################

The PSyclone Internal Representation (PSyIR) is a language-independent
AST that PSyclone uses to represent the PSy layer and the kernel
code. The PSyIR can be constructed from scratch or produced from
existing code using one of the front-ends (Readers) and it can be
transformed back to a particular language using the back-ends (Writers)
provided in PSyclone.

Nodes
=====

All nodes in the AST are sub-classes of the abstract `Node` base class, which
provides the following common interface:

.. autoclass:: psyclone.psyGen.Node
    :members:

.. _container-label:

Container
=========

The Container node contains one or more Containers and/or
KernelSchedules (see :ref:`kernel_schedule-label`). Similarly to
KernelSchedule it contains a SymbolTable
(`psyclone.psyGen.SymbolTable`) that keeps a record of the Symbols
(`psyclone.psyGen.Symbol`) specified in the Container scope (see
:ref:`symbol-label`).

A Container can be used to capture a hierarchical grouping of
KernelSchedules and a hierarchy of Symbol scopes i.e. a Symbol
specified in a Container is visible to all Containers and
KernelSchedules within it and their descendents.

.. autoclass:: psyclone.psyGen.Container
    :members:

Schedule
========

The Schedule node represents a sequence of statements. It is an important node
in PSyclone because two of its specialisations: `InvokeSchedule` and
`KernelSchedule` (described below), are used as the root nodes of PSy-layer
invokes and kernel subroutines. This makes them the starting points for any
walking of the PSyIR tree in PSyclone transformation scripts and a common
target for the application of transformations.

.. autoclass:: psyclone.psyGen.Schedule
    :members:


InvokeSchedule
--------------

The `InvokeSchedule` is a PSyIR node that represents an invoke subroutine in
the PSy-layer. It extends the `psyclone.psyGen.Schedule` functionality
with a `psyclone.psyGen.NameSpace` and a reference to its associated
`psyclone.psyGen.Invoke` object.

.. autoclass:: psyclone.psyGen.InvokeSchedule
    :members:

.. _kernel_schedule-label:

KernelSchedule
---------------

The `KernelSchedule` is a PSyIR node that represents a kernel
subroutine. It extends the `psyclone.psyGen.Schedule` functionality
with a SymbolTable (`psyclone.psyGen.SymbolTable`) that keeps a record
of the Symbols (`psyclone.psyGen.Symbol`) used in the kernel scope
(see :ref:`symbol-label`).


Control Flow Nodes
==================

The PSyIR has two control flow nodes: `IfBlock` and `Loop`. These nodes represent
the canonical structure with which conditional branching constructs and
iteration constructs are built. Additional language-specific syntax for branching
and iteration will be normalized to use these same constructs.
For example, Fortran has the additional branching constructs `ELSE IF`
and `CASE`: when a Fortran code is translated into the PSyIR, PSyclone will
build a semantically equivalent implementation using `IfBlocks`.
However, the necessary nodes in the new tree structure will be annotated
with information to enable the original language-specific syntax to be
recreated if required.


Branching construct
-------------------

.. autoclass:: psyclone.psyGen.IfBlock
    :members:


Iteration construct
-------------------

.. autoclass:: psyclone.psyGen.Loop
    :members:


Operation Nodes
===============

Arithmetic operations and various intrinsic/query functions are represented
in the PSyIR by sub-classes of the `Operation` node:

.. autoclass:: psyclone.psyGen.Operation
   :members:

The operations are classified according to the number of operands:
those having one operand are represented by
`psyclone.psyGen.UnaryOperation` nodes, those having two by
`psyclone.psyGen.BinaryOperation` and those having more than two by
`psyclone.psyGen.NaryOperation`. Note that where an intrinsic (such as
Fortran's `MAX`) can have a variable number of arguments, the class
used to represent it in the PSyIR is determined by the actual number
of arguments in a particular instance. e.g. `MAX(var1, var2)` would be
represented by a `psyclone.psyGen.BinaryOperation` but `MAX(var1,
var2, var3)` would be represented by a
`psyclone.psyGen.NaryOperation`.

The operations supported by the `UnaryOperation` are:

.. autoclass:: psyclone.psyGen.UnaryOperation.Operator
   :members:
   :undoc-members:

The operations supported by the `BinaryOperation` are:

.. autoclass:: psyclone.psyGen.BinaryOperation.Operator
   :members:
   :undoc-members:

The operations supported by the `NaryOperation` are:

.. autoclass:: psyclone.psyGen.NaryOperation.Operator
   :members:
   :undoc-members:


CodeBlock Node
==============

The PSyIR CodeBlock node contains code that has no representation in
the PSyIR. It is useful as it allows the PSyIR to represent complex
code by using CodeBlocks to handle the parts which contain unsupported
language features. One approach would be to work towards capturing all
language features in the PSyIR, which would gradually remove the need
for CodeBlocks. However, the purpose of the PSyIR is to capture code
concepts that are relevant for performance, not all aspects of a code,
therefore it is likely that that CodeBlocks will continue to be an
important part of the PSyIR.

.. autoclass:: psyclone.psyGen.CodeBlock
   :members:
   :undoc-members:

The code represented by a CodeBlock is currently stored as a list of
fparser2 nodes. Therefore, a CodeBlock's input and output language is
limited to being Fortran. This means that only the fparser2 front-end
and Fortran back-end can be used when there are CodeBlocks within a
PSyIR tree. In theory, language interfaces could be written between
CodeBlocks and other PSyIR Nodes to support different back-ends but
this has not been implemented.

The CodeBlock ``structure`` method indicates whether the code contains
one or more Fortran expressions or one or more statements (which may
themselves contain expressions). This is required by the Fortran
back-end as expressions do not need indentation and a newline whereas
statements do.

A feature of the fparser2 node list is that if the first node in the
list is a statement then so are all the other nodes in the list and
that if the first node in the list is an expression then so are all
the other nodes in the list. This allows the ``structure`` method to
return a single value that represents all nodes in the list.

The structure of the PSyIR hierarchy is used to determine whether the
code in a CodeBlock contains expressions or statements. This is
achieved by looking at the parent PSyIR Node. If the parent Node is a
Schedule then the CodeBlock contains one or more statements, otherwise
it contains one or more expressions. This logic works for existing
PSyIR nodes and relies on any future PSyIR nodes being constructed so
this continues to be true. The one exception to this rule is
Directives. Directives currently do not place their children in a
Schedule. As the structure of Directives is under discussion, it was
decided to raise an exception if the parent node of a CodeBlock is a
Directive (for the time being).

.. _symbol-label:

Symbol Table and Symbol
=======================

The Container (see :ref:`container-label` and KernelSchedule (see
:ref:`kernel_schedule-label`) nodes contain a SymbolTable
(`psyclone.psyGen.SymbolTable`) which keeps a record of the Symbols
(`psyclone.psyGen.Symbol`) specified and used within them.  A `Symbol`
is defined as:

.. autoclass:: psyclone.psyGen.Symbol
    :members:

The SymbolTable has the following interface:

.. autoclass:: psyclone.psyGen.SymbolTable
    :members:


Dependence Analysis
===================

Dependence Analysis in PSyclone produces ordering constraints between
instances of the `Argument` class within a PSyIR.

The `Argument` class is used to specify the data being passed into and
out of instances of the `Kern` class, `HaloExchange` class and
`GlobalSum` class (and their subclasses).

As an illustration consider the following invoke::

   invoke(           &
       kernel1(a,b), &
       kernel2(b,c))

where the metadata for `kernel1` specifies that the 2nd argument is
written to and the metadata for `kernel2` specifies that the 1st
argument is read.

In this case the PSyclone dependence analysis will determine that
there is a flow dependence between the second argument of `Kernel1`
and the first argument of `Kernel2` (a read after a write).

Information about arguments is aggregated to the PSyIR node level
(`kernel1` and `kernel2` in this case) and then on to the parent
`loop` node resulting in a flow dependence (a read after a write)
between a loop containing `kernel1` and a loop containing
`kernel2`. This dependence is used to ensure that a transformation is
not able to move one loop before or after the other in the PSyIR
schedule (as this would cause incorrect results).

Dependence analysis is implemented in PSyclone to support
functionality such as adding and removing halo exchanges,
parallelisation and moving nodes in a PSyIR schedule. Dependencies
between nodes in a PSyIR schedule can be viewed as a DAG using the
`dag()` method within the `Node` base class.

DataAccess Class
----------------

The `DataAccess` class is at the core of PSyclone data dependence
analysis. It takes an instance of the `Argument` class on
initialisation and provides methods to compare this instance with
other instances of the `Argument` class. The class is used to
determine 2 main things, called `overlap` and `covered`.

Overlap
+++++++

`Overlap` specifies whether accesses specified by two instances of the
`Argument` class access the same data or not. If they do access the
same data their accesses are deemed to `overlap`. The best way to
explain the meaning of `overlap` is with an example:

Consider a one dimensional array called `A` of size 4 (`A(4)`). If one
instance of the `Argument` class accessed the first two elements of
array `A` and another instance of the `Argument` class accessed the
last two elements of array `A` then they would both be accessing array
`A` but their accesses would *not* `overlap`. However, if one instance
of the `Argument` class accessed the first three elements of array `A`
and another instance of the `Argument` class accessed the last two
elements of array `A` then their accesses would `overlap` as they are
both accessing element `A(3)`.

Having explained the idea of `overlap` in its general sense, in
practice PSyclone currently assumes that *any* two instances of the
`Argument` class that access data with the same name will always
`overlap` and does no further analysis (apart from halo exchanges and
vectors, which are discussed below). The reason for this is that
nearly all accesses to data, associated with an instance of the
`Argument` class, start at index 1 and end at the number of elements,
dofs or some halo depth. The exceptions to this are halo exchanges,
which only access the halo and boundary conditions, which only access
a subset of the data. However these subset accesses are currently not
captured in metadata so PSyclone must assume subset accesses do not
exist.

If there is a field vector associated with an instance of an
`Argument` class then all of the data in its vector indices are
assumed to be accessed when the argument is part of a `Kern` or a
`GlobalSum`. However, in contrast, a `HaloExchange` only acts on a
single index of a field vector. Therefore there is one halo exchange
per field vector index. For example::

    InvokeSchedule[invoke='invoke_0_testkern_stencil_vector_type', dm=True]
    ... HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
    ... HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
    ... HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
    ... Loop[type='',field_space='w0',it_space='cells', upper_bound='cell_halo(1)']
    ... ... CodedKern testkern_stencil_vector_code(f1,f2) [module_inline=False]

In the above PSyIR schedule, the field `f1` is a vector field and the
`CodedKern` `testkern\_stencil\_vector\_code` is assumed to access data in
all of the vector components. However, there is a separate `HaloExchange`
for each component. This means that halo exchanges accessing the
same field but different components do not `overlap`, but each halo
exchange does overlap with the loop node. The current implementation
of the `overlaps()` method deals with field vectors correctly.

Coverage
++++++++

The concept of `coverage` naturally follows from the discussion in the
previous section.

Again consider a one dimensional array called `A` of size 4
(`A(4)`). If one instance (that we will call the `source`) of the
`Argument` class accessed the first 3 elements of array `A`
(i.e. elements 1 to 3) and another instance of the `Argument` class
accessed the first two elements of array `A` then their accesses would
`overlap` as they are both accessing elements `A(1) and A(2)` and
elements `A(1) and A(2)` would be `covered`. However, access `A(3)`
for the `source Argument` class would not yet be `covered`. If a
subsequent instance of the `Argument` class accessed the 2nd and 3rd
elements of array `A` then all of the accesses (`A(1), A(2) and A(3)`)
would now be `covered` so the `source argument` would be deemed to be
covered.

In PSyclone the above situation occurs when a vector field is accessed
in a kernel and also requires halo exchanges e.g.::

   InvokeSchedule[invoke='invoke_0_testkern_stencil_vector_type', dm=True]
      HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
      HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
      HaloExchange[field='f1', type='region', depth=1, check_dirty=True]
      Loop[type='',field_space='w0',it_space='cells', upper_bound='cell_halo(1)']
         CodedKern testkern_stencil_vector_code(f1,f2) [module_inline=False]

In this case the PSyIR loop node needs to know about all 3 halo
exchanges before its access is fully `covered`. This functionality is
implemented by passing instances of the `Argument` class to the
`DataAccess` class `update_coverage()` method and testing the
access.covered property until it returns `True`.

::

   # this example is for a field vector 'f1' of size 3
   # f1_index[1,2,3] are halo exchange accesses to vector indices [1,2,3] respectively
   access = DataAccess(f1_loop)
   access.update_coverage(f1_index1)
   result = access.covered  # will be False
   access.update_coverage(f1_index2)
   result = access.covered  # will be False
   access.update_coverage(f1_index3)
   result = access.covered  # will be True
   access.reset_coverage()

Note the `reset_coverage()` method can be used to reset internal state
so the instance can be re-used (but this is not used by PSyclone at
the moment).

The way in which halo exchanges are placed means that it is not
possible for two halo exchange with the same index to depend on each
other in a schedule. As a result an exception is raised if this
situation is found.

Notice there is no concept of read or write dependencies here. Read or
write dependencies are handled by classes that make use of the
DataAccess class i.e. the `_field_write_arguments()` and
`_field_read_arguments()` methods, both of which are found in the
`Arguments` class.

Variable Accesses
=================

Especially in the NEMO API, it is not possible to rely on pre-defined
kernel information to determine dependencies between loops. So an additional,
somewhat lower-level API has been implemented that can be used to determine
variable accesses (READ, WRITE etc.), which is based on the PSyIR information.
The only exception to this is if a kernel is called, in which case the
metadata for the kernel declaration will be used to determine the variable
accesses for the call statement. The information about all variable usage
of a node can be gathered by creating an object of type
`psyclone.core.access_info.VariablesAccessInfo`, and then calling
the function `reference_accesses()` for the node:

.. autofunction:: psyclone.psyGen.Node.reference_accesses

.. autoclass:: psyclone.core.access_info.VariablesAccessInfo
    :members:
    :special-members: __str__

This class collects information for each variable used in the tree
starting with the given node. A `VariablesAccessInfo` instance can store
information about variables in any arbitrary code, not only for a PSyIR
node. You can pass it to more than one `reference_accesses()` function
to add more variable access information, or use the `merge()` function to
combine two `VariablesAccessInfo` objects into one. It is up to the user to
keep track of which access information is stored in a `VariablesAccessInfo`
instance.

For each variable used an instance of
`psyclone.core.access_info.VariableAccessInfo` is created, which collects
all accesses for that variable using `psyclone.config.access_info.AccessInfo`
instances:

.. autoclass:: psyclone.core.access_info.VariableAccessInfo
    :members:

.. autoclass:: psyclone.core.access_info.AccessInfo
    :members:

Access Location
---------------

Variable accesses are stored in the order in which they happen. For example,
an assignment `a=a+1` will store two access for the variable `a`, the
first one being a READ access, followed by a WRITE access, since this is the
order in which the accesses are executed.
Additionally, the function `reference_accessess()` keeps track of the location
at which the accesses happen. A location is an integer number, starting with 0,
which is increased for each new statement. This makes it possible to
compare accesses to variables: if two accesses have the same location value,
it means the accesses happen in the same statement, for example `a=a+1`:
the READ and WRITE access to `a` will have the same location number. If on the
other hand the accesses happen in two separate statements, e.g. `a=b+1; c=a+1`
then the first access to `a` (and the access to `b`) will have a smaller
location number than the second access to `a` (and the access to `c`).
If two statements have consecutive locations, this does not necessarily mean
that the statements are executed one after another. For example in if-statements
the statements in the if-body are counted first, then the statements in the
else-body. It is the responsibility of the user to handle these cases - for
example by creating separate `VariablesAccessInfo` for statements in the if-body
and for the else-body.

.. note:: When using different instances for an if- and else-body, the first
    statement of the if-body will
    have the same location number as the first statement of the else-body. So
    you can only compare location numbers from the same `VariablesAccessInformation`
    instance. If you merge two instances together, the locations of the merged-in
    instance will be appropriately increased to follow the locations of the
    instance to which it is merged.


The location number is not exactly a line number - several statements can be
on one line, which will get different location numbers. And certain lines
will not have a location number (e.g. comment lines).

As stated above, one instance of `VariablesAccessInfo` can be extended by adding
additional variable information. It is the responsibility of the user to make
sure the accesses are added in the right order - the `VariablesAccessInfo` object
will always assume accesses happen at the current location, and a call to 
`next_location()` is required to increase the location number.

.. note:: It is not possible to add access information about an earlier
     usage to an existing `VariablesAccessInfo` object. 


Access Examples
---------------

Below we show a simple example of how to use this API. This is from the
`psyclone.psyGen.OMPParallelDirective` (so `self` is an instance of this
node), and this code is used to determine a list of all the scalar
variables that must be declared as thread-private::

  var_accesses = VariablesAccessInfo()
  self.reference_accesses(var_accesses)
  for var_name in var_accesses.all_vars:
      accesses = var_accesses[var_name].all_accesses
      # Ignore variables that are arrays, we only look at scalar ones.
      # If we do have a symbol table, use the shape of the variable
      # to determine if the variable is scalar or not
      if symbol_table:
          if len(symbol_table.lookup(var_name).shape) > 0:
              continue

      # If there is no symbol table, check instead if the first access of
      # the variable has indices, and assume it is an array if it has:
      elif accesses[0].indices is not None:
          continue

      # If a variable is only accessed once, it is either a coding error
      # or a shared variable - anyway it is not private
      if len(accesses) == 1:
          continue

      # We have at least two accesses. If the first one is a write,
      # assume the variable should be private:
      if accesses[0].access_type == AccessType.WRITE:
          result.add(var_name.lower())


The next, hypothetical example shows how the `VariablesAccessInfo` class
can be used iteratively. Assume that you have a function that determines
if the given variable accesses can be parallelised, and the aim is to
determine the largest consecutive block of statements that can be
executed in parallel. The accesses of one statement at a time are added
until we find accesses that would prevent parallelisation::

   # Create an empty instance to store accesses
   accesses = VariablesAccessInfo()
   list_of_parallelisable_statements = []
   while next_statement is not None:
       # Add the variable accesses of the next statement to
       # the existing accesses:
       next_statement.reference_accesses(accesses)
       # Stop when the next statement can not be parallelised
       # together with the previous accesses:
       if not can_be_parallelised(accesses):
           break
       list_of_parallelisable_statements.append(next_statement)
       # Assume there is a function that gives you the next statement:
       next_statement = next_statement.next()


.. note:: There is a certain overlap in the dependency analysis code
          and the variable access API. More work on unifying those two
          approaches will be undertaken in the future. Also, when calling
          `reference_accesses()` for a Dynamo or GOcean kernel, the 
          variable access mode for parameters is taken
          from the kernel metadata, not from the actual kernel source 
          code.

Dependency Tools
----------------
PSyclone contains a class that provides useful tools for dependency analaysis.
It especially provides messages for the user to indicate why parallelisation
was not possible.

.. autoclass:: psyclone.psyir.tools.dependency_tools.DependencyTools
    :members:

.. note:: There is limited support for detecting index expression that are
    identical because of the commutative law, e.g. `i+k` and `k+i` would be
    considered equal. But this only applies if two items are switched that
    are part of the same PSyIR node. An expression like `i+k+1` is stored as
    `(i+k)+1`, so if it is compared with `i+1+k` they are not considered to
    be equal, because `i+1` and `i+k` are not the same.


An example of how to use this class is shown below. It takes a list of statements
(i.e. nodes in the PSyIR), and adds 'OMP DO' directives around loops that
can be parallelised::

  parallel_loop = OMPLoopTrans()
  # The loops in the Fortran functions that must be parallelised
  # are over the 'grid' domain. Note that the psyclone config
  # file specifies the mapping of loop variable to type, e.g.:
  #
  #   mapping-grid = var: np, start: Ns, stop: Ne,  order: 0
  #
  # This means any loop using the variable 'np' is considered a
  # loop of type 'grid'
  dt = DependencyTools(["grid"])

  for statement in statements:
      if isinstance(statement, NemoLoop):
          # Check if there is a variable dependency that might 
          # prevent this loop from being parallelised:
          if dt.can_loop_be_parallelised(statement):
              parallel_loop.apply(statement)
          else:
              # Print all messages from the dependency analysis
              # as feedback for the user:
              for message in dt.get_all_messages():
                  print(message)


PSyIR back-ends
###############

PSyIR back-ends translate PSyIR into another form (such as Fortran, C
or OpenCL). Until recently this back-end support has been implemented
within the PSyIR `Node` classes themselves via various `gen*`
methods. However, this approach is getting a little unwieldy.

Therefore a `Visitor` pattern has been used in the latest back-end
implementation (translating PSyIR kernel code to Fortran). This
approach separates the code to traverse a tree from the tree being
visited. It is expected that the existing back-ends will migrate to
this new approach over time. The back-end visitor code is stored in
`psyclone/psyir/backend`.

Visitor Base code
=================

`visitor.py` in `psyclone/psyir/backend` provides a base class -
`PSyIRVisitor` - that implements the visitor pattern and is designed
to be subclassed by each back-end.

`PSyIRVisitor` is implemented in such a way that the PSyIR classes do
not need to be modified. This is achieved by translating the class
name of the object being visited in the PSyIR tree into the method
name that the visitor attempts to call (using the Python `eval`
function). `_node` is postfixed to the method name to avoid name
clashes with Python keywords.

For example, an instance of the `Loop` PSyIR class would result in
`PSyIRVisitor` attempting to call a `loop_node` method with the PSyIR
instance as an argument. Note the names are always translated to lower
case. Therefore, a particular back-end needs to subclass
`PSyIRVisitor`, provide a `loop_node` method (in this particular example) and
this method would then be called when the visitor finds an instance of
`Loop`. For example:

::

    from __future__ import print_function
    class TestVisitor(PSyIRVisitor):
        ''' Example implementation of a back-end visitor. '''

        def loop_node(self, node):
            ''' This method is called if the visitor finds a loop. '''
            print("Found a loop node")

    test_visitor = TestVisitor()
    test_visitor._visit(psyir_tree)

It is up to the sub-class to call any children of the particular
node. This approach was chosen as it allows the sub-class to control
when and how to call children. For example:

::

    from __future__ import print_function
    class TestVisitor(PSyIRVisitor):
        ''' Example implementation of a back-end visitor. '''

        def loop_node(self, node):
            ''' This method is called if the visitor finds a loop. '''
            print("Found a loop node")
            for child in node.children:
                self._visit(child)

    test_visitor = TestVisitor()
    test_visitor._visit(psyir_tree)

If a `node` is called that does not have an associated method defined
then `PSyIRVisitor` will raise a `VisitorError` exception. This
behaviour can be changed by setting the `skip_nodes` option to `True`
when initialising the visitor i.e.

::

    test_visitor = TestVisitor(skip_nodes=True)

Any unsupported nodes will then be ignored and their children will be
called in the order that they appear in the tree.

PSyIR nodes might not be direct subclasses of `Node`. For example,
`GOKernelSchedule` subclasses `KernelSchedule` which subclasses
`Schedule` which subclasses `Node`. This can cause a problem as a
back-end would need to have a different method for each class e.g. both
a `gokernelschedule_node` and a `kernelschedule_node` method, even if the
required behaviour is the same. Even worse, expecting someone to have
to implement a new method in all back-ends when they subclass a node
(if they don't require the back-end output to change) is overly
restrictive.

To get round the above problem, if the attempt to call a method with
the name of the PSyIR class (with `_node` appended) fails, then the
`PSyIRVisitor` will subsequently call the method name of its parent
(with `_node` appended). This will continue with the `PSyIRVisitor`
working its way through the class hierarchy in method resolution order
until it is successful (or fails for all names and raises an
exception).

This implementation gives the behaviour one would expect from standard
inheritance rules. For example, if a `kernelschedule_node` method is
implemented in the back-end and a `GOKernelSchedule` is found then a
`gokernelschedule_node` method is first tried which fails, then a
`kernelschedule_node` method is called which succeeds. Therefore all
subclasses of `KernelSchedule` will call the `kernelschedule_node`
method (if their particular specialisation has not been added).

One example of the power of this approach makes use of the fact that
all PSyIR nodes have `Node` as a parent class. Therefore, some base
functionality can be added there and all nodes that do not have a
specific method implemented will call this. To see the
class hierarchy, the following code can be written:

::

   from __future__ import print_function
    class PrintHierarchy(PSyIRVisitor):
        ''' Example of a visitor that prints the PSyIR node hierarchy. '''

        def node_node(self, node):
        ''' This method is called if no specific methods have been
            written. '''
            print("[ {0} start]".format(type(node).__name__))
            for child in node.children:
                self._visit(child)
            print("[ {0} end]".format(type(node).__name__))

    print_hierarchy = PrintHierarchy()
    print_hierarchy._visit(psyir_tree)

In the examples presented up to now, the information from a back-end
has been printed. However, a back-end will generally not want to use
print statements. Output from a `PSyIRVisitor` is supported by
allowing each method call to return a string. Reimplementing the
previous example using strings would give the following:

::
   
    from __future__ import print_function class
    PrintHierarchy(PSyIRVisitor):
        ''' Example of a visitor that prints the PSyIR node hierarchy'''

        def node_node(self, node):
            ''' This method is called if the visitor finds a loop '''
            result = "[ {0} start ]".format(type(node).__name__)
            for child in node.children:
                result += self._visit(child)
            result += "[ {0} end ]".format(type(node).__name__)
            return result

    print_hierarchy = PrintHierarchy()
    result = print_hierarchy._visit(psyir_tree)
    print(result)

As most back-ends are expected to indent their output based in some
way on the PSyIR node hierarchy, the `PSyIRVisitor` provides support
for this. The `self._nindent` variable contains the current
indentation as a string and the indentation can be increased by
increasing the value of the `self._depth` variable. The initial depth
defaults to 0 and the initial indentation defaults to two
spaces. These defaults can be changed when creating the back-end
instance. For example:

::

    print_hierarchy = PrintHierarchy(initial_indent_depth=2,
                                     indent_string="***")

The `PrintHierarchy` example can be modified to support indenting by
writing the following:

::

    from __future__ import print_function
    class PrintHierarchy(PSyIRVisitor):
    ''' Example of a visitor that prints the PSyIR node hierarchy
        with indentation'''

        def node_node(self, node):
            ''' This method is called if the visitor finds a loop '''
            result = "{0}[ {1} start ]\n".format(self._nindent,
                                                 type(node).__name__)
        self._depth += 1
        for child in node.children:
            result += self._visit(child)
        self._depth -= 1
        result += "{0}[ {1} end ]\n".format(self._nindent,
                                            type(node).__name__)
        return result

    print_hierarchy = PrintHierarchy()
    result = print_hierarchy._visit(psyir_tree)
    print(result)

As a visitor instance always calls the `_visit` method, an alternative
(functor) implementation is provided via the `__call__` method in the
base class. This allows the above example to be called in the
following simplified way (as if it were a function):

::

    print_hierarchy = PrintHierarchy()
    result = print_hierarchy(psyir_tree)
    print(result)

The primary reason for providing the above (functor) interface is to
hide users from the use of the visitor pattern. This is the interface
to expose to users (which is why `_visit` is used for the visitor
method, rather than `visit`).

Available back-ends
===================

Currently, there are two back-ends capable of generating Kernel
code (a KernelSchedule with all its children), these are:

- `FortranWriter()` in `psyclone.psyir.backend.fortran`
- `OpenCLWriter()` in `psyclone.psyir.backend.opencl`

Additionally, there are two partially-implemented back-ends

- `psyclone.psyir.backend.c` which is currently limited to processing
  partial PSyIR expressions.
- `SIRWriter()` in `psyclone.psyir.backend.sir` which can generate
  valid SIR from simple Fortran code conforming to the NEMO API.

SIR back-end
============

The SIR back-end is limited in a number of ways:

- only Fortran code containing 3 dimensional directly addressed
  arrays, with simple stencil accesses, iterated with triply nested
  loops is supported. Imperfectly nested loops, doubly nested loops,
  etc will cause a ``VisitorError`` exception.
- anything other than real arrays (integer, logical etc.) will cause
  incorrect SIR code to be produced (see issue #468).
- calls are not supported (and will cause a VisitorError exception).
- loop bounds are not analysed so it is not possible to add in offset
  and loop ordering for the vertical. This also means that the ordering
  of loops (lat/lon/levels) is currently assumed.
- Fortran literals such as `0.0d0` are output directly in the
  generated code (but this could also be a frontend issue).
- the only unary operator currently supported is '-' and the subject
  of this unary operator must be a literal.

The current implementation also outputs text rather than running Dawn
directly. This text needs to be pasted into another script in order to
run Dawn, see :ref:`user_guide:nemo-eg4-sir` the NEMO API example 4.

Currently there is no way to tell PSyclone to output SIR. Outputting
SIR is achieved by writing a script which creates an SIRWriter and
outputs the SIR (for kernels) from the PSyIR. Whilst the main
'psyclone' program could have a '-backend' option added it is not
clear this would be useful here as it is expected that the SIR will be
output only for certain parts of the PSyIR and (an)other back-end(s)
used for the rest. It is not yet clear how best to do this - perhaps
mark regions using a transformation.

It is unlikely that the SIR will be able to accept full NEMO code due
to its complexities (hence the comment about using different
back-ends in the previous paragraph). Therefore the approach that will
be taken is to use PSyclone to transform NEMO to make regions that
conform to the SIR constraints and to make these as large as
possible. Once this is done then PSyclone will be used to generate and
optimise the code that the SIR is not able to optimise and will let
the SIR generate code for the bits that it is able to do. This
approach seems a robust one but would require interface code between
the Dawn generated cuda (or other) code and the PSyclone generated
Fortran. In theory PSyclone could translate the remaining code to C
but this would require no codeblocks in the PSyIR when parsing NEMO
(which is a difficult thing to achieve), or interface code between
codeblocks and the rest of the PSyIR.

As suggested by the Dawn developers, PSyIR local scalar variables are
translated into temporary SIR fields (which are 3D arrays by
default). The reason for doing this is that it is easy to specify
variables in the SIR this way (whereas I did not manage to get scalar
declarations working) and Dawn optimises a temporary field, reducing
it to its required dimensionality (so PSyIR local scalar variables are
output as scalars by the Dawn back end even though they are specified
as fields). A limitation of the current translation from PSyIR to SIR
is that all PSyIR scalars are assumed to be local and all PSyIR arrays
are assumed to be global, which may not be the case. This limitation
is captured in issue #521.

Parsing Code
############

The PSyclone `parse` module is responsible for parsing science
(algorithm and kernel) code and extracting the required information
for the algorithm translation and PSy generation phases.

The `parse` module contains modules for parsing algorithm
(`algorithm.py`) and kernel (`kernel.py`) code as well as a utility
module (`utils.py`) for common functionality.

Parsing Algorithm Code
======================

The first thing PSyclone typically does is parse an input file. This
input file is expected to contain Fortran source code conforming to
the particular API in question. For the `nemo` API this is standard
code, for the other API's this is algorithm code (conforming to the
PSyKAl separation of concerns). The PSyclone code to do this is found
in `parse/algorithm.py`.

An input file can be parsed via the `parse` function or via an
instance of the `Parser` class. In practice the `parse` function
simply calls the `Parser` class so we will concentrate on the latter
in this section. The `parse` function could be removed from PSyclone
but it is simple and is used in existing PSyclone scripts and
examples.

The `Parser` class is initialised with a number of optional
arguments. A particular `api` can be specified (this is required so
the parser knows what sort of code and metadata to expect, how to
parse it and which `builtins` are supported). The name used to specify
an invoke (defaulting to `invoke`) can be changed, a path to where to
look for associated kernel files can be provided and a particular
maximum line length can be specified.

.. autoclass:: psyclone.parse.algorithm.Parser

Once an instance of the `Parser()` class is created and configured
with required values for the optional arguments, then the parse method
can be called. This takes the name of the input code as its argument
and returns a parse tree of the input code and a FileInfo object that
captures the required invoke information found in the input code and
in the associated kernel codes.

If the `nemo` API is specified then the `parse` method of the `Parser`
instance simply parses the code with `fparser2` and returns the
resultant `fparser2` parse tree.

For all other APIs the `parse` method of the `Parser` instance returns
the resultant `fparser2` parse tree and a `FileInfo` instance which
captures the invoke and kernel metadata in a hierarchy of classes.

When the `Parser` instance parses the code it expects to find Fortran
code containing a program, module, subroutine or function (and it
aborts if not). Currently the first of these (there may be more than
one subroutine for example) is assumed to be the one that is
required. This limititation is captured in issue #307.

The native `fparser2` tree of the Fortran code is then walked and all
use statement names are captured and stored in a map (called
`_arg_name_to_module_name`). This map allows the module name to be
found given a particular argument name. Also, if an `invoke` call is
found then an `InvokeCall` object is created and added to a list of
such instances. Once all invokes have been found the `FileInfo`
instance is created.

.. note:: In the future we want to be able to simply replace one parse
          tree with another. So how should we do this? One option
          would be to try to minimise the parser-specific parts and
          create some form of interface. The question is really what
          level of interface we should use.

An `InvokeCall` object is created in the `create_invoke_call` method
by first parsing each of the kernels specified in an invoke call and
creating a list of `kernel` objects which are then used to create
an `InvokeCall`. These objects capture information on the way each
kernel is being called from the Algorithm layer.

A `kernel` object is created in the `create_kernel_call` method which
extracts the kernel name and kernel arguments, then creates either an
`algorithm.BuiltInCall` instance (via the `create_builtin_kernel_call`
method) or an `algorithm.KernelCall` instance (via the
`create_coded_kernel_call` method). `BuiltInCalls` are created if the
kernel name is the same as one of those specified in the builtin names
for this particular API (see the variable `_builtin_name_map` which is
initialised by the `get_builtin_defs` function).

The `create_kernel_call` method uses the `get_kernel` function to find
out the kernel name and create a list of `Arg` instances representing
the arguments. The `get_kernel` function parses each kernel argument
using the fparser2 AST and determines the required argument
information. An advantage of `fparser2` when compared with `fparser1`
is that it parses all of a code, so we can use the parse tree to
determine the type of each kernel argument appearing in the `invoke`
call (e.g. scalar variable, array reference, literal constant) and
create the appropriate `Arg` instance. Previously we relied on the
`expression` module to do this (which has limitations).

.. note:: the analysis in the `get_kernel` function is the place to
          extend if we were to support arithmetic operations in an
          invoke call.

Parsing Kernel Code (Metadata)
==============================

An `algorithm.BuiltInCall` instance is created by being passed a
`kernel.BuiltinKernelType` instance for the particular API via the
`BuiltInKernelTypeFactory` class which is found in the `parse.kernels`
module. This class parses the Fortran module file which specifies
builtin description metadata. Currently `fparser1` is used but we will
be migrating to `fparser2` in the future. The builtin metadata is
specified in the same form as coded kernel metadata so the same logic
can be used (i.e. the `KernelTypeFactory.create` method is called)
which is why `BuiltInKernelTypeFactory` subclasses
`KernelTypeFactory`.

An `algorithm.KernelCall` instance is created by being passed the
module name of the kernel and a `kernel.KernelType` instance for the
particular API via the `KernelTypeFactory` class which is also found
in the `parse.kernel` module. This class is given the parsed kernel
module (via the `get_kernel_ast` function - which searches for the
kernel file using the kernel path information introduced
earlier). Again, currently `fparser1` is used but we will be migrating
to `fparser2` in the future.

The `KernelTypeFactory create` method is used for both coded kernels
and builtin kernels to specify the API-specific class to use. As an
example, in the case of the `dynamo0.3` API, the class is
`DynKernMetadata` which is found in `psyclone.dynamo0p3`. Once this
instance has been created (by passing it an `fparser1` parse tree) it can
return information about the metadata contained therein. Moving from
`fparser1` to `fparser2` would required changing the parse code logic
in each of the API-specific classes.


Generic Code
############

PSyclone is designed to be configurable so that new front-ends (called
APIs) can be built, re-using as much existing code as possible. The
generic code is kept in the `psyGen.py` file for psy-code generation.

New APIs
########

TBD

.. Generating API-specific code
.. ============================
.. 
.. This section explains how to create a new API in PSyclone. PSyclone
.. currently supports the following APIs; dynamo versions 0.1 and 0.3
.. and gocean versions 0.1 and 1.0.
.. 
.. config.py
.. ---------
.. 
.. The names of the supported APIs and the default API are specified in
.. `configuration.py`. When adding a new API you must add the name you would like
.. to use to the ``_supported_api_list`` (and change the ``_default_api`` if
.. required).
.. 
.. parse.py
.. --------
.. 
.. The parser reads the algorithm code and associated kernel metadata.
.. 
.. The parser currently assumes that all APIs will use the `invoke()`
.. API for the algorithm-to-psy layer but that the content and structure
.. of the metadata in the kernel code may differ. If the algorithm API
.. differs, then the parser will need to be refactored. This is beyond
.. the scope of this document and is currently not considered in the
.. PSyclone software architecture.
.. 
.. The kernel metadata however, will be different from one API to
.. another. To parse this kernel-API-specific metadata a
.. `KernelTypeFactory` is provided which should return the appropriate
.. `KernelType` object. When adding a new API a new API-specific subclass
.. of `KernelType` should be created and added to the `create()` method
.. in the `KernelTypeFactory` class. If the kernel metadata happens to be
.. the same as another existing API then the existing `KernelType`
.. subclass can be used for the new API.
.. 
.. The `KernelType` subclass needs to specialise the class constructor
.. and initialise the `KernelType` base class with the
.. supplied arguments. The role of the `KernelType` subclass is to create
.. a kernel-metadata-specific subclass of the `Descriptor` class and
.. populate this with the relevant API-specific metadata. After doing
.. this is appends the kernel-metadata-specific subclass instance is
.. appended to the `_arg_descriptors` list provided by the `KernelType`
.. base class.
.. 
.. TBC
.. 
.. This information
.. 
.. KernelType base class assumes kernel metadata stored as a type. Searches for that type.
.. Checks whether the metadata is public (it should be ?)
.. Assumes iterates_over variable.
.. Binding to a procedure - assumes one of two styles.
.. Assumes a meta_args type
.. *What about our func_args type???*
.. 
.. type x
.. meta_args=
.. *meta_func=*
.. iterates_over=
.. code => or code =
.. end type x
.. 
.. The descriptor class ...
.. 
.. psyGen.py
.. ---------
.. 
.. factory
.. +++++++
.. 
.. A new file needs to be created and the following classes found in
.. psyGen.py need to be subclassed.
.. 
.. PSy, Invokes, Invoke, InvokeSchedule, Loop, Kern, Arguments, Argument
.. You may also choose to subclass the Inf class if required.
.. 
.. The subclass of the PSy class then needs to be added as an option to
.. the create method in the PSyFactory class.
.. 
.. Initialisation
.. ++++++++++++++
.. 
.. The parser information passed to the PSy layer is used to create an
.. invokes object which in turn creates a list of invoke objects. Each
.. invoke object contains an InvokeSchedule which consists of loops and
.. calls. Finally, a call contains an arguments object which itself
.. contains a list of argument objects.
.. 
.. To make sure the subclass versions of the above objects are created
.. the __init__ methods of the subclasses must make sure they create
.. the appropriate objects.
.. 
.. Some of the baseclass constructors (__init__ methods) support the
.. classname being provided. This allow them to instantiate the
.. appropriate objects without knowing what they are.
.. 
.. gen_code()
.. ++++++++++
.. 
.. All of the above classes (with the exception of PSy which supports a
.. gen() method) have the gen_code() method. This method passes the
.. parent of the generation tree and expect the object to add the code
.. associated with the object as a child of the parent. The object is
.. then expected to call any children. This approach is powerful as it
.. lets each object concentrate on the code that it is responsible for.
.. 
.. Adding code in gen_code()
.. +++++++++++++++++++++++++
.. 
.. The f2pygen classes have been developed to help create appropriate
.. fortran code in the gen_code() method.
.. 
.. When writing a gen_code() method for a particular object and API it is
.. natural to add code as a child of the parent provided by the callee of
.. the method. However, in some cases we do not want code to appear at
.. the current position in the hierarchy.
.. 
.. The add() method
.. ++++++++++++++++
.. 
.. PSyclone supports this via the add() method
.. 
.. explicitly place at the appropriate place in the hierarchy. For example,
.. parent.parent.add(...)
.. 
.. optional argument. default is auto. This attempts to place code in the
.. expected place. For example, specify a declaration. auto finds a
.. correct place to put this code.
.. 
.. Specify position explicitly
.. "before", "after", "first", "last"
.. 
.. Sometimes don't know exactly where to place. On example that is
.. supported is when you want to add something before or after a loop
.. nest. start_parent_loop(). This method recurses up until the parent is
.. not a loop, it then skips any comments (as they may be directives) and
.. return this position. Therefore supports an arbitrary number of loops
.. and directives.

Existing APIs
#############

.. _dynamo0.3-developers:

Dynamo0.3
=========

Mesh
----

The Dynamo0.3 API supports meshes that are unstructured in the
horizontal and structured in the vertical. This is often thought of as
a horizontal 2D unstructured mesh which is extruded into the
vertical. The LFRic infrastructure represents this mesh as a list of
2D cells with a scalar value capturing the number of levels in the
vertical "column".

Cells
-----

The Dynamo0.3 API currently assumes that all kernels which support
iterating over cells work internally on a column of cells. This means
that PSyclone need only be concerned with iterating over cell-columns
in the horizontal. As a result the LFRic infrastructure presents the
mesh information to PSyclone as if the mesh were 2-dimensional. From
now on this 2D view will be assumed i.e. a cell will actually be a
column of cells. The LFRic infrastracture provides a global 2D cell
index from 1 to the number of cells.

For example, a simple quadrilateral element mesh with 4 cells might be
indexed in the following way.

.. image:: cells_global.png
	   :width: 120

When the distributed memory option is switched on in the Dynamo0.3 API
(see the :ref:`distributed_memory` Section) the cells in the model are
partitioned amongst processors and halo cells are added at the
boundaries to a depth determined by the LFRic infrastructure. In this
case the LFRic infrastructure maintains the global cell index and
adds a unique local cell index from 1 to the number of cells in each
partition, including any halo cells.

An example for a depth-1 halo implementation with the earlier mesh
split into 2 partitions is given below, with the halo cells being
coloured red. An example local indexing scheme is also provided below
the cells. Notice the local indexing scheme is set up such that owned
cells have lower indices than halo cells.

.. image:: cells_distributed.png
	   :width: 200

Dofs
----

In the LFRic infrastracture the degrees-of-freedom (dofs) are indexed
from 1 to the total number of dofs. The infrastructure also indexes
dofs so that the values in a column are contiguous and their values
increase in the vertical. Thus, given the dof indices for the "bottom"
cell, the rest of the dof indices can be determined for the
column. This set of dof indices for the bottom cell is called a
dofmap.

Dofs represent a field's values at various locations in the
mesh. Fields can either be continuous or discontinuous. Continuous
fields are so named because their values are continuous across cell
boundaries. Dofs that represent continuous fields are shared between
neighbouring cells. Discontinuous fields have values that are not
necessarily related between neighbouring cells (there can be
discontinuities across cell boundaries). Dofs that represent
discontinuous fields are local to a cell.

Discontinuous Dofs
------------------

A simple example of discontinuous dofs is given below. In this case
each cell contains 1 dof and there are 10 cells in a column. We only
show the bottom cells and their corresponding dof indices. As
explained earlier, the dof indices increase contiguously up the
column, so the cell above the cell containing dof index 1 contains dof
index 2 and the cell above that contains dof index 3 etc.

.. image:: dofs_disc_global.png
	   :width: 120

As discussed in the previous section, when the distributed memory
option is switched on in the Dynamo0.3 API (see the
:ref:`distributed_memory` Section) the cells in the model are
partitioned amongst processors and halo cells are added at the
boundaries to a depth determined by the LFRic infrastructure. This
results in the dofs being replicated in the halo cells, leading to a
dof halo. As for cells, the LFRic infrastructure maintains the global
dof indexing scheme and adds a local dof indexing scheme from 1 to the
number of dofs in each partition, including any halo dofs.

An example for a depth-1 halo implementation with the earlier mesh
split into 2 partitions is given below, with the halo cells drawn in
grey and halo dofs coloured red. An example local partition indexing
scheme is also provided below the dofs. As with cells, notice the
local indexing scheme ensures that owned dofs have lower indices than
halo dofs.

.. image:: dofs_disc_distributed.png
	   :width: 200

Continuous Dofs
---------------

A simple continuous dof example is given below for the same mesh as
before. In this case dofs are on cell edges in the horizontal and
there are 10 cells in a column. Again we only show the bottom cells
and their corresponding dof indices. As explained earlier, the dof
indices increase contiguously up the column, so the cell above the
cell containing dof index 1 contains dof index 2 and the cell above
that contains dof index 3 etc.

.. image:: dofs_cont_global.png
	   :width: 140

As already explained, when the distributed memory option is switched
on in the Dynamo0.3 API (see the :ref:`distributed_memory` Section)
the cells in the model are partitioned amongst processors and halo
cells are added at the boundaries.

In the example below we ignore the additional halo cells and just look
at the partitioning of cells amongst processors (with the same mesh
and 2 partitions as shown earlier). It can be seen that the dofs
shared between cells which are on different partitions now need to be
replicated if fields on continuous dofs are going to be able to be
computed locally on each partition. This concept is different to halos
as there are no halo cells here, the fact that the cells are
partitioned has meant that continuous dofs on the edge of the
partition are replicated. The convention used in Dynamo0.3 is that the
cell with the lowest global id determines which partition owns a
dof and which has the copy. Dofs which are copies are called
`annexed`. Annexed dofs are coloured blue in the example:

.. image:: dofs_cont_annexed.png
	   :width: 160

If we now extend the above example to include the halo cells (coloured
grey) then we get:

.. image:: dofs_cont_halos.png
	   :width: 230
		   
An example for a depth-1 halo implementation with the earlier mesh
split into 2 partitions is given below, with the halo cells drawn in
grey and halo dofs coloured red. An example local indexing scheme is
also provided below the dofs. Notice the local indexing scheme ensures
that owned dofs have lower indices than annexed dofs, which in turn
have lower indices than halo dofs.


Cell and Dof Ordering
---------------------

Cells in a partition are sequentially indexed by the LFRic
infrastructure, starting at 1, so that local cells occur first, then
level-1 halo cells, then level-2 halo cells etc. A benefit of this
layout is that it makes it easy for PSyclone to specify the required
iteration space for cells as a single range, allowing a single Fortran
do loop (or other language construct as required) to be generated. The
LFRic infrastructure provides an API that returns the index of the
last owned cell, the index of the last halo cell at a particular depth
and the index of the last halo cell, to support PSyclone code
generation.

Dofs on a partition are also sequentially indexed by the LFRic
infrastructure, starting at 1, so that local dofs occur first, then
annexed dofs (if the field is continuous), then level-1 halo dofs,
then level-2 halo dofs etc. Again, this layout makes it easy for
PSyclone to specify the required iteration space for dofs as a single
range. As before, the LFRic infrastructure provides an API that
returns the index of the last owned dof, the index of the last annexed
dof, the index of the last halo dof at a particular depth and the
index of the last halo dof, to support PSyclone code generation.

.. _multigrid:

Multi-grid
----------

The Dynamo 0.3 API supports kernels that map fields between meshes of
different horizontal resolutions; these are termed "inter-grid"
kernels. As indicated in :numref:`fig-multigrid` below, the change in
resolution between each level is always a factor of two in both the
``x`` and ``y`` dimensions.

.. _fig-multigrid:

.. figure:: multigrid.png
	   :width: 600
	   :align: center

	   The arrangement of cells in the multi-grid hierarchy used
	   by LFRic. (Courtesy of R. Wong, Met Office.)

Inter-grid kernels are only permitted to deal with fields on two,
neighbouring levels of the mesh hierarchy. In the context of a single
inter-grid kernel we term the coarser of these meshes the "coarse"
mesh and the other the "fine" mesh.

There are two types of inter-grid operation; the first is
"prolongation" where a field on a coarse mesh is mapped onto a fine
mesh. The second is "restriction" where a field on a fine mesh is
mapped onto a coarse mesh.  Given the factor of two difference in
resolution between the fine and coarse meshes, the depth of any halo
accesses for the field on the fine mesh must automatically be double
that of those on the coarse mesh.

Loop iterators
--------------

In the current implementation of the Dynamo0.3 API it is possible to
iterate (loop) either over cells or dofs. At the moment all coded
kernels are written to iterate over cells and all builtin kernels are
written to iterate over dofs, but that does not have to be the case.

The loop iteration information is specified in the kernel metadata. In
the case of builtin's there is kernel metadata but it is part of
PSyclone and is specified in
`src/psyclone/dynamo0p3_builtins_mod.f90`.

For inter-grid kernels, it is the coarse mesh that provides the iteration
space. (The kernel is passed a list of the cells in the fine mesh that are
associated with the current coarse cell.)

Cell iterators: Continuous
--------------------------

When a kernel is written to iterate over cells and modify a continuous
field, PSyclone always computes dofs on owned cells and redundantly
computes dofs in the level-1 halo (or to depth 2 if the field is on
the fine mesh of an inter-grid kernel - see :ref:`multigrid`). Users
can apply a redundant computation transformation to increase the halo
depth for additional redundant computation but it must always at least
compute the level-1 halo. The reason for this is to ensure that the
shared dofs on cells on the edge of the partition (both owned and
annexed) are always correctly computed. Note that the outermost halo
dofs are not correctly computed and therefore the outermost halo of
the modified field is dirty after redundant computation. Also note
that if we do not know whether a modified field is discontinuous or
continuous then we must assume it is continuous.

An alternative solution could have been adopted in Dynamo0.3 whereby
no redundant computation is performed and partial-sum results are
shared between processors in a communication pattern similar to halo
exchanges. However, a decision was made to always perform redundant
computation.

A downside of performing redundant computation in the level-1 halo is
that any fields being read by the kernel must have their level-1 halo
clean (up-to-date), which can result in halo exchanges. Note that this
is not the case for the modified field, it does not need its halo to
be clean.

Cell iterators: Discontinuous
-----------------------------

When a kernel is written to iterate over cells and modify a
discontinuous field, PSyclone only needs to compute dofs on owned
cells. Users can apply a redundant computation transformation to
redundantly compute into the halo but this is not done by default.

.. _annexed_dofs:

Dof iterators
-------------

When a kernel that is written to iterate over dofs modifies a field,
PSyclone must ensure that all dofs in that field are updated. If the
distributed memory flag is set to ``false`` then PSyclone must iterate
over all dofs. PSyclone simply needs to create a loop that iterates
from 1 to the total number of dofs. The latter value is provided by
the LFRic API.

If the distributed memory flag is set to ``true`` then PSyclone must
ensure that each partition only iterates over owned dofs. Again PSyclone
just needs to create a loop that iterates from 1 to the total number
of owned dofs on that partition. The latter value is provided by the
LFRic API.

When the distributed memory flag is set to ``true`` an aditional
configuration option can be set which makes PSyclone always create
loops which iterate over both owned and annexed dofs. Whilst this is
not necessary for correctness, it can improve performance by reducing
the number of halo exchanges required (at the expense of computing
annexed dofs redundantly). The only change for PSyclone is that it
calls a different LFRic routine which returns the index of the last
annexed dof. This iteration space will necessarily also include all
owned dofs due to the ordering of dof indices discussed earlier.

The configuration variable is called `COMPUTE_ANNEXED_DOFS` and is
found in the `dynamo0.3` section of the `psyclone.cfg`
configuration file (see :ref:`configuration`). If it is ``true`` then
annexed dofs are always computed in loops that iterate over dofs and
if it is ``false`` then annexed dofs are not computed. The default in
PSyclone is ``false``.

The computation of annexed dofs could have been added as a
transformation optimisation. The reason for using a configuration
switch is that it is then guaranteed that annexed dofs are always
computed for loops that iterate over dofs which then allows us to
always remove certain halo exchanges without needing to add any new
ones.

If we first take the situation where annexed dofs are not computed for
loops that iterate over dofs i.e. (`COMPUTE_ANNEXED_DOFS` is ``false``),
then a field's annexed dofs will be dirty (out-of-date) after the loop
has completed. If a following kernel needs to read the field's
annexed dofs, then PSyclone will need to add a halo exchange to make
them clean.

There are 4 cases to consider:

1) the field is read in a loop that iterates over dofs,
2) the field is read in a loop that iterates over owned cells and
   level-1 halo cells,
3) the field is incremented in a loop that iterates over owned cells and
   level-1 halo cells, and
4) the field is read in a loop that iterates over owned cells

In case 1) the annexed dofs will not be read as the loop only iterates
over owned dofs so a halo exchange is not required. In case 2) the
full level-1 halo will be read (including annexed dofs) so a halo
exchange is required. In case 3) the annexed dofs will be updated so a
halo exchange is required. In case 4) the annexed dofs will be read so
a halo exchange will be required.

If we now take the case where annexed dofs are computed for loops that
iterate over dofs (`COMPUTE_ANNEXED_DOFS` is ``true``) then a field's
annexed dofs will be clean after the loop has completed. If a
following kernel needs to read the field's annexed dofs, then
PSyclone will no longer need a halo exchange.

We can now guarantee that annexed dofs will always be clean after a
continuous field has been modified by a kernel. This is because loops
that iterate over either dofs or cells now compute annexed dofs and
there are no other ways for a continuous field to be updated.

We now consider the same four cases. In case 1) the annexed dofs will
now be read, but annexed dofs are guaranteed to be clean, so no halo
exchange is required. In case 2) the full level-1 halo is read so a
halo exchange is still required. Note, as part of this halo exchange
we will update annexed dofs that are already clean. In case 3) the
annexed dofs will be updated but a halo exchange is not required as
the annexed dofs are guaranteed to be clean. In case 4) the annexed
dofs will be read but a halo exchange is not required as the annexed
dofs are guaranteed to be clean.

Furthermore, in the 3rd and 4th cases (in which annexed dofs are read
or updated but the rest of the halo does not have to be clean), where
the previous writer is unknown (as it comes from a different invoke
call) we need to add a speculative halo exchange (one that makes use of
the runtime clean and dirty flags) when `COMPUTE_ANNEXED_DOFS` is
`False`, as the previous writer *may* have iterated over dofs, leaving
the annexed dofs dirty. In contrast, when `COMPUTE_ANNEXED_DOFS` is
`True`, we do not require a speculative halo exchange as we know that
annexed dofs are always clean.

Therefore no additional halo exchanges are required when
`COMPUTE_ANNEXED_DOFS` is changed from ``false`` to ``true`` i.e. case 1)
does not require a halo exchange in either situation and case 2)
requires a halo exchange in both situations. We also remove halo
exchanges for cases 3) and 4) so the number of halo exchanges may be
reduced.

If a switch were not used and it were possible to use a transformation
to selectively perform computation over annexed dofs for loops that
iterate over dofs, then we would no longer be able to guarantee that
annexed dofs would always be clean. In this situation, if the dofs
were known to be dirty then PSyclone would need to add a halo exchange
and if it were unknown whether the dofs were dirty or not, then a halo
exchange would need to be added that uses the run-time flags to
determine whether a halo exchange is required. As run-time flags are
based on whether the halo is dirty or not (not annexed dofs) then a
halo exchange would be performed if the halo were dirty, even if the
annexed dofs were clean, potentially resulting in more halo exchanges
than are necessary.


Halo Exchange Logic
-------------------

Halo exchanges are required when the `DISTRIBUTED_MEMORY` flag is set to
``true`` in order to make sure any accesses to a field's halo or to its
annexed dofs receive the correct value.

Operators and Halo Exchanges
++++++++++++++++++++++++++++

Halo Exchanges are only created for fields. This causes an issue for
operators. If a loop iterates over halos to a given depth and the loop
includes a kernel that reads from an operator then the operator must
have valid values in the halos to that depth. In the current
implementation of PSyclone all loops which write to, or update an
operator are computed redundantly in the halo up to depth-1 (see the
`load()` method in the `DynLoop` class). This implementation therefore
requires a check that any loop which includes a kernel that reads from
an operator is limited to iterating in the halo up to
depth-1. PSyclone will raise an exception if an optimisation attempts
to increase the iteration space beyond this (see the `gen_code()`
method in the `DynKern` class).

To alleviate the above restriction one could add a configurable depth with
which to compute operators e.g. operators are always computed up to
depth-2, or perhaps up to the maximum halo depth. An alternative would
be to halo exchange operators as required in the same way that halo
exchanges are used for fields.

First Creation
++++++++++++++

When first run, PSyclone creates a separate InvokeSchedule for each of the
invokes found in the algorithm layer. This schedule includes all required
loops and kernel calls that need to be generated in the PSy layer for
the particular invoke call. Once the loops and kernel calls have been
created then (if the `DISTRIBUTED_MEMORY` flag is set to ``true``) PSyclone
adds any required halo exchanges and global sums. This work is all
performed in the `DynInvoke` constructor (`__init__`) method.

In PSyclone we apply a lazy halo exchange approach (as opposed to an
eager one), adding a halo exchange just before it is required.

It is simple to determine where halo exchanges should be added for the
initial schedule. There are three cases:

1) loops that iterate over cells and modify a continuous field will
   access the level-1 halo. This means that any field that is read
   within such a loop must have its level-1 halo clean (up-to-date)
   and therefore requires a halo exchange. A modified field (specified
   as `GH_INC` which involves a read before a write) will require a
   halo exchange if its annexed dofs are not clean, or if their
   status is unknown. Whilst it is only the annexed dofs that need to
   be made clean in this case, the only way to acheive this is
   via a halo exchange (which updates the halo i.e. more than is
   required). Note, if the `COMPUTE_ANNEXED_DOFS` configuration
   variable is set to ``true`` then no halo exchange is required as
   annexed dofs will always be clean.

2) continuous fields that are read from within loops that iterate over
   cells and modify a discontinuous field will access their annexed
   dofs. If the annexed dofs are known to be dirty (because the
   previous modification of the field is known to be from within a
   loop over dofs) or their status is unknown (because the previous
   modification to the field is outside of the current invoke) then a
   halo exchange will be required (As already mentioned, currently the
   only way to make annexed dofs clean is to perform a halo
   swap. Note, if the `COMPUTE_ANNEXED_DOFS` configuration variable is
   set to ``true`` then no halo exchange is required as annexed dofs
   will always be clean.

3) fields that have a stencil access will access the halo and need
   halo exchange calls added.

Halo exchanges are created separately (for fields with halo reads) for
each loop by calling the `create_halo_exchanges()` method within the
`DynLoop` class.

In the situation where a field's halo is read in more than one kernel
in different loops, we do not want to add too many halo exchanges -
one will be enough as long as it is placed correctly. To avoid this
problem we add halo exchange calls for loops in the order in which
they occur in the schedule. A halo exchange will be added before the
first loop for a field but the same field in the second loop will find
that there is a dependence on the previously inserted halo exchange so
no additional halo exchange will be added.

The algorithm for adding the necessary halo exchanges is as follows:
For each loop in the schedule, the `create_halo_exchanges()` method
iterates over each field that reads from its halo (determined by the
`unique_fields_with_halo_reads()` method in the `DynLoop` class).

For each field we then look for its previous dependencies (the
previous writer(s) to that field) using PSyclone's dependence
analysis. Three cases can occur: 1) there is no dependence, 2) there
are multiple dependencies and 3) there is one dependence.

1) If no previous dependence is found then we add a halo exchange call
   before the loop (using the internal helper method
   `_add_halo_exchange()`). If the field is a vector field then a halo
   exchange is added for each component. The internal helper method
   `_add_halo_exchange` itself uses the internal helper method
   `_add_halo_exchange_code()`. This method creates an instance of the
   `DynHaloExchange` class for the field in question and adds it to
   the schedule before the loop. You might notice that this method
   then checks that the halo exchange is actually required and removes
   it again if not. In our current situation the halo exchange will
   always be needed so this check is not required but in more complex
   situations after transformations have been applied to the schedule
   this may not be the case. We discuss this type of situation later.

2) If multiple previous dependencies are found then the field must be
   a vector field as this is the only case where this can occur. We
   then choose the closest one and treat it as a single previous
   dependency (see 3).

3) If a single previous dependency is found and it is a halo exchange
   then we do nothing, as it is already covered. This will only happen
   when more than one reader depends on a writer, as discussed
   earlier. If the dependence is not a halo exchange then we add one.

After completing the above we have all the halo exchanges required for
correct execution.

Note that we do not need to worry about halo depth or whether a halo
is definitely required, or whether it might be required, as this is
determined by the halo exchange itself at code generation time. The
reason for deferring this information is that it can change as
transformations are applied.

Asynchronous Halo Exchanges
+++++++++++++++++++++++++++

The Dynamo0p3AsynchronousHaloExchange transformation allows the
default synchronous halo exchange to be split into a halo exchange
start and a halo exhange end which are represented separately as nodes
in the schedule. These can then be moved in the schedule to allow
overlapping of communication and computation, as long as data
dependencies are honoured.

A halo exchange both reads and modifies a field so has a readwrite
access for dependence analysis purposes. An asynchronous halo exchange
start reads the field and an asynchronous halo exchange end writes to
the field. Therefore the obvious thing to do would be to have the
associated field set to read and write access respectively. However,
the way the halo exchange logic works means that it is simplest to set
the halo exchange end access to readwrite. The reason for this is that
the logic to determine whether a halo exchange is required
(`_required()`) needs information from all fields that read from the
halo after the halo exchange has been called (and therefore must be
treated as a write with following reads for dependence analysis) and
it needs information from all fields that write to the field before
the halo exchange has been called (and therefore must be treated as a
read with previous writes for dependence analysis). An alternative
would be to make the `_required()` method use the halo exchange start
for previous writes and the halo exchange end for following
reads. However, it was decided that this would be more complicated
than the solution chosen.

Both halo exchange start and halo exchange end inherit from halo
exchange. However, the halo exchange start and end are really two
parts of the same thing and need to have consistent properties
including after transformations have been performed. This is achieved by
having the halo exchange start find and use the methods from the halo
exchange end, rather than implement them independently. The actual
methods needed are `_compute_stencil_type()`,
`_compute_halo_depth()` and `_required()`. It is unclear how much
halo exhange start really benefits from inheriting from halo exchange
and this could probably be removed at the expense of returning
appropriate names for the dag, colourmap, declaration etc.

.. note:: The dependence analysis for halo exchanges for field vectors
   is currently over zealous. It does not allow halo exchanges for
   independent vector components to be moved past one another. For
   example, a halo exchange for vector component 2, if placed after a halo
   exchange for component 1 could not be moved before the halo exchange
   for component 1, even though the accesses are independent of each
   other. This is also the case for asynchronous halo exchanges. See
   issue #220.

Evaluators
----------

Evaluators consist of basis and/or differential basis functions for a
given function space, evaluated at the nodes of another, 'target',
function space. A kernel can request evaluators on multiple target
spaces through the use of the `gh_evaluator_targets` metadata entry.
Every evaluator used by that kernel will then be provided on all of the
target spaces.

When constructing a `DynKernMetadata` object from the parsed kernel
metadata, the list of target function-space names (as they appear in
the meta-data) is stored in `DynKernMetadata._eval_targets`. This
information is then used in the `DynKern._setup()` method which
populates `DynKern._eval_targets`. This is an `OrderedDict` which has
the (mangled) names of the target function spaces as keys and 2-tuples
consisting of `FunctionSpace` and `DynKernelArgument` objects as
values. The `DynKernelArgument` object provides the kernel argument
from which to extract the function space and the `FunctionSpace` object
holds full information on the target function space.

The `DynInvokeBasisFunctions` class is responsible for managing the
evaluators required by all of the kernels called from an Invoke.
`DynInvokeBasisFunctions._eval_targets` collects all of the unique target
function spaces from the `DynKern._eval_targets` of each kernel.

`DynInvokeBasisFunctions._basis_fns` is a list holding information on
each basis/differential basis function required by a kernel within the
invoke. Each entry in this list is a `dict` with keys:

============= =================================== ===================
Key           Entry                      	  Type
============= =================================== ===================
shape         Shape of the evaluator              `str`
type          Whether basis or differential basis `str`
fspace        Function space             	  `FunctionSpace`
arg           Associated kernel argument 	  `DynKernelArgument`
qr_var        Quadrature argument name   	  `str`
nodal_fspaces Target function spaces     	  list of `(FunctionSpace, DynKernelArgument)`
============= =================================== ===================

Modifying the Schedule
----------------------

Transformations modify the schedule. At the moment only one of these
transformations - the `Dynamo0p3RedundantComputationTrans` class in
`transformations.py` - affects halo exchanges. This transformation can
mean there is a requirement for new halo exchanges, it can mean
existing halo exchanges are no longer required and it can mean that
the properties of a halo exchange (e.g. depth) can change.

The redundant computation transformation is applied to a loop in a
schedule. When this is done the `update_halo_exchanges()` method for
that loop is called - see the `apply()` method in
`Dynamo0p3RedundantComputationTrans`.

The first thing that the `update_halo_exchanges()` method does is call
the `create_halo_exchanges()` method to add in any new halo exchanges
that are required before this loop, due to any fields that now have a
read access to their halo when they previously did not. For example, a
loop containing a kernel that writes to a certain field might
previously have iterated up to the number of owned cells in a
partition (`ncells`) but now iterates up to halo depth 1.

However, a field that has its halo read no longer guarantees that a
halo exchange is required, as the previous dependence may now compute
redundantly to halo depth 2, for example. The solution employed in
`create_halo_exchanges()` is to add a halo exchange speculatively and
then remove it if it is not required. The halo exchange itself
determines whether it is required or not via the `required()` method. The
removal code is found at the end of the `_add_halo_exchange_code()`
method in the `DynLoop()` class.

The second thing that the `update_halo_exchanges()` method does is check
that any halo exchanges after this loop are still required. It finds
all relevant halo exchanges, asks them if they are required and if
they are not it removes them.

We only need to consider adding halo exchanges before the loop and
removing halo exchanges after the loop. This is because redundant
computation can only increase the depth of halo to which a loop
computes so can not remove existing halo exchanges before a loop (as
an increase in depth will only increase the depth of an existing halo
exchange before the loop) or add existing halo exchanges after a loop
(as an increase in depth will only make it more likely that a halo
exchange is no longer required after the loop).

Kernel Transformations
++++++++++++++++++++++

Since PSyclone is invoked separately for each Algorithm file in an
application, the naming of the new, transformed kernels is done with
reference to the kernel output directory. All transformed kernels (and
the modules that contain them) are re-named following the PSyclone
Fortran naming conventions (:ref:`fortran_naming`). This enables the
reliable identification of transformed versions of any given kernel
within the output directory.

If the "multiple" kernel-renaming scheme is in use, PSyclone simply
appends an integer to the original kernel name, checks whether such a
kernel is present in the output directory and if not, creates it. If a
kernel with the generated name is present then the integer is
incremented and the process repeated. If the "single" kernel-renaming
scheme is in use, the same procedure is followed but if a matching
kernel is already present in the output directory then the new kernel
is not written (and we check that the contents of the existing kernel
are the same as the one we would create).

If an application is being built in parallel then it is possible that
different invocations of PSyclone will happen simultaneously and
therefore we must take care to avoid race conditions when querying the
filesystem. For this reason we use ``os.open``::
  
    fd = os.open(<filename>, os.O_CREAT | os.O_WRONLY | os.O_EXCL)

The ``os.O_CREATE`` and ``os.O_EXCL`` flags in combination mean that
``open()`` raises an error if the file in question already exists.

Colouring
+++++++++

If a loop contains one or more kernels that write to a field on a
continuous function space then it cannot be safely executed in
parallel on a shared-memory device. This is because fields on a
continuous function space share dofs between neighbouring cells. One
solution to this is to 'colour' the cells in a mesh so that all cells
of a given colour may be safely updated in parallel
(:numref:`fig-colouring`).

.. _fig-colouring:

.. figure:: lfric_colouring.png
	   :width: 300
	   :align: center

	   Example of the colouring of the horizontal cells used to
	   ensure the thread-safe update of shared dofs (black
	   circles).  (Courtesy of S. Mullerworth, Met Office.)
	   
The loop over colours must then be performed sequentially but the loop
over cells of a given colour may be done in parallel. A loop that
requires colouring may be transformed using the ``Dynamo0p3ColourTrans``
transformation.

Each mesh in the multi-grid hierarchy is coloured separately
(https://code.metoffice.gov.uk/trac/lfric/wiki/LFRicInfrastructure/MeshColouring)
and therefore we cannot assume any relationship between the colour
maps of meshes of differing resolution.

However, the iteration space for inter-grid kernels (that map a field
from one mesh to another) is always determined by the coarser of the
two meshes.  Consequently, it is always the colouring of this mesh
that must be used.  Due to the set-up of the mesh hierarchy (see
:numref:`fig-multigrid`), this guarantees that there will not be any
race conditions when updating shared quantities on either the fine or
coarse mesh.

GOcean1.0
=========

TBD

.. OpenMP Support
.. --------------
.. 
.. Loop directives are treated as first class entities in the psyGen
.. package. Therefore they can be added to psyGen's high level
.. representation of the fortran code structure in the same way as calls
.. and loops. Obviously it is only valid to add a loop directive outside
.. of a loop.
.. 
.. When adding a call inside a loop the placement of any additional calls
.. or declarations must be specified correctly to ensure that they are
.. placed at the correct location in the hierarchy. To avoid accidentally
.. splitting the loop directive from its loop the start_parent_loop()
.. method can be used. This is available as a method in all fortran
.. generation calls. *We could have placed it in psyGen instead of
.. f2pygen*.  This method returns the location at the top of any loop
.. hierarchy and before any comments immediately before the top level
.. loop.
.. 
.. The OpenMPLoopDirective object needs to know which variables are
.. shared and which are private. In the current implementation default
.. shared is used and private variables are listed. To determine the
.. objects private variables the OpenMP implementation uses its internal
.. xxx_get_private_list() method. This method first finds all loops
.. contained within the directive and adds each loops variable name as a
.. private variable. this method then finds all calls contained within
.. the directive and adds each calls list of private variables, returned
.. with the local_vars() method. Therefore the OpenMPLoopDirective object
.. relies on calls specifying which variables they require being local.
.. 
.. Next ...
.. 
.. Update transformation for colours
.. 
.. OpenMPLoop transformation in transformations.py. 
.. 
.. Create third transformtion which goes over all loops in a schedule and
.. applies the OpenMP loop transformation.

NEMO
====

Implicit Loops
--------------

When constructing the PSyIR of NEMO source code, PSyclone identifies loops
that are implied by the use of Fortran array notation. Such use of array
notation is encouraged in the NEMO Coding Conventions :cite:`nemo_code_conv`
and identifying these loops can be important when introducing, e.g. OpenMP.

However, not all uses of Fortran array notation in NEMO imply a
loop. For instance,
::

   ascalar = afunc(twodarray1(:,:))

means that the function ``afunc`` is passed the (whole of the)
``twodarray1`` and returns a scalar value. (The requirement for
explicit array shapes in the NEMO Coding Convention means that any
quantity without such a shape must therefore be a scalar.)

Alternatively, a statement that assigns to an array must imply a loop::

  twodarray2(:,:) = bfunc(twodarray1(:,:))

but it can only be converted into an explicit loop by PSyclone if the
function ``bfunc`` returns a scalar. Since PSyclone does not currently
attempt to fully resolve all symbols when parsing NEMO code, this
information is not available and therefore such statements are not
identified as loops (issue
https://github.com/stfc/PSyclone/issues/286). This may then mean that
opportunities for optimisation are missed.

Modules
#######

This section describes the functionality of the various Python modules
that make up PSyclone.

Module: f2pygen
===============

`f2pygen` provides functionality for generating Fortran code from
scratch and supports the addition of a use statement to an existing
parse tree.

Variable Declarations
---------------------

Three different classes are provided to support the creation of
variable declarations (for intrinsic, character and derived-type
variables). An example of their use might be:

>>> from psyclone.f2pygen import ModuleGen, SubroutineGen, DeclGen, \
CharDeclGen, TypeDeclGen
>>> module = ModuleGen(name="testmodule")
>>> sub = SubroutineGen(module, name="testsubroutine")
>>> module.add(sub)
>>> sub.add(DeclGen(sub, datatype="integer", entity_decls=["my_int"]))
>>> sub.add(CharDeclGen(sub, length="10", entity_decls=["my_char"]))
>>> sub.add(TypeDeclGen(sub, datatype="field_type", entity_decls=["ufld"]))
>>> gen = str(module.root)
>>> print(gen)
  MODULE testmodule
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE testsubroutine()
      TYPE(field_type) ufld
      CHARACTER(LEN=10) my_char
      INTEGER my_int
    END SUBROUTINE testsubroutine
  END MODULE testmodule

The full interface to each of these classes is detailed below:

.. autoclass:: psyclone.f2pygen.DeclGen
    :members:
    :noindex:

.. autoclass:: psyclone.f2pygen.CharDeclGen
    :members:
    :noindex:

.. autoclass:: psyclone.f2pygen.TypeDeclGen
    :members:
    :noindex:

Adding code
-----------

`f2pygen` supports the addition of use statements to an existing
`fparser1` parse tree:

.. autofunction:: psyclone.f2pygen.adduse


The PSyclone code where the `adduse` function was used has recently
been migrated from using `fparser1` to using `fparser2`. In
recognition of this change a new version of `adduse` has been
developed which adds use statements to an existing `fparser2` parse
tree. For the timebeing this new version is located in the same file
it is used - `alg_gen.py` - but will be migrated to `f2pygen` (or
equivalent) in the future:

.. autofunction:: psyclone.alg_gen.adduse


.. _dev_configuration:

Module: configuration
======================

PSyclone uses the Python ``ConfigParser`` class
(https://docs.python.org/3/library/configparser.html) for reading the
configuration file. This is managed by the ``psyclone.configuration``
module which provides a ``Config``
class. This class is a singleton, which can be (created and) accessed
using  ``Config.get()``. Only one such instance will ever exist:

.. autoclass:: psyclone.configuration.Config
    :members:

The ``Config`` class is responsible for finding the configuration file
(if no filename is passed to the constructor), parsing it and then storing
the various configuration options.
If PSyclone is started via ``pytest``, the environment variable
``PSYCLONE_CONFIG`` is set to ``<PSYCLONEHOME/config>``. This will
guarantee that all tests use the config file provided in the PSyclone
repository, and not a (potentially modified) user installed version.

The ``Config`` class also stores the list of supported
APIs (``Config._supported_api_list``) and the default API to use if none
is specified in either a config file or the command line
(``Config._default_api``). Additionally, it performs some basic
consistency checks on the values it obtains from the configuration file.

Since the PSyclone API to use can be read from the configuration
file, it is not possible to have API-specifc sub-classes of ``Config``
as we don't know which API is in use before we read the file. However, the
configuration file can contain API-specific settings. These are placed in
separate sections, named for the API to which they apply, e.g.::

  [dynamo0.3]
  COMPUTE_ANNEXED_DOFS = false

Having parsed and stored the options from the default section of the
configuration file, the ``Config`` constructor then creates a
dictionary using the list of supported APIs to provide the keys. The
configuration file is then checked for API-specific sections (again
using the API names from the default section) and, if any are found,
an API-specifc sub-class is created using the parsed entries from the
corresponding section. The resulting object is stored in the
dictionary under the appropriate key. The API-specific values may then
be accessed as, e.g.::

  Config.get().api_conf("dynamo0.3").compute_annexed_dofs

The API-specific sub-classes exist to provide validation/type-checking and
encapsulation for API-specific options. They do not sub-class ``Config``
directly but store a reference back to the ``Config`` object to which they
belong.

Module: transformations
=======================

As one might expect, the transformations module holds the various
transformation classes that may be used to modify the Schedule of an
Invoke and/or the kernels called from within it.

The base class for any transformation must be the class ``Transformation``:

.. autoclass:: psyclone.psyGen.Transformation
    :members:
    :private-members:

Those transformations that work on a region of code (e.g. enclosing
multiple kernel calls within an OpenMP region) must sub-class the
``RegionTrans`` class:

.. autoclass:: psyclone.transformations.RegionTrans
    :members:
    :private-members:
    :noindex:

Finally, those transformations that act on a Kernel must sub-class the
``KernelTrans`` class:

.. autoclass:: psyclone.transformations.KernelTrans
   :members:
   :private-members:
   :noindex:

In all cases, the `apply` method of any sub-class *must* ensure that
the `validate` method of the parent class is called.

Module: psyGen
==============

Provides the base classes for PSy-layer code generation.

Module: dynamo0p3
=================

Specialises various classes from the ``psyclone.psyGen`` module
in order to support the Dynamo 0.3 API.

When constructing the Fortran subroutine for either an Invoke or
Kernel stub (see :ref:`stub-generation`), there are various groups of
related quantities for which variables must be declared and
(for Invokes) initialised. Each of these groupings is managed by a distinct
sub-class of the ``DynCollection`` abstract class:

.. autoclass:: psyclone.dynamo0p3.DynCollection
   :members:
   :private-members:
   :noindex:

(A single base class is used for both Invokes and Kernel stubs since it
allows the code dealing with variable declarations to be shared.)
A concrete sub-class of ``DynCollection`` must provide an
implementation of the ``_invoke_declarations`` method. If the
quantities associated with the collection require initialisation
within the PSy layer then the ``initialise`` method must also be
implemented. If stub-generation is to be supported for kernels that
make use of the collection type then an implementation must also be
provided for ``_stub_declarations.``

Although instances of (sub-classes of) ``DynCollection`` handle all
declarations and initialisation, there remains the problem of
constructing the list of arguments for a kernel (or kernel stub). The
``psyclone.dynamo0p3.ArgOrdering`` base class provides support for
this:

.. autoclass:: psyclone.dynamo0p3.ArgOrdering
    :members:
    :private-members:
    :noindex:

This class is then sub-classed in order to support the generation of
argument lists when *calling* kernels (``KernCallArgList``) and when
*creating* kernel stubs (``KernStubArgList``).  ``KernCallArgList`` is
only used in ``DynKernelArguments.raw_arg_list()``.
``KernStubArgList`` is only used in ``DynKern.gen_stub()``. These
classes make use of ``DynCollection`` sub-classes in order
to ensure that argument naming is consistent.

Transformations
###############

Kernel Transformations
======================

PSyclone is able to perform kernel transformations. Currently it has
two ways to apply transformations: by directly manipulating the
language AST or by translating the language AST to PSyIR, applying the
transformation in the PSyIR and using one of the back-ends to generate
the resulting code.

For now, both methods only support the fparser2 AST for kernel code.
This AST is obtained by converting the fparser1 AST (stored
when the kernel code was originally parsed to process the meta-data)
back into a Fortran string and then parsing that with fparser2.
(Note that in future we intend to adopt fparser2 throughout PSyclone so that
this translation between ASTs will be unnecessary.)
The `ast` property of the `psyclone.psyGen.Kern` class is responsible
for performing this translation the first time it is called. It also
stores the resulting AST in `Kern._fp2_ast` for return by future calls.

See `psyclone.transformations.ACCRoutineTrans` for an example of directly
manipulating the fparser2 AST.

Alternatively, one can call the `psyclone.psyGen.CodedKern.get_kernel_schedule()`
to generate the PSyIR representation of the kernel code. 

.. automethod:: psyclone.psyGen.CodedKern.get_kernel_schedule

The language AST to PSyIR transformation is done using a PSyIR front-end.
This are found in the `psyclone.psyir.frontend` module. 
The only currently available front-end is `Fparser2Reader` but this can
be specialized for by the application APIs (e.g. Nemo has `NemoFparser2Reader`
sub-class).
The naming convention used for the PSyIR front-ends is
<API><languageAST>Reader.

.. autoclass:: psyclone.psyir.frontend.fparser2.Fparser2Reader
    :members:

The results of `psyclone.psyGen.Kern.get_kernel_schedule` is a
`psyclone.psyGen.KernelSchedule` which has the same functionality as
a PSyIR Schedule but with the addition of a Symbol Table
(see :ref:`kernel_schedule-label`).

OpenACC
=======

PSyclone is able to generate code for execution on a GPU through the
use of OpenACC. Support for generating OpenACC code is implemented via
:ref:`transformations`. The specification of parallel regions and
loops is very similar to that in OpenMP and does not require any
special treatment.  However, a key feature of GPUs is the fact that
they have their own, on-board memory which is separate from that of
the host. Managing (i.e. minimising) data movement between host and
GPU is then a very important part of obtaining good performance.

Since PSyclone operates at the level of Invokes, it has no information
about when an application starts and thus no single place in which to
initiate data transfers to a GPU. (We assume that the host is
responsible for model I/O and therefore for populating fields with
initial values.) Fortunately, OpenACC provides support for this kind of
situation with the ``enter data`` directive. This may be used to
"define scalars, arrays and subarrays to be allocated in the current
device memory for the remaining duration of the program"
:cite:`openacc_enterdata`. The ``ACCEnterDataTrans`` transformation adds
an ``enter data`` directive to an Invoke:

.. autoclass:: psyclone.transformations.ACCEnterDataTrans
   :noindex:

The resulting generated code will then contain an ``enter data``
directive.

Of course, a given field may already be on the device (and have been
updated) due to a previous Invoke. In this case, the fact that the
OpenACC run-time does not copy over the now out-dated host version of
the field is essential for correctness.

In order to support the incremental porting and/or debugging of an
application, PSyclone also supports the OpenACC ``data`` directive
that creates a statically-scoped data region. See the
description of the ``ACCDataTrans`` transformation in the
:ref:`sec_transformations_available` section for more details.

.. _opencl_dev:

OpenCL
======

PSyclone is able to generate an OpenCL :cite:`opencl` version of
PSy-layer code for the GOcean 1.0 API and its associated kernels.
Such code may then be executed
on devices such as GPUs and FPGAs (Field-Programmable Gate
Arrays). Since OpenCL code is very different to that which PSyclone
normally generates, its creation is handled by ``gen_ocl`` methods
instead of the normal ``gen_code``. Which of these to use is
determined by the value of the ``InvokeSchedule.opencl`` flag.  In turn,
this is set at a user level by the ``transformations.OCLTrans``
transformation.

The PSyKAl model of calling kernels for pre-determined iteration
spaces is a natural fit to OpenCL's concept of an
``NDRangeKernel``. However, the kernels themselves must be created or
loaded at runtime, their arguments explicitly set and any arrays
copied to the compute device. All of this 'boilerplate' code is
generated by PSyclone. In order to minimise the changes required, the
generated code is still Fortran and makes use of the FortCL library
(https://github.com/stfc/FortCL) to access OpenCL functionality. We
could of course generate the PSy layer in C instead but this would
require further extension of PSyclone.

Consider the following invoke::

    call invoke( compute_cu(CU_fld, p_fld, u_fld) )

When creating the OpenCL PSy layer for this invoke, PSyclone creates
three subroutines instead of the usual one. The first, ``psy_init``
is responsible for ensuring that a valid kernel object is created
for each kernel called by the invoke, e.g.::

    use fortcl, only: ocl_env_init, add_kernels
    ...
    ! Initialise the OpenCL environment/device
    CALL ocl_env_init
    ! The kernels this PSy layer module requires
    kernel_names(1) = "compute_cu_code"
    ! Create the OpenCL kernel objects. Expects to find all of the
    ! compiled kernels in PSYCLONE_KERNELS_FILE.
    CALL add_kernels(1, kernel_names)

As indicated in the comment, the ``FortCL::add_kernels`` routine
expects to find all kernels in a pre-compiled file pointed to by the
PSYCLONE_KERNELS_FILE environment variable. (A pre-compiled file is
used instead of run-time kernel compilation in order to support
execution on FPGAs.)

The second routine created by PSyclone sets the kernel arguments, e.g.::

    SUBROUTINE compute_cu_code_set_args(kernel_obj, nx, cu_fld, p_fld, u_fld)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      ...
      INTEGER(KIND=c_intptr_t), target :: cu_fld, p_fld, u_fld
      INTEGER(KIND=c_intptr_t), target :: kernel_obj
      INTEGER, target :: nx
      ! Set the arguments for the compute_cu_code OpenCL Kernel
      ierr = clSetKernelArg(kernel_obj, 0, C_SIZEOF(nx), C_LOC(nx))
      ierr = clSetKernelArg(kernel_obj, 1, C_SIZEOF(cu_fld), C_LOC(cu_fld))
      ...
    END SUBROUTINE compute_cu_code_set_args

The third routine generated is the ususal psy-layer routine that is
responsible for calling all of the kernels. However, it must now also
call ``psy_init``, create buffers on the compute device (if they are
not already present) and copy data over::

    SUBROUTINE invoke_compute_cu(...)
      ...
      IF (first_time) THEN
        first_time = .false.
        CALL psy_init
        num_cmd_queues = get_num_cmd_queues()
        cmd_queues => get_cmd_queues()
        kernel_compute_cu_code = get_kernel_by_name("compute_cu_code")
      END IF 
      globalsize = (/p_fld%grid%nx, p_fld%grid%ny/)
      ! Ensure field data is on device
      IF (.NOT. cu_fld%data_on_device) THEN
        size_in_bytes = int(p_fld%grid%nx*p_fld%grid%ny, 8)* &
                        c_sizeof(cu_fld%data(1,1))
        ! Create buffer on device
        cu_fld%device_ptr = create_rw_buffer(size_in_bytes)
        ierr = clEnqueueWriteBuffer(cmd_queues(1), cu_fld%device_ptr,  &
                                    CL_TRUE, 0_8, size_in_bytes,       &
      			            C_LOC(cu_fld%data), 0, C_NULL_PTR, &
      			            C_LOC(write_event))
        cu_fld%data_on_device = .true.
      END IF 
      ...

Note that we use the ``data_on_device`` member of the field derived
type (implemented in github.com/stfc/dl_esm_inf) to keep track of
whether a given field has been copied to the compute device.  Once all
of this setup is done, the kernel itself is launched by calling
``clEnqueueNDRangeKernel``::

    ierr = clEnqueueNDRangeKernel(cmd_queues(1), kernel_compute_cu_code, &
                                  2, C_NULL_PTR, C_LOC(globalsize),      &
				  C_NULL_PTR, 0, C_NULL_PTR, C_NULL_PTR)

Limitations
-----------

In OpenCL, all tasks to be performed (whether copying data or kernel
execution) are associated with a command queue. Tasks submitted to
different command queues may then be executed concurrently,
potentially giving greater performance. The OpenCL PSy code currently
generated by PSyclone makes use of just one command queue but again,
this could be extended in the future.

The current implementation only supports the conversion of a whole
Invoke to use OpenCL. In the future we may refine this functionality
so that it may be applied to just a subset of kernels within an
Invoke.

Since PSyclone knows nothing about the I/O performed by a model, the
task of ensuring that the correct data is written out by a model
(including when doing halo exchanges for distributed memory) is left
to the dl_esm_inf library since that has the information on whether
field data is local or on a remote compute device.

