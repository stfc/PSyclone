.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2024, Science and Technology Facilities Council
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
.. Written by I. Kavcic, Met Office
.. Modified by J. Henrichs, Bureau of Meteorology
.. Modified by R. W. Ford, STFC Daresbury Lab

.. highlight:: fortran

.. _psyke:

PSy Kernel Extractor (PSyKE)
============================

.. _psyke-intro:

Introduction
------------

PSyclone has the ability to define regions of a PSyclone-conformant code
to be extracted and run as a stand-alone application. This ability, called
PSyKE (PSy Kernel Extractor), can be useful for benchmarking parts of a
model, such as LFRic, without the need for using its infrastructure.

.. _psyke-intro-mechanism:

Mechanism
+++++++++

The code marked for extraction can be (subject to 
:ref:`psyke-intro-restrictions`):

* One or more Nodes in an Invoke (e.g. Loops containing Kernel or
  Built-In calls, a Directive enclosing one or more Loops) or

* The entire Invoke (extraction applied to all Nodes).

The basic mechanism of code extraction is through applying the
``ExtractTrans`` transformation to selected Nodes. This
transformation is further sub-classed into API-specific implementations,
``LFRicExtractTrans`` and ``GOceanExtractTrans``. Both
sub-classed transformations insert an instance of the ``ExtractNode``
object into the Schedule of a specific Invoke. All Nodes marked for
extraction become children of the ``ExtractNode``.

The ``ExtractNode`` class uses the dependency analysis to detect
which variables are input-, and which ones are output-parameters.
The lists of variables are then passed to the ``PSyDataNode``,
which is the base class of any ``ExtractNode`` (details of
the ``PSyDataNode`` can be found in :ref:`dev_guide:psy_data`). This
node then creates the actual code, as in the following LFRic example::

      ! ExtractStart
      !
      CALL extract_psy_data%PreStart("testkern_mod", "testkern_code", 4, 2)
      CALL extract_psy_data%PreDeclareVariable("a", a)
      CALL extract_psy_data%PreDeclareVariable("f2", f2)
      CALL extract_psy_data%PreDeclareVariable("m1", m1)
      CALL extract_psy_data%PreDeclareVariable("m2", m2)
      CALL extract_psy_data%PreDeclareVariable("map_w1", map_w1)
      ...
      CALL extract_psy_data%PreDeclareVariable("undf_w3", undf_w3)
      CALL extract_psy_data%PreDeclareVariable("f1_post", f1)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("a", a)
      CALL extract_psy_data%ProvideVariable("f2", f2)
      CALL extract_psy_data%ProvideVariable("m1", m1)
      CALL extract_psy_data%ProvideVariable("m2", m2)
      CALL extract_psy_data%ProvideVariable("map_w1", map_w1)
      ...
      CALL extract_psy_data%ProvideVariable("undf_w3", undf_w3)      
      CALL extract_psy_data%PreEnd
      DO cell=1,f1_proxy%vspace%get_ncell()
        !
        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data,  &
             m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1,           &
             map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, &
             undf_w3, map_w3(:,cell))
      END DO 
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("f1_post", f1)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd

The :ref:`PSyData API <psy_data>` relies on generic Fortran interfaces to
provide the  field-type-specific implementations of the ``ProvideVariable``
for different types. This means that a different version of the external
PSyData library that PSyKE uses must be supplied for each PSyclone API.

.. _psyke-intro-restrictions:

Restrictions
++++++++++++

Code extraction can be applied to unoptimised or optimised code. There are
restrictions that check for correctness of optimising transformations when
extraction is applied, as well as restrictions that eliminate dependence on
the specific model infrastructure.

.. _psyke-intro-restrictions-gen:

General
#######

This group of restrictions is enforced irrespective of whether optimisations
are used or not.

* Extraction can be applied to a single Node or a list of Nodes in a
  Schedule. For the latter, Nodes in the list must be consecutive children
  of the same parent Schedule.

* Extraction cannot be applied to an ``ExtractNode`` or a Node list that
  already contains one (otherwise we would have an extract region within
  another extract region).

* A Kernel or a Built-In call cannot be extracted without its parent Loop.

.. _psyke-intro-restrictions-dm:

Distributed memory
##################

Kernel extraction for distributed memory is supported in as much as each
process will write its own output file by adding its rank to the output
file name. So each kernel and each rank will produce one file. It is possible
to extract several consecutive kernels, but there must be no halo exchange
calls between the kernels. The extraction transformation will test for this
and raise an exception if this should happen.
The compiled driver program accepts the name of the extracted kernel file as
a command line parameter. If this is not specified, it will use the default
name (``module-region`` without a rank).


.. _psyke-intro-restrictions-shared:

Shared memory and API-specific
##############################

The ``ExtractTrans`` transformation cannot be applied to:

* A Loop without its parent Directive,

* An orphaned Directive (e.g. ``OMPDoDirective``, ``ACCLoopDirective``)
  without its parent Directive (e.g. ACC or OMP Parallel Directive),

* A Loop over cells in a colour without its parent Loop over colours in
  the LFRic API,

* An inner Loop without its parent outer Loop in the GOcean API.

* Kernels that have a halo exchange call between them.

.. _psyke-use:

Use
---

The code extraction is currently enabled by utilising a transformation
script (see :ref:`sec_transformations_script` section for more details).

For example, the transformation script which extracts the first Kernel call
in LFRic API test example ``15.1.2_builtin_and_normal_kernel_invoke.f90``
would be written as:

.. code-block:: python

  from psyclone.domain.lfric.transformations import LFRicExtractTrans

  # Get instance of the ExtractRegionTrans transformation
  etrans = LFRicExtractTrans()

  # Get Invoke and its Schedule
  invoke = psy.invokes.get("invoke_0")
  schedule = invoke.schedule

  # Apply extract transformation to the selected Node
  etrans.apply(schedule.children[2])
  print(schedule.view())

and called as:

.. code-block:: bash

  > psyclone -nodm -s ./extract_single_node.py \
      <path-to-example>/15.1.2_builtin_and_normal_kernel_invoke.f90

PSyclone modifies the Schedule of the selected ``invoke_0``:

::

  Schedule[invoke='invoke_0' dm=False]
      0: Loop[type='dofs',field_space='any_space_1',it_space='dofs',
              upper_bound='ndofs']
          Literal[value:'NOT_INITIALISED']
          Literal[value:'NOT_INITIALISED']
          Literal[value:'1']
          Schedule[]
              0: BuiltIn setval_c(f5,0.0)
      1: Loop[type='dofs',field_space='any_space_1',it_space='dofs',
              upper_bound='ndofs']
          ...
          Schedule[]
              0: BuiltIn setval_c(f2,0.0)
      2: Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
          ...
          Schedule[]
              0: CodedKern testkern_code_w2_only(f3,f2) [module_inline=False]
      3: Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
          ...
          Schedule[]
              0: CodedKern testkern_wtheta_code(f4,f5) [module_inline=False]
      4: Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
          ...
          Schedule[]
              0: CodedKern testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]

to insert the extract region. As shown below, all children of an
``ExtractNode`` will be part of the region:

::

  Schedule[invoke='invoke_0' dm=False]
      0: Loop[type='dofs',field_space='any_space_1',it_space='dofs',
              upper_bound='ndofs']
          ...
          Schedule[]
              0: BuiltIn setval_c(f5,0.0)
      1: Loop[type='dofs',field_space='any_space_1',it_space='dofs',
              upper_bound='ndofs']
          ...
          Schedule[]
              0: BuiltIn setval_c(f2,0.0)
      2: Extract
          Schedule[]
              0: Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
                  ...
                  Schedule[]
                      0: CodedKern testkern_code_w2_only(f3,f2) [module_inline=False]
      3: Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
          ...
          Schedule[]
              0: CodedKern testkern_wtheta_code(f4,f5) [module_inline=False]
      4: Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
          ...
          Schedule[]
              0: CodedKern testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]

To extract multiple Nodes, ``ExtractTrans`` can be applied to the list
of Nodes (subject to :ref:`psyke-intro-restrictions-gen` restrictions above):

.. code-block:: python

  # Apply extract transformation to the selected Nodes
  etrans.apply(schedule.children[1:3])

This modifies the above Schedule as:

::

  ...
      Extract
          Schedule[]
              0: Loop[type='dofs',field_space='any_space_1',it_space='dofs',
                      upper_bound='ndofs']
                  ...
                  Schedule[]
                      0: BuiltIn setval_c(f2,0.0)
              1: Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
                  ...
                  Schedule[]
                      0: CodedKern testkern_code_w2_only(f3,f2) [module_inline=False]
  ...

As said above, extraction can be performed on optimised code. For example,
the following example transformation script first adds ``!$OMP PARALLEL DO``
directive and then extracts the optimised code in LFRic API test
example ``15.1.2_builtin_and_normal_kernel_invoke.f90``:

.. code-block:: python

  from psyclone.domain.lfric.transformations import LFRicExtractTrans
  from psyclone.transformations import DynamoOMPParallelLoopTrans

  # Get instances of the transformations
  etrans = LFRicExtractTrans()
  otrans = DynamoOMPParallelLoopTrans()

  # Get Invoke and its Schedule
  invoke = psy.invokes.get("invoke_0")
  schedule = invoke.schedule

  # Add OMP PARALLEL DO directives
  otrans.apply(schedule.children[1])
  otrans.apply(schedule.children[2])
  # Apply extract transformation to the selected Nodes
  etrans.apply(schedule.children[1:3])
  print(schedule.view())

The generated code is now:

.. code-block:: fortran

      ! ExtractStart
      CALL extract_psy_data%PreStart("unknown-module", "setval_c", 0, 4)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f2_post", f2)
      CALL extract_psy_data%PreDeclareVariable("f3_post", f3)
      ...
      CALL extract_psy_data%PreEndDeclaration
      ...
      CALL extract_psy_data%PreEnd
      !
      !$omp parallel do default(shared), private(df), schedule(static)
      DO df=1,undf_aspc1_f2
        f2_proxy%data(df) = 0.0
      END DO
      !$omp end parallel do
      !$omp parallel do default(shared), private(cell), schedule(static)
      DO cell=1,f3_proxy%vspace%get_ncell()
        !
        CALL testkern_code_w2_only(nlayers, f3_proxy%data, f2_proxy%data, ndf_w2, undf_w2, map_w2(:,cell))
      END DO
      !$omp end parallel do
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("df_post", df)
      CALL extract_psy_data%ProvideVariable("f2_post", f2)
      CALL extract_psy_data%ProvideVariable("f3_post", f3)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd

Examples in ``examples/lfric/eg12`` directory demonstrate how to
apply code extraction by utilising PSyclone transformation scripts
(see :ref:`examples` section for more information). The code
in ``examples/lfric/eg17/full_example_extract`` can be compiled and
run, and it will create two kernel data files.

.. _extraction_libraries:

Extraction Libraries
--------------------
PSyclone comes with two extraction libraries: one is based on NetCDF
and will create NetCDF files to contain all input- and output-parameters.
The second one is a stand-alone library which uses only standard Fortran
IO to write and read kernel data. The binary files produced using this
library may not be portable between machines and compilers. If you
require such portability then please use the NetCDF extraction library.

The two extraction :ref:`libraries <libraries>` are in
`lib/extract/standalone
<https://github.com/stfc/PSyclone/tree/master/lib/extract/standalone>`_.
and in
`lib/extract/netcdf
<https://github.com/stfc/PSyclone/tree/master/lib/extract/netcdf>`_.

All versions of the extraction libraries can be compiled with MPI
support by setting the variable ``MPI=yes``:

.. code-block:: shell

  make MPI=yes ...

The only difference is that the output files will now have the process
rank in the name. The compiled driver program accepts the name of the
extracted kernel file as a command line parameter. If this is not specified,
it will use the default name (``module-region`` without a rank).


.. _extraction_for_gocean:

Extraction for GOcean
+++++++++++++++++++++

The extraction libraries in 
`lib/extract/standalone/dl_esm_inf
<https://github.com/stfc/PSyclone/tree/master/lib/extract/standalone/dl_esm_inf>`_
and 
`lib/extract/netcdf/dl_esm_inf
<https://github.com/stfc/PSyclone/tree/master/lib/extract/netcdf/dl_esm_inf>`_
implement the full PSyData API for use with the
:ref:`GOcean <gocean-api>` dl_esm_inf infrastructure library.
When running the instrumented executable, it will create either a binary or
a NetCDF file for each instrumented
code region. It includes all variables that are read before the code
is executed, and all variables that have been modified. The output
variables have the postfix ``_post`` attached to the names,
e.g. a variable ``xyz`` that is read and written will be stored
with the name ``xyz`` containing the input values, and the name
``xyz_post`` containing the output values. Arrays have their size
explicitly stored (in case of NetCDF as dimensions): again the
variable ``xyz`` will have its
sizes stored as ``xyzdim1``, ``xyzdim2`` for the input values,
and output arrays use the name ``xyz_postdim1``, ``xyz_postdim2``.

.. note:: The stand-alone library does not store the names of the
    variables in the output file, but these names will be used
    as variable names in the created driver.

The output file contains the values of all variables used in the
subroutine. The ``GOceanExtractTrans`` transformation can automatically
create a driver program which will read the corresponding output file,
call the instrumented region, and compare the results. In order to create
this driver program, the options parameter ``create_driver`` must
be set to true:

.. code-block:: python

    extract = GOceanExtractTrans()
    extract.apply(schedule.children,
                  {"create_driver": True,
                   "region_name": ("main", "init")})

This will create a Fortran file called ``driver-main-init.f90``, which
can then be compiled and executed. This stand-alone program will read
the output file created during an execution of the actual program, call
the kernel with all required input parameter, and compare the output
variables with the original output variables. This can be used to create
stand-alone test cases to reproduce a bug, or for performance
optimisation of a stand-alone kernel.

.. warning:: Care has to be taken that the driver matches the version
    of the code that was used to create the output file, otherwise the
    driver will likely crash. The stand-alone driver relies on a
    strict ordering of variable values in the output file and e.g.
    even renaming one variable can affect this. The NetCDF version
    stores the variable names and will not be able to find a variable
    if its name has changed.

Extraction for LFRic
++++++++++++++++++++

The libraries in 
`lib/extract/standalone/lfric
<https://github.com/stfc/PSyclone/tree/master/lib/extract/standalone/lfric>`_
and
`lib/extract/netcdf/lfric
<https://github.com/stfc/PSyclone/tree/master/lib/extract/netcdf/lfric>`_
implement the full PSyData API for use with the
:ref:`LFRic <lfric-api>` infrastructure library. When running the
code, it will create an output file for each instrumented code region.
The same logic for naming variables (using ``_post`` for output variables)
used in :ref:`extraction_for_gocean` is used here.

Check :ref:`integrating_psy_data_lfric` for the recommended way of linking
an extraction library to LFRic.

The output file contains the values of all variables used in the
subroutine. The ``LFRicExtractTrans`` transformation can automatically
create a driver program which will read the corresponding output file,
call the instrumented region, and compare the results. In order to create
this driver program, the options parameter ``create_driver`` must
be set to true:

.. code-block:: python

    extract = LFRicExtractTrans()
    extract.apply(schedule.children,
                  {"create_driver": True,
                   "region_name": ("main", "init")})

This will create a Fortran file called ``driver-main-init.F90``, which
can then be compiled and executed. This stand-alone program will read
the output file created during an execution of the actual program, call
the kernel with all required input parameter, and compare the output
variables with the original output variables. This can be used to create
stand-alone test cases to reproduce a bug, or for performance
optimisation of a stand-alone kernel.

.. warning:: Care has to be taken that the driver matches the version
    of the code that was used to create the output file, otherwise the
    driver will likely crash. The stand-alone driver relies on a
    strict ordering of variable values in the output file and e.g.
    even renaming one variable can affect this. The NetCDF version
    stores the variable names and will not be able to find a variable
    if its name has changed.

The LFRic kernel driver will inline all required external modules into the
driver. It uses a ``ModuleManager`` to find the required modules, based on the
assumption that a file ``my_special_mod.f90`` will define exactly one module
called ``my_special_mod`` (the ``_mod`` is required to be part of the
filename). The driver creator will sort the modules in the appropriate order
and add the source code directly into the driver. As a result, the driver
program is truly stand-alone and does not need any external dependency (the
only exception being NetCDF if the NetCDF-based extraction library is used).
The ``ModuleManager`` uses all kernel search paths specified on the
command line (see ``-d`` option in :ref:`psyclone_command`), and it will
recursively search for all files under each path specified on the command
line.

Therefore, compilation for a created driver, e.g. the one created in
``examples/lfric/eg17/full_example_extract``, is simple:

.. code-block:: output

   $ gfortran -g -O0 driver-main-update.F90 -o driver-main-update
   $ ./driver-main-update
       Variable      max_abs      max_rel      l2_diff       l2_cos    identical    #rel<1E-9    #rel<1E-6    #rel<1E-3
           cell .0000000E+00 .0000000E+00 .0000000E+00 .1000000E+01 .1000000E+01 .0000000E+00 .0000000E+00 .0000000E+00
    field1_data .0000000E+00 .0000000E+00 .0000000E+00 .1000000E+01 .5390000E+03 .0000000E+00 .0000000E+00 .0000000E+00
     dummy_var1 .0000000E+00 .0000000E+00 .0000000E+00 .1000000E+01 .1000000E+01 .0000000E+00 .0000000E+00 .0000000E+00

(see :ref:`driver_summary_statistics` for details about the statistics`).
Note that the Makefile in the example will actually provide additional include
paths (infrastructure files and extraction library) for the compiler, but
these flags are actually only required for compiling the example program, not
for the driver.

Restrictions of Kernel Extraction and Driver Creation
#####################################################
A few restrictions still apply to the current implementation of the driver
creation code:

- Distributed memory is not yet supported. See #1992.
- The extraction code will now write variables that are used from other
  modules to the kernel data file, and the driver will read these values in.
  Unfortunately, if a variable is used that is defined as private,
  the value cannot be written to the file, and compilation will abort.
  The only solution is to modify this file and make all variables public.
  This mostly affects ``log_mod.F90``, but a few other modules as well.
- The new build system FAB will be able to remove ``private`` and
  ``protected`` declarations in any source files, meaning no manual
  modification of files is required anymore (TODO #2536).

Extraction for NEMO
++++++++++++++++++++
The libraries in
`lib/extract/standalone/nemo
<https://github.com/stfc/PSyclone/tree/master/lib/extract/standalone/nemo>`_
and
`lib/extract/netcdf/nemo
<https://github.com/stfc/PSyclone/tree/master/lib/extract/netcdf/nemo>`_
implement the full PSyData API for use with the
:ref:`NEMO <nemo-api>` API. When running the
code, it will create an output file for each instrumented code region.
The same logic for naming variables used in :ref:`extraction_for_gocean`
is used here.

.. note::

  Driver creation in NEMO is not yet supported, and is
  tracked in issue #2058.

.. _driver_summary_statistics:

Driver Summary Statistics
-------------------------
When a driver is executed, it will print summary statistics at the end
for each variable that was modified, indicating the difference between the
`original` values from when the data file was created, and the `new` ones
computed when executing the kernel. These differences can be caused
by changing the compilation options, or compiler version. Example output:

.. code-block:: output

       Variable      max_abs      max_rel      l2_diff       l2_cos    identical    #rel<1E-9    #rel<1E-6    #rel<1E-3
           cell .0000000E+00 .0000000E+00 .0000000E+00 .1000000E+01 .1000000E+01 .0000000E+00 .0000000E+00 .0000000E+00
    field1_data .0000000E+00 .0000000E+00 .0000000E+00 .1000000E+01 .5390000E+03 .0000000E+00 .0000000E+00 .0000000E+00
     dummy_var1 .0000000E+00 .0000000E+00 .0000000E+00 .1000000E+01 .1000000E+01 .0000000E+00 .0000000E+00 .0000000E+00

The columns from left to right are:

..
  In order to avoid a dependency to dvipng (which depends on latex)
  by default do not use maths mode for html (instead represent the math
  formulas textually). But if latex is being used, or the tag
  `has_dvipng` is defined (by the build environment using `-t has_dvipng`)
  still use the math support.
  We also have to duplicate the whole bullet list, sphinx `only`
  directive cannot be applied to a single bullet line only.

.. only:: latex or has_dvipng

  * The variable name.
  * The maximum absolute error of all elements.
  * The maximum relative error of all elements. If an element has the value
    0, the relative error for this element is considered to be 1.0.
  * The L2 difference: :math:`\sqrt{\sum{(original-new)^2}}`.
  * The cosine of the angle between the two vectors: :math:`\frac{\sum{original*new}}{\sqrt{\sum{original*original}}*\sqrt{\sum{new*new}}}`.
  * How many values are identical.
  * How many values have a relative error of less than 10\ :sup:`-9` but are not identical.
  * How many values have a relative error of less than 10\ :sup:`-6` but more than 10\ :sup:`-9`.
  * How many values have a relative error of less than 10\ :sup:`-3` but more than 10\ :sup:`-6`.

.. only:: html and not has_dvipng

  * The variable name.
  * The maximum absolute error of all elements.
  * The maximum relative error of all elements. If an element has the value
    0, the relative error for this element is considered to be 1.0.
  * The L2 difference: `sqrt(sum((original-new)`\ :sup:`2` `))`.
  * The cosine of the angle between the two vectors: `sum(original*new)/(sqrt(sum(original*original))*sqrt(sum(new*new)))`.
  * How many values are identical.
  * How many values have a relative error of less than 10\ :sup:`-9` but are not identical.
  * How many values have a relative error of less than 10\ :sup:`-6` but more than 10\ :sup:`-9`.
  * How many values have a relative error of less than 10\ :sup:`-3` but more than 10\ :sup:`-6`.

.. note:: The usefulness of the columns printed is still being evaluated. Early
    indications are that the cosine of the angle between the two vectors,
    which is commonly used in AI, might not be sensitive enough to give
    a good indication of the differences.



