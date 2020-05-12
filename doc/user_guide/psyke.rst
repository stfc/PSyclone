.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2020, Science and Technology Facilities Council
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
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("f1_post", f1)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("a", a)
      CALL extract_psy_data%ProvideVariable("f2", f2)
      CALL extract_psy_data%ProvideVariable("m1", m1)
      CALL extract_psy_data%ProvideVariable("m2", m2)
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

.. note::
    At this stage the LFRic API is not fully supported, as can be seen
    by missing paramters like ``nlayers``, ``ndf_w1``, ... This is
    tracked in issue #646.

The PSyData API relies on generic Fortran interfaces to provide the 
field-type-specific implementations of the ``ProvideVariable`` for different
types. This means that a different version of the external PSyData
library that PSyKE uses must be supplied for each PSyclone API.

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

As noted in the :ref:`distributed_memory` section, support for distributed
memory in PSyclone is currently limited to the Dynamo0.3 API. Since the
implementation generates calls to LFRic infrastructure (e.g. runtime checks
for status of field halos), code extraction is not allowed when distributed
memory is enabled.

.. _psyke-intro-restrictions-shared:

Shared memory and API-specific
##############################

The ``ExtractTrans`` transformation cannot be applied to:

* A Loop without its parent Directive,

* An orphaned Directive (e.g. ``OMPDoDirective``, ``ACCLoopDirective``)
  without its parent Directive (e.g. ACC or OMP Parallel Directive),

* A Loop over cells in a colour without its parent Loop over colours in
  the Dynamo0.3 API,

* An inner Loop without its parent outer Loop in the GOcean1.0 API.

.. _psyke-use:

Use
---

The code extraction is currently enabled by utilising a transformation
script (see :ref:`sec_transformations_script` section for more details).

For example, the transformation script which extracts the first Kernel call
in Dynamo0.3 API test example ``15.1.2_builtin_and_normal_kernel_invoke.f90``
would be written as:

.. code-block:: python

  from psyclone.domain.lfric.transformations import LFRicExtractTrans

  # Get instance of the ExtractRegionTrans transformation
  etrans = LFRicExtractTrans()

  # Get Invoke and its Schedule
  invoke = psy.invokes.get("invoke_0")
  schedule = invoke.schedule

  # Apply extract transformation to the selected Node
  schedule, _ = etrans.apply(schedule.children[2])
  schedule.view()

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
  schedule, _ = etrans.apply(schedule.children[1:3])

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
directive and then extracts the optimised code in Dynamo0.3 API test
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
  schedule, _ = otrans.apply(schedule.children[1])
  schedule, _ = otrans.apply(schedule.children[2])
  # Apply extract transformation to the selected Nodes
  schedule, _ = etrans.apply(schedule.children[1:3])
  schedule.view()

The generated code is now:

.. code-block:: fortran

      ! ExtractStart
      CALL extract_psy_data%PreStart("unknown-module", "setval_c", 1, 3)
      CALL extract_psy_data%PreDeclareVariable("f2", f2)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f3_post", f3)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("f2", f2)
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
      CALL extract_psy_data%ProvideVariable("f3_post", f3)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd

.. note::

    At this stage builtins are not fully supported, resulting in ``f2``
    being incorrectly detected as an input parameter, and not as an
    output parameter. This issue is tracked in #637.


Examples in ``examples/lfric/eg12`` directory demonstrate how to
apply code extraction by utilising PSyclone transformation scripts
(see :ref:`examples` section for more information).

.. _psyke_netcdf:

NetCDF Extraction Example
-------------------------
PSyclone comes with an example NetCDF based extraction library in
`lib/extract/dl_esm_inf/netcdf 
<https://github.com/stfc/PSyclone/tree/master/lib/extract/dl_esm_inf/netcdf>`_.
This library implements the full PSyData
API for use with the GOcean 1.0 dl_esm_inf infrastructure library.
In order to compile this library, you must have NetCDF installed.
When running the code, it will create a NetCDF file for the instrumented
code region. It includes all variables that are read before the code
is executed, and all variables that have been modified. The output
variables have the postfix ``_post`` attached to the NetCDF names,
e.g. a variable ``xyz`` that is read and written will be stored
with the name ``xyz`` containing the input values, and the name
``xyz_post`` containing the output values. Arrays have their size
stored as NetCDF dimensions: again the variable ``xyz`` will have its
sizes stored as ``xyzdim1``, ``xyzdim2`` for the input values,
and output arrays use the name ``xyz_postdim1``, ``xyz_postdim2``.

The output file contains the values of all variables used in the
subroutine. The ``GOceanExtractTrans`` can automatically create a
driver program which will read the netcdf file and then call the
instrumented region. In order to create this driver program, the
options parameter ``create_driver`` must be set to true::

    extract = GOceanExtractTrans()
    extract.apply(schedule.children,
                  {"create_driver": True,
                   "region_name": ("main", "init")})

This will create a Fortran file called ``driver-main-init.f90``, which
can then be compiled and executed.
