.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019, Science and Technology Facilities Council
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
.. Written I. Kavcic, Met Office

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
**ExtractRegionTrans** transformation to selected Nodes. This
transformation is further sub-classed into API-specific implementations,
**DynamoExtractRegionTrans** and **GOceanExtractRegionTrans**. Both
sub-classed transformations insert an instance of the **ExtractNode**
object into the Schedule of a specific Invoke. All Nodes marked for
extraction become children of the **ExtractNode**.

For now, the **ExtractNode** class simply adds comments at the beginning
and the end of the extract region, that is at the position of the extract
Node and after all its children. For example, marking a single Loop
containing a Kernel call in Dynamo0.3 API generates:

.. code-block:: fortran

      ! ExtractStart
      ! CALL write_extract_arguments(argument_list)
      !
      DO cell=1,f3_proxy%vspace%get_ncell()
        !
        CALL testkern_code_w2_only(nlayers, f3_proxy%data, f2_proxy%data, ...)
      END DO 
      !
      ! ExtractEnd

The ``! CALL write_extract_arguments(argument_list)`` comment will be
replaced by appropriate calls to write out arguments of extracted Node(s)
or Kernel(s) in Issue #234. The **ExtractNode** base class can be sub-classed
by API-specific code. This will be required for constructing the list of
quantities required by Kernel (and Built-In) calls in the extracted Nodes.

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

* Extraction cannot be applied to an **ExtractNode** or a Node list that
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

The **ExtractRegionTrans** transformation cannot be applied to:

* A Loop without its parent Directive,

* An orphaned Directive (e.g. **OMPDoDirective**, **ACCLoopDirective**)
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

  from psyclone.transformations import DynamoExtractRegionTrans

  # Get instance of the ExtractRegionTrans transformation
  etrans = DynamoExtractRegionTrans()

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
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          BuiltIn setval_c(f5,0.0)
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          BuiltIn setval_c(f2,0.0)
      Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
          CodedKern testkern_code_w2_only(f3,f2) [module_inline=False]
      Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
          CodedKern testkern_wtheta_code(f4,f5) [module_inline=False]
      Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
          CodedKern testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]

to insert the extract region. As shown below, all children of an
**ExtractNode** will be part of the region:

::

  Schedule[invoke='invoke_0' dm=False]
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          BuiltIn setval_c(f5,0.0)
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          BuiltIn setval_c(f2,0.0)
      Extract
          Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
              CodedKern testkern_code_w2_only(f3,f2) [module_inline=False]
      Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
          CodedKern testkern_wtheta_code(f4,f5) [module_inline=False]
      Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
          CodedKern testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]

To extract multiple Nodes, **ExtractRegionTrans** can be applied to the list
of Nodes (subject to :ref:`psyke-intro-restrictions-gen` restrictions above):

.. code-block:: python

  # Apply extract transformation to the selected Nodes
  schedule, _ = etrans.apply(schedule.children[1:3])

This modifies the above Schedule as:

::

  ...
      Extract
          Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
              BuiltIn setval_c(f2,0.0)
          Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
              CodedKern testkern_code_w2_only(f3,f2) [module_inline=False]
  ...

As said above, extraction can be performed on optimised code. For example,
the following example transformation script first adds ``!$OMP PARALLEL DO``
directive and then extracts the optimised code in Dynamo0.3 API test
example ``15.1.2_builtin_and_normal_kernel_invoke.f90``:

.. code-block:: python

  from psyclone.transformations import DynamoOMPParallelLoopTrans, \
      DynamoExtractRegionTrans

  # Get instances of the transformations
  etrans = DynamoExtractRegionTrans()
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
      ! CALL write_extract_arguments(argument_list)
      !
      !$omp parallel do default(shared), private(df), schedule(static)
      DO df=1,undf_any_space_1_f2
        f2_proxy%data(df) = 0.0
      END DO
      !$omp end parallel do
      !$omp parallel do default(shared), private(cell), schedule(static)
      DO cell=1,f3_proxy%vspace%get_ncell()
        !
        CALL testkern_code_w2_only(nlayers, f3_proxy%data, f2_proxy%data, ndf_w2, undf_w2, map_w2(:,cell))
      END DO
      !$omp end parallel do
      !
      ! ExtractEnd

Examples in ``examples/dynamo/eg12`` directory demonstrate how to
apply code extraction by utilising PSyclone transformation scripts
(see :ref:`examples` section for more information).
