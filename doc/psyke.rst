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

The basic mechanism of code extraction is applying the **ExtractRegionTrans**
transformation to selected Nodes. This transformation inserts an instance
of the **ExtractNode** object into the Schedule of a specific Invoke. All
Nodes marked for extraction become children of the **ExtractNode**.

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

The ``! CALL write_extract_arguments(argument_list)`` comment will be replaced
by appropriate calls to write out arguments of extracted Node(s) or Kernel(s).
Both **ExtractRegionTrans** and **ExtractNode** are base classes that can be
added to both Dynamo0.3 and GOcean1.0 APIs. However, generating argument list
of extracted Nodes is API-specific so it will be delegated to children of
the **ExtractNode** base class.

.. _psyke-intro-restrictions:

Restrictions
++++++++++++

Code extraction can be applied to unoptimised or optimised code. Hence, it
is reasonable to introduce restrictions that prevent failures of optimising
transformations when extraction is applied. It is also reasonable to introduce
restrictions that eliminate dependence on the specific model infrastructure.

.. _psyke-intro-restrictions-gen:

General
#######

This group of restrictions is enforced irrespective of whether optimisations
are used or not.

* Extraction can be applied to a single Node in or a list of Nodes in a
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
memory in PSyclone is currently limited the Dynamo0.3 API. Since the
implementation depends on the LFRic infrastructure, code extraction when
distributed memory is enabled is not allowed.

.. _psyke-intro-restrictions-shared:

Shared memory and API-specific
##############################

As noted above, optimisations in the extracted code may fail if the extract
region is defined without correctnes checks for optimisation transformations. 
The restrictions below do not allow the **ExtractRegionTrans** to be
applied on:

* A Loop without its parent Directive,

* An orphaned Directive (e.g. **OMPDoDirective**, **ACCLoopDirective**)
  without its parent Directive (e.g. ACC or OMP Parallel Directive),

* A Loop over cells in a colour without its parent Loop over colours in
  Dynamo0.3 API,

* An inner Loop without its parent outer Loop in GOcean1.0 API.

.. _psyke-use:

Use
---

The code extraction is currently enabled by utilising a transformation
script (see :ref:`sec_transformations_script` section for more details).

For example, the transformation script which extracts the first Kernel call
in Dynamo0.3 API test example ``15.1.2_builtin_and_normal_kernel_invoke.f90``
would be written as:

.. code-block:: python

  from psyclone.transformations import ExtractRegionTrans

  # Get instance of the ExtractRegionTrans transformation
  etrans = ExtractRegionTrans()

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
          Call setval_c(f5,0.0)
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          Call setval_c(f2,0.0)
      Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
          KernCall testkern_code_w2_only(f3,f2) [module_inline=False]
      Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
          KernCall testkern_wtheta_code(f4,f5) [module_inline=False]
      Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
          KernCall testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]

to insert the extract region. As shown below, all children of an
**ExtractNode** will be part of the region:

::

  Schedule[invoke='invoke_0' dm=False]
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          Call setval_c(f5,0.0)
      Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
          Call setval_c(f2,0.0)
      Extract[position='2',depth='2']
          Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
              KernCall testkern_code_w2_only(f3,f2) [module_inline=False]
      Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
          KernCall testkern_wtheta_code(f4,f5) [module_inline=False]
      Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
          KernCall testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]

To extract multiple Nodes, **ExtractRegionTrans** can be applied to the list
of Nodes (subject to :ref:`psyke-intro-restrictions-gen` restrictions above):

.. code-block:: python

  # Apply extract transformation to the selected Nodes
  schedule, _ = etrans.apply(schedule.children[1:3])

This modifies the above Schedule as:

::

  ...
      Extract[position='1',depth='2']
          Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
              Call setval_c(f2,0.0)
          Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
              KernCall testkern_code_w2_only(f3,f2) [module_inline=False]
  ...

As said above, extraction can be performed on optimised code. For example,
the below example of transformation script first adds ``!$OMP PARALLEL DO``
directive and then extracts the optimised code in Dynamo0.3 API test
example ``15.1.2_builtin_and_normal_kernel_invoke.f90``:

.. code-block:: python

  from psyclone.transformations import DynamoOMPParallelLoopTrans, \
      ExtractRegionTrans

  # Get instances of the transformations
  etrans = ExtractRegionTrans()
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

.. _psyke-utilities:

Utilites
--------

.. _psyke-utilites-extractor:

Extractor Module
++++++++++++++++

This module contains helper functions for code extraction. For now it
provides the function to extract the specific Kernel from an Invoke Schedule
(:ref:`psyke-utilites-extract-kernel` below). Another planned
functionality is to wrap settings for generating Driver for the extracted
code.

.. _psyke-utilites-extract-kernel:

``extract_kernel`` Function
###########################

This function marks one or more Kernel call (with its parent Loop) within
a specified Invoke for extraction. It takes two mandatory and one optional
argument:

* ``schedule`` - Invoke schedule to extract the Kernel from,

* ``kernel_name`` - Name of the specified Kernel as represented in a Kernel
  call (ending in ``_code`` instead of ``_type``, e.g. ``ru_kernel_code``),

* ``root_node_position`` (optional) - position of the Kernel call root Node.

For example, the transformation script which extracts the second
``ru_kernel_type`` Kernel call in Dynamo0.3 API test example
``4.8_multikernel_invokes.f90`` would be written as:

.. code-block:: python

  from psyclone.extractor import Extractor

  # Get Invoke and its Schedule
  invoke = psy.invokes.get("invoke_0")
  schedule = invoke.schedule

  # Extract the selected Kernel
  schedule = Extractor.extract_kernel(schedule, "ru_kernel_code", 3)
  schedule.view()

If there are multiple calls to the specified Kernel within the Invoke
and ``root_node_position`` argument is not provided, this function will
extract all of them. Finally, the function will raise **GenerationError**:

* If there are no Kernels with the specified name in the Invoke Schedule,

* If the optional ``root_node_position`` argument does not point to a
  valid root Node location for the specified Kernel call.

Names of Invokes containing calls to the specified Kernel, as well as
positions of the Kernel calls' root Nodes, can be found using the Python
helper script ``find_kernel.py`` described in more detail in
:ref:`psyke-utilites-find-kernel` section below.

.. _psyke-examples:

Examples
++++++++

Examples in ``examples/dynamo/eg12`` directory demonstrate how to
apply code extraction by utilising PSyclone transformation scripts.

The first example marks a list of Nodes for extraction:

.. code-block:: bash

  > psyclone -nodm -s ./extract_nodes.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

The second marks the specific Kernel:

.. code-block:: bash

  > psyclone -nodm -s ./extract_single_kernel.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

The third marks the specific Kernel in multiple Invokes which contain
the Kernel call:

.. code-block:: bash

  > psyclone -nodm -s ./extract_kernel_multi_invokes.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

The fourth example marks the specific Kernel in multiple Invokes which
contain the Kernel call after applying optimisations (here colouring
and OpenMP):

.. code-block:: bash

  > psyclone -nodm -s ./extract_kernel_with_optimisations.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

.. _psyke-utilites-find-kernel:

``find_kernel`` Script
######################

The ``examples/dynamo/eg12`` directory also contains a Python helper
script which returns the information useful for Kernel extraction: the
names of Invokes which contain calls to the specified Kernel and
positions of the root Nodes containing the Kernel calls. Use:

.. code-block:: bash

  > python <path/to/script/>find_kernel.py

Input parameters can be specified or modified in the first section of
the script:

* ``TEST_API`` - PSyclone API (the example here is "dynamo0.3"),

* ``ALG_NAME`` - Algorithm file name to be searched for Kernel calls,

* ``ALG_PATH`` - Path to the Algorithm file (absolute or relative from
  the location where this script is run),

* ``KERNEL_BASENAME`` - Base name of the Kernel to be found (without the
  ``_kernel_mod`` and file extension),

* ``OPTIMISE`` - Switch for applying optimisations to PSy layer before
  searching for the Kernel call,

* ``OPT_SCRIPT`` - Name of the optimisation script which applies PSyclone
  transformations to the code. As pointed out in Transformations
  :ref:`sec_transformations_script` section, a valid script file must
  contain a **trans** function which modifies the **PSy** object.

If ``OPTIMISE`` switch is enabled, the specified ``OPT_SCRIPT``
transformation script will be imported as a Python module and relevant
optimisations will be applied before looking for the specified Kernel call.
