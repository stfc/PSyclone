.. _psyke:

PSy Kernel Extractor (PSyKE)
============================

.. _psyke-intro:

Introduction
------------

PSyclone has the ability to define regions of a PSyclone-conformant code
to be extracted and run as stand-alone applications. This ability, called
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

The basic mechanism of code extraction is applying **ExtractRegionTrans**
transformation to the selected Nodes. This transformation inserts an instance
of the **ExtractNode** object into the Schedule of a specific Invoke. All
Nodes marked for extraction become children of the **ExtractNode**.

For now, the **ExtractNode** class simply adds comments at the beginning
and the end of the extract region, that is at the position of the extract
Node and after all its children. For example, marking a single Loop
containing a Kernel call in Dynamo0.3 API generates:

::
  ...
      ! ExtractStart
      ! CALL write_extract_arguments(argument_list)
      !
      DO cell=1,f3_proxy%vspace%get_ncell()
        !
        CALL testkern_code_w2_only(nlayers, f3_proxy%data, f2_proxy%data, ...)
      END DO 
      !
      ! ExtractEnd
  ...

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

* Kernel or Built-In call cannot be extracted without its parent Loop.

Distributed memory
##################

As noted in :ref:`distributed_memory` section, support for distributed memory
in PSyclone is currently limited the Dynamo0.3 API. As the implementation
depends on the LFRic infrastructure, extraction of distributed memory code
is not allowed.

Shared memory and API-specific
##############################

As noted above, optimisations in the extracted code may fail if the extract
region is defined without correctnes checks for optimisation transformations. 
The restrictions below will do not allow the **ExtractRegionTrans** to be
apllied on:

* A Loop without its parent Directive,

* An orphaned Directive (e.g. **OMPDoDirective**, **ACCLoopDirective**)
  without its parent Directive (e.g. ACC or OMP Parallel Directive),

* A Loop over cells in a colour without its parent Loop over colours in
  Dynamo0.3 API,

* An inner Loop without its parent outer Loop in GOcean1.0 API.

.. _psyke-use:

Use
---

The code extraction is currently enabled by
utilising a transformation script.

By default the ``psyclone`` script will generate 'vanilla' PSy layer
code. The -s option allows a Python script to be specified which can
transform the PSy layer. This option is discussed in more detail in
the :ref:`sec_transformations_script` section.

PSyclone will modify the schedule of each invoke to insert the
extraction regions. Below we show an example - all children 
of an Extract-Node will be part of the region, including all 
loops created by PSyclone and all kernel calls::

Example Dynamo0.3 API, Single Node

::

schedule, _ = etrans.apply(schedule.children[0])

::

Schedule[invoke='invoke_0_testkern_type' dm=False]
    Extract[position='0',depth='2']
        Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
            KernCall testkern_code(a,f1,f2,m1,m2) [module_inline=False]


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



Example GOcean1.0 API, single node

::

schedule, _ = etrans.apply(schedule.children[1])

::

GOSchedule[invoke='invoke_0',Constant loop bounds=True]
    Loop[type='outer',field_space='go_cu',it_space='go_internal_pts']
        Loop[type='inner',field_space='go_cu',it_space='go_internal_pts']
            KernCall compute_cu_code(cu_fld,p_fld,u_fld) [module_inline=False]
    Extract[position='1',depth='2']
        Loop[type='outer',field_space='go_cv',it_space='go_internal_pts']
            Loop[type='inner',field_space='go_cv',it_space='go_internal_pts']
                KernCall compute_cv_code(cv_fld,p_fld,v_fld) [module_inline=False]
    Loop[type='outer',field_space='go_every',it_space='go_internal_pts']
        Loop[type='inner',field_space='go_every',it_space='go_internal_pts']
            KernCall time_smooth_code(u_fld,unew_fld,uold_fld) [module_inline=False]



Example Dynamo0.3 API, Multiple Nodes

::

schedule, _ = etrans.apply(schedule.children[1:3])

::

Schedule[invoke='invoke_0' dm=False]
    Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
        Call setval_c(f5,0.0)
    Extract[position='1',depth='2']
        Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
            Call setval_c(f2,0.0)
        Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
            KernCall testkern_code_w2_only(f3,f2) [module_inline=False]
    Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
        KernCall testkern_wtheta_code(f4,f5) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]


Optimisations, example Dynamo0.3 API, Multiple Nodes

::

 from psyclone.transformations import DynamoOMPParallelLoopTrans
    schedule, _ = otrans.apply(schedule.children[1])
    schedule, _ = otrans.apply(schedule.children[2])
    schedule, _ = etrans.apply(schedule.children[1:3])

::

Schedule[invoke='invoke_0' dm=False]
    Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
        Call setval_c(f5,0.0)
    Extract[position='1',depth='2']
        Directive[OMP parallel do]
            Loop[type='dofs',field_space='any_space_1',it_space='dofs', upper_bound='ndofs']
                Call setval_c(f2,0.0)
        Directive[OMP parallel do]
            Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
                KernCall testkern_code_w2_only(f3,f2) [module_inline=False]
    Loop[type='',field_space='wtheta',it_space='cells', upper_bound='ncells']
        KernCall testkern_wtheta_code(f4,f5) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(scalar,f1,f2,f3,f4) [module_inline=False]


.. _psyke-utilities:

Utilites
--------

.. _psyke-utilites-extractor:

Extractor Module
++++++++++++++++

This class contains is helper functions for code extraction.
For now it only provides the function to extract the specific Kernel
from an Invoke Schedule. Another planned functionality is to wrap
settings for generating driver for the extracted code.

.. _psyke-utilites-extract-kernel:

`extract_kernel`
################

This function inserts ExtractNode(s) around one or more Nodes
in a Schedule which contains calls to a particular Kernel.
First we construct the lists of relative and absolute positions
of root Nodes which contain the Kernel call within the Schedule.
The list of relative positions instructs the ExtractRegionTrans
where to insert an ExtractNode. The list of absolute positions is
used as a control mechanism for cases where two or more Kernels
with the same name are descendants of the same root Node (for
instance if they are enclosed within an OMPParallelDirective or
an OMPParallelDoDirective). In these cases the repeated values of
root Node(s)' absolute and the corresponding relative positions
are not counted. Otherwise the ExtractRegionTrans would try to
insert and ExtractNode repeatedly and fail with the appropriate
TransformationError. If the specified Kernel is called within
more than one root Node then this function will insert ExtractNodes
in all returned locations, unless the optional argument "position"
specifies just one of these locations (relative positions of the
root Nodes in the Schedule).

test_extract_single_identical_kernel

::

    kernel_name = "ru_code"
    kernel_position = 3
    schedule = Extractor.extract_kernel(schedule, kernel_name, kernel_position)

::

Schedule[invoke='invoke_0' dm=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(rdt,h,f,c,d) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(rdt,b,f,c,d) [module_inline=False]
    Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
        KernCall ru_code(b,a,istp,rdt,c,e) [module_inline=False]
    Extract[position='3',depth='2']
        Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
            KernCall ru_code(g,a,istp,rdt,c,e) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(ascalar,f,b,c,d) [module_inline=False]


test_extract_single_kernel

::
    kernel_name = "ru_code"
    kernel_position = 3
    schedule = Extractor.extract_kernel(schedule, kernel_name, kernel_position)

::

Schedule[invoke='invoke_0' dm=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(rdt,h,f,c,d) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(rdt,b,f,c,d) [module_inline=False]
    Extract[position='2',depth='2']
        Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
            KernCall ru_code(b,a,istp,rdt,c,e) [module_inline=False]
    Extract[position='3',depth='2']
        Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
            KernCall ru_code(g,a,istp,rdt,c,e) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(ascalar,f,b,c,d) [module_inline=False]


test_loop_fuse

::

    from psyclone.transformations import DynamoLoopFuseTrans

    ltrans = DynamoLoopFuseTrans()
    schedule, _ = ltrans.apply(schedule.children[2], schedule.children[3])

    kernel_name = "ru_code"
    schedule = Extractor.extract_kernel(schedule, kernel_name)

::

Schedule[invoke='invoke_0' dm=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(rdt,h,f,c,d) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(rdt,b,f,c,d) [module_inline=False]
    Extract[position='2',depth='2']
        Loop[type='',field_space='w2',it_space='cells', upper_bound='ncells']
            KernCall ru_code(b,a,istp,rdt,c,e) [module_inline=False]
            KernCall ru_code(g,a,istp,rdt,c,e) [module_inline=False]
    Loop[type='',field_space='w1',it_space='cells', upper_bound='ncells']
        KernCall testkern_code(ascalar,f,b,c,d) [module_inline=False]

::

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    kernel_name = "ru_code"
    schedule = Extractor.extract_kernel(schedule, kernel_name)

::

Schedule[invoke='invoke_0' dm=False]
    Loop[type='colours',field_space='w1',it_space='cells', upper_bound='ncolours']
        Directive[OMP parallel do]
            Loop[type='colour',field_space='w1',it_space='cells', upper_bound='ncolour']
                KernCall testkern_code(rdt,h,f,c,d) [module_inline=False]
    Loop[type='colours',field_space='w1',it_space='cells', upper_bound='ncolours']
        Directive[OMP parallel do]
            Loop[type='colour',field_space='w1',it_space='cells', upper_bound='ncolour']
                KernCall testkern_code(rdt,b,f,c,d) [module_inline=False]
    Extract[position='2',depth='2']
        Loop[type='colours',field_space='w2',it_space='cells', upper_bound='ncolours']
            Directive[OMP parallel do]
                Loop[type='colour',field_space='w2',it_space='cells', upper_bound='ncolour']
                    KernCall ru_code(b,a,istp,rdt,c,e) [module_inline=False]
    Extract[position='3',depth='2']
        Loop[type='colours',field_space='w2',it_space='cells', upper_bound='ncolours']
            Directive[OMP parallel do]
                Loop[type='colour',field_space='w2',it_space='cells', upper_bound='ncolour']
                    KernCall ru_code(g,a,istp,rdt,c,e) [module_inline=False]
    Loop[type='colours',field_space='w1',it_space='cells', upper_bound='ncolours']
        Directive[OMP parallel do]
            Loop[type='colour',field_space='w1',it_space='cells', upper_bound='ncolour']
                KernCall testkern_code(ascalar,f,b,c,d) [module_inline=False]

Examples
--------

The twelfth example demonstrates how to apply code extraction to Nodes
in an Invoke Schedule or from one or more Kernels in Invokes. For now
it only inserts an `ExtractNode` in appropriate locations. The full
support for code extraction is being developed (please note that
distributed memory will not be supported). This example can extract
a list of Nodes:
::
    cd eg12/
    ${PSYCLONE} -nodm -s ./extract_nodes.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

a specific Kernel from one Invoke:
::
    cd eg12/
    ${PSYCLONE} -nodm -s ./extract_single_kernel.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

a specific Kernel from multiple Invokes which contain the Kernel call:
::
    cd eg12/
    ${PSYCLONE} -nodm -s ./extract_kernel_multi_invokes.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

or a specific Kernel from multiple Invokes which contain the Kernel
call after applying optimisations (here colouring and OpenMP):
::
    cd eg12/
    ${PSYCLONE} -nodm -s ./extract_kernel_with_optimisations.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

The example also contains a Python helper script which returns
information useful for Kernel extraction: names of one or more
Invokes which contain call to the specified Kernel and positions
of the root Nodes containing the Kernel calls:
::
    cd eg12/
    python find_kernel.py
