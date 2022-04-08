.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2022, Science and Technology Facilities Council
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
.. Written by: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab.
..             A. B. G. Chalk, STFC Daresbury Lab.
..             I. Kavcic, Met Office.

.. _transformations:

Transformations
===============

As discussed in the previous section, transformations can be applied
to PSyclone's internal representation (PSyIR) to modify it. Typically
transformations will be used to optimise the Algorithm, PSy and/or
Kernel layer(s) for a particular architecture, however transformations
could be added for other reasons, such as to aid debugging or for
performance monitoring.

Finding
-------

Transformations can be imported directly, but the user needs to know
what transformations are available. A helper class **TransInfo** is
provided to show the available transformations

.. note:: The directory layout of PSyclone is currently being restructured.
          As a result of this some transformations are already in the new
          locations, while others have not been moved yet. Transformations
          in the new locations can at the moment not be found using the
          **TransInfo** approach, and need to be imported directly from
          the path indicated in the documentation.

.. autoclass:: psyclone.psyGen.TransInfo
    :members:

.. _sec_transformations_available:

Standard Functionality
----------------------
Each transformation must provide at least two functions for the
user: one for validation, i.e. to verify that a certain transformation
can be applied, and one to actually apply the transformation. They are
described in detail in the
:ref:`overview of all transformations<available_trans>`,
but the following general guidelines apply.

Validation
+++++++++++
Each transformation provides a function ``validate``. This function
can be called by the user, and it will raise an exception if the
transformation can not be applied (and otherwise will return nothing).
Validation will always be called when a transformation is applied.
The parameters for ``validate`` can change from transformation to
transformation, but each ``validate`` function accepts a parameter
``options``. This parameter is either ``None``, or a dictionary of
string keys, that will provide additional parameters to the validation
process. For example, some validation functions allow part of the
validation process to be disabled in order to allow the HPC expert
to apply a transformation that they know to be safe, even if the
more general validation process might reject it. Those parameters
are documented for each transformation, and will show up as
a parameter, e.g.: ``options["node-type-check"]``. As a simple
example::

    # The validation might reject the application, but in this
    # specific case it is safe to apply the transformation,
    # so disable the node type check:
    my_transform.validate(node, {"node-type-check": False})


.. _transformations_application:

Application
+++++++++++
Each transformation provides a function ``apply`` which will
apply the transformation. It will first validate the transform
by calling the ``validate`` function. Each ``apply`` function
takes the same ``options`` parameter as the ``validate`` function
described above. Besides potentially modifying the validation
process, optional parameters for the transformation are also
provided this way. A simple example::

    kctrans = Dynamo0p3KernelConstTrans()
    kctrans.apply(kernel, {"element_order": 0, "quadrature": True})

The same ``options`` dictionary will be used when calling ``validate``.

.. _available_trans:

Available transformations
-------------------------

Some transformations are generic as the schedule structure is
independent of the API, however it often makes sense to specialise
these for a particular API by adding API-specific errors checks. Some
transformations are API-specific. Currently these different types of
transformation are indicated by their names.

The generic transformations currently available are listed in
alphabetical order below (a number of these have specialisations which
can be found in the API-specific sections).

.. note:: PSyclone currently only supports OpenCL and
          KernelImportsToArguments transformations for the GOcean 1.0
          API, the OpenACC Data transformation is limited to
          the NEMO and GOcean 1.0 APIs and the OpenACC Kernels
          transformation is limited to the NEMO and Dynamo0.3 APIs.

.. note:: The directory layout of PSyclone is currently being restructured.
          As a result of this some transformations are already in the new
          locations, while others have not been moved yet.

####

.. autoclass:: psyclone.psyir.transformations.Abs2CodeTrans
      :members: apply
      :noindex:

.. warning:: This transformation assumes that the ABS Operator acts on
             PSyIR Real scalar data and does not check that this is
             not the case. Once issue #658 is on master then this
             limitation can be fixed.

####

.. autoclass:: psyclone.transformations.ACCDataTrans
    :noindex:
    :members: apply

####

.. autoclass:: psyclone.transformations.ACCEnterDataTrans
    :noindex:
    :members: apply

####

.. autoclass:: psyclone.transformations.ACCKernelsTrans
    :noindex:
    :members: apply

####

.. autoclass:: psyclone.transformations.ACCLoopTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.ACCParallelTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.ArrayRange2LoopTrans
    :members: apply
    :noindex:
  
####

.. autoclass:: psyclone.psyir.transformations.ChunkLoopTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.ColourTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.DotProduct2CodeTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.extract_trans.ExtractTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.HoistLocalArraysTrans
      :members: apply
      :noindex:

####

.. autoclass:: psyclone.psyir.transformations.HoistTrans
      :members: apply
      :noindex:

####

.. autoclass:: psyclone.transformations.KernelModuleInlineTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.LoopFuseTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.LoopSwapTrans
   :members: apply
   :noindex:

####

.. autoclass:: psyclone.psyir.transformations.LoopTiling2DTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.Matmul2CodeTrans
    :members: apply
    :noindex:

.. note:: This transformation is currently limited to translating the
          matrix vector form of MATMUL to equivalent PSyIR code.

####

.. autoclass:: psyclone.psyir.transformations.Max2CodeTrans
      :members: apply
      :noindex:

.. warning:: This transformation assumes that the MAX Operator acts on
             PSyIR Real scalar data and does not check that this is
             not the case. Once issue #658 is on master then this
             limitation can be fixed.

####

.. autoclass:: psyclone.psyir.transformations.Min2CodeTrans
      :members: apply
      :noindex:

.. warning:: This transformation assumes that the MIN Operator acts on
             PSyIR Real scalar data and does not check that this is
             not the case. Once issue #658 is on master then this
             limitation can be fixed.

####

.. _sec_move_trans:

.. autoclass:: psyclone.transformations.MoveTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.domain.gocean.transformations.GOOpenCLTrans
      :members: apply
      :noindex:

####

.. autoclass:: psyclone.transformations.OMPDeclareTargetTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPLoopTrans
    :members: apply, omp_schedule, omp_worksharing
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPMasterTrans
    :inherited-members:
    :exclude-members: name
    :noindex:

.. note:: PSyclone does not support (distributed-memory) halo swaps or
          global sums within OpenMP master regions.  Attempting to
          create a master region for a set of nodes that includes
          halo swaps or global sums will produce an error. In such
          cases it may be possible to re-order the nodes in the
          Schedule such that the halo swaps or global sums are
          performed outside the single region. The
          :ref:`MoveTrans <sec_move_trans>` transformation may be used
          for this.

####

.. autoclass:: psyclone.transformations.OMPParallelLoopTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPParallelTrans
    :inherited-members:
    :exclude-members: name
    :noindex:

.. note:: PSyclone does not support (distributed-memory) halo swaps or
          global sums within OpenMP parallel regions.  Attempting to
          create a parallel region for a set of nodes that includes
          halo swaps or global sums will produce an error. In such
          cases it may be possible to re-order the nodes in the
          Schedule such that the halo swaps or global sums are
          performed outside the parallel region. The
          :ref:`MoveTrans <sec_move_trans>` transformation may be used
          for this.

####

.. autoclass:: psyclone.transformations.OMPSingleTrans
    :inherited-members:
    :exclude-members: name
    :noindex:

.. note:: PSyclone does not support (distributed-memory) halo swaps or
          global sums within OpenMP single regions.  Attempting to
          create a single region for a set of nodes that includes
          halo swaps or global sums will produce an error. In such
          cases it may be possible to re-order the nodes in the
          Schedule such that the halo swaps or global sums are
          performed outside the single region. The
          :ref:`MoveTrans <sec_move_trans>` transformation may be used
          for this.

####

.. autoclass:: psyclone.transformations.OMPTargetTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPTaskloopTrans
    :members: apply, omp_grainsize, omp_num_tasks
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.OMPTaskwaitTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.ProfileTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.ReadOnlyVerifyTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.psyir.transformations.Sign2CodeTrans
      :members: apply
      :noindex:

.. warning:: This transformation assumes that the SIGN Operator acts
             on PSyIR Real scalar data and does not check whether or not
             this is the case. Once issue #658 is on master then this
             limitation can be fixed.

Algorithm-layer
---------------

The gocean1.0 API supports the transformation of the algorithm
layer. In the future the LFRic (dynamo0.3) API will also support
this. However, this is not relevant to the nemo API as it does not
have the concept of an algorithm layer (just PSy and Kernel
layers). The ability to transformation the algorithm layer is new and
at this time no relevant transformations have been developed.

Kernels
-------

PSyclone supports the transformation of Kernels as well as PSy-layer
code. However, the transformation of kernels to produce new kernels
brings with it additional considerations, especially regarding the
naming of the resulting kernels. PSyclone supports two use cases:

  1. the HPC expert wishes to optimise the same kernel in different ways,
     depending on where/how it is called;
  2. the HPC expert wishes to transform the kernel just once and have the
     new version used throughout the Algorithm file.

The second case is really an optimisation of the first for the case
where the same set of transformations is applied to every instance of
a given kernel.

Since PSyclone is run separately for each Algorithm in a given
application, ensuring that there are no name clashes for kernels in
the application as a whole requires that some state is maintained
between PSyclone invocations. This is achieved by requiring that the
same kernel output directory is used for every invocation of PSyclone
when building a given application. However, this is under the control
of the user and therefore it is possible to use the same output
directory for a subset of algorithms that require the same kernel
transformation and then a different directory for another subset
requiring a different transformation. Of course, such use would
require care when building and linking the application since the
differently-optimised kernels would have the same names.

By default, transformed kernels are written to the current working
directory. Alternatively, the user may specify the location to which
to write the modified code via the ``-okern`` command-line flag.

In order to support the two use cases given above, PSyclone supports
two different kernel-renaming schemes: "multiple" and "single"
(specified via the ``--kernel-renaming`` command-line flag). In the
default, "multiple" scheme, PSyclone ensures that each transformed
kernel is given a unique name (with reference to the contents of the
kernel output directory). In the "single" scheme, it is assumed that
any given kernel that is transformed is always transformed in the same
way (or left unchanged) and thus just one transformed version of it is
created. This assumption is checked by examining the Fortran code for
any pre-existing transformed version of that kernel. If another
transformed version of that kernel exists and does not match that
created by the current transformation then PSyclone will raise an
exception.

Rules
+++++

Kernel code that is to be transformed is subject to certain
restrictions. These rules are intended to make kernel transformations
as robust as possible, in particular by limiting the amount of
code that must be parsed by PSyclone (via fparser). The rules are
as follows:

1) Any variable or procedure accessed by a kernel must either be explicitly
   declared or named in the ``only`` clause of a module ``use`` statement
   within the scope of the subroutine containing the kernel implementation.
   This means that:

   1) Kernel subroutines are forbidden from accessing data using COMMON
      blocks;
   2) Kernel subroutines are forbidden from calling procedures declared via
      the EXTERN statement;
   3) Kernel subroutines must not access data or procedures made available
      via their parent (containing) module.

2) The full Fortran source of a kernel must be available to PSyclone.
   This includes the source of any modules from which it accesses
   either routines or data. (However, kernel routines are permitted to make
   use of Fortran intrinsic routines.)

For instance, consider the following Fortran module containing the
``bc_ssh_code`` kernel:

.. code-block:: fortran

  module boundary_conditions_mod
    real :: forbidden_var

  contains

    subroutine bc_ssh_code(ji, jj, istep, ssha)
      use kind_params_mod, only: go_wp
      use model_mod, only: rdt
      integer,                     intent(in)    :: ji, jj, istep
      real(go_wp), dimension(:,:), intent(inout) :: ssha
      real(go_wp) :: rtime

      rtime = real(istep, go_wp) * rdt
      ...
    end subroutine bc_ssh_code

  end module boundary_conditions_mod

Since the kernel subroutine accesses data (the ``rdt`` variable) from
the ``model_mod`` module, the source of that module must be available
to PSyclone if a transformation is applied to this kernel. Should
``rdt`` not actually be defined in ``model_mod`` (i.e. ``model_mod``
itself imports it from another module) then the source containing its
definition must also be available to PSyclone. Note that the rules
forbid the ``bc_ssh_code`` kernel from accessing the ``forbidden_var``
variable that is available to it from the enclosing module scope.

.. note:: these rules *only* apply to kernels that are the target of
      PSyclone kernel transformations.

Available Kernel Transformations
++++++++++++++++++++++++++++++++

The transformations listed below have to be applied specifically to a PSyclone
kernel. There are a number of transformations not listed here that can be
applied to either or both the PSy-layer and Kernel-layer PSyIR.

.. note:: Some of these transformations modify the PSyIR tree of both the
          InvokeSchedule where the transformed CodedKernel is located and its
          associated KernelSchedule.

####

.. autoclass:: psyclone.transformations.ACCRoutineTrans
   :noindex:
   :members:

####

.. autoclass:: psyclone.psyir.transformations.FoldConditionalReturnExpressionsTrans
   :noindex:
   :members:

####

.. autoclass:: psyclone.transformations.KernelImportsToArguments
    :members: apply
    :noindex:


.. note:: This transformation is only supported by the GOcean 1.0 API.


Applying
--------

Transformations can be applied either interactively or through a
script.

.. _sec_transformations_interactive:

Interactive
+++++++++++

To apply a transformation interactively we first parse and analyse the
code. This allows us to generate a "vanilla" PSy layer. For example::

    >>> from fparser.common.readfortran import FortranStringReader
    >>> from psyclone.parse.algorithm import Parser
    >>> from psyclone.psyGen import PSyFactory
    >>> from fparser.two.parser import ParserFactory

    >>> example_str = (
    ...     "program example\n"
    ...     "  use field_mod, only: field_type\n"
    ...     "  type(field_type) :: field\n"
    ...     "  call invoke(setval_c(field, 0.0))\n"
    ...     "end program example\n")

    >>> parser = ParserFactory().create(std="f2008")
    >>> reader = FortranStringReader(example_str)
    >>> ast = parser(reader)
    >>> invoke_info = Parser().invoke_info(ast)

    # This example uses the LFRic (dynamo0.3) API
    >>> api = "dynamo0.3"

    # Create the PSy-layer object using the invokeInfo
    >>> psy = PSyFactory(api, distributed_memory=False).create(invoke_info)

    # Optionally generate the vanilla PSy layer fortran
    >>> print(psy.gen)
      MODULE example_psy
        USE constants_mod, ONLY: r_def, i_def
        USE field_mod, ONLY: field_type, field_proxy_type
        IMPLICIT NONE
        CONTAINS
        SUBROUTINE invoke_0(field)
          TYPE(field_type), intent(in) :: field
          INTEGER df
          INTEGER(KIND=i_def) loop0_start, loop0_stop
          TYPE(field_proxy_type) field_proxy
          INTEGER(KIND=i_def) undf_aspc1_field
          !
          ! Initialise field and/or operator proxies
          !
          field_proxy = field%get_proxy()
          !
          ! Initialise number of DoFs for aspc1_field
          !
          undf_aspc1_field = field_proxy%vspace%get_undf()
          !
          ! Set-up all of the loop bounds
          !
          loop0_start = 1
          loop0_stop = undf_aspc1_field
          !
          ! Call our kernels
          !
          DO df=loop0_start,loop0_stop
            field_proxy%data(df) = 0.0
          END DO
          !
        END SUBROUTINE invoke_0
      END MODULE example_psy

We then extract the particular schedule we are interested
in. For example::

    # List the various invokes that the PSy layer contains
    >>> print(psy.invokes.names)
    dict_keys(['invoke_0'])

    # Get the required invoke
    >>> invoke = psy.invokes.get('invoke_0')

    # Get the schedule associated with the required invoke
    > schedule = invoke.schedule
    > print(schedule.view())
    InvokeSchedule[invoke='invoke_0', dm=True]
        0: Loop[type='dof', field_space='any_space_1', it_space='dof', upper_bound='ndofs']
            Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
            Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
            Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
            Schedule[]
                0: BuiltIn setval_c(field,0.0)

Now we have the schedule we can create and apply a transformation to
it to create a new schedule and then replace the original schedule
with the new one. For example::

    # Create an OpenMPParallelLoopTrans
    > from psyclone.transformations import OMPParallelLoopTrans
    > ol = OMPParallelLoopTrans()

    # Apply it to the loop schedule of the selected invoke
    > ol.apply(schedule.children[0])
    > print(schedule.view())

    # Generate the Fortran code for the new PSy layer
    > print(psy.gen)

.. _sec_transformations_script:

Script
++++++

PSyclone provides a Python script (**psyclone**) that can be used from
the command line to generate PSy layer code and to modify algorithm
layer code appropriately. By default this script will generate
"vanilla" (unoptimised) PSy-layer and algorithm layer code. For
example::

    > psyclone algspec.f90
    > psyclone -oalg alg.f90 -opsy psy.f90 -api dynamo0.3 algspec.f90

The **psyclone** script has an optional **-s** flag which allows the
user to specify a script file to modify the PSy layer as
required. Script files may be specified without a path. For
example::

    > psyclone -s opt.py algspec.f90

In this case the Python search path **PYTHONPATH** will be used to try
to find the script file.

Alternatively, script files may be specified with a path. In this case
the file is expected to be found in the specified location. For
example ...
::

    > psyclone -s ./opt.py algspec.f90
    > psyclone -s ../scripts/opt.py algspec.f90
    > psyclone -s /home/me/PSyclone/scripts/opt.py algspec.f90

PSyclone also provides the same functionality via a function (which is
what the **psyclone** script calls internally).

###.. autofunction:: psyclone.generator.generate
###          :noindex:

A valid script file must contain a **trans** function which accepts a **PSy**
object as an argument and returns a **PSy** object, i.e.:
::

    >>> def trans(psy):
    ...     # ...
    ...     return psy

It is up to the script what it does with the PSy object. The example
below does the same thing as the example in the
:ref:`sec_transformations_interactive` section.
::

    >>> def trans(psy):
    ...     from psyclone.transformations import OMPParallelLoopTrans
    ...     invoke = psy.invokes.get('invoke_0_v3_kernel_type')
    ...     schedule = invoke.schedule
    ...     ol = OMPParallelLoopTrans()
    ...     ol.apply(schedule.children[0])
    ...     return psy

In the gocean1.0 API (and in the future the lfric (dynamo0.3) API) an
optional **trans_alg** function may also be supplied. This function
accepts **PSyIR** (reprenting the algorithm layer) as an argument and
returns **PSyIR** i.e.:
::

   >>> def trans_alg(psyir):
   ...     # ...
   ...    return psyir

As with the `trans()` function it is up to the script what it does with
the algorithm PSyIR. Note that the `trans_alg()` script is applied to
the algorithm layer before the PSy-layer is generated so any changes
applied to the algorithm layer will be reflected in the **PSy** object
that is passed to the `trans()` function.

For example, if the `trans_alg()` function in the script merged two
`invoke` calls into one then the **Alg** object passed to the
`trans()` function of the script would only contain one schedule
associated with the merged invoke.

Of course the script may apply as many transformations as is required
for a particular algorithm and/or schedule and may apply
transformations to all the schedules (i.e. invokes and/or kernels)
contained within the PSy layer.

Examples of the use of transformation scripts can be found in many of
the examples, such as examples/lfric/eg3 and
examples/lfric/scripts. Please read the examples/lfric/README file
first as it explains how to run the examples (and see also the
examples/check_examples script).

An example of the use of a script making use of the `trans_alg()`
function can be found in examples/gocean/eg7.

OpenMP
------

OpenMP is added to a code by using transformations. The OpenMP
transformations currently supported allow the addition of:

* an **OpenMP Parallel** directive
* an **OpenMP Target** directive
* an **OpenMP Declare Target** directive
* an **OpenMP Do/For/Loop** directive
* an **OpenMP Single** directive
* an **OpenMP Master** directive
* an **OpenMP Taskloop** directive
* multiple **OpenMP Taskwait** directives; and
* an **OpenMP Parallel Do** directive.

The generic versions of these transformations (i.e. ones that
theoretically work for all APIs) were given in the
:ref:`sec_transformations_available` section. The API-specific versions
of these transformations are described in the API-specific sections of
this document.

.. _openmp-reductions:

Reductions
++++++++++

PSyclone supports parallel scalar reductions.  If a scalar reduction is
specified in the Kernel metadata (see the API-specific sections for
details) then PSyclone ensures the appropriate reduction is performed.

In the case of distributed memory, PSyclone will add **GlobalSum's**
at the appropriate locations. As can be inferred by the name, only
"summation" reductions are currently supported for distributed memory.

In the case of an OpenMP parallel loop the standard reduction support
will be used by default. For example
::

    !$omp parallel do, reduction(+:x)
    !loop
    !$omp end parallel do

OpenMP reductions do not guarantee to give bit reproducible results
for different runs of the same problem even if the same problem is run
using the same resources. The reason for this is that the order in
which data is reduced is not mandated.

Therefore, an additional **reprod** option has been added to the
**OpenMP Do** transformation. If the reprod option is set to "True"
then the OpenMP reduction support is replaced with local per-thread
reductions which are reduced serially after the loop has
finished. This implementation guarantees to give bit-wise reproducible
results for different runs of the same problem using the same
resources, but will not bit-wise compare if the code is rerun with
different numbers of OpenMP threads.

Restrictions
++++++++++++

If two reductions are used within an OpenMP region and the same
variable is used for both reductions then PSyclone will raise an
exception. In this case the solution is to use a different variable
for each reduction.

PSyclone does not support (distributed-memory) halo swaps or global
sums within OpenMP parallel regions.  Attempting to create a parallel
region for a set of nodes that includes halo swaps or global sums will
produce an error.  In such cases it may be possible to re-order the
nodes in the Schedule using the :ref:`MoveTrans <sec_move_trans>`
transformation.

OpenMP Tasking
++++++++++++++
PSyclone supports OpenMP Tasking, through the `OMPTaskloopTrans` and
`OMPTaskwaitTrans` transformations. `OMPTaskloopTrans`
transformations can be applied to loops, whilst the `OMPTaskwaitTrans`
operator is applied to an OpenMP Parallel Region, and computes the dependencies
caused by Taskloops, and adds OpenMP Taskwait statements to satisfy those
dependencies. An example of using OpenMP tasking is available in 
`PSyclone/examples/nemo/eg1/openmp_taskloop_trans.py`.

OpenCL
------

OpenCL is added to a code by using the ``GOOpenCLTrans`` transformation (see the
:ref:`sec_transformations_available` Section above).
Currently this transformation is only supported for the GOcean1.0 API and
is applied to the whole InvokeSchedule of an Invoke.
This transformation will add an OpenCL driver infrastructure to the PSy layer
and generate an OpenCL kernel for each of the Invoke kernels.
This means that all kernels in that Invoke will be executed on the OpenCL
device.
The PSy-layer OpenCL code generated by PSyclone is still Fortran and makes use
of the FortCL library (https://github.com/stfc/FortCL) to access
OpenCL functionality. It also relies upon the device acceleration support
provided by the dl_esm_inf library (https://github.com/stfc/dl_esm_inf).


.. note:: The generated OpenCL kernels are written in a file called
    opencl_kernels_<index>.cl where the index keeps increasing if the
    file name already exist.


The ``GOOpenCLTrans`` transformation accepts an `options` argument with a
map of optional parameters to tune the OpenCL host code in the PSy layer.
These options will be attached to the transformed InvokeSchedule.
The current available options are:

+------------------+----------------------------------------------+---------+
| Option           |  Description                                 | Default |
+==================+==============================================+=========+
| end_barrier      | Whether a synchronization                    | True    |
|                  | barrier should be placed at the end of the   |         |
|                  | Invoke.                                      |         |
+------------------+----------------------------------------------+---------+
| enable_profiling | Enables the profiling of OpenCL Kernels.     | False   |
+------------------+----------------------------------------------+---------+
| out_of_order     | Allows the OpenCL implementation to execute  | False   |
|                  | the enqueued kernels out-of-order.           |         |
+------------------+----------------------------------------------+---------+

Additionally, each individual kernel (inside the Invoke that is going to
be transformed) also accepts a map of options which
are provided by the `set_opencl_options()` method of the `Kern` object.
This can affect both the driver layer and/or the OpenCL kernels.
The current available options are:

+--------------+---------------------------------------------+---------+
| Option       |  Description                                | Default |
+==============+=============================================+=========+
| local_size   | Number of work-items to group together      | 64      |
|              | in a work-group execution (kernel instances |         |
|              | executed at the same time).                 |         |
+--------------+---------------------------------------------+---------+
| queue_number | The identifier of the OpenCL command_queue  | 1       |
|              | to which the kernel should be submitted. If |         |
|              | the kernel has a dependency on another      |         |
|              | kernel submitted to a different             |         |
|              | command_queue a barrier will be added to    |         |
|              | guarantee the execution order.              |         |
+--------------+---------------------------------------------+---------+


Below is an example of a PSyclone script that uses a ``GOOpenCLTrans`` with
multiple InvokeSchedule and kernel-specific optimization options.


.. literalinclude:: ../../examples/gocean/eg3/ocl_trans.py
    :language: python
    :linenos:
    :lines: 39-79


OpenCL delays the decision of which and where kernels will execute until
run-time, therefore it is important to use the environment variables provided
by FortCL and DL_ESM_INF to inform how things should execute. Specifically:

- ``FORTCL_KERNELS_FILE``: Point to the file containing the kernels to execute,
  they can be compiled ahead-of-time or providing the source for JIT
  compilation. To link more than a single kernel, one must merge all the
  kernels generated by PSyclone in a single source file.
- ``FORTCL_PLATFORM``: If the system has more than 1 OpenCL platform. This
  environment variable may be used to select which platform on which to execute
  the kernels.
- ``DL_ESM_ALIGNMENT``: When using OpenCL <= 1.2 the local_size should be
  exactly divisible by the total size. If this is not the case some
  implementations fail silently. A way to solve this issue is to set the
  `DL_ESM_ALIGNMENT` variable to be equal to the local size.


.. note:: The OpenCL generation can be combined with distributed memory
    generation. In the case where there is more than one accelerator available
    on each node, the PSyclone configuration file parameter
    ``OCL_DEVICES_PER_NODE`` has to be set to the appropriate value and
    the number of MPI-ranks-per-node set by the `mpirun` command has to match
    this value accordingly.

    For instance if there are 2 accelerators per nodes, `psyclone.cfg` should
    have ``OCL_DEVICES_PER_NODE=2`` and the program must be executed with
    ``mpirun -n <total_ranks> -ppn 2 ./application`` (Note: `-ppn` is an
    Intel MPI specific parameter, use equivalent configuration parameters for
    other MPI implementations.)


For example, an execution of a PSyclone generated OpenCL code using all the
mentioned run-time configuration options could look something like::

    FORTCL_PLATFORM=3 FORTCL_KERNELS_FILE=allkernels.cl DL_ESM_ALIGNMENT=64 \
    mpirun -n 2 ./application.exe

OpenACC
-------

PSyclone supports the generation of code targetting GPUs through the
addition of OpenACC directives. This is achieved by a user applying
various OpenACC transformations to the PSyIR before the final Fortran
code is generated. The steps to parallelisation are very similar to
those in OpenMP with the added complexity of managing the movement of
data to and from the GPU device. For the latter task PSyclone provides
the ``ACCDataTrans`` and ``ACCEnterDataTrans`` transformations, as
described in the :ref:`sec_transformations_available` Section above.
These two transformations add statically- and dynamically-scoped data
regions, respectively. The former manages what data is on the remote
device for a specific section of code while the latter allows run-time
control of data movement. This second option is essential for
minimising data movement as, without it, PSyclone-generated code would
move data to and from the device upon every entry/exit of an
Invoke. The first option is mainly provided as an aid to incremental
porting and/or debugging of an OpenACC application as it provides
explicit control over what data is present on a device for a given
(part of an) Invoke routine.

The PGI compiler provides an alternative approach to controlling data
movement through its 'unified memory' option
(``-ta=tesla:managed``). When this is enabled the compiler itself takes
on the task of ensuring that data is copied to/from the GPU when
required. (Note that this approach can struggle with Fortran code
containing derived types however.)

As well as ensuring the correct data is copied to and from the remote
device, OpenACC directives must also be added to a code in order to
tell the compiler how it should be parallelised. PSyclone provides the
``ACCKernelsTrans``, ``ACCParallelTrans`` and ``ACCLoopTrans``
transformations for this purpose. The simplest of these is
``ACCKernelsTrans`` (currently only supported for the NEMO and
Dynamo0.3 APIs) which encloses the code represented by a sub-tree of
the PSyIR within an OpenACC ``kernels`` region.  This essentially
gives free-reign to the compiler to automatically parallelise any
suitable loops within the specified region. An example of the use of
``ACCDataTrans`` and ``ACCKernelsTrans`` may be found in
PSyclone/examples/nemo/eg3 and an example of ``ACCKernelsTrans`` may
be found in PSyclone/examples/lfric/eg14.

However, as with any "automatic" approach, a more performant solution
can almost always be obtained by providing the compiler with more
explicit direction on how to parallelise the code.  The
``ACCParallelTrans`` and ``ACCLoopTrans`` transformations allow the
user to define thread-parallel regions and, within those, define which
loops should be parallelised. For an example of their use please see
PSyclone/examples/gocean/eg2 or PSyclone/examples/lfric/eg14.

In order for a given section of code to be executed on a GPU, any
routines called from within that section must also have been compiled
for the GPU.  This then requires either that any such routines are
in-lined or that the OpenACC ``routine`` directive be added to any
such routines.  This situation will occur routinely in those PSyclone
APIs that use the PSyKAl separation of concerns since the
user-supplied kernel routines are called from within
PSyclone-generated loops in the PSy layer. PSyclone therefore provides
the ``ACCRoutineTrans`` transformation which, given a Kernel node in
the PSyIR, creates a new version of that kernel with the ``routine``
directive added. Again, please see PSyclone/examples/gocean/eg2 for an
example. This transformation is currently not supported for kernels in
the Dynamo0.3 API.

SIR
---

It is currently not possible for PSyclone to output SIR code without
using a script. Two examples of such scripts are given in example 4
for the NEMO API, one of which includes transformations to remove
PSyIR intrinsics, hoist code out of a loop, translate array-index
notation into explicit loops and translate a single access to an array
dimension to a one-trip loop (to make the code suitable for the SIR
backend).
