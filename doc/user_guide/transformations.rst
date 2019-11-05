.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2019, Science and Technology Facilities Council
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
.. Written by: R. W. Ford and A. R. Porter, STFC Daresbury Lab.
..             I. Kavcic, Met Office.

.. _transformations:

Transformations
===============

As discussed in the previous section, transformations can be applied
to a schedule to modify it. Typically transformations will be used to
optimise the PSy layer for a particular architecture, however
transformations could be added for other reasons, such as to aid
debugging or for performance monitoring.

Finding
-------

Transformations can be imported directly, but the user needs to know
what transformations are available. A helper class **TransInfo** is
provided to show the available transformations

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

Most transformations are generic as the schedule structure is
independent of the API, however it often makes sense to specialise
these for a particular API by adding API-specific errors checks. Some
transformations are API-specific (or specific to a set of APIs
e.g. dynamo). Currently these different types of transformation are
indicated by their names.

The generic transformations currently available are listed in
alphabetical order below (a number of these have specialisations which
can be found in the API-specific sections).

.. note:: PSyclone currently only supports OpenCL transformations for
	  the GOcean 1.0 API, the OpenACC Data transformation is
	  limited to the NEMO and GOcean 1.0 APIs and the OpenACC
	  Kernels transformation is limited to the NEMO and Dynamo0.3
	  APIs.

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

.. autoclass:: psyclone.transformations.ColourTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.ExtractRegionTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.KernelModuleInlineTrans
    :members: apply
    :noindex:

.. note:: PSyclone does not currently permit module-inlining of
	  transformed kernels (issue #229).

####

.. autoclass:: psyclone.transformations.LoopFuseTrans
    :members: apply
    :noindex:

####

.. _sec_move_trans:

.. autoclass:: psyclone.transformations.MoveTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.OCLTrans
      :members: apply
      :noindex:

####

.. autoclass:: psyclone.transformations.OMPLoopTrans
    :members: apply, omp_schedule
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPParallelLoopTrans
    :members: apply
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPParallelTrans
    :inherited-members:
    :exclude-members: name, psyGen
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

.. autoclass:: psyclone.transformations.ProfileRegionTrans
    :members: apply
    :noindex:

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

PSyclone currently provides just one kernel transformation:

.. autoclass:: psyclone.transformations.ACCRoutineTrans
   :noindex:
   :members:

.. note:: PSyclone does not currently permit transformed kernels to be
	  module-inlined. (Issue #229.)

Applying
--------

Transformations can be applied either interactively or through a
script.

.. _sec_transformations_interactive:

Interactive
+++++++++++

To apply a transformation interactively we first parse and analyse the
code. This allows us to generate a "vanilla" PSy layer. For example ...
::

    from psyclone.parse.algorithm import parse
    from psyclone.psyGen import PSyFactory

    # This example uses version 0.1 of the Dynamo API
    api = "dynamo0.1"

    # Parse the file containing the algorithm specification and
    # return the Abstract Syntax Tree and invokeInfo objects
    ast, invokeInfo = parse("dynamo.F90", api=api)

    # Create the PSy-layer object using the invokeInfo
    psy = PSyFactory(api).create(invokeInfo)

    # Optionally generate the vanilla PSy layer fortran
    print psy.gen

We then extract the particular schedule we are interested
in. For example ...
::

    # List the various invokes that the PSy layer contains
    print psy.invokes.names

    # Get the required invoke
    invoke = psy.invokes.get('invoke_0_v3_kernel_type')

    # Get the schedule associated with the required invoke
    schedule = invoke.schedule
    schedule.view()


Now we have the schedule we can create and apply a transformation to
it to create a new schedule and then replace the original schedule
with the new one. For example ...
::

    # Get the list of possible loop transformations
    from psyclone.psyGen import TransInfo
    t = TransInfo()
    print t.list

    # Create an OpenMPLoop-transformation
    ol = t.get_trans_name('OMPParallelLoopTrans')

    # Apply it to the loop schedule of the selected invoke
    new_schedule,memento = ol.apply(schedule.children[0])
    new_schedule.view()

    # Replace the original loop schedule of the selected invoke
    # with the new, transformed schedule
    invoke.schedule=new_schedule

    # Generate the Fortran code for the new PSy layer
    print psy.gen

More examples of use of the interactive application of transformations
can be found in the runme*.py files within the examples/dynamo/eg1 and
examples/dynamo/eg2 directories. Some simple examples of the use of
transformations are also given in the previous section.

.. _sec_transformations_script:

Script
++++++

PSyclone provides a Python script (**psyclone**) that can be used from
the command line to generate PSy layer code and to modify algorithm
layer code appropriately. By default this script will generate
"vanilla" (unoptimised) PSy layer code. For example::

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

.. autofunction:: psyclone.generator.generate
		  :noindex:

A valid script file must contain a **trans** function which accepts a **PSy**
object as an argument and returns a **PSy** object, i.e.:
::

    def trans(psy)
        ...
        return psy

It is up to the script what it does with the PSy object. The example
below does the same thing as the example in the
:ref:`sec_transformations_interactive` section.
::

    def trans(psy):
	from psyclone.transformations import OMPParallelLoopTrans
        invoke = psy.invokes.get('invoke_0_v3_kernel_type')
        schedule = invoke.schedule
        ol = OMPParallelLoopTrans()
        new_schedule, _ = ol.apply(schedule.children[0])
        invoke.schedule = new_schedule
	return psy

Of course the script may apply as many transformations as is required
for a particular schedule and may apply transformations to all the
schedules (i.e. invokes) contained within the PSy layer.

Examples of the use of transformation scripts can be found in the
examples/dynamo/eg3 and examples/dynamo/scripts directories. Please
read the examples/dynamo/README file first as it explains how to run
the examples (and see also the examples/check_examples script).

OpenMP
------

OpenMP is added to a code by using transformations. The three
transformations currently supported allow the addition of an
**OpenMP Parallel** directive, an **OpenMP Do** directive and an
**OpenMP Parallel Do** directive, respectively, to a code.

The generic versions of these three transformations (i.e. ones that
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

OpenCL
------

OpenCL is added to a code by using the ``OCLTrans`` transformation (see the
:ref:`sec_transformations_available` Section above).
Currently this transformation is only supported for the GOcean1.0 API and
is applied to the whole InvokeSchedule of an Invoke.
This transformation will add an OpenCL driver infrastructure to the PSy layer
and generate an OpenCL kernel for each of the Invoke kernels.
This means that all kernels in that Invoke will be executed on the OpenCL
device.
The PSy-layer OpenCL code generated by PSyclone is still Fortran and makes use
of the FortCL library (https://github.com/stfc/FortCL) to access
OpenCL functionality. It also relies upon the OpenCL support provided
by the dl_esm_inf library (https://github.com/stfc/dl_esm_inf).


.. note:: The generated OpenCL files follow the `--kernel-renaming` argument
    conventions, but in addition to the modulename they also include the
    kernelname as part of the filename in the format:
    `modulename_kernelname_index.cl`



The ``OCLTrans`` transformation accepts an `options` argument with a
map of optional parameters to tune the OpenCL host code in the PSy layer.
These options will be attached to the transformed InvokeSchedule.
The current available options are:

+--------------+----------------------------------------------+---------+
| Option       |  Description                                 | Default |
+==============+==============================================+=========+
| end_barrier  | Whether a synchronization                    | True    |
|              | barrier should be placed at the end of the   |         |
|              | Invoke.                                      |         |
+--------------+----------------------------------------------+---------+

Additionally, each individual kernel (inside the Invoke that is going to
be transformed) also accepts a map of options which
are provided by the `set_opencl_options()` method of the `Kern` object.
This can affect both the driver layer and/or the OpenCL kernels.
The current available options are:

+--------------+---------------------------------------------+---------+
| Option       |  Description                                | Default |
+==============+=============================================+=========+
| local_size   | Number of work-items to compute             | 1       |
|              | in a single kernel.                         |         |
+--------------+---------------------------------------------+---------+
| queue_number | The identifier of the OpenCL Command Queue  | 1       |
|              | to which the kernel should be submitted.    |         |
+--------------+---------------------------------------------+---------+


Below is an example of a PSyclone script that uses an ``OCLTrans`` with
multiple InvokeSchedule and kernel-specific optimization options.


.. literalinclude:: ../../examples/gocean/eg3/ocl_trans.py
    :language: python
    :linenos:
    :lines: 51-65


.. note:: The OpenCL support is still in active development and the options
    presented above should be considered at risk of changing or being implemented
    as transformations in the near future.


Because OpenCL kernels are linked at run-time, it will be up to the run-time
environment to specify which of the kernels to use. For instance, one could
merge multiple kernels together in a single binary file and
use the `PSYCLONE_KERNELS_FILE` provided by the FortCL library.

The introduction of OpenCL code generation in PSyclone has been
largely motivated by the need to target Field Programmable Gate Array
(FPGA) accelerator devices. It is not currently designed to target the other
compute devices that OpenCL supports (such as GPUs and multi-core CPUs) but
this is a potentially fruitful area for future work.

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
be found in PSyclone/examples/dynamo/eg14.

However, as with any "automatic" approach, a more performant solution
can almost always be obtained by providing the compiler with more
explicit direction on how to parallelise the code.  The
``ACCParallelTrans`` and ``ACCLoopTrans`` transformations allow the
user to define thread-parallel regions and, within those, define which
loops should be parallelised. For an example of their use please see
PSyclone/examples/gocean/eg2 or PSyclone/examples/dynamo/eg14.

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
using a script. Examples of such scripts are given in example 4 for
the NEMO API. Whilst there are no transformations relating to the
generation of the SIR, a script is associated with transformations and
it is possible that transformations could be useful in the future
e.g. to mark which bits of code should be optimised using the dawn
tool.
