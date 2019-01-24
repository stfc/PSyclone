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

Available
---------

Most transformations are generic as the schedule structure is
independent of the API, however it often makes sense to specialise
these for a particular API by adding API-specific errors checks. Some
transformations are API-specific (or specific to a set of API's
e.g. dynamo). Currently these different types of transformation are
indicated by their names.

The generic transformations currently available are listed in
alphabetical order below (a number of these have specialisations which
can be found in the API-specific sections).

.. note:: PSyclone currently only supports OpenACC transformations
	  for the GOcean 1.0 API. Attempts to apply these
	  transformations to (members of) Schedules from other
	  APIs will be rejected.

####

.. autoclass:: psyclone.transformations.ACCDataTrans
    :noindex:
    :members:

####

.. autoclass:: psyclone.transformations.ACCLoopTrans
    :members:
    :noindex:

####

.. autoclass:: psyclone.transformations.ACCParallelTrans
    :members:
    :inherited-members:
    :noindex:

####
	       
.. autoclass:: psyclone.transformations.ColourTrans
    :members:
    :noindex:

####

.. autoclass:: psyclone.transformations.KernelModuleInlineTrans
    :members:
    :noindex:

.. note:: PSyclone does not currently permit module-inlining of
	  transformed kernels (issue #229).

####

.. autoclass:: psyclone.transformations.LoopFuseTrans
    :members:
    :noindex:

####

.. _sec_move_trans:

.. autoclass:: psyclone.transformations.MoveTrans
    :members:
    :noindex:

####

.. autoclass:: psyclone.transformations.ProfileRegionTrans
    :members:
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPLoopTrans
    :members:
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPParallelLoopTrans
    :members:
    :noindex:

####

.. autoclass:: psyclone.transformations.OMPParallelTrans
    :members:
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

Kernels
-------

PSyclone supports the transformation of Kernels as well as PSy-layer
code. However, the transformation of kernels to produce new kernels
brings with it additional considerations, especialy regarding the
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

    from parse import parse
    from psyGen import PSyFactory

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
    from psyGen import TransInfo
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
	from transformations import OMPParallelLoopTrans
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
transformations currently supported allow the addition of an **OpenMP
Parallel** directive, an **OpenMP Do** directive and an **OpenMP
Parallel Do** directive, respectively, to a code.

The generic versions of these three transformations (i.e. ones that
theoretically work for all API's) were given in the
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
