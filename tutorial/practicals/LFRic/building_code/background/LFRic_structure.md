# LFRic code structure

This section gives a brief overview of the LFRic code structure and
functionality of each layer in the [*separation of concerns hierarchy*](
LFRic_intro.md#separation-of-concerns).

## Driver layer

The driver layer in LFRic sets up (e.g. domain, partition, science
configuration and controls a model run (e.g. time-step loop, checkpoint),
including wrappers to external libraries.

The setting-up part mainly consists of setting up the LFRic object stack
in the following order: **global 2D mesh** -> **partition** ->
**local 3D partitioned mesh** -> **function space** -> **field** (see
e.g. [this full LFRic example](
../../../../../examples/lfric/eg17/full_example/README.md) and individual
tutorials for more details).

Both the driver and the [algorithm layer](#algorithm-layer) can create
fields, with the globally used fields set up in the driver and passed to
algorithms. The set up of mesh and model configuration, however, is always
done in the driver layer.

## Algorithm layer

*Note: See also the description of the LFRic Algorithm layer in the*
[*PSyclone User Guide.*](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#algorithm)

LFRic algorithms perform mathematical operations on LFRic objects:
**field**s, **operator**s and **scalar**s. Each of these objects
contains data and functionality to manipulate this data (for instance,
a field can write itself out). For the purpose of this documentation
they will be referred to as "LFRic data objects".

The important thing to note here is that algorithms (as well as other
high-level code such as drivers) operate on [full objects](
https://psyclone.readthedocs.io/en/stable/algorithm_layer.html) but
**do not operate directly on object data** (or, in OO terminology, *must
not break encapsulation*). The data is accessed in the [PSy layer](
#psy-layer) via the required object accessor class, referred to as
a *proxy* in the LFRic code.

The above mentioned classes are defined in the LFRic infrastructure as:

* Fields: `field_type` and `integer_field_type`;
* Operators: `operator_type` and `columnwise_operator_type`;
* Scalars: `scalar_type`;

and their implementations can be found in similarly-named modules in
the `infrastructure` directory of the LFRic repository.

The **object data is private**, whilst the methods (Fortran procedures)
can be `private` (if used only by the object) or `public`ly available
(e.g. a method to initialise a field or return a pointer to it).

LFRic algorithms manipulate data objects via `invoke` calls to
[user-defined LFRic kernels](#kernel-layer) and [PSyclone built-ins](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins).
The `invoke` calls are not standard Fortran calls. They are a way of
specifying a sequence of kernels to call (with their associated
arguments), prescribed by the DSL defined in the
[PSyclone LFRic (Dynamo 0.3) API](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html). The use of
`invoke`s in LFRic is exclusive to the algorithm layer.

Roughly speaking, the main parts of an LFRic algorithm are:
* Definitions of global data objects (e.g. fields) and supporting
  structures (e.g. function space, mesh);
* `invoke` calls to kernels and built-ins.

All of the above is stored in a module. The naming convention of an
algorithm module is not as strict as for [kernels](
../1_simple_kernels/LFRic_kernel_structure.md) and it can be summarised
as `<base_name>_alg_mod`. Also, unlike for kernels, it is usual for an
algorithm to have more than one subroutine (for instance, algorithms
that perform time-stepping in LFRic often have `<base_name>_init`,
`<base_name>_step` and `<base_name>_final` subroutines for different
requirements in a time-stepping scheme).

## PSy layer

*Note: See also the description of the LFRic PSy layer in the*
[*PSyclone User Guide.*](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#psy-layer)

As explained in the [*Algorithm layer* section](#algorithm-layer),
LFRic algorithms perform mathematical operations on LFRic data objects
and pass them as arguments to kernel and built-in calls in `invoke`s.
As explained in the [*Kernel layer* section](#kernel-layer),
LFRic kernels perform mathematical operations on a subset of data
points of these objects.

In the *Parallel Systems* or, in short, [PSy layer](
https://psyclone.readthedocs.io/en/stable/psy_layer.html), PSyclone
generates calls to the accessor classes for the LFRic data objects
passed from the algorithm layer, referred to as `<class_name>_proxy`
(e.g. `field_proxy`) and accesses the object data by dereferencing
the object proxies. The data and other required information is then
passed to kernel and built-in calls.

PSyclone also generates code for [distributed](../../distributed_memory) and
[shared memory](../../single_node) support, however this is not utilised
in this tutorial as explained in the main [README document](../README.md).
The generated PSy-layer code in these tutorials is purely serial.

## Kernel layer

*Note: See also the description of the LFRic Kernel layer in the*
[*PSyclone User Guide.*](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#kernel)

LFRic kernels perform mathematical operations on [a subset of data points](
https://psyclone.readthedocs.io/en/stable/kernel_layer.html#kernel-layer)
of LFRic data objects (fields, operators and scalars) passed
from the [algorithm](#algorithm-layer) through the [PSy layer](#psy-layer).

Scalar values are passed to a kernel from the [algorithm layer](
#algorithm-layer) as they are. In the case of fields and
operators, however, LFRic kernels operate on a subset of the degrees
of freedom (DoFs) of these objects, e.g. those on a single column of
cells or even a single DoF. Such a subset occupies a portion
of the computational domain (e.g. an entire mesh or a partition of a
mesh that the field's DoFs are placed on).

LFRic kernels have a strictly prescribed structure. For the illustration
on this structure and how the relevant information is passed to a kernel
please refer to the [*LFRic kernel structure* section](
../1_simple_kernels/LFRic_kernel_structure.md) of the first tutorial on
[building and using kernels in LFRic](../1_simple_kernels).
