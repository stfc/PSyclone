*** Please add this documentation to the GOcean section of the
developers documentation when the developers documentation is added to
master. ***

The logic for checking and storing stencil information is contained in
the `GOStencil` class in the `gocean1p0.py` file. This class is
written so that it can be instantiated in isolation from the rest of
`PSyclone` with the actual input data being added via a `load()`
method. This should help with testing and for adding new types of data
loading in the future if required.

The strategy for capturing stencil information differs between the
`GOcean1.0` and `Dynamo0.3` APIs. The `GOcean` strategy of keeping
everything in a stencil class is arguably better.

An instance of the `GOStencil` class is attached to all field and
scalar argument objects (the `GOKernelArgument` class) with the name
`stencil`.

A grid argument (class `GOKernelGridArgument`) will not have a stencil
attribute, however a scalar and a field argument will always have a
stencil attribute with a valid `GOStencil` object. In this case the
stencil object can be queried to see if the argument has a stencil or
not using the `has_stencil` method. This might seem strange, but it
was the natural way to implement stencil support given the existing
use of the pointwise stencil attribute to mean that there is no
stencil access.

The use of `pointwise` is redundant for scalars and also for fields
with `write` or `readwrite` access (because kernels are only permitted
to modify the 'current' i,j'th grid-point in the field). At the
moment this information must be provided in any case but the
implementation could be changed to only require stencil information
when there is a stencil access. This would avoid the need for the
`pointwise` keyword altogether.

There is an expectation that the API will support stencil names, such
as `5-point-stencil`. The `VALID_STENCIL_NAMES = [...]` list within
the `GOcean1p0.py` file has been kept for this purpose as well as the
`name` attribute in the `GOStencil` class. The expected approach would
be to add new names and then provide a mapping to the actual stencil
layout which could be used to populate the appropriate stencil
information in the associated `GOStencil` object. One option that has
been discussed is to allow users to specify their own names and
mappings.
