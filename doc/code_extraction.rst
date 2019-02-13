.. _code_extraction:

PSy Kernel Extractor (PSyKE)
============================

PSyclone has the ability to define regions that can be extracted as
stand-alone applications. The code extraction is currently enabled by
utilising an optimisation script. 

PSyKE (PSy Kernel Extractor) will allow part of a PSyclone conformant code
 to be extracted and run separately. This is potentially useful
 for benchmarking and for removing/reducing the need to use the
 LFRic infrastructure. An earlier implementation that never made
 it onto master was limited to individual kernels. 
In this implementation we will allow any nodes within an
 invoke to be specified.

In the first instance we will add a new Node class called Extract and
 an associated transformation called ExtractTrans which
 adds an instance of this Node class to a schedule.
 For example ...

schedule
    loop ...
        kernel ...
    extract
        loop ...
            builtin ...
        halo exchange ...
       loop ...
           kernel ...
           kernel ...

This extract class should simply add a comment 
`! extract start` and `! extract end` at appropriate locations to
 start off with. All tests and examples should run as before 
- note some errors may be found due to code assuming parenthood
 rules e.g. it may be assumed that a loop is always a child of
 a schedule. This code should be added as base classes so
 it works for both dynamo0p3 and gocean1p0.

It is not unreasonable to limit the extract to being added
 after all optimising transformations have been completed
 if that helps i.e. if extract gets in the way of 
optimising transformations and causes them to fail 
then disallowing it when optimising transformations 
are being applied is OK.

.. _ExtractAPI:

Extract API
-----------

This module provides support for extraction of code within a specified
invoke. The extracted code may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke (extraction
applied to all Nodes).

It marks region for code extraction as children of the `ExtractNode`. For
now it inserts comments at the position of the `ExtractNode` and after all
children of the `ExtractNode`. These comments will later be replaced by
calls to write out arguments of extracted Node(s) or Kernel(s).


ExtractNode
+++++++++++

This class can be inserted into a Schedule to mark Nodes for 
code extraction using the ExtractRegionTrans transformation. By 
applying the transformation the Nodes marked for extraction become
children of an ExtractNode.


Extractor
+++++++++

This class contains is helper functions for code extraction.
For now it only provides the function to extract the specific Kernel
from an Invoke Schedule. Another planned functionality is to wrap
settings for generating driver for the extracted code.

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


Code extraction in Scripts - ExtractRegionTransform
---------------------------------------------------

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

Limitations
+++++++++++

Please note that `RegionTrans` for consecutive Nodes in an
Invoke Schedule (the Nodes also need to be children of the same parent).

Specific to `ExtractRegionTrans` - raises `TransformationError` for validity
checks on Node lists

* if distributed memory is configured,
* if transformation is applied to the list of Nodes which already contain
  an ExtractNode (otherwise we would have an extract region within another extract region),
* if transformation is applied to a Kernel or a BuiltIn call without its
  parent Loop,
* if transformation is applied to a Loop without its parent Directive when
  optimisations are applied,
* if transformation is applied to an orphaned Directive (e.g. OMPDoDirective,
  ACCLoopDirective) without its parent Directive (e.g. ACC or OMP Parallel Directive),
* if transformation is applied to a Loop over cells in a colour without its
  parent Loop over colours in Dynamo0.3 API,
* if transformation is applied to an inner Loop without its parent outer
  Loop in GOcean1.0 API.

Also, distributed memory is not supported.