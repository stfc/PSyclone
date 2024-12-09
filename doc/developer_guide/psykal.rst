
.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
.. Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
.. Modified by I. Kavcic and L. Turner, Met Office

.. _develop_psykal_dsl:

Developing PSyKAL DSLs
######################

.. testsetup::

    # Define SOURCE_FILE to point to an existing gocean 1.0 file.
    SOURCE_FILE = ("../../src/psyclone/tests/test_files/"
        "gocean1p0/test11_different_iterates_over_one_invoke.f90")


Parsing DSL Code (new approach)
===============================

The GOcean and LFRic APIs support the concept of algorithm, psy and
kernel layers.

Kernel-layer Fortran written by users consists of the kernel code
itself and metadata describing the kernel code.  PSyclone needs this
metadata to generate the PSy-layer code. The new approach takes the
generic PSyIR representation of the kernel metadata (which is actually
captured as a string within a PSyIR UnsupportedFortranType as the generic
PSyIR does not understand its structure) and 'raises' this into
domain-specific classes (using the ``RaisePSyIR2LFRicKernelTrans`` and
``RaisePSyIR2GOceanKernelTrans`` transformations for the LFRic and
GOcean API's, respectively). These classes allow the metadata to be
simply read when generating psy-layer code, but also to be simply
modified if required (e.g. when generating adjoint code - see the
:ref:`user guide <psyad_user_guide:introduction>` for more
details). As with existing code, these domain-specific classes can be
'lowered' to produce generic PSyIR and PSyclone's back-ends used to
output the resultant metadata and code.

Algorithm-layer Fortran, also written by users, consists of ``invoke``
calls containing references to kernels. PSyclone needs the information
contained in an invoke call to generate the PSy-layer code. Another
one of PSyclone's roles is to transform the algorithm code and
specifically the invoke calls within the algorithm code. In order to
support these requirements in the new approach the algorithm code
is first parsed into generic PSyIR. The generic PSyIR is then 'raised'
to either GOcean-specific algorithm code or LFRic-specific algorithm
code depending on the API (using the ``RaisePSyIR2AlgTrans`` and
``RaisePSyIR2LFRicAlgTrans`` transformations respectively) making use
of domain-specific PSyIR classes for the invoke calls
(``AlgorithmInvokeCall`` and ``LFRicAlgInvokeCall`` respectively) and
references to kernels (``KernelFunctor`` and ``LFRicKernelFunctor``
respectively).

The domain-specific algorithm layer PSyIR is then lowered back to generic
PSyIR and at the same time the invoke calls replaced with calls to the
generated PSy-layer. This is achieved using the
``GOceanAlgInvoke2PSyCallTrans`` and ``LFRicAlgInvoke2PSyCallTrans``
transformations for the GOcean and LFRic APIs respectively, with these
transformations operating on individual ``AlgorithmInvokeCall`` or
``LFRicAlgorithmInvokeCall`` nodes rather than the whole of the
domain-specific algorithm PSyIR.

The new approach described in this section is not yet fully
implemented in PSyclone. The current status is that it is used in the
NEMO API to transform code and is used in the GOcean API to modify
algorithm code. The GOcean and LFRic APIs are also able to raise
kernel metadata to domain-specific classes, but these classes are not
yet used by the the rest of PSyclone (see `generator.py` for the
relevant GOcean code and prototype LFRic code).


Parsing DSL Code (original approach)
====================================

The original way to parse DSL code is to use the PSyclone `parse` module
which is responsible for parsing science (algorithm and kernel) code
and extracting the required information for the algorithm translation
and PSy generation phases. The original approach is gradually being
replaced by the use of the PSyIR and its front-ends and back-ends
(please see the previous section for more details).

The `parse` module contains modules for parsing algorithm
(`algorithm.py`) and kernel (`kernel.py`) code as well as a utility
module (`utils.py`) for common functionality. This implementation is
discussed further in the following sections.

Parsing Algorithm Code
++++++++++++++++++++++

The first thing PSyclone typically does is parse an input file. This
input file is expected to contain Fortran source code conforming to
the particular API in question. For the `nemo` API this is standard
code, for the other API's this is algorithm code (conforming to the
PSyKAl separation of concerns). The PSyclone code to do this is found
in `parse/algorithm.py`.

An input file can be parsed via the `parse` function or via an
instance of the `Parser` class. In practice the `parse` function
simply calls the `Parser` class so we will concentrate on the latter
in this section. The `parse` function could be removed from PSyclone
but it is simple and is used in existing PSyclone scripts and
examples.

The `Parser` class is initialised with a number of optional
arguments. A particular `api` can be specified (this is required so
the parser knows what sort of code and metadata to expect, how to
parse it and which `built-ins` are supported). The name used to specify
an invoke (defaulting to `invoke`) can be changed, a path to where to
look for associated kernel files can be provided and a particular
maximum line length can be specified.

.. autoclass:: psyclone.parse.algorithm.Parser

Once an instance of the `Parser()` class is created and configured
with required values for the optional arguments, then the parse method
can be called. This takes the name of the input code as its argument
and returns a parse tree of the input code and a FileInfo object that
captures the required invoke information found in the input code and
in the associated kernel codes.

If the `nemo` API is specified then the `parse` method of the `Parser`
instance simply parses the code with `fparser2` and returns the
resultant `fparser2` parse tree.

For all other APIs the `parse` method of the `Parser` instance returns
the resultant `fparser2` parse tree and a `FileInfo` instance which
captures the invoke and kernel metadata in a hierarchy of classes.

When the `Parser` instance parses the code it expects to find Fortran
code containing a program, module, subroutine or function (and it
aborts if not). Currently the first of these (there may be more than
one subroutine for example) is assumed to be the one that is
required. This limititation is captured in issue #307.

The native `fparser2` tree of the Fortran code is then walked and all
use statement names are captured and stored in a map (called
`_arg_name_to_module_name`). This map allows the module name to be
found given a particular argument name. Also, if an `invoke` call is
found then an `InvokeCall` object is created and added to a list of
such instances. Once all invokes have been found the `FileInfo`
instance is created.

.. note:: In the future we want to be able to simply replace one parse
          tree with another. So how should we do this? One option
          would be to try to minimise the parser-specific parts and
          create some form of interface. The question is really what
          level of interface we should use.

An `InvokeCall` object is created in the `create_invoke_call` method
by first parsing each of the kernels specified in an invoke call and
creating a list of `kernel` objects which are then used to create
an `InvokeCall`. These objects capture information on the way each
kernel is being called from the Algorithm layer.

A `kernel` object is created in the `create_kernel_call` method which
extracts the kernel name and kernel arguments, then creates either an
`algorithm.BuiltInCall` instance (via the `create_builtin_kernel_call`
method) or an `algorithm.KernelCall` instance (via the
`create_coded_kernel_call` method). `BuiltInCalls` are created if the
kernel name is the same as one of those specified in the built-in names
for this particular API (see the variable `_builtin_name_map` which is
initialised by the `get_builtin_defs` function).

The `create_kernel_call` method uses the `get_kernel` function to find
out the kernel name and create a list of `Arg` instances representing
the arguments. The `get_kernel` function parses each kernel argument
using the fparser2 AST and determines the required argument
information. An advantage of `fparser2` when compared with `fparser1`
is that it parses all of a code, so we can use the parse tree to
determine the type of each kernel argument appearing in the `invoke`
call (e.g. scalar variable, array reference, literal constant) and
create the appropriate `Arg` instance. Previously we relied on the
`expression` module to do this (which has limitations).

.. note:: the analysis in the `get_kernel` function is the place to
          extend if we were to support arithmetic operations in an
          invoke call.

Mixed Precision
+++++++++++++++

Support for mixed precision kernels has been added to PSyclone. The
approach being taken is for the user to provide kernels with a generic
interface and precision-specific implementations. As the PSyclone kernel
metadata does not specify precision, this does not need to change. The
actual precision being used will be specified by the scientist in the
algorithm layer (by declaring variables with appropriate precision).

.. highlight:: fortran

For example::

    module kern_mod
      type kern_type
        ! metadata description
      contains
	procedure, nopass :: kern_code_32, kern_code_64
        generic :: kern_code => kern_code_32, kern_code_64
      end type kern_type
    contains
      subroutine kern_code_32(arg)
        real*4 :: arg
        print *, "kern_code_32"
      end subroutine kern_code_32
      subroutine kern_code_64(arg)
        real*8 :: arg
        print *, "kern_code_64"
      end subroutine kern_code_64
    end module kern_mod

    program alg
      use kern_mod, only : kern_type
      real*4 :: a
      real*8 :: b
      call invoke(kern_type(a), kern_type(b))
    end program alg

In the above example, the first call to kern will call `kern_code_32`
and the second will call `kern_code_64`. Note, the actual code will
use types for arguments in the algorithm layer, but for clarity
precision has been used.

In order to support the above mixed precision kernel implementation,
the PSy-layer generated by PSyclone must declare data passed into an
invoke call with the same precision as is declared in algorithm layer,
i.e. in the above example variable `a` must be declared as `real*4`
and variable `b` as `real*8`. The only way for PSyclone to determine
the required precision for a variable is by extracting the information
from the algorithm layer.

To support the extraction of precision information, the name and
precision of a variable are stored in a dictionary when the algorithm
layer is parsed (see `psyclone.parser.algorithm.py`). This allows
PSyclone to look up the precision of a variable when it is specified
in an `invoke` call. The reason for implementing support in this way
is because `fparser2` does not currently support a symbol table,
therefore there is no link between variables names and their types. In
the future, the algorithm layer will be translated into PSyIR, which
does have a symbol table, so this dictionary will no longer be
required (see issue #753).

All declarations that specify variables or types are stored in the
dictionary, including those specified within locally defined
types. Variables may also be single valued or arrays.

.. highlight:: fortran

For example::

    program alg
      use my_mod, only : my_type
      real(kind=r_def) :: rscalar
      type(my_type) :: field(10)
      type fred
        integer(kind=i_def) :: iscalar
      end type fred
    end program alg

As the current implementation only stores variable names and does not
know about variable scope there is a restriction that any variables
within an algorithm code with the same name must have the same
precision/type. This restriction will be removed when the algorithm
layer is translated to PSyIR (see issue #753). The current
implementation could be improved but in practice the lfric code does
not fall foul of this restriction.

There is also a constraint that invoke arguments cannot be expressions
(involving variables) or functions as it is then difficult to
determine the datatype of the argument. However, arbitrary structures
and arrays are supported, as are literal expressions.

.. highlight:: fortran

For example the following are supported (where `b`, `f`, `g` and `i`
are arrays, not functions)::

    program alg
      ! declare vars
      call invoke(kern(a, b(c), d%e, f(10)%g(4)%h, self%i(j), 1, 1.0*2.0))
    end program alg

.. highlight:: fortran

But the following are not::

    program alg
      ! delare vars
      call invoke(kern(a%b(), c*d, e+1.0))
    end program alg

The other issue is that, in general, it may not be possible to
determine the type/precision of a variable from the algorithm layer
code. In particular, the variable may be included from another module
via use association. Potential solutions to this problem are 1)
disallow this in the algorithm layer, 2) use a naming convention for
the module and/or variable to determine its precision, or 3) search the
modules for datatype information. At the moment only 1) or 2) will be
feasible solutions. When we move to using the PSyIR (see issue #753),
it may be possible to support 3).

	  
Parsing Kernel Code (Metadata)
++++++++++++++++++++++++++++++

An `algorithm.BuiltInCall` instance is created by being passed a
`kernel.BuiltinKernelType` instance for the particular API via the
`BuiltInKernelTypeFactory` class which is found in the `parse.kernels`
module. This class parses the Fortran module file which specifies
built-in description metadata. Currently `fparser1` is used but we will
be migrating to `fparser2` in the future. The built-in metadata is
specified in the same form as coded kernel metadata so the same logic
can be used (i.e. the `KernelTypeFactory.create` method is called)
which is why `BuiltInKernelTypeFactory` subclasses
`KernelTypeFactory`.

An `algorithm.KernelCall` instance is created by being passed the
module name of the kernel and a `kernel.KernelType` instance for the
particular API via the `KernelTypeFactory` class which is also found
in the `parse.kernel` module. This class is given the parsed kernel
module (via the `get_kernel_ast` function - which searches for the
kernel file using the kernel path information introduced
earlier). Again, currently `fparser1` is used but we will be migrating
to `fparser2` in the future.

The `KernelTypeFactory create` method is used for both coded kernels
and built-in kernels to specify the API-specific class to use. As an
example, in the case of the `lfric` API, the class is
`LFRicKernMetadata` which is found in `psyclone.domain.lfric`. Once this
instance has been created (by passing it an `fparser1` parse tree) it can
return information about the metadata contained therein. Moving from
`fparser1` to `fparser2` would required changing the parse code logic
in each of the API-specific classes.

The PSy-Invokes-Invoke-InvokeSchedule tree
==========================================

The PSy-layer of a single algorithm file is represented by the **PSy** class.
The PSy class has an **Invokes** object which contains one or more
**Invoke** instances (one for each ``invoke`` in the algorithm layer).
Each **Invoke** has an **InvokeSchedule** object which is the PSyIR tree
that describes the corresponding PSy-layer subroutine.
This subroutine is called by the Algorithm layer and itself calls one or
more kernels and/or implements any required Built-in operations.

All these classes can be specialised in each PSyclone API to support the
specific features of a particular API. The class diagram for the above base classes
is shown below using the LFRic API as an illustration. This class diagram
was generated from the source code with pyreverse and edited with inkscape.

.. image:: dynamo0p3_topclasses.png
    :width: 80%
    :align: center

The InvokeSchedule can currently contain nodes of type: **Loop**,
**Kernel**, **Built-in** (see the :ref:`built-ins` section),
**Directive** (of various types), **HaloExchange**, or
**GlobalSum** (the latter two are only used if distributed memory is
supported and is switched on; see the :ref:`psykal_usage`
section). The order of the tree (depth first) indicates the order of
the associated Fortran code.

Adding new Built-in operations to an API
========================================

To add a new built-in operation into an API use the following steps:

 1. Identify the PSyclone source file for the API to be extended. *e.g.* for
    the LFRic API it is ``src/psyclone/domain/lfric/lfric_builtins.py``.
 2. Edit this source file to create the class for this new call. It must
    inherit from the API-specific parent class for Built-in operations
    (``LFRicBuiltInKern`` for the LFRic API).
 3. Implement ``__str__`` and ``lower_to_language_level()`` methods for
    this new class.
 4. Add the name of the new Built-in operation and its corresponding class
    to the ``BUILTIN_MAP`` dictionary in that source file.
 5. Add metadata describing this call to the appropriate file specified in
    the ``BUILTIN_DEFINITIONS_FILE`` in that source file. For the LFRic API
    this is ``src/psyclone/parse/lfric_builtins_mod.f90``.
 6. Add relevant tests to the PSyclone test files for the API to be extended.
    *e.g.* for the LFRic API they are
    * ``src/psyclone/tests/domain/lfric/lfric_builtins_test.py``,
    * ``src/psyclone/tests/domain/lfric/lfric_integer_builtins_test.py``.
    The tests rely on ``single_invoke`` Fortran examples in the relevant
    ``src/psyclone/tests/test_files/`` subdirectory.
 7. Add an appropriate Fortran ``single_invoke`` example for the new
    Built-in in the relevant ``src/psyclone/tests/test_files/`` subdirectory.
    *e.g.* for the LFRic API it is
    ``src/psyclone/tests/test_files/dynamo0p3/``.
    Names of examples follow the template
    ``<category.number>.<subcategory.number>_<built-in_name>.f90``.
    *e.g.* for the LFRic API ``<category.number>`` is 15 and
    ``<built-in_name>`` follows the :ref:`LFRic API Built-in naming
    scheme <lfric-built-ins-names>`.
 8. Document the new Built-in in the documentation of the
    relevant API (*e.g.* ``doc/dynamo0p3.rst`` for LFRic (Dynamo0.3) API).


If the API being extended does not currently support any Built-ins
then the ``BUILTIN_MAP`` and ``BUILTIN_DEFINITIONS_FILE`` module
variables must be added to the source file for the API.  A Fortran
module file must be created in the PSyclone ``src/parse`` directory
(with the name specified in ``BUILTIN_DEFINITIONS_FILE``) containing
metadata describing the Built-in operations. Finally,
``parse.get_builtin_defs()`` must be extended to import
``BUILTIN_MAP`` and ``BUILTIN_DEFINITIONS_FILE`` for this API.

