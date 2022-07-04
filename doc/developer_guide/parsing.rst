.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office

.. testsetup::

    # Define SOURCE_FILE to point to an existing gocean 1.0 file.
    SOURCE_FILE = ("../../src/psyclone/tests/test_files/"
        "gocean1p0/test11_different_iterates_over_one_invoke.f90")

Parsing Code
############

The original way to parse code was to use the PSyclone `parse` module
which is responsible for parsing science (algorithm and kernel) code
and extracting the required information for the algorithm translation
and PSy generation phases. This is gradually being replaced by the use
of the PSyIR and its front-ends and back-ends.

The current status is that the GOcean API uses PSyIR to capture and
output algorithm code. This is achieved by first reading the algorithm
file into generic PSyIR, then specialising the PSyIR (raising) to have
GOcean-specific classes for invoke and kernel calls, then applying any
transformations if required, then lowering the GOcean-specific classes
back down to generic PSyIR (which also translates invokes and kernel
calls to an appropriate call to the PSy-layer) and finally using the
Fortran back-end to output the tranformed code. The same approach is
in development for algorithm-layer code for the LFRic API, but
currently the original approach is used.

In the original approach the `parse` module contains modules for
parsing algorithm (`algorithm.py`) and kernel (`kernel.py`) code as
well as a utility module (`utils.py`) for common functionality. This
approach is discussed further in the following sections.

Parsing Algorithm Code
======================

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
===============

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
==============================

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
example, in the case of the `dynamo0.3` API, the class is
`DynKernMetadata` which is found in `psyclone.dynamo0p3`. Once this
instance has been created (by passing it an `fparser1` parse tree) it can
return information about the metadata contained therein. Moving from
`fparser1` to `fparser2` would required changing the parse code logic
in each of the API-specific classes.

