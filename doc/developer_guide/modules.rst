.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
.. Modified by L. Turner, Met Office

Modules
#######

This section describes the functionality of the various Python modules
that make up PSyclone.

.. _dev_configuration:

Module: configuration
======================

PSyclone uses the Python ``ConfigParser`` class
(https://docs.python.org/3/library/configparser.html) for reading the
configuration file. This is managed by the ``psyclone.configuration``
module which provides a ``Config``
class. This class is a singleton, which can be (created and) accessed
using  ``Config.get()``. Only one such instance will ever exist:

.. autoclass:: psyclone.configuration.Config
    :members:

The ``Config`` class is responsible for finding the configuration file
(if no filename is passed to the constructor), parsing it and then storing
the various configuration options.
If PSyclone is started via ``pytest``, the environment variable
``PSYCLONE_CONFIG`` is set to ``<PSYCLONEHOME/config>``. This will
guarantee that all tests use the config file provided in the PSyclone
repository, and not a (potentially modified) user installed version.

The ``Config`` class also stores the list of supported
APIs (``Config._supported_api_list``). Additionally, it performs some basic
consistency checks on the values it obtains from the configuration file.

Since the PSyclone API to use can be read from the configuration
file, it is not possible to have API-specific sub-classes of ``Config``
as we don't know which API is in use before we read the file. However, the
configuration file can contain API-specific settings. These are placed in
separate sections, named for the API to which they apply, e.g.::

  [lfric]
  COMPUTE_ANNEXED_DOFS = false

Having parsed and stored the options from the default section of the
configuration file, the ``Config`` constructor then creates a
dictionary using the list of supported APIs to provide the keys. The
configuration file is then checked for API-specific sections (again
using the API names from the default section) and, if any are found,
an API-specific sub-class is created using the parsed entries from the
corresponding section. The resulting object is stored in the
dictionary under the appropriate key. The API-specific values may then
be accessed as, e.g.::

  Config.get().api_conf("lfric").compute_annexed_dofs

The API-specific sub-classes exist to provide validation/type-checking and
encapsulation for API-specific options. They do not sub-class ``Config``
directly but store a reference back to the ``Config`` object to which they
belong.

Constants Objects
-----------------
Each API provides a specific object that stores required constants.
Most of these constants are hard-coded in the object, but
some are taken from a section of the configuration file.
The constants are provided as class variables, but an instance of
it needs to be created (at least once) in order to make sure all
class variables are initialised. It is therefore recommended to
always use an instance of the corresponding constant class to access
these constants. The constant objects make sure that this initialisation
only happens the very first time - creating an instance is therefore
very cheap.

There two constant objects can be imported as follows:

- ``from psyclone.domain.gocean import GOceanConstants``
- ``from psyclone.domain.lfric import LFRicConstants``

These objects can be used in two different ways:

  #) If the API is known, e.g. because the constant is used in an
     API-specific file, an instance can simply be
     created and used, e.g.:

       .. code-block:: python

         from psyclone.domain.lfric import LFRicConstants

         const = LFRicConstants()

         if var is in const.VALID_LOOP_BOUNDS_NAMES:
             ...

     This usage pattern can be seen in many API-specific files.

  #) In some cases a value of an API-specific constant is required
     in a generic function. In this case the API-specific constant
     object can be accessed using the config file as follows:

       .. code-block:: python

         from psyclone.configuration import Config

         const = Config.get().api_conf().get_constants()

         if some_variable is in const.VALID_INTRINSIC_TYPES:
             ...

     This pattern is used in some functions
     that can be called with different APIs. The following
     constants are used this way:

     - ``VALID_ARG_TYPE_NAMES``
     - ``VALID_INTRINSIC_TYPES``
     - ``VALID_SCALAR_NAMES``
     - ``VALID_LOOP_TYPES``

     These are the only variables that are defined across all
     constant objects.

Module: transformations
=======================

As one might expect, the transformations module holds the various
transformation classes that may be used to modify the Schedule of an
Invoke and/or the kernels called from within it.

.. note:: The directory layout of PSyclone is currently being restructured.
          As a result of this some transformations are already in the new
          locations, while others have not been moved yet.

The base class for any transformation must be the class ``Transformation``:

.. autoclass:: psyclone.psyGen.Transformation
    :members:
    :private-members:

Those transformations that work on a region of code (e.g. enclosing
multiple kernel calls within an OpenMP region) must sub-class the
``RegionTrans`` class:

.. autoclass:: psyclone.psyir.transformations.RegionTrans
    :members:
    :private-members:
    :noindex:

In all cases, the `apply` method of any sub-class *must* ensure that
the `validate` method of the parent class is called.

Module: psyGen
==============

Provides the base classes for PSy-layer code generation.

Module: dynamo0p3
=================

Specialises various classes from the ``psyclone.psyGen`` module
in order to support the LFRic API.

When constructing the Fortran subroutine for either an Invoke or
Kernel stub (see :ref:`stub-generation`), there are various groups of
related quantities for which variables must be declared and
(for Invokes) initialised. Each of these groupings is managed by a distinct
sub-class of the ``LFRicCollection`` abstract class:

.. autoclass:: psyclone.domain.lfric.LFRicCollection
   :members:
   :private-members:
   :noindex:

A single ``LFRicCollection`` class is used for both Invokes and Kernel stubs
since it allows the code dealing with variable declarations to be shared.
A concrete sub-class of ``LFRicCollection`` must provide an
implementation of the ``invoke_declarations`` method. If the
quantities associated with the collection require initialisation
within the PSy layer then the ``initialise`` method must also be
implemented. If stub-generation is to be supported for kernels that
make use of the collection type then an implementation must also be
provided for ``stub_declarations``.

Although instances of (sub-classes of) ``LFRicCollection`` handle all
declarations and initialisation, there remains the problem of
constructing the list of arguments for a kernel (or kernel stub). The
``psyclone.domain.lfric.ArgOrdering`` base class provides support for
this:

.. autoclass:: psyclone.domain.lfric.ArgOrdering
    :members:
    :private-members:
    :noindex:

This class is then sub-classed in order to support the generation of
argument lists when *calling* kernels (``KernCallArgList``) and when
*creating* kernel stubs (``KernStubArgList``).
``KernStubArgList`` is only used in ``LFRicKern.gen_stub()``. These
classes make use of ``LFRicCollection`` sub-classes in order
to ensure that argument naming is consistent.
