.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
.. Written by J. Henrichs, Bureau of Meteorology

.. highlight:: fortran

.. _psy_data:

.. testsetup::

    # Define SOURCE_FILE to point to an existing gocean 1.0 file.
    SOURCE_FILE = ("../../src/psyclone/tests/test_files/"
        "gocean1p0/test11_different_iterates_over_one_invoke.f90")

PSyData API
===========
PSyclone provides transformations that will insert callbacks to
an external library at runtime. These callbacks allow third-party
libraries to access data structures at specified locations in the
code.

Introduction to PSyData Classes
-------------------------------
The PSyData API imports a user-defined data type from a PSyData
module and creates an instance of this data type for each code
region. It then adds a sequence of calls to methods in that
instance. A simplified example::

    USE psy_data_mod, ONLY: PSyDataType
    TYPE(PSyDataType), target, save :: psy_data

    CALL psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)
    ...

In order to allow several different callback libraries to be used
at the same time, for example to allow in-situ visualisation at
the same time as checking that read-only values are indeed not modified,
different module names and data types must be used.

PSyData divides its application into different classes. For example,
the class "profile" is used for all profiling tools (e.g. DrHook or the
NVIDIA profiling tools). This class name is used as a prefix for
the module name, the ``PSyDataType`` and functions. So if a profiling application
is linked the above code will actually look like this::

    USE profile_psy_data_mod, ONLY: profile_PSyDataType
    TYPE(profile_PSyDataType), target, save :: profile_psy_data

    CALL profile_psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)

.. note::
    While adding the class prefix to the name of the instance variable
    is not necessary, it helps improves the readability of the created code.

The list of valid class prefixes is specified in the configuration file. It
can be extended by the user to support additional classes::

    [DEFAULT]
    ...
    VALID_PSY_DATA_PREFIXES = profile, extract

The class prefixes supported at the moment are:

.. tabularcolumns:: |l|L|

======================= =======================================================
Class Prefix            Description
======================= =======================================================
profile                 All libraries related to profiling tools like DrHook,
                        NVIDIA's profiling tools etc. See
                        :ref:`user_guide:profiling` for details.
extract                 For libraries used for kernel data extraction. See
                        :ref:`user_guide:psyke` for details.
read_only_verify        Use a checksum to verify that variables that are 
                        read-only are not modified in a subroutine.
======================= =======================================================

In the following documentation the string ``PREFIX_`` is used
to indicate the class prefix used (e.g. ``profile``).

.. note:: 
    The transformations for profiling or kernel extraction allow
    to overwrite the default-prefix (see 
    :ref:`psy_data_parameters_to_constructor`).
    This can be used to link with two different libraries of the
    same class at the same time, e.g. you could use ``drhook_profile``
    and ``nvidia_profile`` as class prefixes. However, this would also
    require that the corresponding wrapper libraries be modified to use
    this new prefix.

Full Example
------------
The following example shows the full code created by PSyclone for a
kernel extraction (excluding declarations for user data). This code
is automatically inserted by the various transformations that are
based on ``PSyDataTrans``, like ``ProfileTrans`` and
``GOCeanExtractTrans``. More details can be found in
:ref:`psy_data_transformation`. ::

    USE extract_psy_data_mod, ONLY: extract_PSyDataType
    TYPE(extract_PSyDataType), target, save :: extract_psy_data

    CALL extract_psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)
    CALL extract_psy_data%PreDeclareVariable("a_fld", a_fld)
    CALL extract_psy_data%PreDeclareVariable("b_fld", b_fld)
    CALL extract_psy_data%PreEndDeclaration
    CALL extract_psy_data%ProvideVariable("a_fld", a_fld)
    CALL extract_psy_data%PreEnd

    DO j=1,jstop+1
      DO i=1,istop+1
        CALL update_field_code(i, j, a_fld%data, b_fld%data)
      END DO 
    END DO 
    
    CALL extract_psy_data%PostStart
    CALL extract_psy_data%ProvideVariable("b_fld", b_fld)
    CALL extract_psy_data%PostEnd


#. A user defined type ``PREFIX_PSyDataType`` is imported from the module
   ``PREFIX_psy_data_mod``.
#. A static variable of type ``PREFIX_PSyDataType`` is declared.
#. ``PreStart`` is called to indicate that a new that a new instrumented code
   region is started.
#. ``PreDeclareVariable`` is called for each variable passed to the data API
   either before or after the instrumented region.
#. ``PreEndDeclaration`` is called to indicate the end of the variable
   declarations.
#. ``ProvideVariable`` is called for each variable to be passed to the wrapper
   library before the instrumented region.
#. ``PreEnd`` is called to signal the end of all PSyData activity before the
   instrumented region.
#. Then the actual instrumented code region is entered.
#. After the instrumented region, a call to ``PostStart``
   is added to indicate that all further data output occurs after the
   instrumented region.
#. For each variable to be passed on to the wrapper library after the
   instrumented region a call to ``ProvideVariable`` is added.
#. A call to ``PostEnd`` is added once all variables have been provided.

.. note::
    Depending on the options provided to the PSyData transformation,
    some of the calls might not be created. For example, for a performance
    profiling library no variables will be declared or provided.



.. _psy_data_api:

API
---
This section describes the actual PSyData API in detail. It contains
all functions, data types and methods that need to be implemented for
a wrapper library.

The PSyData API includes two function calls that allow initialisation and
shut down of a wrapper library. These two calls must be inserted
manually into the program, and their location might depend on 
the wrapper library used - e.g. some libraries might need to be
initialised before ``MPI_Init`` is called, others might need to
be called after. Similarly the shutdown function might need to
be called before or after ``MPI_Finalize``.

 .. note::
    Not all PSyData libraries require these calls (for example,
    the NVIDIA profiling library does not need it, the data
    extraction libraries do not need it, ... ), but any PSyData
    library should include (potentially empty) subroutines in
    order to avoid linking problems. It is the responsibility
    of the user to decide if the initialisation and shutdown
    calls are unnecessary.

Similarly, the PSyData API also includes two function calls that allow
programmatic control of whether or not the underlying data-capture
mechanism is active. If this functionality is required then these
calls must be inserted manually into the program. They are intended to
be used e.g. when profiling only a part of a program's execution or
perhaps to switch on/off output of data for on-line visualisation. As
with the initialisation and shutdown subroutines, any PSyData library
should include implementations of these routines, even if they are
empty.

 .. note::
    Currently only the profiling wrapper libraries and
    read-only-verification libraries implement the Start and Stop
    routines. Wider support for all PSyData-based APIs will be addressed
    in Issue #824.


Init and Shutdown Functions
+++++++++++++++++++++++++++
.. method:: PREFIX_PSyDataInit()

    Initialises the wrapper library used. It takes no parameters, and must
    be called exactly once before any other PSyData-related function
    is invoked. Example::

        use profile_psy_data_mod, only : profile_PSyDataInit
        ...
        call profile_PSyDataInit()

.. method:: PREFIX_PSyDataShutdown()

    Cleanly shuts down the wrapper library used. It takes no parameters,
    and must be called exactly once. No more PSyData-related functions
    can be called after ``PSyDataShutdown`` has been executed. Example::

        use profile_psy_data_mod, only : profile_PSyDataShutdown
        ...
        call profile_PSyDataShutdown()

.. _psydata_start_stop_functions:

Start and Stop Functions
++++++++++++++++++++++++
.. method:: PREFIX_PSyDataStart()

   Currently not implemented in the kernel extraction wrapper.

   Starts or enables the PSyData library so that subsequent calls to
   the API cause data to be output. For instance, if we have a time-stepping
   application where ``timestep`` holds the value of the current time step
   then we could turn on profiling after the first 5 steps by doing::

       use profile_psy_data_mod, only: profile_PSyDataStart
       ...
       if(timestep == 6) call profile_PSyDataStart()

   (Assuming that profiling was disabled at application start by the
   runtime environment or by a call to ``profile_PSyDataStop`` - see below.)

   This routine may be called any number of times but must be after
   ``PSyDataInit()`` and before ``PSyDataShutdown()`` (if present).

.. method:: PREFIX_PSyDataStop()

   Currently not implemented in the kernel extraction wrapper.

   Stops or disables the PSyData library so that subsequent calls to
   the PSyData API have no effect. Continuing the above time-stepping
   example, we could turn off profiling after time step 10 by doing::

       use profile_psy_data_mod, only: profile_PSyDataStop
       ...
       if(timestep == 11) call profile_PSyDataStop()

   This routine may be called any number of times but must be after
   ``PSyDataInit()`` and before ``PSyDataShutdown()`` (if present).

.. _psy_data_type:

``PREFIX_PSyDataType``
++++++++++++++++++++++
The library using the PSyData API must provide a user-defined data type
called ``PREFIX_PSyDataType``. It is up to the application how this variable is
used. PSyclone will declare the variables to be static, meaning that they
can be used to accumulate data from call to call. An example of
the PSyDataType can be found in the NetCDF example extraction code
(see ``lib/extract/netcdf/dl_esm_inf``, or :ref:`user_guide:psyke_netcdf` for
a detailed description), any of the profiling wrapper libraries
(all contained in ``lib/profiling``) or the read_only wrappers
(in ``lib/read_only``).

.. method:: PreStart(this, module_name, kernel_name, num_pre_vars, num_post_vars)

    This is the first method called when the instrumented region is
    executed. It takes 4 parameters (besides the implicit ``PREFIX_PSyDataType``
    instance):
    
    ``module_name``
      This is the name of the module in which the original Fortran source
      code is contained. Together with ``kernel_name`` it can be used to
      create a unique name for each instrumented region.

    ``kernel_name``
      The name of the kernel that is being executed.
    
    ``num_pre_vars``
      This is the number of variables that will be supplied using
      ``ProvideVar`` before the instrumented region is executed.
    
    ``num_post_vars``
      This is the number of variables that will be supplied using
      ``ProvideVar`` after the instrumented region is executed.
      The sum ``num_pre_vars+num_post_vars`` is the number of
      variable declarations that will follow.
    
    Typically the static ``PREFIX_PSyDataType`` instance can be used to store
    the module and kernel names if they are required later, or to allocate
    arrays to store variable data. This call is always created, even if
    no variables are to be provided.

.. method:: PreDeclareVariable(this, name, value)

    This method is called for each variable that will be written
    before or after the user-instrumented region. If a variable
    is written both before and after the region, the transformations will
    add two calls to ``PreDeclareVariable`` (it can be useful to
    provide a variable using a different name before and after,
    see :ref:`user_guide:psyke_netcdf`). If no variables are to be
    provided to the wrapper library, this call will not be created
    (and there is no need to implement this function in a wrapper
    library).
    
    ``name``
      This is the name of the variable as a string.
    
    ``value``
      This is the actual content of the variable.
    
    The same call is used for different arguments, so a generic
    interface is recommended to distinguish between
    the data types provided. The netcdf kernel writer 
    (see :ref:`user_guide:psyke_netcdf`) uses the following declaration
    (with types defined in the dl_esm_inf library)::    
    
        generic, public :: PreDeclareVariable => DeclareScalarInteger, &
                                                 DeclareScalarReal,    &
                                                 DeclareFieldDouble
        ...
        subroutine DeclareScalarInteger(this, name, value)
            implicit none
            class(extract_PSyDataType), intent(inout), target :: this
            character(*), intent(in) :: name
            integer, intent(in) :: value
        ...
        subroutine DeclareScalarReal(this, name, value)
            implicit none
            class(extract_PSyDataType), intent(inout), target :: this
            character(*), intent(in) :: name
            real, intent(in) :: value
        ...
        subroutine DeclareFieldDouble(this, name, value)
            use field_mod, only : r2d_field
            implicit none
            class(extract_PSyDataType), intent(inout), target :: this
            character(*), intent(in) :: name
            type(r2d_field), intent(in) :: value
        ...

.. method:: PreEndDeclaration(this)

    Called once all variables have been declared. This call is only
    inserted if any variables are to be provided either before or after
    the instrumented region (thus this call is not created for
    performance profiling).

.. method:: ProvideVariable(this, name, value)

    This method is called for each variable to be provided to the
    runtime library. 
    
    ``name``
      This is the name of the variable as a string.
    
    ``value``
      This is the actual content of the variable.
    
    The same method ``ProvideVariable`` is called to provide variable
    name and content before and after the user-instrumented region.
    Again it is expected that a library using the API will provide
    a generic interface to distinguish between the various possible data
    types, which will be different for each infrastructure library::
    
        generic, public :: ProvideVariable => WriteScalarInteger, &
                                              WriteScalarReal,    &
                                              WriteFieldDouble
    
.. method:: PreEnd(this)

    The method ``PreEnd`` is called after all variables have been
    provided before the instrumented region. This call is also not
    inserted if no variables are provided.


.. method:: PostStart(this)

    This is the first call after the instrumented region. It does not take
    any parameters, but the static ``PREFIX_PSyDataType`` instance can be used
    to store the name and number of variables if required. This will be
    followed by calls to ``ProvideVariable``, which is described above.
    This call is not used if no variables are provided.

.. method:: PostEnd(this)

    This method is the last call after an instrumented region. It indicates
    that all variables have been provided. It will always be created,
    even if no variables are to be provided.

    .. note::

        Only the ``PreDataStart`` call takes the module-
        and region-name as parameters. If these names are required
        by the PSyData runtime library in different calls, they must
        be stored in the ``PSyData`` object so that they are
        available when required.

.. _psy_data_transformation:

``PSyDataTrans``
----------------
Any transformation that uses the PSyData API works
by inserting a special node into the PSyclone tree representation
of the program. Only at program creation time is the
actual code created that implements the API. The
``PSyDataTrans`` transformation contained in
``psyir/transformations/psy_data_trans.py`` is the base
class for other transformations like profiling and
kernel data extraction. All derived transformations
mostly add specific validations, and provide
parameters to ``PSyDataTrans``, including the class
of the node to insert. After passing validation,
``PSyDataTrans`` creates an instance of the class
requested, and inserts it into the tree.

.. autoclass:: psyclone.psyir.transformations.PSyDataTrans
    :members:

``PSyDataNode``
----------------
This is the base class for any node that is being inserted
into PSyclone's program tree to use the PSyData API.
The derived classes will typically control the behaviour
of ``PSyDataNode`` by providing additional parameters.

.. autoclass:: psyclone.psyir.nodes.PSyDataNode
    :members: gen_code

There are two ways of passing options to the
``PSyDataNode``. The first one is used to pass
parameters from the user's transformation script to the constructor
of the node inserted, the second for passing parameters
from a derived node to the ``PSyDataNode`` base class.

.. _psy_data_parameters_to_constructor:

Passing Parameters From the User to the Node Constructor
++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Options can be passed from the user via the
transformation to the node that will create the code.
This is done by using the ``options`` dictionary that is
a standard parameter for all ``validate`` and
``apply`` calls of a transformation (see
:ref:`user_guide:transformations_application`). Besides using
this dictionary for validation and application parameters,
``PSyDataTrans`` passes it to the constructor
of the node that is being inserted. An example
of a parameter is the ``region_name``, where the user
can overwrite the default name given to a region (which
can be somewhat cryptic due to the need to be unique).
The region name is validated by ``PSyDataTrans``, and
then passed to the node constructor. The ``PSyDataNode``
stores the name as an instance attribute, so that they can
be used at code creation time (when ``gen_code`` is being
called). Below is the list of all options that the PSyData
node supports in the option dictionary:

.. table::
    :widths: 2,10

    =============== =========================================
    Parameter Name  Description
    =============== =========================================
    region_name     Overwrites the region name
                    used by the ``PSyDataNode``. It must
                    be a pair of strings: the first one being
                    the name of the module, the second the
                    name of the region. The names are used
                    e.g. by the ``ProfileNode`` to define
                    a unique region name for a profiled
                    code region, or by ``ExtractNode``
                    to define the file name for the output
                    data- and driver-files.
    prefix          A prefix to be used for the module name,
                    the user-defined data type and the
                    variables declared for the API.
    =============== =========================================


Passing Parameter From a Derived Node to the ``PSyDataNode``
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
The ``PSyDataTrans.gen_code`` function also accepts
an option dictionary, which is used by derived nodes
to control code creation. The ``gen_code`` function
is called internally, not directly by the user. If
the ``gen_code`` function of a node derived from
``PSyDataNode`` is called, it can define this
option directory to pass the parameters to the ``PSyDataNode``'s
``gen_code`` function. Here are the options that are currently
supported by ``PSyDataNode``:

================ =========================================
Parameter Name   Description
================ =========================================
pre_var_list     A list of the variable names to be
                 extracted before the instrumented region.
post_var_list    A list of variable names to be extracted
                 after the instrumented region.
pre_var_postfix  An optional postfix that will be appended
                 to each variable name in the
                 ``pre_var_list``.
post_var_postfix An optional postfix that will be appended
                 to each variable name in the
                 ``post_var_list``.
================ =========================================

If there is no variable to be provided by the PSyData API (i.e both
``pre_variable_list`` and ``post_variable_list`` are empty), then the
``PSyDataNode`` will only create a call to ``PreStart`` and
``PostEnd``. This is utilised by the profiling node to make the profiling
API libraries (see :ref:`user_guide:profiling`) independent of the infrastructure
library (since a call to ``ProvideVariable`` can contain API-specific
variable types). It also reduces the number of calls required before
and after the instrumented region which can affect overall
performance and precision of any measurements; see :ref:`user_guide:profiling`
for more details.

The kernel extraction node ``ExtractNode`` uses the dependency
module to determine which variables are input- and output-parameters,
and provides these two lists to the ``gen_code()`` function of its base class,
a ``PSyDataNode`` node. It also uses the ``post_var_postfix`` option
as described under ``gen_code()`` above (see also :ref:`user_guide:psyke_netcdf`).

.. _psydata_base_class:

PSyData Base Class
-------------------
PSyclone provides a base class for all PSyData wrapper libraries. The
base class is independent of the API, but it can provide implementations for
scalars and arrays for all native Fortran types that are required by the
API-specific implementation. The base class does not have to be used, but it
provides useful functionality:

Verbosity:
    It will check the ``PSYDATA_VERBOSE`` environment flag. If it exists, it
    must have a value of either 0 (no messages), 1 (some messages, typically
    only ``PSyDataStart`` and ``PSyDataEnd``), or 2 (detailed messages,
    depending on wrapper library). All other values will result in a warning
    message being printed (and verbosity will be disabled). The verbosity level
    is available as ``this%verbosity``.

Module- and Region-Name Handling:
    The module stores the module name in ``this%module_name``, and the region
    name as ``this%region_name``.

Variable Index:
    It automatically sets ``this%next_var_index`` to 1 in ``PSyDataPreStart``
    ``PSyDataPreEndDeclaration`` and ``PostStart``. This variable will also
    be increased by one for each call to a Declare- or Provide-subroutine.
    It can be used to provide a reproducible index for declaring and
    providing a variable (and it also counts the number of declared variables,
    which can be used in e.g. ``PSyDataPreEndDeclaration`` to allocate arrays).

Start/Stop Handling:
    The base class maintains a module variable ``is_enabled``. This is set
    to true at startup, and gets enabled and disabled by calling the module
    functions ``PREFIX_PSyDataStart()`` and ``PREFIX_PSyDataStop()``
    (see :ref:`psydata_start_stop_functions`).
    It is up to the derived classes to actually use this setting. Of
    course it is also possible to ignore ``is_enabled`` and use a different
    mechanism. For example, the NVIDIA profiling wrapper library calls corresponding
    start and stop functions in the NVIDIA profiler.

Jinja Support:
    The base class is creating using the template language Jinja. It is
    therefore easy to automatically create the base functions for the
    argument types actually required by the wrapper library. See
    :ref:`jinja` for details.


.. _jinja:

Jinja Support in the Base Class
+++++++++++++++++++++++++++++++
Code written for a PSyData library is often very repetitive. For example, an
implementation of ``PreDeclareVariable`` must be provided for each data type.
For LFRic that can easily result in over 10 very similar subroutines (3 basic
types integer, 4- and 8-byte reals; and 4- and 8-byte arrays of one to four
dimensions). In order to simplify the creation of these subroutines the templating
language Jinja is being used. Jinja creates code based on an template,
which makes it possible to maintain just one template implementation of a subroutine,
from which the various Fortran-type specific implementation will be generated.

Jinja is used in the generic base class ``PSyDataBase``, and the base class for all
``ReadOnly`` libraries. It is not required that any library using one of these base
classes itself uses Jinja. For example, the ReadOnly library for
dl_esm_inf does not use Jinja (except for processing the base class templates
of course), while the ReadOnly library for LFRic does. In case of dl_esm_inf,
there were only 5 data types that need to be supported, so it was easy to just
list these 5 functions in a generic interface. LFRic on the other hand uses
many more Fortran basic types, so it uses Jinja to create the code that declares
the generic interfaces. The additional advantage is that if new data types are
required by LFRic (e.g. if 5-dimensional arrays are used), there will be no code
change required (except for declaring the new types in the Makefile).

The PSyData base class ``PSyDataBaseType`` is contained in ``lib/psy_data_base.jinja``.
It is processed with the script ``process.py``, which will print the processed
file to stdout. The ``Makefile`` will automatically create the file
``psy_data_base.f90`` from the Jinja template and compile it. If you use the
base class in a wrapper library, you have to process the template in your library
directory with additional parameters to specify the required types and the prefix.
Besides the name of the template file to process the ``process.py`` script
takes the following parameters:

-types:
    A comma-separated list of Fortran basic types (no spaces allowed).
    The following type names are supported:

    ``real``:
        32-bit floating point value
    ``double``:
        64-bit floating point value
    ``int``:
        32-bit integer value
    ``long``:
        64-bit integer value

    Default value is ``real,double,int``.

-dims:
    A comma-separated list of dimensions (no spaces allowed). Default
    value is ``1,2,3,4``.

-prefix:
    The prefix to use for the PSyData type and functions. Default is
    emtpy (i.e. no prefix). If you specify a prefix, you have to
    add the ``_`` between the prefix and name explicitly.

-generic-declare:
    If this flag is specified, the processed template will also
    declare a generic subroutine with all ``declareXXX`` functions
    (see :ref:`generic_interfaces` for details).  

-generic-provide:
    If this flag is specified, the processed template will also
    declare a generic subroutine with all ``provideXXX`` functions
    (see :ref:`generic_interfaces` for details).

.. _generic_interfaces:

Details About Generic Interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Fortran standard requires that a generic subroutine is declared
in the same program unit in which it is defined. Therefore, if a
derived class falls back to the implementation of a function in the
base class, the generic subroutine must be declared in the base class,
not in the derived class (though some compilers, e.g. gfortran 9
accept it). The two options ``-generic-declare`` and ``-generic-provide``
are supported so that a derived class can control where the generic
subroutines should be declared: if the derived class does not implement
say the 'declaration' functions itself, the ``-generic-declare`` command
line option guarantees that the base class defines the declaration
functions all as one generic subroutine. The derived class can of
course extend this generic subroutine with API-specific implementations.
On the other hand, if the
derived class implements the 'declaration' functions (potentially
calling their implementation in the base class), then the derived class
must declare the generic subroutine, and the base class must not
declare them as well. The same approach is used for the 'provide'
functions. As an example, the ReadOnly verification library for
LFRic in ``lib/read_only/lfric/`` uses ``-generic-declare`` when
processing the PSyData base class (i.e. all declaration functions
are implemented in the PSyData base class), and uses
``-generic-provide`` when processing the ReadOnly base class (i.e.
all provide functions are implemented in the ReadOnly base class).
The LFRic-specific implementation extends the generic subroutine
with two new subroutines: ``DeclareFieldDouble`` and
``DeclareFieldVectorDouble`` (and the same for the corresponding
'provide' functions). 


Explanation of Jinja Use
~~~~~~~~~~~~~~~~~~~~~~~~

For each specified type name the Jinja template will create methods called
``DeclareScalar{{type}}`` and ``ProvideScalar{{type}}`` for handling
scalar parameters. For array parameters, the functions
``DeclareArray{{dim}}d{{type}}`` and ``ProvideArray{{dim}}d{{type}}``
will be created for each type and each specified number of dimensions.

Below is an example of using the ``process.py`` script in a Makefile for a read-only
verification library (taken from ``lib/read_only/lfric/Makefile``):

.. code-block:: Makefile

    PROCESS_ARGS = -prefix=read_only_verification -types=real,int,double \
                   -dims=1,2,3,4

    psy_data_base.f90: ../../psy_data_base.jinja
        ../../process.py $(PROCESS_ARGS) $< > psy_data_base.f90

This will create the processed file ``psy_data_base.f90`` in the directory
of the library, where it will be compiled. Having a separate pre-processed
source code version of the base class for each library (as opposed to one
compiled base class library that is used for all libraries) has the advantage
that consistent compiler settings will be used in your library, and consistent
parameters have been provided specifying the required types and dimensions.

The ``process.py`` script provides the two variables for a template
(based on its command line parameters of course): ``ALL_DIMS`` stores the list of 
required dimensions, and ``ALL_TYPES`` stores the type information
requested when invoking ``process.py``. ``ALL_TYPES`` is a three-tuple
that lists the name to use when creating subroutine names, the Fortran data
type, and number of bits for the data type. While number of bits is not used
in the base class, the read-only-verification base class (see
:ref:`psydata_read_only_base_class`) uses it. If more types are required,
they can be defined in ``process.py``. If the additional types need
different numbers of bits and are required in a read-only library, the
read-only-verification base class (``lib/read_only/read_only_base.jinja``)
needs to be adjusted as well. 

Below is a short excerpt that shows how these variables are defined by
default, and how they are used to create subroutines and declare their
parameters in ``lib/read_only/read_only_base.jinja``:

.. code-block:: jinja

    {% if ALL_DIMS is not defined %}
       {# Support 1 to 4 dimensional arrays if not specified #}
       {% set ALL_DIMS = [1, 2, 3, 4] %}
    {% endif %}

    {# The types that are supported. The first entry of each tuple
       is the name used when naming subroutines and in user messages.
       The second entry is the Fortran declaration. The third entry
       is the number of bits. There is slightly different code
       required for 32 and 64 bit values (due to the fact that the
       Fortran ``transfer(value, mould)`` function leaves undefined
       bits when mould is larger than value.) #}

    {% if ALL_TYPES is not defined %}
       {% set ALL_TYPES = [ ("Double", "real(kind=real64)",   64),
                            ("Real",   "real(kind=real32)",   32),
                            ("Int",    "integer(kind=int32)", 32) ] %}
    {% endif %}
    ...
    {% for name, type, bits in ALL_TYPES %}
    subroutine DeclareScalar{{name}}(this, name, value)
        {{type}}, intent(in) :: value
    ...

      {# Now create the declarations of all array implementations #}
      {% for dim in ALL_DIMS %}
        {# Create the ':,:...' string - DIM-times
           We repeat the list [":"] DIM-times, which is then joined #}
        {% set DIMENSION=([":"]*dim)|join(",") %}
    ! ---------------------------------------------------------------------
    subroutine DeclareArray{{dim}}d{{name}}(this, name, value)
        {{type}}, dimension({{DIMENSION}}), intent(in) :: value
      {% endfor %}
    {% endfor %}


Since the PSyData API relies on a generic interface to automatically call the
right subroutine depending on type, a library must declare these automatically
created subroutines (together with additional, library-specific versions)
as one generic interface. This can either be done by explicitly listing the
subroutines (which is for example done in ``lib/read_only/dl_esm_inf/read_only.f90``,
since it uses only 5 different data types), or using Jinja as well.
The code below shows how the base class and Jinja are used in
``lib/read_only/lfric/read_only.f90``. The ``FieldDouble`` and ``FieldVectorDouble``
related functions are explicitly coded in the LFRic-specific library, the
rest is taken from the base class. Jinja is then
used to create the list of all automatically created subroutines, which 
allows the declaration of one generic interface for each ``PreDeclareVariable``
and ``ProvideVariable`` function. While the use of Jinja here is only very
minimal (only the list of subroutine names in the generic interfaces is created),
the advantage of using Jinja here is that if a new data type is added to LFRic
(e.g. a 5-dimensional array), only the parameter to ``process.py`` need to be 
changed, and the correct subroutines will be created in the base class, and the
corresponding generic interfaces will be declared without further code changes:

.. code-block:: jinja

    procedure :: DeclareFieldDouble,  ProvideFieldDouble
    procedure :: DeclareFieldVectorDouble,  ProvideFieldVectorDouble

    {# Collect the various procedures for the same generic interface #}
    {# ------------------------------------------------------------- #}
    {% set all_declares=[] %}
    {% set all_provides=[] %}
    {% for name, type, bits in ALL_TYPES %}
      {{ all_declares.append("DeclareScalar"~name) or "" -}}
      {{ all_provides.append("ProvideScalar"~name) or "" -}}
      {% for dim in ALL_DIMS %}
        {{ all_declares.append("DeclareArray"~dim~"d"~name) or "" -}}
        {{ all_provides.append("ProvideArray"~dim~"d"~name) or "" -}}
      {% endfor %}
    {% endfor %}

    {% set indent="            " %}
    generic, public :: PreDeclareVariable => &
        DeclareFieldDouble, &
        DeclareFieldVectorDouble, &
        {{all_declares|join(", &\n"+indent) }}

    generic, public :: ProvideVariable => &
        ProvideFieldDouble,       &
        ProvideFieldVectorDouble, &
        {{all_provides|join(", &\n"+indent) }}

.. note::
    Ending the Jinja statements with '``or ""``' avoids having ``None``
    added to the files (which would be the output of e.g. the ``append``
    instruction). The '``-``' before the closing '``}}``' also prevents this
    line from creating any white-spaces. As a result of this the
    processed file will not have unusual empty lines or indentation.


Static Functions in the Base Class
++++++++++++++++++++++++++++++++++
The base class provides the four static functions defined in the API (see
:ref:`psy_data_api`). The subroutines ``PREFIX_PSyDataInit()`` and
``PREFIX_PSyDataShutdown()`` are empty, and ``PREFIX_PSyDataStart()`` and
``PREFIX_PSyDataStop()`` function just change the module variable
``is_enabled`` (which can then be used by a wrapper library to enable
or disable its functionality).

If you don't need specific functionality for these functions, you
can just declare them in your wrapper library::

    module MyProfileWrapperLibrary
      use psydata_base_mod, only : profile_PSyDataInit, profilePSyDataShutdown, &
                                   profile_PSyDataStart, profile_PSyDataStop

If you need to partially change the behaviour of these functions,
you have to rename them in your ``use`` statement, and then call the renamed
base class function like this::

    module MyProfileWrapperLibrary
    ...
    contains

    subroutine profile_PSyDataStart()
      use psydata_base_mod, only : base_PSyDataStart => profile_PSyDataStart
      ! Do something
      call base_PSyDataStart()
      ! Do something else
    end subroutine profile_PSyDataStart

.. note::
    The ``PSyDataBase`` template will use the prefix that you have specified
    as parameter when using the ``process.py`` script  for naming
    the static functions. So as an alternative to renaming the symbol when
    importing them you could also specify a different prefix when processing
    the base Jinja file, and use this name.

.. _psydata_read_only_base_class:

PSyData Read-Only-Verification Base Class
-----------------------------------------
The ReadOnlyVerification transformation uses the PSyData API
to verify that read-only arguments to a subroutine call are not changed.
It does this by computing and storing a checksum of each read-only parameter
to a subroutine before the call, and verifying that this checksum
is not changed after the subroutine call. Since the API-specific
instances share a significant part of code (all functions for
the non API-specific Fortran types, e.g. scalar values and plain
Fortran arrays), a common base class is implemented for these,
based on the PSyData Base class (see :ref:`psydata_base_class`).
The ``ReadOnlyBaseType`` is provided as a Jinja template as well (see
:ref:`jinja`), see ``lib/read_only/read_only_base.jinja``. It 
takes the same parameters as the ``PSyDataBaseType``, which makes it
easy to make sure the PSyDataBaseClass and ReadOnly base classes are
created with the same settings (like supported Fortran types
and number of dimensions).

This Read-Only-Verification base class uses the PSyData base
class. It uses the Declaration functions to count how many variables
will be provided. In ``PreEndDeclaration`` a checksum array will be
allocated.

.. note::
    The ``PreStart`` function gets the number of variables as
    a parameter. The decision not to use this value for allocating
    the array is that the LFRic read-only implementation stores
    several checksums for one variable of a certain type (VectorField).
    The ``DeclareVariable`` functions for VectorFields counts the
    right number of checksums required.

The ReadOnlyBase base class uses the information about the number of bits for
the data types to implement the checksum functions. One complication
is that the Fortran ``transfer`` function results in undefined bits
when transferring e.g. a 32-bit value into a 64-bit variable.
Therefore any 32-bit value is first transferred to a 32-bit integer value,
which is then assigned to the 64-bit integer value, which is added to
the overall checksum value.

The two API-specific ReadOnlyVerification libraries are both based on
this base class. Therefore they need only implement the checksum functions
for the API-specific types - ``Field`` and ``VectorFields`` in LFRic, and
``Field`` in GOcean.

.. _profiling:

Profiling
---------

The command line options and transformations available to a user
are described in the PSyclone User's guide (:ref:`user_guide:profiling`).
This section describes how the PSyData API is used to implement
the profiling wrapper API, and which functions must be provided in
a wrapper library to allow other existing profiling tools to 
be used.

.. _ProfilingAPI:

Profiling API
+++++++++++++
PSyclone uses the PSyData API to allow implementation of profile wrapper libraries
that connect to various existing profiling tools. For each existing profiling
tool a simple interface library needs to be implemented that maps the PSyclone
PSyData calls to the corresponding call for the profiling tool.

Since the profiling API does not need access to any fields or variables,
PSyclone will only create calls to ``PreStart`` and ``PostEnd``.
The profiling wrapper libraries also
need the static initialisation and shutdown functions ``profile_PSyDataInit``
and ``profile_PSyDataShutdown``. Details can be found in the section
:ref:`psy_data_api`.

The examples in the ``lib/profiling`` directory show various ways
in which the opaque data type ``profile_PSyDataType`` can be used to interface
with existing profiling tools - for example by storing
an index used by the profiling tool in ``profile_PSyDataType``, or
by storing pointers to the profiling data to be able to
print all results in a ProfileFinalise() subroutine.
Some of the wrapper libraries use the PSyData base class (e.g. dl_timer,
simple_timing, template), others do not (e.g. NVIDIA profiling,
DrHook wrapper).


Kernel Extraction (PSyKE)
-------------------------
The PSyclone Kernel Extraction functionality (see :ref:`user_guide:psyke`)
also relies on the PSyData API to write kernel input- and output-arguments
to a file.

When an extraction transformation is applied, it will determine the input-
and output-variable for the code region using the dependency analysis
(see :ref:`variable_accesses`).
The code created by PSyclone's PSyData transformation will then call the
PSyData extraction library as follows:

- For each input variable the variable with its original name will
  be stored in the file before the kernel is called.
- For each output variable the variable will be stored with in the file
  with the postfix ``_post`` added after the kernel has been executed.

As example, an input- and output-variable ``my_field`` will therefore have
two values stored in the file: the input value using the name ``my_field``
and the output value using ``my_field_post``. If the variable is a member
of a structure, the key-name of the variable to be written to
the file will contain the ``%``, e.g.
``my_field%whole%xstart``. It is therefore important that the output
format supports special characters (e.g. NetCDF does allow the use of
``%`` in names).

.. note::
   If a name clash is detected, e.g. the user has a variable called
   ``my_field`` and another variable called ``my_field_post``, the
   output postfix ``_post`` will be changed to make sure unique names
   are created by appending a number to the postfix, e.g. ``_post0``,
   ``_post1``. The same postfix will be applied to all variables,
   not only to variables that have a name clash.

An excerpt of the created code (based on ``examples/gocean/eg5/extract``):

.. code-block:: Fortran

      CALL extract_psy_dataPreStart("main", "update", 11, 5)
      ...
      CALL extract_psy_dataPreDeclareVariable("a_fld", a_fld)
      CALL extract_psy_dataPreDeclareVariable("a_fld_post", a_fld)
      CALL extract_psy_dataPreEndDeclaration
      CALL extract_psy_dataProvideVariable("a_fld", a_fld)
      ...
      CALL extract_psy_dataPreEnd
      ! Kernel execution
      ...
      CALL extract_psy_dataPostStart
      CALL extract_psy_dataProvideVariable("a_fld_post", a_fld)
      ...

The variable ``a_fld`` is declared twice, once with its unmodified
name representing the input value, and once as ``a_fld_post``, which
will be used to store the output value. The values are provided once
before the kernel to allow the PSyData library to capture the input
values, and once using the name ``a_fld_post`` after the kernel
execution to store the results.

.. note::
    The following section on driver creation applies at this stage
    only to GOcean. Support for driver creation for LFRic is
    tracked in issue #1392.

The creation of a stand-alone driver can be requested when applying
the extraction transformation, e.g.:

.. code-block:: python

    extract = GOceanExtractTrans()
    extract.apply(schedule.children, {"create_driver": True})

When compiled and executed, this driver will read in the values of
all input- and output-variables, execute the
instrumented code region, and then compare the results of the output
variables (see :ref:`user_guide:psyke_netcdf`). This program does
not depend on any infrastructure library (like 'dl_esm_inf`'), it
only needs the PSyData wrapper library (e.g.
``lib/extract/netcdf/dl_esm_inf``), plus any libraries the wrapper
depends on (e.g. NetCDF).

The following changes are applied by the ``ExtractionDriverCreator``
in order to generate stand-alone code for GOcean:

1. The `dl_esm_inf` field type is replaced with 2d Fortran arrays.
   The structure name used is 'flattened', i.e. each ``%`` is replaced
   with a ``_`` to make a standard Fortran name, but the final
   ``%data`` is removed. So ``my_field%data`` becomes ``my_field``.
2. Any other structure access is converted into a variable with the
   same intrinsic type and flattened name. E.g. ``my_field%whole%xstart``
   becomes the variable ``my_field_whole_xstart``.
3. For each input-only variable one variable (with a potentially
   flattened name) is created. It calls ``ReadVariable`` from the
   PSyData extraction library, which will allocate the variable
   if it is an array.
4. For each input+output variable two variables are created and
   initialised (especially allocated if the variable is an array)
   with a call to ``ReadVariable``. The output variable will
   have a postfix appended (``_post`` by default) to distinguish
   it from the input value. The value of the input array will be
   provided to the kernel call. At the end, the newly computed
   values in the variable will be compared with the corresponding
   ``_post`` variable, which was read from the file.
5. An output only variable will be declared with the postfix
   attached, and allocated and initialised in ``ReadData``.
   Then the variable without postfix will also be declared,
   and explicitly allocated to have the same shape as the
   output variable. This variable will be initialised to 0,
   and then provided to the kernel call. Again, at the end
   of the call these two variables should have the same value.

Here an example showing some of the driver code created:

.. code-block:: Fortran

    integer :: a_fld_whole_ystart

    real*8, allocatable, dimension(:,:) :: a_fld
    real*8, allocatable, dimension(:,:) :: a_fld_post
    real*8, allocatable, dimension(:,:) :: b_fld
    real*8, allocatable, dimension(:,:) :: b_fld_post

    call extract_psy_data%OpenRead('main', 'update')
    call extract_psy_data%ReadVariable('a_fld', a_fld)
    call extract_psy_data%ReadVariable('a_fld_post', a_fld_post)
    call extract_psy_data%ReadVariable('a_fld%whole%ystart', a_fld_whole_ystart)

    call extract_psy_data%ReadVariable('b_fld_post', b_fld_post)
    ALLOCATE(b_fld(SIZE(b_fld_post, 1), SIZE(b_fld_post, 2)))
    b_fld = 0

    do j = a_fld_whole_ystart, a_fld_whole_ystop, 1
      do i = a_fld_whole_xstart, a_fld_whole_xstop, 1
          call update_field_code(i, j, a_fld, b_fld, ...)
      enddo
    enddo

    if (ALL(a_fld - a_fld_post == 0.0)) then
      PRINT *, "a_fld correct"
    else
      PRINT *, "a_fld incorrect. Values are:"
      PRINT *, a_fld
      PRINT *, "a_fld values should be:"
      PRINT *, a_fld_post
    end if
    if (ALL(b_fld - b_fld_post == 0.0)) then
      PRINT *, "b_fld correct"
    else
      PRINT *, "b_fld incorrect. Values are:"
      PRINT *, b_fld
      PRINT *, "b_fld values should be:"
      PRINT *, b_fld_post
    end if

The variable ``a_fld`` is an input- and output-field,
so both values for ``a_fld`` and ``a_fld_post`` are read
in, and after the kernel execution the values are checked
for correctness.
The variable ``b_fld`` on the other hand is an output-variable
only, no input values are stored in the file. The code
created calls to ``ReadVariable`` for ``b_fld_post``, which
allocates the variable corresponding to the array size
information stored in the data file. Then the driver allocates
an array ``b_fld`` with the same shape as ``b_fld_post``, which
is initialised to 0. This ``b_fld`` is provided in the kernel call.
After the kernel call, ``b_fld`` should be equal to
``b_fld_post``.
