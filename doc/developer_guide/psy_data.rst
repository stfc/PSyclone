.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2020, Science and Technology Facilities Council.
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

PSyData API
===========
PSyclone provides transformations that will insert callbacks to
an external library at runtime. These callbacks allow third-party
libraries to access data structures at specified locations in the
code.

Introduction
------------
The PSyData API imports a user-defined data type from a PSyData
module and creates an instance of this data type for each code
region. It then adds a sequence of calls to methods in that
instance. A simple example::

    USE psy_data_mod, ONLY: PSyDataType
    TYPE(PSyDataType), target, save :: psy_data

    CALL psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)

In order to allow several different callback libraries to be used
at the same time, for example to allow in-situ visualisation at
the same time as checking that read-only values are indeed not modified,
different module names and data types must be used.

PSyData divides its application into different classes. For example,
the class "profile" combines all profiling tools (e.g. DrHook or the
NVIDIA profiling tools). This class name is used as a prefix for
the module name and the user type. So if a profiling application
is linked the above code will be::

    USE profile_psy_data_mod, ONLY: profile_PSyDataType
    TYPE(profile_PSyDataType), target, save :: profile_psy_data

    CALL profile_psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)

While adding the class prefix to the name of the instance variable
is not necessary, it helps readability of the created code. The list
of valid classes is specified in the configuration file:

    [DEFAULT]
    ...
    VALID_PSY_DATA_PREFIXES = profile extract

The classes supported at the moment are:

.. tabularcolumns:: |l|L|

======================= =======================================================
Class Prefix            Description
======================= =======================================================
profile                 All libraries related to profiling tools like DrHook,
                        NVIDIA's profiling tools etc. See
                        :ref:`user_guide:profiling` for details.
extract                 For libraries that allow kernel data extraction. See
                        :ref:`user_guide:psyke` for details.
======================= =======================================================

If you want to add additional class prefixes, just add them to the
config file that you are using. 

.. Note:: 
    Transformations for profiling or kernel extraction allow
    to overwrite the default-prefix (see 
    :ref:`psy_data_parameters_to_constructor`).
    This can be used to link with
    two different libraries of the same class at the same time, 
    e.g. you could use ``drhook_profile`` and ``nvidia_profile``
    as class prefix. Though this also requires that the
    corresponding wrapper libraries are modified to use this
    new prefix.


API
---
The PSyData API consists of two calls that allow initialisation and
shut down of a wrapper library. These two calls must be inserted
manually into the program, and their location might depend on 
the wrapper library used - e.g. some libraries might need to be
initialised before ``MPI_Init`` is called, others might need to
be called after. Similarly the shutdown function might need to
be called before or after ``MPI_Finalize``.

.. method:: PREFIX_PSyDataInit()
    Initialises the wrapper library used. It takes no parameters, and must
    called exactly once before any other PSyData-related function
    is invoked.

.. method:: PREFIX_PSyDataShutdown()
    Cleanly shuts down the wrapper library used. It takes no parameters,
    and must called exactly once. No more PSyData-related functions
    must be called after PSyDataShutdown has been called.


The callbacks are inserted into the code by the PSyData transformation,
see :ref:`psy_data_transformation` for details. The following example
shows the code created by PSyclone for an extraction transformation::

    USE extract_psy_data_mod, ONLY: extract_PSyDataType
    TYPE(extract_PSyDataType), target, save :: extract_psy_data

    CALL extract_psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)
    CALL extract_psy_data%PreDeclareVariable("a_fld", a_fld)
    CALL extract_psy_data%PreDeclareVariable("b_fld", b_fld)
    CALL extract_psy_data%PreEndDeclaration
    CALL extract_psy_data%ProvideVariable("a_fld", a_fld)
    CALL extract_psy_data%PreEnd

    ! Begin of PSY-layer kernel call:
    DO j=1,jstop+1
      DO i=1,istop+1
        CALL update_field_code(i, j, a_fld%data, b_fld%data)
      END DO 
    END DO 
    ! End of PSY-layer kernel call
    
    CALL extract_psy_data%PostStart
    CALL extract_psy_data%ProvideVariable("b_fld", b_fld)
    CALL extract_psy_data%PostEnd

In the following documentation the string ``PREFIX_`` is used
to indicate the class-prefix used (e.g. ``profile`` or ``extract``).
The following code sequence will typically be created:

#. A user defined type ``PREFIX_PSyDataType`` is imported from the module
   ``PREFIX_psy_data_mod``.
#. A static variable of type ``PREFIX_PSyDataType`` is declared.
#. ``PreStart`` is called to indicate that a new that a new instrumented code
   region is started.
#. ``PreDeclareVariable`` is called for each variable passed to the data API
   either before or after the instrumented region.
#. ``PreEndDeclaration`` is called to indicate the end of the variable
   declarations.
#. ``ProvideVariable`` is called for each variable to be written before the
   instrumented region.
#. ``PreEnd`` is called to signal the end of all PSyData activity before the
   instrumented region.
#. Then the actual instrumented code region is entered.
#. After the instrumented region, a call to ``PostStart``
   is added to indicate that all further data output occurs after the
   instrumented region.

#. For each variable to be written after the instrumented region a call to
   ``ProvideVariable`` is added.
#. A call to ``PostEnd`` is added once all variables have been written.

.. note::
    Depending on the options provided to the PSyData transformation
    some of the calls might not be created. For example, for a performance
    profiling library no variables will be declared or provided.

The following sections describe the API in detail.

.. _psy_data_type:

``PREFIX_PSyDataType``
++++++++++++++++++++++
The library using the PSyData API must provide a user-defined data type
called ``PREFIX_PSyDataType``. It is up to the application how this variable is
used. PSyclone will declare the variables to be static, meaning that they
can be used to accumulate data from call to call. An example of
the PSyDataType can be found in the NetCDF example extraction code
(see ``lib/extract/dl_esm_inf/netcdf``, or :ref:`user_guide:psyke_netcdf` for
a detailed description) or any of the profiling wrapper libaries
(all contained in ``lib/profiling``)

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
      ``WriteVar`` before the instrumented region is executed.
    
    ``num_post_vars``
      This is the number of variables that will be supplied using
      ``WriteVar`` after the instrumented region is executed.
      The sum ``num_pre_vars+num_post_vars`` is the number of
      variable declarations that will follow.
    
    Typically the static ``PREFIX_PSyDataType`` instance can be used to store
    the module and kernel names if they are required later, or to allocate
    arrays to store variable data.

.. method:: PreDeclareVariable(this, name, value)

    This method is called for each variable that will be written
    before or after the user instrumented region. If a variable
    is written both before and after the region, the transformations will
    add two calls to ``PreDeclareVariable`` (it can be useful to
    provide a variable using a different name before and after,
    see :ref:`user_guid:psyke_netcdf`).
    
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
        subroutine DeclareFieldDouble(this, name, value)
            use field_mod, only : r2d_field
            implicit none
            class(extract_PSyDataType), intent(inout), target :: this
            character(*), intent(in) :: name
            type(r2d_field), intent(in) :: value
        ...
        subroutine DeclareFieldDouble(this, name, value)
            use field_mod, only : r2d_field
            implicit none
            class(extract_PSyDataType), intent(inout), target :: this
            character(*), intent(in) :: name
            type(r2d_field), intent(in) :: value
        ...

.. method:: PreEndDeclaration(this)

    Called once all variables have been declared.

.. method:: ProvideVariable(this, name, value)

    This method is called for each variable to be provided to the
    runtime library. 
    
    ``name``
      This is the name of the variable as a string.
    
    ``value``
      This is the actual content of the variable.
    
    The same method ``ProvideVariable`` is called to provide variable
    name and content before and after the user instrumented region.
    Again it is expected that a library using the API will provide
    a generic interface to distinguish between the various possible data
    types, which will be different for each infrastructure library::
    
        generic, public :: ProvideVariable => WriteScalarInteger, &
                                              WriteScalarReal,    &
                                              WriteFieldDouble
    
.. method:: PreEnd(this)

    The method ``PreEnd`` is called after all variables before the instrumented
    region have been provided.

.. method:: PostStart(this)

    This is the first call after the instrumented region. It does not take
    any parameters, but the static ``PREFIX_PSyDataType`` instance can be used
    to store the name and number of variables if required. This will be
    followed by calls to ``ProvideVariable``, which is described above.

.. method:: PostEnd(this)

    This method is the last call after an instrumented region. It indicates
    that all variables have been provided.

    An example of a library using PSyData is included in PSyclone in the
    directory ``.../lib/extract/netcdf``. This library is used to extract
    kernel input- and output-parameters and store them in a NetCDF file.

    .. note::

        Note that only the ``PreDataStart`` call takes the module-
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
    :members:

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
                    code region, or by ``GOceanExtractNode``
                    to define the file name for the output
                    data- and driver-files.
    class           A prefix to be used for the module name,
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
post_var_postfix an optional postfix that will be appended
                 to each variable name in the
                 ``post_var_list``.
================ =========================================

If there is no variable to be provided by the PSyData API (i.e both
``pre_variable_list`` and ``post_variable_list`` are empty), then the
``PSyDataNode`` will only create a call to ``PreStart`` and
``PostEnd``. This is utilised by the profiling node to make the profiling
API libraries (see :ref:`user_guide:ProfilingAPI`) independent of the infrastructure
library (since a call to ``ProvideVariable`` can contain API-specific
variable types). It also reduces the number of calls required before
and after the instrumented region which can affect overall
performance and precision of any measurements, see :ref:`user_guide:profiling`
for more details.

The kernel extraction node ``ExtractNode`` uses the dependency
module to determine which variables are input- and output-parameters,
and provides these two lists to the ``gen_code()`` function of its base class,
a ``PSyDataNode`` node. It also uses the ``post_var_postfix`` option
as described under ``gen_code()`` above (see also :ref:`user_guide:psyke_netcdf`).
