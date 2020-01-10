.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2019, Science and Technology Facilities Council.
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

.. _psy_data:

PSyData API
===========
PSyclone provides transformations that will insert callbacks to
an external library at runtime. These callbacks allow third-party
libraries to access data structures at specified locations in the
code. Some example use cases are:

Profiling:
  By inserting callbacks before and after a region of code
  performance measurements can be added. PSyclone provides
  wrapper library for some common performance profiling tools,
  see :ref:`profiling` for details.

Kernel Data Extraction:
  PSyclone provides the ability to add callbacks that provide access
  to all input variables before, and output variables after a kernel
  invocation. This can be used to automatically create tests for
  a kernel, or to write a stand-alone driver that just calls one
  kernel, which can be used for performance tuning. An example
  library that extracts input- and output-data into a netcdf file
  is included with PSyclone.

In-situ Visualisation:
  By giving access to output fields of a kernel, an in-situ visualisation
  library can be used to plot fields while a PSyclone application is
  running.

Access Verification:
  The callbacks can be used to make sure a field declared as read-only
  is not modified during a kernel (either because of an incorrect
  declaration, or because memory is overwritten). A checksum can be
  used to detect any changes to a read-only field.

The PsyData API should be general enough to allow these and other
applications to be developed and used.

API
---
The callbacks are inserted into the code by the PSyData transformation,
see :ref:`psy_data_transformation` for details. The following example
shows the code created by PSyclone::

    USE psy_data_mod, ONLY: PSyDataType
    TYPE(PSyDataType), save :: psy_data

    CALL psy_data%PreStart("update_field_mod", "update_field_code", 1, 1)
    CALL psy_data%PreDeclareVariable("a_fld", a_fld)
    CALL psy_data%PreDeclareVariable("b_fld", b_fld)
    CALL psy_data%PreEndDeclaration
    CALL psy_data%ProvideVariable("a_fld", a_fld)
    CALL psy_data%PreEnd

    ! Begin of PSY-layer kernel call:
    DO j=1,jstop+1
      DO i=1,istop+1
        CALL update_field_code(i, j, a_fld%data, b_fld%data)
      END DO 
    END DO 
    ! End of PSY-layer kernel call

    CALL psy_data%PostStart
    CALL psy_data%ProvideVariable("b_fld", b_fld)
    CALL psy_data%PostEnd

The following code sequence will typically be created:

#. A static variable of type ``PSyDataType`` is declared.
#. ``PreStart`` indicating that a new instrumented code region is started.
#. ``PreDeclareVariable`` is called for each variable that is written either
   before or after the instrumented region.
#. ``PreEndDeclaration`` to indicate the end of the variable declarations.
#. ``ProvideVariable`` for each variable to be written before the instrumented
   region.
#. ``PreEnd`` to declare that all variables before the instrumented region
   have been written.
#. Then the actual instrumented code region is used.
#. When the instrumented region is executed, a call to ``PostStart``
   is added to indicate that all further variables written will be
   after the user code.
#. For each variable to be written after the user code a call to
   ``ProvideVariable`` is added.
#. A call to ``PostEnd`` is added once all variables have been written.

.. note::
    Depending on the options provided to the PSyData transformation
    some of the calls might not be created. For example, for a performance
    profiling library no variables will be declared or written.

The following sections will describe the API in detail.

.. _psy_data_type:

``PSyDataType``
+++++++++++++++
The library using the PSyData API must provide a user-defined data type
called ``PSyDataType``. It is up to the application how this variable is
used. PSyclone will declare the variables to be static, meaning that they
can be used to accumulate data from call to call.

``PreStart(this, module_name, kernel_name, num_pre_vars, num_post_vars)``
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
This function is called first each time the instrumented region is
executed. It takes 4 parameters (besides the implicit PSyDataType
instance):

``module_name``::
  This is the name of the module in which the original Fortran source
  code is contained. Together with ``kernel_name`` it can be used to
  create a unique name for each instrumented region.

``kernel_name``::
  The name of the kernel that is being executed.

``num_pre_vars``::
  This is the number of variables that will be supplied using
  ``WriteVar`` before the instrumented region is executed.

``num_post_vars``::
  This is the number of variables that will be supplied using
  ``WriteVar`` after the instrumented region is executed.
  The sum ``num_pre_vars+num_post_vars`` is the number of
  variable declarations that will follow.

Typically the static ``PSyDataType`` instance can be used to store
module and kernel name if they are required later, or to allocate
arrays to store variable data.

``PreDeclareVariable(this, name, value)``
+++++++++++++++++++++++++++++++++++++++++
This function is called for each variable that will be written
before or after the user instrumented region. If a variable
is written before and after, it will also be declared twice
(it can be useful to provide a variable using a different
name before and after, see :ref:`psyke`).

``name``:
  This is the name of the variable as a string.

``value``:
  This is the actual content of the variable.

The same call is used for different arguments, so in general
an general interface is recommended to distinguish between
the data types provided. The netcdf kernel writer uses the
following declaration with dl_esm_inf::

    generic, public :: PreDeclareVariable => DeclareScalarInteger, &
                                             DeclareScalarReal,    &
                                             DeclareFieldDouble
    ...
    subroutine DeclareScalarInteger(this, name, value)
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
    ...
    subroutine DeclareFieldDouble(this, name, value)
        use field_mod, only : r2d_field
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
    ...


``PreEndDeclaration(this)``
+++++++++++++++++++++++++++
Called once all variables have been declared.

``ProvideVariable(this, name, value)``
++++++++++++++++++++++++++++++++++++
This function is called for each variable to be provided to the
runtime library. 

``name``:
  This is the name of the variable as a string.

``value``:
  This is the actual content of the variable.

The same function ``ProvideVariable`` is called to provide variable
name and content before and after the user instrumented region.
And again it is expected that a library using the API will provide
a generic interface to distinguish between the various possible data
types, which will be different for each infrastructure library::

    generic, public :: ProvideVariable => WriteScalarInteger, &
                                          WriteScalarReal,    &
                                          WriteFieldDouble

``PreEnd(this)``
++++++++++++++++
The method ``PreEnd`` is called after all variables before the instrumented
region have been provided.

``PostStart(this)``
+++++++++++++++++++
This is the first call after the instrumented region. It does not take
any parameters, but the static ``PSyDataType`` instance can be used
to store the name and number of variables if required. This will be
followed by calls to ``ProvideVariable``, which is described above.

``PostEnd(this)``
+++++++++++++++++
This function is the last call after an instrumented region. It indicates
that all variables have been provided.
 
An example of a library using PSyData is included in PSyclone in the
directory ``.../lib/extract/netcdf``. This library is used to extract
kernel input- and output-parameter and store them in a NetCDF file.

.. note::

    Note that only the ``PreDataStart`` call takes the module-
    and region-name as parameters. If these names are required
    by the profiling library in different calls, they must
    be stored in the ``PSyData`` object so that they are
    available when required.

.. _psy_data_transformation:

PSyData Transformation and PSyData-Node
---------------------------------------
The base transformation to create the PSyData callbacks is the 
``PSyData`` transformation:

.. autoclass:: psyclone.psyir.transformations.psy_data_trans.PSyDataTrans
    :members:

It is very similar to the profile transformation and kernel extraction
transformation. Those transforms insert a special node into the AST,
which at code creation time will generate the code to give access to
the data fields. Both profile and kernel extraction
nodes use ``PSyDataNode`` as a base class, which will create the 
PSyData calls as described above. The difference is that those
classes will provide different parameters to the ``PSyDataNode``,
resulting in different code being created:

.. autoclass:: psyclone.psyir.nodes.psy_data_node.PSyDataNode
    :members:

The behaviour of the ``PSyDataNode`` is controlled using the ``option``
dictionary, as documented above. If there is no variable to be provided
(i.e both ``pre_variable_list`` and ``post_variable_list`` are empty),
then the ``PSyDataNode`` will only create a call to ``PreStart`` and 
``PostEnd``. This is utilised by the profiling node to make the profiling
API libraries (see :ref:`ProfilingAPI`) independent of the infrastructure
library, which will contain API-specific parameters in any call to
``ProvideVariable``. It also reduces the number of calls required before
and after the instrumented region which can affect overall
performance and precision of any measurements, see :ref:`profiling`
for more details.

The kernel extraction node ``ExtractNode`` uses the dependency
module to determine which variables are input- and output-parameter,
and provides these two lists to the ``PSyDataNode``. It also uses
``post-var-postfix``, see :ref:`psyke`.
