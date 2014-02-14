Infrastructure calls
====================

Infrastructure calls are calls which can be specified within an invoke
call in the algorithm layer but do not require an associated kernel to
be implemented as they are supported directly by the infrastructure.

One use of infrastructure calls is for commonly used operations. In
this case infrastructure calls simplify the use of the system as users
do not need to write kernel routines. Infrastructure routines also
offer a potential performance advantage as they provide a
specification of what is required without an implementation. Therefore
the PSy layer is free to implement these routines in whatever way it
chooses.

As infrastructure calls have no kernel implementation, they do not
need to specify a particular type of data (kernels specify the type of
data expected in their metadata description). Therefore infrastructure
calls may be polymorphic with respect to functionspaces (they may
support different functionspaces through the same api).

.. note:: In general, psyclone will need to know the types of fields being passed to the infrastructure calls. The parser currently does not provide this information. At the moment this is not an issue as the ``set`` infrastructure call can be generated in a generic way (by using array notation).

Example
-------

In the following example the invoke call includes an infrastructure
call (``set``) and a kernel call (``testkern_type``). The
infrastructure call sets all values in the field ``one`` to
``1.0``. Notice that, unlike the kernel call, no use association is
required for the infrastructure call.
::

	use testkern, only: testkern_type
	use inf,      only: field_type
	implicit none
	type(field_type) :: one,f2,f3
	
	call invoke(                                          &
     	        set(one,1.0),                                 &
     	        testkern_type(one,f2,f3)                      &
                )

See the full :ref:`examples-infrastructure-label` example in the
:ref:`examples-label` section for more details.

Supported infrastructure calls
------------------------------

set
^^^

**set** ( *field* , *value* )

Set all elements of the field *field* to the value *value*.
All types of field are supported.

* type(field_type),intent(out) :: *field*
* real,intent(in) :: *value*


