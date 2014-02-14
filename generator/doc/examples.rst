.. _examples-label:

Examples
========

.. _examples-infrastructure-label:

Infrastructure
--------------

In the following example the invoke call includes an infrastructure
call (``set``) and a kernel call (``testkern_type``). The
infrastructure call sets all values in the field ``one`` to
``1.0``. Notice that, unlike the kernel call, no use association is
required for the infrastructure call.
::

  program kernel_and_inf_mixed_invoke
  
  ! Description: single infrastructure set routine specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: one,f2,f3
  
  call invoke(                                           &
       set(one,1.0),                                     &
       testkern_type(one,f2,f3)                          &
       )
  
  end program kernel_and_inf_mixed_invoke

Below is an example of a kernel that is consistent with the testkern
kernel specified in the example above.
::

  module testkern
    type, extends(kernel_type) :: testkern_type
       type(arg), dimension(3) :: meta_args =    &
            (/ arg(READ,  (CG(1)*CG(1)),    FE), &
               arg(READ,  (DG(0)*DG(0)),    FE), &
               arg(WRITE, (CG(1)*CG(1))**3, FE)  &
             /)
       integer, parameter :: iterates_over = CELLS
     contains
       procedure() :: code => testkern_code
    end type testkern_type
  contains
    subroutine testkern_code(nLayers,p1dofmap,p0dofmap,a,b,c)
    end subroutine testkern_code
  end module testkern

We now translate the algorithm layer code and generate the psy layer
code. The algorithm code is assumed to be in a file call
`2_mixed_kernel_and_set.f90`. In this case we use the top level python
interface. See the :ref:`api-label` section for different ways to
translate/generate code.
::

	>>> from generator import generate
	>>> alg,psy=generate("2_mixed_kernel_and_set.f90")
	>>> print alg
	>>> print psy

The resultant generated algorithm code is given below.

Ignoring the difference in case (which is due to the output format of
the code parser) the differences between the original algorithm code
and the translated algorithm code are:

* the generic ``call invoke`` has been replaced by a specific ``CALL invoke_0``.The calls within the invoke routine are removed, as are duplicate arguments and any literals leaving the three fields being passed in.
* a use statement is added for the new ``CALL invoke_0`` which will call the generated PSy layer code.

The existance of an infrastructure call has made no difference at this point.
::

  PROGRAM kernel_and_inf_mixed_invoke
    USE psy_kernel_and_inf_mixed_invoke, ONLY: invoke_0

    ! Description: single infrastructure set routine specified in an invoke call
    USE testkern, ONLY: testkern_type
    USE inf, ONLY: field_type
    IMPLICIT NONE
    TYPE(field_type) one, f2, f3

    CALL invoke_0(one, f2, f3)

  END PROGRAM kernel_and_inf_mixed_invoke

A vanilla (not optimised) version of the generated PSy layer is given
below. As expected the kernel code is called from the PSy
layer. However, in the case of the `set` infrastructure call, the code
for this has been written directly in the PSy layer (the line
`one%data = 1.0`). This example shows how infrastructure calls may be
implemented in whatever way the generator see fit with no change to
the algorithm and kernel layers.
::

  MODULE psy_kernel_and_inf_mixed_invoke
    USE lfric
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0(one, f2, f3)
      USE testkern, ONLY: testkern_code
      TYPE(field_type), intent(inout) :: one, f2, f3
      INTEGER column
      INTEGER, pointer :: p1dofmap(:,:)
      TYPE(FunctionSpace_type), pointer :: one_space
      INTEGER, pointer :: p0dofmap(:,:)
      TYPE(FunctionSpace_type), pointer :: f2_space
      INTEGER nlayers
      TYPE(ColumnTopology), pointer :: topology
      SELECT TYPE ( f2_space=>f2%function_space )
        TYPE IS ( FunctionSpace_type )
        topology => f2_space%topology
        nlayers = topology%layer_count()
        p0dofmap => f2_space%dof_map(cells, fe)
      END SELECT 
      SELECT TYPE ( one_space=>one%function_space )
        TYPE IS ( FunctionSpace_type )
        p1dofmap => one_space%dof_map(cells, fe)
      END SELECT 
      one%data = 1.0
      DO column=1,topology%entity_counts(cells)
        CALL testkern_code(nLayers, p1dofmap(:,column), p0dofmap(:,column), one%data, f2%data, f3%data)
      END DO 
    END SUBROUTINE invoke_0
  END MODULE psy_kernel_and_inf_mixed_invoke

This example and further test examples for infrastructure calls can be viewed in the GungHo repository here https://puma.nerc.ac.uk/trac/GungHo/browser/GungHo/trunk/src/generator/tests/3_infrastructure_calls.
