!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> @brief A module providing operator related classes.
!>
!> @details Implements the following types:
!>   * abstract operator base type
!>   * locally assembled operator (i.e. the stencil is assembled in each cell
!>      of the 3d grid)
!>   * columnwise assembled operator for horizontally discontinuous spaces
!>      (i.e. the matrix is assembled in each vertical column of the grid)
module operator_mod

  use constants_mod,            only : r_def, i_def
  use function_space_mod,       only : function_space_type
  use mesh_mod,                 only : mesh_type
  use log_mod,                  only : log_event, LOG_LEVEL_ERROR

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> Algorithm layer representation of an (abstract) operator base type
  !>
  !> Objects of this type hold all the data of the operator privately.
  !>
  type,   public :: base_operator_type
     private
    !> Each operator has pointers to the two function spaces on which it is
    !! defined as a map from one to the other
     type( function_space_type ), pointer :: fs_from => null( )
     type( function_space_type ), pointer :: fs_to => null( )
     integer(i_def), allocatable          :: gnu_dummy

  contains

    !> function returns functions_space which the local stencil maps from
    procedure, public :: get_fs_from

    !> function returns the the functions_space which the local stencil mapsto
    procedure, public :: get_fs_to

    !> Returns a pointer to the mesh on which the function spaces, used by
    !> function returns the enumerated integer for the functions_spaces which
    !! the local stencil maps from
    procedure, public :: which_fs_from

    !> function returns the enumerated integer for the functions_spaces which
    !! the local stencil mapsto
    procedure, public :: which_fs_to

    !> Returns a pointer to the mesh on which the function spaces, used by
    !> this operator, are built
    procedure, public :: get_mesh

 end type base_operator_type

   type, public, extends(base_operator_type) :: operator_type
    private

    !> Allocatable array of type real which holds the values of the operator
    real(kind=r_def), allocatable         :: local_stencil( :, :, : )
    !> Size of the outermost dimemsion of the local_stencil array, equal to
    !! ncell*nlayers
    integer(i_def) :: ncell_3d

  contains

    !> Function to get a proxy with public pointers to the data in a
    !! operator_type.
    procedure, public :: get_proxy => get_proxy_op
    !> Deep copy methods
    procedure, private :: operator_type_deep_copy
    procedure, public  :: deep_copy => operator_type_deep_copy

    !> Destroys object
    procedure, public :: operator_final

    ! Finalizer for the object
    final :: operator_destructor

 end type operator_type

 interface operator_type
    module procedure operator_constructor
 end interface operator_type

  !> Columnwise assembled operator type.
  !>
  !> For an operator which maps between horizontally discontinuous spaces,
  !> the matrix is block-diagonal, only coupling dofs in one vertical column.
  !> This type stores this matrix for each column c as a generalised banded
  !> matrix \f$A^{(c)}_{i,j}\f$, which only has entries for
  !> \f$ -gamma_- \le \alpha i - \beta j \le \gamma_+ \f$
  !> Internally this is stored as a three dimensional array
  !> columnwise_stencil(k,i,c) where k is the band index, i the matrix column
  !> and c the index of the horizontal cell the vertical column is attached
  !> to. \f$\alpha\f$, \f$\beta\f$ and \f$\gamma\f$ depend on the function
  !> spaces the operator maps between.
  type, public, extends(base_operator_type) :: columnwise_operator_type
    private
    !> banded matrix parameter \f$\alpha\f$
    integer(i_def) :: alpha
    !> banded matrix parameter \f$\beta\f$
    integer(i_def) :: beta
    !> banded matrix parameters \f$gamma_-\f$ and \f$\gamma_+\f$
    integer(i_def) :: gamma_m, gamma_p
    !> banded matrix bandwidth
    integer(i_def) :: bandwidth
    !> number of rows in banded matrix
    integer(i_def) :: nrow
    !> number of columns in banded matrix
    integer(i_def) :: ncol
    !> number of dofs associated with horizontal faces
    integer(i_def) :: ndof_face_to, ndof_face_from
    !> number of dofs associated with interior of cells
    integer(i_def) :: ndof_interior_to, ndof_interior_from
    !> total number of dofs associated with a cell
    integer(i_def) :: ndof_cell_to, ndof_cell_from
    !> data array for banded matrix storage
    real(kind=r_def), allocatable :: columnwise_matrix( :, :, : )
    integer(i_def) :: ncell_2d !> Number of columns (= cells in 2d grid)
    !> columnwise dof-map \f$\pi_{to}\f$, using native ordering (to-space)
    integer(kind=i_def), allocatable :: column_dofmap_to( :, : )
    !> columnwise dof-map \f$\pi_{from}\f$ , using native ordering (from-space)
    integer(kind=i_def), allocatable :: column_dofmap_from( :, : )
    !> columnwise dof-map \f$\tilde{\pi}_{to}\f$, ordering which makes the
    !> matrix banded (to-space)
    integer(kind=i_def), allocatable :: column_banded_dofmap_to( :, : )
    !> columnwise dof-map \f$\tilde{\pi}_{from}\f$, ordering which makes the
    !> matrix banded (from-space)
    integer(kind=i_def), allocatable :: column_banded_dofmap_from( :, : )
    !> indirection map \f$\mu_{to} := \pi_{to}\circ\tilde{\pi}^{-1}_{to}\f$
    integer(kind=i_def), allocatable :: indirection_dofmap_to( :)
    !> indirection map \f$\mu_{from} := \pi_{from}\circ\tilde{\pi}^{-1}_{from}\f$
    integer(kind=i_def), allocatable :: indirection_dofmap_from( :)

  contains

    !> Function to get a proxy with public pointers to the data in a
    !! operator_type.
    procedure, public :: get_proxy => get_proxy_columnwise
    !> Function to calculate greatest common divisor of two integers
    procedure, nopass, private :: get_gcd
    !> Divide alpha, beta, gamma_m and gamma_p by the GCD of alpha and beta
    procedure, private :: divide_by_gcd
    !> Procedure to build the dof- and indirectionmaps
    procedure, private :: build_dofmaps
    !> Procedures to finalize the operator
    final             :: columnwise_operator_destructor
    !> Procedure to allocate memory
    procedure, private :: allocate_memory
    !> Procedure to extract information related to function spaces and mesh
    procedure, private :: extract_mesh_fs_info
    !> Get alpha
    procedure, public :: get_alpha
    !> Get beta
    procedure, public :: get_beta
    !> Get gamma_m
    procedure, public :: get_gamma_m
    !> Get gamma_p
    procedure, public :: get_gamma_p
    procedure         :: columnwise_operator_product
    procedure         :: columnwise_operator_sum

  end type columnwise_operator_type

 interface columnwise_operator_type
    module procedure columnwise_operator_constructor, &
                     columnwise_operator_constructor_custom
 end interface columnwise_operator_type

  !> Psy layer representation of an operatpr
  !>
  !> This is an accessor class that allows access to the actual operator information
  !> with each element accessed via a public pointer.
  !>
 type, public :: operator_proxy_type

    private

    !> Each operator has pointers to the function spaces which it lives "between"
    type( function_space_type ), pointer, public :: fs_to
    type( function_space_type ), pointer, public :: fs_from
    !> Allocatable array of type real which holds the values of the operator
    real(kind=r_def), public, pointer            :: local_stencil( :, :, : )
    !> size of the outermost dimension
    integer(i_def), public                       :: ncell_3d
    integer(i_def), allocatable                  :: gnu_dummy(:)

  contains
    final :: destroy_op_proxy
 end type operator_proxy_type

  !> Psy layer representation of a columnwise operatpr
  !>
  !> This is an accessor class that allows access to the actual columnwise
  !> operator information within each element accessed via a public pointer.
  !>
  type, public :: columnwise_operator_proxy_type

    private

    integer(kind=i_def), allocatable :: gnu_dummy(:)
    !> Each operator has pointers to the function spaces which it
    !> maps "between"
    type( function_space_type ), pointer, public :: fs_to
    type( function_space_type ), pointer, public :: fs_from
    !> banded matrix parameter \f$\alpha\f$
    integer(i_def), public :: alpha
    !> banded matrix parameter \f$\beta\f$
    integer(i_def), public :: beta
    !> banded matrix parameters \f$gamma_-\f$ and \f$\gamma_+\f$
    integer(i_def), public :: gamma_m, gamma_p
    !> banded matrix bandwidth
    integer(i_def), public :: bandwidth
    !> number of rows in banded matrix
    integer(i_def), public :: nrow
    !> number of columns in banded matrix
    integer(i_def), public :: ncol
    !> data array for banded matrix storage
    real(kind=r_def), public, pointer :: columnwise_matrix( :, :, : )
    integer(i_def), public :: ncell_2d !> Number of columns (= cells in 2d grid)
    !> columnwise dof-map \f$\tilde{\pi}_{to}\f$, ordering which makes the
    !> matrix banded (to-space)
    integer(kind=i_def), public, pointer :: column_banded_dofmap_to( :, : )
    !> columnwise dof-map \f$\tilde{\pi}_{from}\f$, ordering which makes the
    !> matrix banded (from-space)
    integer(kind=i_def), public, pointer :: column_banded_dofmap_from( :, : )
    !> indirection map \f$\mu_{to} := \pi_{to}\circ\tilde{\pi}^{-1}_{to}\f$
    integer(kind=i_def), public, pointer :: indirection_dofmap_to( :)
    !> indirection map \f$\mu_{from} := \pi_{from}\circ\tilde{\pi}^{-1}_{from}\f$
    integer(kind=i_def), public, pointer :: indirection_dofmap_from( :)

  contains
    final :: destroy_col_op_proxy
 end type columnwise_operator_proxy_type

!------------------------------------------------------------------------------
! Module parameters
!------------------------------------------------------------------------------

contains

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !--------------------------------------------------------------------------

  ! base type procedures
  !>@brief Get the function space the operator maps from
  !>@return fs Function space the operator maps from
  function get_fs_from(self) result(fs)
    implicit none
    class(base_operator_type), intent(in) :: self
    type( function_space_type ), pointer :: fs

    fs => self%fs_from

    return
  end function get_fs_from

  !>@brief Get the function space the operator maps to
  !>@return fs Function space the operator maps to
  function get_fs_to(self) result(fs)
    implicit none
    class(base_operator_type), intent(in)  :: self
    type( function_space_type ), pointer :: fs

    fs => self%fs_to

    return
  end function get_fs_to

  !>@brief Get the id of the function space the operator maps from
  !>@return fs Id of function space the operator maps from
  function which_fs_from(self) result(fs)
    implicit none
    class(base_operator_type), intent(in) :: self
    integer(i_def) :: fs

    fs = self%fs_from%which()

    return
  end function which_fs_from

  !>@brief Get the id of the function space the operator maps to
  !>@return fs Id of function space the operator maps to
  function which_fs_to(self) result(fs)
    implicit none
    class(base_operator_type), intent(in)  :: self
    integer(i_def) :: fs
    fs = self%fs_to%which()

    return
  end function which_fs_to

  !>@brief Get the mesh the operator lives on
  !>@return mesh Mesh the operator lives on
  function get_mesh(self) result(mesh)

    implicit none

    class (base_operator_type), intent(in) :: self
    type(mesh_type), pointer :: mesh
    mesh => self%fs_from%get_mesh()

    return
  end function get_mesh

  ! Operator type procedures

  !> Construct an <code>operator_type</code> object.
  !>
  !> @param [in] fs_from the function space that the operator maps from
  !> @param [in] fs_to the function space that the operator maps to
  !> @return self the operator
  !>
  function operator_constructor( fs_to,fs_from ) result(self)

    implicit none

    class(function_space_type), target, intent(in) :: fs_to
    class(function_space_type), target, intent(in) :: fs_from

    type(operator_type), target :: self
    self%fs_to   => fs_to
    self%fs_from => fs_from

    self%ncell_3d = fs_from%get_ncell() * fs_from%get_nlayers()
    ! allocate the array in memory
    allocate(self%local_stencil( fs_to%get_ndf(),fs_from%get_ndf(), self%ncell_3d ) )

  end function operator_constructor

  function operator_type_deep_copy(self) result(other)

    implicit none

    class(operator_type), intent(inout) :: self
    type(operator_type) :: other
    ! make field_vector
    other = operator_type(self%fs_to,self%fs_from)
    other%local_stencil(:,:,:) = self%local_stencil(:,:,:)
  end function operator_type_deep_copy

  !> Function to create a proxy with access to the data in the operator_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> operator_type
  type(operator_proxy_type ) function get_proxy_op(self)
    implicit none
    class(operator_type), target, intent(in)  :: self

    get_proxy_op % fs_from                 => self % fs_from
    get_proxy_op % fs_to                   => self % fs_to
    get_proxy_op % local_stencil           => self % local_stencil
    get_proxy_op % ncell_3d                =  self%ncell_3d
    allocate(get_proxy_op%gnu_dummy(2))
  end function get_proxy_op

  !>@brief Finalizer for the object type
  subroutine operator_destructor(self)

    implicit none
    type(operator_type), intent(inout) :: self

    call self%operator_final()

  end subroutine operator_destructor

  !>@brief Destroys the operator type
  subroutine operator_final(self)

    implicit none

    class(operator_type), intent(inout) :: self

    nullify(self%fs_to)
    nullify(self%fs_from)
    if(allocated(self%local_stencil)) then
       deallocate(self%local_stencil)
    end if

  end subroutine operator_final

  !>@brief Destroy the operator proxy
  subroutine destroy_op_proxy(self)
    implicit none
    type(operator_proxy_type) :: self
    if(allocated(self%gnu_dummy)) then
       deallocate(self%gnu_dummy)
    end if
    nullify(self%fs_to)
    nullify(self%fs_from)
    nullify(self%local_stencil)
  end subroutine destroy_op_proxy

  ! columnwise operator type procedures

  !> Construct an <code>columnwise_operator_type</code> object.
  !>
  !> The default values of alpha, beta, gamma_m and gamma_p are derived from
  !> the function spaces.
  !>
  !> @param [in] fs_from the function space that the operator maps from
  !> @param [in] fs_to the function space that the operator maps to
  !> @return self the operator
  !>
  function columnwise_operator_constructor( fs_to,fs_from ) result(self)
    implicit none
    class(function_space_type), target, intent(in) :: fs_to
    class(function_space_type), target, intent(in) :: fs_from
    type(columnwise_operator_type), target :: self
    self%fs_to   => fs_to
    self%fs_from => fs_from
    ! Extract information related to mesh and function spaces
    call self%extract_mesh_fs_info()
    ! Set parameter parameters alpha, beta and gamma
    self%alpha = self%ndof_interior_from + self%ndof_face_from
    self%beta = self%ndof_interior_to + self%ndof_face_to
    self%gamma_m = (self%ndof_cell_from-1) &
         * (self%ndof_interior_to+self%ndof_face_to)
    self%gamma_p = (self%ndof_cell_to-1) &
         * (self%ndof_interior_from+self%ndof_face_from)
    ! Divide alpha, beta, gamma_m and gamma_p by gcd(alpha,beta)
    call self%divide_by_gcd()
    ! Bandwidth = 1+ceil((gamma_m+gamma_p)/beta)
    self%bandwidth = 1+ceiling( (real( (self%gamma_m+self%gamma_p), kind=r_def) / &
         (real(self%beta,kind=r_def))),i_def)
    ! allocate memory
    call self%allocate_memory()
    self%columnwise_matrix(:,:,:) = 0.0_r_def
    ! build dof-maps
    call self%build_dofmaps()
  end function columnwise_operator_constructor

  !> Construct an <code>columnwise_operator_type</code> object.
  !>
  !> The values of alpha, beta, gamma_m and gamma_p are given explicitly.
  !> This is necessary when building a columnwise operator which is the
  !> sum, product etc. of other columnwise operators.
  !>
  !> @param [in] fs_from the function space that the operator maps from
  !> @param [in] fs_to the function space that the operator maps to
  !> @param [in] alpha banded matrix parameter \f$\alpha\f$
  !> @param [in] beta banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p banded matrix parameter \f$\gamma_+\f$
  !> @return self the operator
  !>
  function columnwise_operator_constructor_custom( fs_to,fs_from, &
                                                   alpha,         &
                                                   beta,          &
                                                   gamma_m,       &
                                                   gamma_p ) result(self)
    implicit none
    class(function_space_type), target, intent(in) :: fs_to
    class(function_space_type), target, intent(in) :: fs_from
    integer(kind=i_def), intent(in) :: alpha, beta, gamma_m, gamma_p
    type(columnwise_operator_type), target :: self

    self%fs_to   => fs_to
    self%fs_from => fs_from
   ! Extract information related to mesh and function spaces
    call self%extract_mesh_fs_info()
   ! Set parameter parameters alpha, beta and gamma
    self%alpha = alpha
    self%beta = beta
    self%gamma_m = gamma_m
    self%gamma_p = gamma_p
   ! Divide alpha, beta, gamma_m and gamma_p by gcd(alpha,beta)
    call self%divide_by_gcd()
   ! Bandwidth = 1+ceil((gamma_m+gamma_p)/beta)
    self%bandwidth = 1+ceiling((self%gamma_m+self%gamma_p)/(real(self%beta,kind=r_def)),i_def)
   ! allocate memory
    call self%allocate_memory()
    self%columnwise_matrix(:,:,:) = 0.0_r_def
   ! build dof-maps
    call self%build_dofmaps()
  end function columnwise_operator_constructor_custom

  !> @brief calculate data members that depend on the function space
  !>        and mesh
  !> param [inout] self instance of the type itself
  subroutine extract_mesh_fs_info(self)
    implicit none
    class(columnwise_operator_type), intent(inout) :: self
    integer(kind=i_def) :: nlayers ! Number of vertical layers
    ! Extract function types and other function space information
    self%ndof_face_to = self%fs_to%get_ndof_face()
    self%ndof_face_from = self%fs_from%get_ndof_face()
    self%ndof_interior_to = self%fs_to%get_ndof_interior()
    self%ndof_interior_from = self%fs_from%get_ndof_interior()
   ! The following two formulae for the total number of dofs only
   ! hold for horizontally discontinuous function space
    self%ndof_cell_to = self%ndof_interior_to + 2*self%ndof_face_to
    self%ndof_cell_from = self%ndof_interior_from + 2*self%ndof_face_from
   ! Check that we are really horizontally discontinuous by comparing to
   ! total number of dofs in general formula
    if (self%ndof_cell_to /= self%fs_to%get_ndf()) then
       call log_event("Operator_mod:extract_mesh_fs_info():Function space " // &
          "mapped to in columnwise operator is not horizontally discontinuous",&
          LOG_LEVEL_ERROR)
    endif
    if (self%ndof_cell_from /= self%fs_from%get_ndf()) then
       call log_event("Operator_mod:extract_mesh_fs_info():Function space " //   &
          "mapped from in columnwise operator is not horizontally discontinuous",&
          LOG_LEVEL_ERROR)
    endif

    ! number of vertical layers
    nlayers = self%fs_to%get_nlayers()
    self%ncell_2d = self%fs_from%get_ncell()

    ! Work out parameters of banded matrix
    ! Number of matrix rows
    self%nrow = nlayers * self%ndof_interior_to &
              + (nlayers+1) * self%ndof_face_to
    self%ncol = nlayers * self%ndof_interior_from &
              + (nlayers+1) * self%ndof_face_from

  end subroutine extract_mesh_fs_info

  !> @brief Allocate memory
  !> param [inout] self instance of the type itself
  subroutine allocate_memory(self)
    implicit none
    class(columnwise_operator_type), intent(inout) :: self
    integer(kind=i_def) :: nlayers ! Number of vertical layers

    nlayers = self%fs_to%get_nlayers()
    ! allocate the array in memory
    allocate(self%columnwise_matrix(self%bandwidth, &
                                    self%nrow,      &
                                    self%ncell_2d))
    ! allocate memory for dofmaps
    allocate(self%column_dofmap_to(self%ndof_cell_to,nlayers))
    allocate(self%column_dofmap_from(self%ndof_cell_from,nlayers))
    allocate(self%column_banded_dofmap_to(self%ndof_cell_to,nlayers))
    allocate(self%column_banded_dofmap_from(self%ndof_cell_from,nlayers))
    allocate(self%indirection_dofmap_to(self%nrow))
    allocate(self%indirection_dofmap_from(self%ncol))
  end subroutine allocate_memory

  !> @brief Divide alpha, beta, gamma_m and gamma_p by the GCD of alpha and
  !>        beta
  !> @param [inout] self Instance of type
  subroutine divide_by_gcd( self )
    implicit none
    class(columnwise_operator_type), intent(inout) :: self
    integer(kind=i_def) :: gcd
    gcd = get_gcd(self%alpha,self%beta)
    if (gcd > 1) then
       self%alpha = self%alpha/gcd
       self%beta = self%beta/gcd
       self%gamma_m = self%gamma_m/gcd
       self%gamma_p = self%gamma_p/gcd
    end if
  end subroutine divide_by_gcd

  !> @brief Calculate GCD of two integers
  !>
  !> @details Auxilliary function, calculates and returns greatest common
  !> divisor of two integer numbers.
  !>
  !> @param [in] a First integer
  !> @param [in] b Second integer
  function get_gcd(a,b) result(gcd)
    implicit none
    integer(kind=i_def), intent(in) :: a,b
    integer(kind=i_def) :: gcd ! result
    integer(kind=i_def) :: x_large, x_small, x_tmp
    ! x_large = absolute value of larger number
    ! x_small = absolute value of smaller number
    x_large = abs(a)
    x_small = abs(b)
    if (x_large < x_small) then
       ! Swap numbers if abs(a) < abs(b)
       x_tmp = x_large
       x_large = x_small
       x_small = x_tmp
    end if
    if (x_large == 0) then
       ! special case: a=b
       gcd = 1
    else
       ! Non-recursive implementation
       do while (x_small > 0)
          x_tmp = x_small
          x_small = MOD(x_large,x_small)
          x_large = x_tmp
       end do
       gcd = x_large
    end if
  end function get_gcd

  !> @brief Build the dof- and indirection- maps
  !>
  !> @details Construct the following indirection maps (each for both the to-
  !> and from-functionspaces of the operator):
  !> * column_dofmap_XX(i,k): offset (relative to index of first unknown in
  !>   column) of the i-th dof on level k, using the native DYNAMO
  !>   ordering
  !> * column_banded_dofmap_XX(i,k): offset (relative to first index of first
  !>   unknown in the column) of the i-th dof on level k, using an ordering
  !>   which makes the columnwise assembled matrix banded
  !> * indirection_map_XX = column_dofmap_XX^{-1}*column_banded_dofmap_XX
  !>   allows conversion between the two numberings
  subroutine build_dofmaps(self)
    implicit none
    class(columnwise_operator_type), target, intent(inout) :: self
    integer(kind=i_def) :: k, df ! Loop variables
    integer(kind=i_def) :: nlayers ! Number of vertical layers
    integer(kind=i_def), pointer :: dofmap_to(:)
    integer(kind=i_def), pointer :: dofmap_from(:)
    ! Get dof-maps for first vertical column (any would do, since we are
    ! horizontally discontinuous
    dofmap_to => self%fs_to%get_cell_dofmap(1)
    dofmap_from => self%fs_from%get_cell_dofmap(1)

    ! Check that the first entry in the dofmaps is smaller than all other
    ! entries
    if (any(dofmap_to(2:)<dofmap_to(1))) then
       call log_event("First entry in dofmap is not smallest entry for to-space", LOG_LEVEL_ERROR)
    endif
    if (any(dofmap_from(2:)<dofmap_from(1))) then
       call log_event("First entry in dofmap is not smallest entry for from-space",LOG_LEVEL_ERROR)
    endif

    nlayers = self%fs_to%get_nlayers()
    do k=1, nlayers
       ! to-space
       do df=1, self%ndof_cell_to
          self%column_dofmap_to(df,k) = (dofmap_to(df)-dofmap_to(1)+1)+(k-1)
          self%column_banded_dofmap_to(df,k) = (k-1) * ( self%ndof_interior_to &
                                                  + self%ndof_face_to) + df
          self%indirection_dofmap_to(self%column_banded_dofmap_to(df,k)) &
            = self%column_dofmap_to(df,k)
       end do
       ! from-space
       do df=1, self%ndof_cell_from
          self%column_dofmap_from(df,k) = (dofmap_from(df)-dofmap_from(1)+1)+(k-1)
          self%column_banded_dofmap_from(df,k) = (k-1) * ( self%ndof_interior_from &
                                                    + self%ndof_face_from) + df
          self%indirection_dofmap_from(self%column_banded_dofmap_from(df,k)) &
            = self%column_dofmap_from(df,k)
       end do
    end do
  end subroutine build_dofmaps

  !> @brief Destroy instance of columnwise operator
  !> @param [in] self Instance to be destroyed
  subroutine columnwise_operator_destructor(self)
    implicit none
    type(columnwise_operator_type), intent(inout) :: self
    nullify(self%fs_to)
    nullify(self%fs_from)
    ! Columnwise banded matrix
    if(allocated(self%columnwise_matrix)) then
       deallocate(self%columnwise_matrix)
    end if
    ! dof-maps
    if (allocated(self%column_dofmap_to)) then
       deallocate(self%column_dofmap_to)
    endif
    if (allocated(self%column_dofmap_from)) then
       deallocate(self%column_dofmap_from)
    end if
    if (allocated(self%column_banded_dofmap_to)) then
       deallocate(self%column_banded_dofmap_to)
    end if
    if (allocated(self%column_banded_dofmap_from)) then
       deallocate(self%column_banded_dofmap_from)
    end if
    ! indirection-maps
    if (allocated(self%indirection_dofmap_to)) then
       deallocate(self%indirection_dofmap_to)
    end if
    if (allocated(self%indirection_dofmap_from)) then
       deallocate(self%indirection_dofmap_from)
    end if
  end subroutine columnwise_operator_destructor

  ! Getter functions

  function get_alpha ( self ) result (alpha)
    implicit none
    class(columnwise_operator_type), intent(in) :: self
    integer(kind=i_def) :: alpha
    alpha = self%alpha
  end function get_alpha
  function get_beta ( self ) result (beta)
    implicit none
    class(columnwise_operator_type), intent(in) :: self
    integer(kind=i_def) :: beta
    beta = self%beta
  end function get_beta
  function get_gamma_m ( self ) result (gamma_m)
    implicit none
    class(columnwise_operator_type), intent(in) :: self
    integer(kind=i_def) :: gamma_m
   gamma_m = self%gamma_m
 end function get_gamma_m

  function get_gamma_p ( self ) result (gamma_p)
    implicit none
    class(columnwise_operator_type), intent(in) :: self
    integer(kind=i_def) :: gamma_p
    gamma_p = self%gamma_p
  end function get_gamma_p

  ! proxy operator functions

  !> Function to create a proxy with access to the data in the
  !> columnwise_operator_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> operator_type

 type(columnwise_operator_proxy_type ) function get_proxy_columnwise(self)
    implicit none
    class(columnwise_operator_type), target, intent(in)  :: self!
    get_proxy_columnwise % fs_from                 => self % fs_from
    get_proxy_columnwise % fs_to                   => self % fs_to
    get_proxy_columnwise % alpha                   =  self % alpha
    get_proxy_columnwise % beta                    =  self % beta
    get_proxy_columnwise % gamma_m                 =  self % gamma_m
    get_proxy_columnwise % gamma_p                 =  self % gamma_p
    get_proxy_columnwise % bandwidth               =  self % bandwidth
    get_proxy_columnwise % nrow                    =  self % nrow
    get_proxy_columnwise % ncol                    =  self % ncol
    get_proxy_columnwise % columnwise_matrix       => self % columnwise_matrix
    get_proxy_columnwise % ncell_2d                =  self % ncell_2d
    get_proxy_columnwise % column_banded_dofmap_to => self % column_banded_dofmap_to
    get_proxy_columnwise % column_banded_dofmap_from => self % column_banded_dofmap_from
    get_proxy_columnwise % indirection_dofmap_to   => self % indirection_dofmap_to
    get_proxy_columnwise % indirection_dofmap_from => self % indirection_dofmap_from
    allocate(get_proxy_columnwise%gnu_dummy(2))

  end function get_proxy_columnwise

  !> @brief create <code>columnwise_operator_type</code> object which can
  !> store the sum of two other columnwise operators op_C = op_A + op_B
  function columnwise_operator_sum( self, op_B ) result(op_C)
    implicit none
    ! Parameters
    class(columnwise_operator_type), target, intent(in) :: self, op_B
    type(columnwise_operator_type) :: op_C
    ! Internal variables
    class(function_space_type), pointer :: fs_to
    class(function_space_type), pointer :: fs_from
    integer(kind=i_def) :: alpha, beta, gamma_m, gamma_p

    ! check that types are compatible
    if (self%which_fs_to() /= op_B%which_fs_to()) then
       call log_event("to-function spaces of operators do not match", &
            LOG_LEVEL_ERROR)
    end if
    if ( self%which_fs_from() /= op_B%which_fs_from()) then
       call log_event("from-function spaces of operators do not match", &
            LOG_LEVEL_ERROR)
    end if
    fs_to => self%fs_to
    fs_from => self%fs_from
    ! Check that alpha and beta are identical
    if (self%alpha /= op_B%alpha) then
       call log_event("parameter alpha of operators do not match", &
            LOG_LEVEL_ERROR)
    end if
    if (self%beta /= op_B%beta) then
       call log_event("parameter beta of operators do not match", &
            LOG_LEVEL_ERROR)
    end if
    alpha = self%alpha
    beta = self%beta
    gamma_m = MAX(self%gamma_m,op_B%gamma_m)
    gamma_p = MAX(self%gamma_p,op_B%gamma_p)
    ! construct operator
    op_C = columnwise_operator_type(fs_to, fs_from, &
                                    alpha, beta, gamma_m, gamma_p)
  end function columnwise_operator_sum
  !> @brief create <code>columnwise_operator_type</code> object which can
  !> store the product of two other columnwise operators op_C = op_A * op_B
  function columnwise_operator_product( self, op_B ) result(op_c)
    implicit none
    ! Parameters
    class(columnwise_operator_type), target, intent(in) :: self, op_B
    type(columnwise_operator_type) :: op_C
    ! Internal variables
    class(function_space_type), pointer :: fs_to
    class(function_space_type), pointer :: fs_from
    integer(kind=i_def) :: alpha, beta, gamma_m, gamma_p
   ! check that types are compatible
    if (self%which_fs_from() /= op_B%which_fs_to()) then
       call log_event("to-function spaces of operator B do not match from-function space of operator A", &
            LOG_LEVEL_ERROR)
    end if
    fs_to => self%fs_to
    fs_from => op_B%fs_from
    alpha = self%alpha * op_B%alpha
    beta = self%beta * op_B%beta
    gamma_m = op_B%alpha * self%gamma_m + self%beta * op_B%gamma_m
    gamma_p = op_B%alpha * self%gamma_p + self%beta * op_B%gamma_p
   ! construct operator
   op_C = columnwise_operator_type(fs_to, fs_from, &
                                    alpha, beta, gamma_m, gamma_p)
  end function columnwise_operator_product

  !>@brief Destroy the column operator proxy
  subroutine destroy_col_op_proxy(self)
    implicit none
    type(columnwise_operator_proxy_type) :: self
    if(allocated(self%gnu_dummy)) then
       deallocate(self%gnu_dummy)
    end if
    nullify( self%fs_to )
    nullify(self%fs_from)
    nullify(self%columnwise_matrix)
    nullify(self%column_banded_dofmap_from)
    nullify(self%indirection_dofmap_to)
    nullify(self%indirection_dofmap_from)

  end subroutine destroy_col_op_proxy

end module operator_mod
