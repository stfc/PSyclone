!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------!
!> @brief A module providing columnwise operator related classes.
!>
!> @details Columnwise assembled operator for horizontally discontinuous spaces
!!          (i.e. the matrix is assembled in each vertical column of the grid).

module columnwise_operator_mod

  use constants_mod,            only : r_solver, i_def
  use function_space_mod,       only : function_space_type
  use mesh_mod,                 only : mesh_type
  use log_mod,                  only : log_event, LOG_LEVEL_ERROR
  use operator_parent_mod,      only : operator_parent_type, &
                                       operator_parent_proxy_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> Algorithm layer representation of a columnwise operator type.
  !>
  !> Columnwise assembled operator type.
  !>
  !> For an operator which maps between horizontally discontinuous spaces,
  !! the matrix is block-diagonal, only coupling DoFs in one vertical column.
  !! This type stores this matrix for each column c as a generalised banded
  !! matrix \f$A^{(c)}_{i,j}\f$, which only has entries for
  !! \f$ -gamma_- \le \alpha i - \beta j \le \gamma_+ \f$
  !! Internally this is stored as a three dimensional array
  !! columnwise_stencil(k,i,c) where k is the band index, i the matrix column
  !! and c the index of the horizontal cell the vertical column is attached
  !! to. \f$\alpha\f$, \f$\beta\f$ and \f$\gamma\f$ depend on the function
  !! spaces the operator maps between.
  type, public, extends(operator_parent_type) :: columnwise_operator_type
    private
    !> Banded matrix parameter \f$\alpha\f$
    integer(kind=i_def) :: alpha
    !> Banded matrix parameter \f$\beta\f$
    integer(kind=i_def) :: beta
    !> Banded matrix parameters \f$gamma_-\f$ and \f$\gamma_+\f$
    integer(kind=i_def) :: gamma_m, gamma_p
    !> Banded matrix bandwidth
    integer(kind=i_def) :: bandwidth
    !> Number of rows in banded matrix
    integer(kind=i_def) :: nrow
    !> Number of columns in banded matrix
    integer(kind=i_def) :: ncol
    !> Number of DoFs associated with horizontal faces
    integer(kind=i_def) :: ndof_face_to, ndof_face_from
    !> Number of DoFs associated with interior of cells
    integer(kind=i_def) :: ndof_interior_to, ndof_interior_from
    !> total number of DoFs associated with a cell
    integer(kind=i_def) :: ndof_cell_to, ndof_cell_from
    !> data array for banded matrix storage
    real(kind=r_solver), allocatable :: columnwise_matrix( :, :, : )
    integer(kind=i_def) :: ncell_2d !> Number of columns (= cells in 2d grid)
    !> Columnwise DoF-map \f$\pi_{to}\f$, using native ordering (to-space)
    integer(kind=i_def), allocatable :: column_dofmap_to( :, : )
    !> Columnwise DoF-map \f$\pi_{from}\f$ , using native ordering (from-space)
    integer(kind=i_def), allocatable :: column_dofmap_from( :, : )
    !> Columnwise DoF-map \f$\tilde{\pi}_{to}\f$, ordering which makes the
    !> matrix banded (to-space)
    integer(kind=i_def), allocatable :: column_banded_dofmap_to( :, : )
    !> Columnwise DoF-map \f$\tilde{\pi}_{from}\f$, ordering which makes the
    !> matrix banded (from-space)
    integer(kind=i_def), allocatable :: column_banded_dofmap_from( :, : )
    !> Indirection map \f$\mu_{to} := \pi_{to}\circ\tilde{\pi}^{-1}_{to}\f$
    integer(kind=i_def), allocatable :: indirection_dofmap_to( :)
    !> Indirection map \f$\mu_{from} := \pi_{from}\circ\tilde{\pi}^{-1}_{from}\f$
    integer(kind=i_def), allocatable :: indirection_dofmap_from( :)

  contains

    !> Initialise a columnwise operator object
    procedure, public :: columnwise_operator_initialiser
    !> Initialise a custom columnwise operator object
    procedure, public :: columnwise_operator_initialiser_custom
    !> Generic interface to the specific initialisers
    generic           :: initialise => columnwise_operator_initialiser, &
                                       columnwise_operator_initialiser_custom
    !> Function to get a proxy with public pointers to the data in an
    !! operator_type.
    procedure, public :: get_proxy => get_proxy_columnwise
    !> Function to calculate greatest common divisor of two integers
    procedure, nopass, private :: get_gcd
    !> Divide alpha, beta, gamma_m and gamma_p by the GCD of alpha and beta
    procedure, private :: divide_by_gcd
    !> Procedure to build the dof- and indirectionmaps
    procedure, private :: build_dofmaps
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
    !> Procedures to finalize the operator
    final             :: columnwise_operator_destructor
  end type columnwise_operator_type

  !> Psy layer representation of a columnwise operatpr
  !>
  !> This is an accessor class that allows access to the actual columnwise
  !> operator information within each element accessed via a public pointer.
  !>
  type, public, extends(operator_parent_proxy_type) :: &
                                                columnwise_operator_proxy_type
    private

    !> Banded matrix parameter \f$\alpha\f$
    integer(kind=i_def), public :: alpha
    !> Banded matrix parameter \f$\beta\f$
    integer(kind=i_def), public :: beta
    !> Banded matrix parameters \f$gamma_-\f$ and \f$\gamma_+\f$
    integer(kind=i_def), public :: gamma_m, gamma_p
    !> Banded matrix bandwidth
    integer(kind=i_def), public :: bandwidth
    !> Number of rows in banded matrix
    integer(kind=i_def), public :: nrow
    !> Number of columns in banded matrix
    integer(kind=i_def), public :: ncol
    !> Data array for banded matrix storage
    real(kind=r_solver), public, pointer :: columnwise_matrix( :, :, : )
    integer(kind=i_def), public :: ncell_2d !> Number of columns (= cells in 2d grid)
    !> Columnwise DoF-map \f$\tilde{\pi}_{to}\f$, ordering which makes the
    !> matrix banded (to-space)
    integer(kind=i_def), public, pointer :: column_banded_dofmap_to( :, : )
    !> Columnwise DoF-map \f$\tilde{\pi}_{from}\f$, ordering which makes the
    !> matrix banded (from-space)
    integer(kind=i_def), public, pointer :: column_banded_dofmap_from( :, : )
    !> Indirection map \f$\mu_{to} := \pi_{to}\circ\tilde{\pi}^{-1}_{to}\f$
    integer(kind=i_def), public, pointer :: indirection_dofmap_to( :)
    !> Indirection map \f$\mu_{from} := \pi_{from}\circ\tilde{\pi}^{-1}_{from}\f$
    integer(kind=i_def), public, pointer :: indirection_dofmap_from( :)

  contains
    final :: columnwise_operator_proxy_destructor
 end type columnwise_operator_proxy_type

!______end of type declarations_______________________________________________

contains

  ! Columnwise operator type procedures

  !> @brief Initialise an <code>columnwise_operator_type</code> object.
  !>
  !> @details The default values of alpha, beta, gamma_m and gamma_p
  !!          are derived from the function spaces.
  !>
  !> @param[in,out] self    Instance of columnwise_operator_type
  !> @param[in]     fs_from The function space that the operator maps from
  !> @param[in]     fs_to   The function space that the operator maps to
  !>
  subroutine columnwise_operator_initialiser( self, fs_to, fs_from )
    implicit none
    class(columnwise_operator_type), target, intent(inout) :: self
    class(function_space_type),      target, intent(in)    :: fs_to
    class(function_space_type),      target, intent(in)    :: fs_from

    ! Initialise the parent
    call self%operator_parent_initialiser( fs_to, fs_from )

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
    self%bandwidth = 1+ceiling( (real( (self%gamma_m+self%gamma_p), kind=r_solver) / &
         (real(self%beta,kind=r_solver))), i_def)
    ! Allocate memory
    call self%allocate_memory()
    self%columnwise_matrix(:,:,:) = 0.0_r_solver
    ! Build DoF-maps
    call self%build_dofmaps()
  end subroutine columnwise_operator_initialiser

  !> @brief Initialise a custom <code>columnwise_operator_type</code>
  !!        object.
  !>
  !> @details The values of alpha, beta, gamma_m and gamma_p are given
  !!          explicitly. This is necessary when building a columnwise
  !!          operator which is the sum, product etc. of other
  !!          columnwise operators.
  !>
  !> @param[in,out] self    Instance of columnwise_operator_type
  !> @param[in]     fs_from The function space that the operator maps from
  !> @param[in]     fs_to   The function space that the operator maps to
  !> @param[in]     alpha   Banded matrix parameter \f$\alpha\f$
  !> @param[in]     beta    Banded matrix parameter \f$\beta\f$
  !> @param[in]     gamma_m Banded matrix parameter \f$\gamma_-\f$
  !> @param[in]     gamma_p Banded matrix parameter \f$\gamma_+\f$
  !>
  subroutine columnwise_operator_initialiser_custom( self,           &
                                                     fs_to, fs_from, &
                                                     alpha,          &
                                                     beta,           &
                                                     gamma_m,        &
                                                     gamma_p )
    implicit none
    class(columnwise_operator_type), target, intent(inout) :: self
    class(function_space_type),      target, intent(in)    :: fs_to
    class(function_space_type),      target, intent(in)    :: fs_from
    integer(kind=i_def), intent(in) :: alpha, beta, gamma_m, gamma_p

    ! Initialise the parent
    call self%operator_parent_initialiser( fs_to, fs_from )

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
    self%bandwidth = 1+ceiling((self%gamma_m+self%gamma_p)/(real(self%beta,kind=r_solver)), i_def)
   ! Allocate memory
    call self%allocate_memory()
    self%columnwise_matrix(:,:,:) = 0.0_r_solver
   ! Build DoF-maps
    call self%build_dofmaps()
  end subroutine columnwise_operator_initialiser_custom

  !> @brief Function to create a proxy with access to the data in the
  !!        columnwise_operator_type.
  !>
  !> @param[in,out] self Instance of columnwise_operator_type
  !>
  !> @return The proxy type with public pointers to the elements of
  !!         columnwise_operator_type
  type(columnwise_operator_proxy_type ) function get_proxy_columnwise(self)
    implicit none
    class(columnwise_operator_type), target, intent(in)  :: self

    ! Call the routine that initialises the proxy for data held in the parent
    call self%operator_parent_proxy_initialiser(get_proxy_columnwise)

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

  end function get_proxy_columnwise

  !> @brief Calculate GCD of two integers.
  !>
  !> @details Auxilliary function, calculates and returns greatest common
  !!          divisor of two integer numbers.
  !>
  !> @param[in] a First integer
  !> @param[in] b Second integer
  !>
  !> @return The greatest common divisor
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
       ! Special case: a=b
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

  !> @brief Divide alpha, beta, gamma_m and gamma_p by the GCD of alpha
  !!        and beta.
  !>
  !> @param[in,out] self Instance of columnwise_operator_type
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

  !> @brief Build the DoF- and indirection- maps
  !>
  !> @details Construct the following indirection maps (each for both the to-
  !!          and from-functionspaces of the operator):
  !!          * column_dofmap_XX(i,k): offset (relative to index of first
  !!            unknown in column) of the i-th dof on level k, using the
  !!            native LFRic ordering;
  !!          * column_banded_dofmap_XX(i,k): offset (relative to first
  !!            index of first unknown in the column) of the i-th DoF on
  !!            level k, using an ordering which makes the columnwise
  !!            assembled matrix banded;
  !!          * indirection_map_XX = column_dofmap_XX^{-1}*column_banded_dofmap_XX,
  !!            allows conversion between the two numberings.
  !>
  !> @param[in,out] self Instance of columnwise_operator_type
  !>
  subroutine build_dofmaps(self)
    implicit none
    class(columnwise_operator_type), target, intent(inout) :: self
    type( function_space_type ), pointer :: fs_to => null( )
    type( function_space_type ), pointer :: fs_from => null( )
    integer(kind=i_def) :: k, df ! Loop variables
    integer(kind=i_def) :: nlayers ! Number of vertical layers
    integer(kind=i_def), pointer :: dofmap_to(:)
    integer(kind=i_def), pointer :: dofmap_from(:)

    ! Get pointers to the function spaces used in this operator
    fs_to=>self%get_fs_to()
    fs_from=>self%get_fs_from()
    ! Get DoF-maps for first vertical column (any would do, since we are
    ! horizontally discontinuous
    dofmap_to => fs_to%get_cell_dofmap(1)
    dofmap_from => fs_from%get_cell_dofmap(1)

    ! Check that the first entry in the dofmaps is smaller than all other
    ! entries
    if (any(dofmap_to(2:)<dofmap_to(1))) then
       call log_event("First entry in dofmap is not smallest entry for to-space", LOG_LEVEL_ERROR)
    endif
    if (any(dofmap_from(2:)<dofmap_from(1))) then
       call log_event("First entry in dofmap is not smallest entry for from-space",LOG_LEVEL_ERROR)
    endif

    nlayers = fs_to%get_nlayers()
    do k=1, nlayers
       ! To-space
       do df=1, self%ndof_cell_to
          self%column_dofmap_to(df,k) = (dofmap_to(df)-dofmap_to(1)+1)+(k-1)
          self%column_banded_dofmap_to(df,k) = (k-1) * ( self%ndof_interior_to &
                                                  + self%ndof_face_to) + df
          self%indirection_dofmap_to(self%column_banded_dofmap_to(df,k)) &
            = self%column_dofmap_to(df,k)
       end do
       ! From-space
       do df=1, self%ndof_cell_from
          self%column_dofmap_from(df,k) = (dofmap_from(df)-dofmap_from(1)+1)+(k-1)
          self%column_banded_dofmap_from(df,k) = (k-1) * ( self%ndof_interior_from &
                                                    + self%ndof_face_from) + df
          self%indirection_dofmap_from(self%column_banded_dofmap_from(df,k)) &
            = self%column_dofmap_from(df,k)
       end do
    end do
  end subroutine build_dofmaps

  !> @brief Allocate memory.
  !>
  !> @param[in,out] self Instance of columnwise_operator_type
  subroutine allocate_memory(self)
    implicit none
    class(columnwise_operator_type), intent(inout) :: self
    type( function_space_type ), pointer :: fs_to => null( )
    integer(kind=i_def) :: nlayers ! Number of vertical layers

    ! Get pointer to the "to" function space used in this operator
    fs_to=>self%get_fs_to()
    nlayers = fs_to%get_nlayers()
    ! Allocate the array in memory
    allocate(self%columnwise_matrix(self%bandwidth, &
                                    self%nrow,      &
                                    self%ncell_2d))
    ! Allocate memory for dofmaps
    allocate(self%column_dofmap_to(self%ndof_cell_to,nlayers))
    allocate(self%column_dofmap_from(self%ndof_cell_from,nlayers))
    allocate(self%column_banded_dofmap_to(self%ndof_cell_to,nlayers))
    allocate(self%column_banded_dofmap_from(self%ndof_cell_from,nlayers))
    allocate(self%indirection_dofmap_to(self%nrow))
    allocate(self%indirection_dofmap_from(self%ncol))
  end subroutine allocate_memory

  !> @brief Calculate data members that depend on the function space
  !!        and mesh.
  !>
  !> @param[in,out] self Instance of columnwise_operator_type
  subroutine extract_mesh_fs_info(self)
    implicit none
    class(columnwise_operator_type), intent(inout) :: self
    type( function_space_type ), pointer :: fs_to => null( )
    type( function_space_type ), pointer :: fs_from => null( )
    integer(kind=i_def) :: nlayers ! Number of vertical layers
    ! Get pointers to the function spaces used in this operator
    fs_to=>self%get_fs_to()
    fs_from=>self%get_fs_from()
    ! Extract function types and other function space information
    self%ndof_face_to = fs_to%get_ndof_face_v()
    self%ndof_face_from = fs_from%get_ndof_face_v()
    self%ndof_interior_to = fs_to%get_ndof_interior()
    self%ndof_interior_from = fs_from%get_ndof_interior()
   ! The following two formulae for the total number of DoFs only
   ! hold for horizontally discontinuous function space
    self%ndof_cell_to = self%ndof_interior_to + 2*self%ndof_face_to
    self%ndof_cell_from = self%ndof_interior_from + 2*self%ndof_face_from
   ! Check that we are really horizontally discontinuous by comparing to
   ! total number of DoFs in general formula
    if (self%ndof_cell_to /= fs_to%get_ndf()) then
       call log_event("Operator_mod:extract_mesh_fs_info():Function space " // &
          "mapped to in columnwise operator is not horizontally discontinuous",&
          LOG_LEVEL_ERROR)
    endif
    if (self%ndof_cell_from /= fs_from%get_ndf()) then
       call log_event("Operator_mod:extract_mesh_fs_info():Function space " //   &
          "mapped from in columnwise operator is not horizontally discontinuous",&
          LOG_LEVEL_ERROR)
    endif

    ! Number of vertical layers
    nlayers = fs_to%get_nlayers()
    self%ncell_2d = fs_from%get_ncell()

    ! Work out parameters of banded matrix
    ! Number of matrix rows
    self%nrow = nlayers * self%ndof_interior_to &
              + (nlayers+1) * self%ndof_face_to
    self%ncol = nlayers * self%ndof_interior_from &
              + (nlayers+1) * self%ndof_face_from

  end subroutine extract_mesh_fs_info

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

  !> @brief Create <code>columnwise_operator_type</code> object which
  !!        can store the product of two other columnwise operators
  !!        op_C = op_A * op_B.
  !>
  !> @param[in] self Instance of columnwise_operator_type
  !> @param[in] op_B Second columnwise operator
  !>
  !> @return op_C Product of two columnwise operators
  !>
  function columnwise_operator_product( self, op_B ) result(op_C)
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
    fs_to => self%get_fs_to()
    fs_from => op_B%get_fs_from()
    alpha = self%alpha * op_B%alpha
    beta = self%beta * op_B%beta
    gamma_m = op_B%alpha * self%gamma_m + self%beta * op_B%gamma_m
    gamma_p = op_B%alpha * self%gamma_p + self%beta * op_B%gamma_p
   ! Construct operator
   call op_C%initialise(fs_to, fs_from, &
                        alpha, beta, gamma_m, gamma_p)
  end function columnwise_operator_product

  !> @brief Create <code>columnwise_operator_type</code> object which
  !!        can store the sum of two other columnwise operators
  !!        op_C = op_A + op_B.
  !>
  !> @param[in] self Instance of columnwise_operator_type
  !> @param[in] op_B Second columnwise operator
  !>
  !> @return op_C Sum of two columnwise operators
  !>
  function columnwise_operator_sum( self, op_B ) result(op_C)
    implicit none
    ! Parameters
    class(columnwise_operator_type), target, intent(in) :: self, op_B
    type(columnwise_operator_type) :: op_C
    ! Internal variables
    class(function_space_type), pointer :: fs_to
    class(function_space_type), pointer :: fs_from
    integer(kind=i_def) :: alpha, beta, gamma_m, gamma_p

    ! Check that types are compatible
    if (self%which_fs_to() /= op_B%which_fs_to()) then
       call log_event("to-function spaces of operators do not match", &
            LOG_LEVEL_ERROR)
    end if
    if ( self%which_fs_from() /= op_B%which_fs_from()) then
       call log_event("from-function spaces of operators do not match", &
            LOG_LEVEL_ERROR)
    end if
    fs_to => self%get_fs_to()
    fs_from => self%get_fs_from()
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
    ! Construct operator
    call op_C%initialise(fs_to, fs_from, &
                         alpha, beta, gamma_m, gamma_p)
  end function columnwise_operator_sum

  !> @brief Destroy instance of columnwise operator.
  !>
  !> @param[in,out] self Instance of columnwise_operator_type
  !>
  subroutine columnwise_operator_destructor(self)
    implicit none
    type(columnwise_operator_type), intent(inout) :: self

    call self%destroy_operator_parent()
    ! Columnwise banded matrix
    if(allocated(self%columnwise_matrix)) then
       deallocate(self%columnwise_matrix)
    end if
    ! DoF-maps
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

  !> @brief Destroy the column operator proxy.
  !>
  !> @param[in,out] self Instance of columnwise_operator_proxy_type
  !>
  subroutine columnwise_operator_proxy_destructor(self)
    implicit none
    type(columnwise_operator_proxy_type) :: self
    call self%destroy_operator_parent_proxy()
    nullify(self%columnwise_matrix)
    nullify(self%column_banded_dofmap_from)
    nullify(self%indirection_dofmap_to)
    nullify(self%indirection_dofmap_from)

  end subroutine columnwise_operator_proxy_destructor

end module columnwise_operator_mod
