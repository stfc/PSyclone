!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Holds support routines for instantiating a function space.
!>
module function_space_constructor_helper_functions_mod

  use constants_mod,         only: i_def, i_halo_index, r_def, IMDI, l_def
  use local_mesh_mod,        only: local_mesh_type
  use mesh_mod,              only: mesh_type
  use fs_continuity_mod,     only: W0, W1, W2, W2V, W2H,   &
                                   W2broken, W2trace,      &
                                   W2Hbroken,              &
                                   W2Vtrace, W2Htrace,     &
                                   W3, Wtheta, Wchi
  use reference_element_mod, only: reference_element_type, &
                                   V,                      &
                                   W, S, E, N, B, T,       &
                                   SWB, SEB, NEB, NWB,     &
                                   SWT, SET, NET, NWT,     &
                                   WB, SB, EB, NB,         &
                                   SW, SE, NE, NW,         &
                                   WT, ST, ET, NT
  use log_mod,               only: log_event, LOG_LEVEL_ERROR
  implicit none

  private
  public :: ndof_setup, basis_setup, dofmap_setup, levels_setup, generate_fs_id

  ! Select entities in the function space
  type select_entity_type
    integer(i_def), allocatable :: faces(:)
    integer(i_def), allocatable :: edges(:)
    integer(i_def), allocatable :: verts(:)
  end type select_entity_type

contains

  ! Allocates and initialises the select entity lists.
  !
  ! mesh[in]          Mesh on which the entities reside.
  ! entity_all[out]   All the entities available on the mesh.
  ! entity_theta[out] Entities on a theta field.
  ! entity_w2h[out]   Entities on a w2h field.
  ! entity_w2v[out]   Entities on a w2v field.
  !
  subroutine setup_select_entities( mesh, entity_all, entity_theta, &
                                    entity_w2h, entity_w2v )

    implicit none

    type(mesh_type),          intent(in)  :: mesh
    type(select_entity_type), intent(out) :: entity_all
    type(select_entity_type), intent(out) :: entity_theta
    type(select_entity_type), intent(out) :: entity_w2v
    type(select_entity_type), intent(out) :: entity_w2h

    class(reference_element_type), pointer :: reference_element => null()

    integer(i_def) :: number_of_faces
    integer(i_def) :: number_of_edges
    integer(i_def) :: number_of_vertices

    reference_element => mesh%get_reference_element()
    number_of_faces    = reference_element%get_number_faces()
    number_of_edges    = reference_element%get_number_edges()
    number_of_vertices = reference_element%get_number_vertices()

    allocate( entity_all%faces(number_of_faces) )
    allocate( entity_all%edges(number_of_edges) )
    allocate( entity_all%verts(number_of_vertices) )

    entity_all%faces = (/ W, S, E, N, B, T /)
    entity_all%edges = (/ WB, SB, EB, NB, SW, SE, NE, NW, WT, ST, ET, NT /)
    entity_all%verts = (/ SWB, SEB, NEB, NWB, SWT, SET, NET, NWT /)

    allocate( entity_theta%faces(number_of_faces) )
    allocate( entity_theta%edges(number_of_edges) )
    allocate( entity_theta%verts(number_of_vertices) )

    entity_theta%faces = (/ IMDI, IMDI, IMDI, IMDI, B, T /)
    entity_theta%edges = IMDI
    entity_theta%verts = IMDI

    allocate( entity_w2v%faces(number_of_faces) )
    allocate( entity_w2v%edges(number_of_edges) )
    allocate( entity_w2v%verts(number_of_vertices) )

    entity_w2v%faces = (/ IMDI, IMDI, IMDI, IMDI, B, T /)
    entity_w2v%edges = IMDI
    entity_w2v%verts = IMDI

    allocate( entity_w2h%faces(number_of_faces) )
    allocate( entity_w2h%edges(number_of_edges) )
    allocate( entity_w2h%verts(number_of_vertices) )

    entity_w2h%faces = (/ W, S, E, N, IMDI, IMDI /)
    entity_w2h%edges = IMDI
    entity_w2h%verts = IMDI

    nullify(reference_element)

  end subroutine setup_select_entities

  !-----------------------------------------------------------------------------
  !> @brief Computes the number of dofs for various entities.
  !>
  !> @details Compute number of dofs for this function space. This subroutine
  !>          computes the number of dofs in this function space and the
  !>          number of dofs associated with various entities, primitive and
  !>          composite.
  !>
  !> @param[in] mesh              Mesh to define the function space on.
  !> @param[in] element_order_h   Polynomial order of the function space in the
  !>                              horizontal directions.
  !> @param[in] element_order_v   Polynomial order of the function space in the
  !>                              vertical direction.
  !> @param[in] gungho_fs         Enumeration of the function space.
  !> @param[out] ndof_vert        Number of dofs on each vertex.
  !> @param[out] ndof_edge_h      Number of dofs on each edge in the horizontal.
  !> @param[out] ndof_edge_v      Number of dofs on each edge in the vertical.
  !> @param[out] ndof_face_h      Number of dofs on each face in the horizontal.
  !> @param[out] ndof_face_v      Number of dofs on each face in the vertical.
  !> @param[out] ndof_vol         Number of dofs in each volume.
  !> @param[out] ndof_cell        Total number of dofs associated with a cell.
  !> @param[out] ndof_glob        Total number of dofs on a rank.
  !> @param[out] ndof_interior    Number of dofs with no vertical connectivity.
  !> @param[out] ndof_exterior    Number of dofs with vertical connectivity.
  !
  !
  !     .+---B--+      In the following an edge is called vertical if it is
  !   .' |    .'|      normal to the horizontal plane (such as edge A), and
  !  +---+--+'  A      horizontal if it is parallel to it (such as edge B).
  !  | P |  |   |
  !  |  ,+--+---+      A face will be called horizontal if it is normal to
  !  |.'  Q | .'       the horizontal plane (such as face P) and vertical if it
  !  +------+'         is parallel to it (such as face Q).
  !
  !                    These are chosen to agree with the naming of W2H and
  !                    W2V.

  subroutine ndof_setup( mesh, element_order_h, element_order_v, gungho_fs,    &
                         ndof_vert, ndof_edge_h, ndof_edge_v, ndof_face_h,     &
                         ndof_face_v, ndof_vol, ndof_cell, ndof_glob,          &
                         ndof_interior, ndof_exterior )

    ! NOTE: ndofs will be used as short hand for Number of Degrees Of Freedom
    implicit none

    ! Input
    type(mesh_type), intent(in), pointer :: mesh
    integer(i_def),  intent(in)          :: element_order_h, element_order_v
    integer(i_def),  intent(in)          :: gungho_fs

    ! Output
    integer(i_def), intent(out) :: ndof_vert     ! ndof per vertex entity
    integer(i_def), intent(out) :: ndof_edge_h   ! ndof per horizontal edge
                                                 ! entity
    integer(i_def), intent(out) :: ndof_edge_v   ! ndof per vertical edge entity
    integer(i_def), intent(out) :: ndof_face_h   ! ndof per horizontal face
                                                 ! entity
    integer(i_def), intent(out) :: ndof_face_v   ! ndof per vertical face entity
    integer(i_def), intent(out) :: ndof_vol      ! ndof per volume entity

    integer(i_def), intent(out) :: ndof_cell     ! ndof per 3D-cell entity
    integer(i_def), intent(out) :: ndof_interior ! ndof per interior entity
                                                 ! (in vertical)
    integer(i_def), intent(out) :: ndof_exterior ! ndof per exterior entity
                                                 ! (in vertical)
    integer(i_def), intent(out) :: ndof_glob     ! ndof per 3D-mesh (on a rank)


    ! Local variables

    class(reference_element_type), pointer :: reference_element => null()

    ! Variables for properties of the local 3D-Mesh
    integer(i_def) :: ncells           ! No. of 2D-cells in 3D-mesh partition
    integer(i_def) :: nlayers          ! No. of layers of 3D-cells
    integer(i_def) :: nedges_2d        ! No. of edges per level

    ! Variables for Exterior-Interior topology (vertical direction)
    integer(i_def) :: nverts_exterior  ! No. of vertices per exterior entity
    integer(i_def) :: nedges_exterior  ! No. of edges    per exterior entity
    integer(i_def) :: nfaces_exterior  ! No. of faces    per exterior entity
    integer(i_def) :: nedges_interior  ! No. of edges    per interior entity
    integer(i_def) :: nfaces_interior  ! No. of faces    per interior entity

    integer(i_def) :: nface_g_h        ! Global No. of horizontal faces
    integer(i_def) :: nface_g_v        ! Global No. of vertical faces
    integer(i_def) :: nedge_g_h        ! Global No. of horizontal edges
    integer(i_def) :: nedge_g_v        ! Global No. of vertical edges
    integer(i_def) :: nvert_g          ! Global No. of vertices

    integer(i_def) :: k_h, k_v

    ! Adding ndof for exterior and interior composite entities:
    !
    ! ndof_exterior = ndof_edge_h*nedges_exterior
    !               + ndof_face_v*nfaces_exterior
    !               + ndof_vert*nverts_exterior

    ! ndof_interior = ndof_edge_v*nedges_interior
    !               + ndof_face_h*nfaces_interior
    !               + ndof_vol
    !
    ! Elements on interior/exterior cell decomposition in vertical.
    ! The vertical faces, horizontal edges, and vertices (i.e. top OR bottom )
    ! are classed as exterior entities.
    ! The horizontal faces and vertical edges are classed as an interior
    ! entities.

    reference_element => mesh%get_reference_element()

    ! Local values:
    ! Values for cell entity calculations
    nverts_exterior = reference_element%get_number_2d_vertices()
    nedges_exterior = reference_element%get_number_2d_edges()
    nfaces_exterior = 1

    nedges_interior = reference_element%get_number_2d_vertices()
    nfaces_interior = reference_element%get_number_2d_edges()

    ! Values for global calculations
    nlayers   = mesh%get_nlayers()
    ncells    = mesh%get_ncells_2d_with_ghost()
    nedges_2d = mesh%get_nedges_2d()

    nface_g_v = ncells*(nlayers + 1)
    nface_g_h = nedges_2d*nlayers
    nedge_g_v = mesh%get_nverts_2d()*nlayers
    nedge_g_h = nedges_2d*(nlayers + 1)
    nvert_g   = mesh%get_nverts()

    ! dof values
    ndof_vert   = 0
    ndof_edge_h = 0
    ndof_edge_v = 0
    ndof_face_h = 0
    ndof_face_v = 0
    ndof_vol    = 0
    ndof_cell   = 0
    ndof_glob   = 0

    k_h = element_order_h
    k_v = element_order_v

    ! Possible modifications to number of dofs
    ! on edges depending on presets
    select case (gungho_fs)

    case (W0)
      ! H1 locates dofs on the element vertices for element order = 0,
      ! though the order for the H1 function space is k+1, i.e.
      ! linear across the element on each axis
      ndof_vert   = 1
      ndof_edge_h = k_h
      ndof_edge_v = k_v
      ndof_face_h = k_h*k_v
      ndof_face_v = k_h*k_h
      ndof_vol    = k_h*k_h*k_v
      ndof_cell   = (k_h + 2)*(k_h + 2)*(k_v + 2)

    case (W1)
      ! Dofs located on edges, as vectors
      ! in direction of edge.

      ! For order 0, the vector is constant along the
      ! edge, but can vary linearly normal to it.
      ndof_edge_h = (k_h + 1)
      ndof_edge_v = (k_v + 1)
      ndof_face_h = (k_h + 1)*k_v + k_h*(k_v + 1)
      ndof_face_v = 2*(k_h + 1)*k_h
      ndof_vol    = 2*k_h*(k_h + 1)*k_v                                        &
                  + k_h*k_h*(k_v + 1)
      ndof_cell   = 2*(k_h + 1)*(k_h + 2)*(k_v + 2)                            &
                  + (k_h + 2)*( k_h + 2)*(k_v + 1)

    case (W2)
      ! Dofs are located on faces for vector fields
      ! and direction is normal to the face.
      !
      ! For order 0 the value of the vector normal to the
      ! face is constant across the face(tangential) but can
      ! vary linearly passing through the face(normal) to
      ! the next cell.
      !
      ! NOTE: Not correct for simplices
      ndof_face_h = (k_h + 1)*(k_v + 1)
      ndof_face_v = (k_h + 1)*(k_h + 1)
      ndof_vol    = 2*k_h*(k_h + 1)*(k_v + 1)                                  &
                  + (k_h + 1)*(k_h + 1)*k_v
      ndof_cell   = 2*(k_h + 2)*(k_h + 1)*(k_v + 1)                            &
                  + (k_h + 1)*(k_h + 1)*(k_v + 2)

    case (W2H)
      ! Dofs are located at the horizontal components of W2, giving variables
      ! the values of the first term in the sums in the W2 case.
      nfaces_exterior = 0
      ndof_face_h = (k_h + 1)*(k_v + 1)
      ndof_vol    = 2*k_h*(k_h + 1)*(k_v + 1)
      ndof_cell   = 2*(k_h + 1)*(k_h + 2)*(k_v + 1)

    case (W2V)
      ! Dofs are located at the vertical components of W2, giving variables
      ! the values of the second term in the sums in the W2 case.
      nfaces_interior = 0
      ndof_face_v = (k_h + 1)*(k_h + 1)
      ndof_vol    = (k_h + 1)*(k_h + 1)*k_v
      ndof_cell   = (k_h + 1)*(k_h + 1)*(k_v + 2)

    case (W2broken)
      ! Dofs are geometrically located on faces for
      ! vector fields and direction is normal to the face.
      ! However, they are topologically associated with
      ! the cell volume. Hence, this function space is
      ! discontinuous between cells.
      !
      ! For order 0 the value of the vector normal to the
      ! face is constant across the face(tangential) but can
      ! vary linearly passing through the face(normal) to
      ! the next cell.
      !
      ! NOTE: Not correct for simplices
      ndof_vol  = 2*(k_h + 1)*(k_h + 2)*(k_v + 1)                              &
                + (k_h + 1)*(k_h + 1)*(k_v + 2)
      ndof_cell = ndof_vol

    case (W2Hbroken)
      ! Dofs are geometrically located on faces for
      ! vector fields and direction is normal to the face.
      ! However, they are topologically associated with
      ! the cell volume. Hence, this function space is
      ! discontinuous between cells.
      !
      ! For order 0 the value of the vector normal to the
      ! face is constant across the face(tangential) but can
      ! vary linearly passing through the face(normal) to
      ! the next cell.
      !
      ! NOTE: Not correct for simplices
      ndof_vol  = 2*(k_h + 1)*(k_h + 2)*(k_v + 1)
      ndof_cell = ndof_vol

    case (W2trace)
      ! This function space is the result of taking the trace
      ! of a W2 Hdiv space. The result is a scalar-valued space
      ! with functions defined only on cell faces.
      !
      ! This space is discontinuous across edges/vertices.
      !
      ! NOTE: Not correct for simplices
      ndof_face_h = (k_h + 1)*(k_v + 1)
      ndof_face_v = (k_h + 1)*(k_h + 1)
      ndof_cell   = 4*ndof_face_h + 2*ndof_face_v

    case (W2Vtrace)
      ! This function space is the result of taking the trace
      ! of a W2V Hdiv space (or equivalently taking only the
      ! vertical components of the trace of the W2 space).
      ! The result is a scalar-valued space
      ! with functions defined only on cell vertical faces.
      !
      ! This space is discontinuous across edges/vertices.
      !
      ! NOTE: Not correct for simplices
      nfaces_interior = 0
      ndof_face_v     = (k_h + 1)*(k_h + 1)
      ndof_cell       = 2*ndof_face_v

    case (W2Htrace)
      ! This function space is the result of taking the trace
      ! of a W2H Hdiv space (or equivalently taking only the
      ! horizontal components of the trace of the W2 space).
      ! The result is a scalar-valued space
      ! with functions defined only on cell horizontal faces.
      !
      ! This space is discontinuous across edges/vertices.
      !
      ! NOTE: Not correct for simplices
      nfaces_exterior = 0
      ndof_face_h     = (k_h + 1)*(k_v + 1)
      ndof_cell       = 4*ndof_face_h

    case (W3)
      ! Order of this function space is same as base order
      ! This function space is discontinuous so all dofs are
      ! located on the cell volume, not the edges or vertices

      ! Dofs located on cell volume entities/discontinuous
      ! between cells.

      ! Number of dofs on each dimension is lowest order + 1
      ndof_vol  = (k_h + 1)*(k_h + 1)*(k_v + 1)
      ndof_cell = ndof_vol

    case (WTHETA)
      nfaces_interior = 0
      ndof_face_v     = (k_h + 1)*(k_h + 1)
      ndof_vol        = (k_h + 1)*(k_h + 1)*k_v
      ndof_cell       = (k_h + 1)*(k_h + 1)*(k_v + 2)

    case (WCHI)
      ndof_vol  = (k_h + 1)*(k_h + 1)*(k_v + 1)
      ndof_cell = ndof_vol

    end select

    ndof_exterior = ndof_vert * nverts_exterior &
                  + ndof_edge_h * nedges_exterior &
                  + ndof_face_v * nfaces_exterior

    ndof_interior = ndof_edge_v * nedges_interior &
                  + ndof_face_h * nfaces_interior &
                  + ndof_vol

    ! Calculated the global number of dofs on the function space
    ndof_glob = ncells*nlayers*ndof_vol &
              + nface_g_h*ndof_face_h   &
              + nface_g_v*ndof_face_v   &
              + nedge_g_h*ndof_edge_h   &
              + nedge_g_v*ndof_edge_v   &
              + nvert_g*ndof_vert

    nullify(reference_element)

  end subroutine ndof_setup

  !---------------------------------------------------------------------------
  !> @brief Setups basis functions for the function space.
  !>
  !> Setup arrays for basis function generation. Computes the arrays required
  !> for "on-the-fly" basis function generation. This routine is only valid
  !> for cube elements. It is used by the function_space_type constructor and
  !> is unlikely to be useful elsewhere.
  !>
  !> @param[in] element_order_h    Polynomial order of the function space in
  !>                               horizontal direction.
  !> @param[in] element_order_v    Polynomial order of the function space in
  !>                               vertical direction.
  !> @param[in] gungho_fs          Enumeration of the function space.
  !> @param[in] ndof_vert          Number dofs on each vertex.
  !> @param[in] ndof_cell          Total number of dofs associated with a cell.
  !> @param[in] reference_element  Object describing the reference element of
  !>                               the mesh.
  !> @param[out] basis_index       Array containing index of polynomial
  !>                               function.
  !> @param[out] basis_order       Polynomial order of basis function.
  !> @param[out] basis_vector      Direction of basis for vector functions.
  !> @param[out] basis_x           Array of nodal points of the x and y basis
  !>                               functions.
  !> @param[out] basis_z           Array of nodal points of the basis z basis
  !>                               functions.
  !> @param[out] nodal_coords      3D coordinates of zeros of the basis
  !>                               functions.
  !> @param[out] dof_on_vert_boundary  Array indication if a dof is on the top
  !>                                   or bottom boundary of a cell.
  !> @param[out] entity_dofs       Array of labels which maps degree of freedom
  !>                               index to geometric entity the dof lies on.
  !>
  subroutine basis_setup( element_order_h, element_order_v, gungho_fs,         &
                          ndof_vert,  ndof_cell, reference_element,            &
                          basis_index, basis_order, basis_vector, basis_x,     &
                          basis_z, nodal_coords, dof_on_vert_boundary,         &
                          entity_dofs )

    implicit none

    ! Input
    integer(i_def), intent(in) :: element_order_h
    integer(i_def), intent(in) :: element_order_v
    integer(i_def), intent(in) :: gungho_fs

    integer(i_def), intent(in) :: ndof_vert ! ndofs (number of dofs) per vertex
    integer(i_def), intent(in) :: ndof_cell ! ndofs (number of dofs) per 3D-cell

    class(reference_element_type), intent(in), pointer :: reference_element

    ! Output
    integer(i_def), intent(out) :: basis_index(:,:)
    integer(i_def), intent(out) :: basis_order(:,:)
    real(r_def),    intent(out) :: basis_vector(:,:)
    real(r_def),    intent(out) :: basis_x(:,:,:)
    real(r_def),    intent(out) :: basis_z(:,:)
    real(r_def),    intent(out) :: nodal_coords(:,:)
    integer(i_def), intent(out) :: dof_on_vert_boundary(:,:)
    integer(i_def), intent(out) :: entity_dofs(:)

    ! Local variables
    integer(i_def) :: k_h, k_v    ! Horizontal and vertical element orders
    integer(i_def) :: k_switch    ! Can be set to k_h or k_v

    integer(i_def) :: i           ! General loop variable
    integer(i_def) :: jx, jy, jz  ! x, y, z loop variables
    integer(i_def) :: idx         ! Index of dof
    integer(i_def) :: j1, j2      ! Face/edge loop variables
    integer(i_def) :: j(3)        ! Tuple containing face or edge indices such
                                  ! as j1, j2, face_idx and edge_idx

    integer(i_def) :: j2l_edge(12, 3), j2l_face(6, 3) ! Indexes conversion from
                                                      ! j to lx, ly and lz

    integer(i_def) :: face_idx(6), edge_idx(12, 2)    ! Indices of nodal points
                                                      ! on faces and edges

    integer(i_def), allocatable :: lx(:), ly(:), lz(:) ! 3d indices of dofs

    real(r_def),    allocatable :: unit_vec(:,:) ! Unit tangent to an edge dof
                                                 ! or normal to a face dof

    real(r_def) :: x1h(element_order_h+2) ! Evenly spaces nodes of continuous
                                          ! 1D element (used in horizontal)
    real(r_def) :: x1v(element_order_v+2) ! Evenly spaces nodes of continuous
                                          ! 1D element (used in vertical)

    real(r_def) :: x2h(element_order_h+2) ! Evenly spaces nodes of discontinuous
                                          ! 1D element (used in horizontal).
                                          ! Note: one larger than required
    real(r_def) :: x2v(element_order_v+2) ! Evenly spaces nodes of discontinuous
                                          ! 1D element (used in vertical).
                                          ! Note: one larger than required

    real(r_def) :: coordinate(3) ! Coordinate of a vertex

    integer(i_def) :: edges_on_face(reference_element%get_number_edges())
    integer(i_def) :: number_faces, number_edges, number_vertices
    integer(i_def) :: number_2d_edges
    integer(i_def) :: number_faces_h

    number_faces    = reference_element%get_number_faces()
    number_edges    = reference_element%get_number_edges()
    number_vertices = reference_element%get_number_vertices()

    number_2d_edges = reference_element%get_number_2d_edges()
    number_faces_h  = reference_element%get_number_2d_edges()

    ! To uniquely specify a 3D tensor product basis function the following is
    ! needed:
    ! basis_order(3): The polynomial order in the x,y,z directions
    ! basis_x(element_order_h + 2, 2, ndof_cell):
    !                 The nodal points of the polynomials in each horizontal
    !                 direction at each dof
    ! basis_z(element_order_v + 2, ndof_cell):
    !                 The nodal points of the polynomials in each vertical
    !                 direction at each dof
    ! basis_index(3): The index of the nodal points array at which the basis
    !                 function is unity
    ! basis_vector(3): Additionally if the function space is a vector then a
    !                  unit vector is needed.

    ! Although not strictly needed the nodal coordinates at which each basis
    ! function equals 1 is stored as nodal_coords.
    ! A flag is also set to 0 if a basis function is associated with an entity
    ! on the top or bottom of the cell, i.e has nodal_coord(3) = 0 or 1

    k_h = element_order_h
    k_v = element_order_v

    allocate( lx(ndof_cell) )
    allocate( ly(ndof_cell) )
    allocate( lz(ndof_cell) )

    lx(:) = 0
    ly(:) = 0
    lz(:) = 0

    ! Positional arrays - need two, i.e quadratic and linear at lowest order.
    ! These arrays will be used in the calculation of the nodal points and
    ! corresponding basis functions

    do i = 1, k_h + 2
      ! Evenly space the points edge to edge
      x1h(i) = real(i - 1, r_def) / real(k_h + 1, r_def)
    end do

    if (k_h == 0) then
      x2h(1) = 0.5_r_def
    else
      if (gungho_fs == W3 .or. gungho_fs == Wtheta) then
        ! Evenly space the points away from the element edges for high order
        ! spaces - this helps with visualising the output
        do i = 1, k_h + 1
          x2h(i) = real(i, r_def) / real(k_h + 2, r_def)
        end do
      else
        do i = 1, k_h + 1
          x2h(i) = real(i - 1, r_def) / real(k_h, r_def)
        end do
      end if
    end if

    ! The same for vertical positional arrays
    do i = 1, k_v + 2
      x1v(i) = real(i - 1, r_def) / real(k_v + 1, r_def)
    end do

    if (k_v == 0) then
      x2v(1) = 0.5_r_def
    else
      if (gungho_fs == W3 .or. gungho_fs == Wtheta) then
        ! Evenly space the points away from the element edges for high order
        ! spaces - this helps with visualising the output
        do i = 1, k_v + 1
          x2v(i) = real(i, r_def) / real(k_v + 2, r_def)
        end do
      else
        do i = 1, k_v + 1
          x2v(i) = real(i - 1, r_def) / real(k_v, r_def)
        end do
      end if
    end if

    ! This value isn't needed and is always multipled by 0
    x2h(k_h + 2) = 0.0_r_def
    x2v(k_v + 2) = 0.0_r_def

    ! Some look arrays based upon reference cube topology:

    ! Index of nodal points for dofs located on faces.
    ! Faces are defined as having one coordinate fixed,
    ! i.e. for face 1 x = 0 for all points on the face
    ! and for face 4 y = 1 for all points on the face.
    ! This array give the index of x1h/x1v that returns the fixed coordinate
    ! of each face.
    ! If a face has fixed coordinate = 0 the index is 1
    ! If a face has fixed coordinate = 1 the index is k+2
    face_idx = (/ 1, 1, k_h + 2, k_h + 2, 1, k_v + 2 /)

    ! Index of nodal points for dofs located on edges.
    ! Edges are defined as having two coordinates fixed,
    ! i.e. for edge 1 x = 0 & z = 0 for all points on the edge,
    ! and for edge 6 x = 1 y = 0 for all points on the edge.
    ! These arrays give the index for the two fixed coordinates for each edge.
    ! If an edge has fixed coordinate = 0 the index is 1
    ! If an edge has fixed coordinate = 1 the index is k+2
    ! The fixed coordinates are stored in order x, y, z so if the fixed
    ! coordinates are x and z then edge_idx(:, 1) stores x and edge_idx(:, 1)
    ! stores z, and for other combinations they remain in this order.
    edge_idx(:, 1) =                                                           &
    (/ 1, 1, k_h + 2, k_h + 2, 1, k_h + 2, k_h + 2, 1, 1, 1, k_h + 2, k_h + 2 /)
    edge_idx(:, 2) =                                                           &
    (/ 1, 1, 1, 1, 1, 1, k_h + 2, k_h + 2, k_v + 2, k_v + 2, k_v + 2, k_v + 2 /)

    ! Each dof living on a face or edge will have its index defined by three
    ! integers (j1, j2, j3) where:
    !  -for faces, one j will be the face index and the other two can vary.
    !  -for edges, two j's will be the edge indices and the final one can vary.
    ! These j's need to be converted to the indices lx ,ly, lz.
    ! For faces the first value of j2l is the l that corresponds to the
    ! constant coordinate, so for face 1: lx = j3, ly = j2 and lz = j1; for
    ! edge 1: lx = j2, ly = j1, and lz = j3.
    j2l_face(1,:) = (/ 3, 2, 1 /)
    j2l_face(2,:) = (/ 2, 3, 1 /)
    j2l_face(3,:) = (/ 3, 2, 1 /)
    j2l_face(4,:) = (/ 2, 3, 1 /)
    j2l_face(5,:) = (/ 1, 2, 3 /)
    j2l_face(6,:) = (/ 1, 2, 3 /)

    j2l_edge(1,:) = (/ 2, 1, 3 /)
    j2l_edge(2,:) = (/ 1, 2, 3 /)
    j2l_edge(3,:) = (/ 2, 1, 3 /)
    j2l_edge(4,:) = (/ 1, 2, 3 /)
    j2l_edge(5,:) = (/ 2, 3, 1 /)
    j2l_edge(6,:) = (/ 2, 3, 1 /)
    j2l_edge(7,:) = (/ 2, 3, 1 /)
    j2l_edge(8,:) = (/ 2, 3, 1 /)
    j2l_edge(9,:) = (/ 2, 1, 3 /)
    j2l_edge(10,:) = (/ 1, 2, 3 /)
    j2l_edge(11,:) = (/ 2, 1, 3 /)
    j2l_edge(12,:) = (/ 1, 2, 3 /)

    ! Array to flag vertices on the top or bottom boundaries
    ! If dof j is on the bottom boundary then  dof_on_vert_boundary(j,1) = 0
    ! If dof j is on the top boundary then  dof_on_vert_boundary(j,2) = 0
    dof_on_vert_boundary(:,:) = 1

    ! Allocate arrays to allow on the fly evaluation of basis functions
    select case (gungho_fs)
    case (W1, W2, W2H, W2V, W2broken, W2Hbroken, W2trace, W2Vtrace, W2Htrace)
      allocate( unit_vec(3, ndof_cell) )
    end select

    select case (gungho_fs)

    case (W0)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of CG spaces
      !-------------------------------------------------------------------------

      ! Compute indices of functions
      idx = 1

      ! ===============================
      ! dofs in volume
      ! ===============================
      do jz = 2, k_v + 1
        do jy = 2, k_h + 1
          do jx = 2, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      ! ===============================
      ! dofs on faces
      ! ===============================
      do i = 1, number_faces
        ! For horizontal face loop over vertical then horizontal orders
        if (i <= number_faces_h) then
          k_switch = k_v
        ! For vertical face loop over horizontal order twice
        else
          k_switch = k_h
        end if

        do j1 = 2, k_switch + 1
          do j2 = 2, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            ! Label face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      ! ===============================
      ! dofs on edges
      ! ===============================
      do i = 1, number_edges
        ! If edge is horizontal loop to k_h+1
        if ((i <= number_2d_edges) .OR.           &
            (i > number_edges - number_2d_edges)) then
          k_switch = k_h
        ! If edge vertical loop to k_v+1
        else
          k_switch = k_v
        end if

        do j1 = 2, k_switch + 1
          j(1) = j1
          j(2) = edge_idx(i, 1)
          j(3) = edge_idx(i, 2)
          lx(idx) = j(j2l_edge(i, 1))
          ly(idx) = j(j2l_edge(i, 2))
          lz(idx) = j(j2l_edge(i, 3))
          ! Label edge degrees of freedom
          entity_dofs(idx) = reference_element%get_edge_entity(i)
          idx = idx + 1
        end do
      end do

      ! ===============================
      ! dofs on vertices
      ! ===============================
      do i = 1, number_vertices
        do j1 = 1, ndof_vert
          coordinate = reference_element%get_vertex(i)
          lx(idx) = 1 + (k_h + 1) * int(coordinate(1))
          ly(idx) = 1 + (k_h + 1) * int(coordinate(2))
          lz(idx) = 1 + (k_v + 1) * int(coordinate(3))
          ! Label vertex degrees of freedom
          entity_dofs(idx) = reference_element%get_vertex_entity(i)
          idx = idx + 1
        end do
      end do

      do i = 1, ndof_cell
        ! Explicitly for quads, as ngp_h = ngp_v * ngp_v
        nodal_coords(1, i) = x1h(lx(i))
        nodal_coords(2, i) = x1h(ly(i))
        nodal_coords(3, i) = x1v(lz(i))

        basis_order(1, i) = k_h + 1
        basis_order(2, i) = k_h + 1
        basis_order(3, i) = k_v + 1
        basis_x(:, 1, i) = x1h(:)
        basis_x(:, 2, i) = x1h(:)
        basis_z(:, i)    = x1v(:)
      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)
      basis_vector(1,:) = 1.0_r_def

    case (W1)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of Hcurl spaces
      !-------------------------------------------------------------------------

      do idx = 1, ndof_cell
        do i = 1, 3
          unit_vec(i, idx) = 0.0_r_def
        end do
      end do

      ! Compute indices of functions
      idx = 1

      ! dofs in volume
      ! u components
      do jz = 2, k_v + 1
        do jy = 2, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_tangent_to_edge(S, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      ! v components
      do jz = 2, k_v + 1
        do jy = 1, k_h + 1
          do jx = 2, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_tangent_to_edge(W, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      ! w components
      do jz = 1, k_v + 1
        do jy = 2, k_h + 1
          do jx = 2, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_tangent_to_edge(B, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      ! dofs on faces
      do i = 1, number_faces
        ! For horizontal face loop over vertical then horizontal orders
        if (i <= number_faces_h) then
          k_switch = k_v
        ! For vertical face loop over horizontal order twice
        else
          k_switch = k_h
        end if

        ! Loop twice to account for two components per face (i.e. vertical
        ! faces contain x and y components)
        do j1 = 2, k_switch + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_edge_on_face(i, edges_on_face)
            call reference_element%get_tangent_to_edge(edges_on_face(1), &
            unit_vec(:, idx))
            if (i == number_faces - 1) dof_on_vert_boundary(idx, 1) = 0
            if (i == number_faces)     dof_on_vert_boundary(idx, 2) = 0
            ! Label face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
        do j1 = 1, k_switch + 1
          do j2 = 2, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_edge_on_face(i, edges_on_face)
            call reference_element%get_tangent_to_edge(edges_on_face(2), &
            unit_vec(:, idx))
            if (i == number_faces - 1) dof_on_vert_boundary(idx, 1) = 0
            if (i == number_faces)     dof_on_vert_boundary(idx, 2) = 0
            ! Label face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      ! dofs on edges
      do i = 1, number_edges
        ! If edge is horizontal loop to k_h+1
        if ((i <= number_2d_edges) .OR.           &
            (i > number_edges - number_2d_edges)) then
          k_switch = k_h
        ! If edge vertical loop to k_v+1
        else
          k_switch = k_v
        end if

        do j1 = 1, k_switch + 1
          j(1) = j1
          j(2) = edge_idx(i, 1)
          j(3) = edge_idx(i, 2)
          lx(idx) = j(j2l_edge(i, 1))
          ly(idx) = j(j2l_edge(i, 2))
          lz(idx) = j(j2l_edge(i, 3))
          call reference_element%get_tangent_to_edge(i, unit_vec(:, idx))
          if (i <= number_2d_edges) dof_on_vert_boundary(idx, 1) = 0
          if (i > number_edges - number_2d_edges) &
            dof_on_vert_boundary(idx, 2) = 0
          ! Label edge degrees of freedom
          entity_dofs(idx) = reference_element%get_edge_entity(i)
          idx = idx + 1
        end do
      end do

      do i = 1, ndof_cell

        nodal_coords(1, i) = abs(unit_vec(1, i)) * x2h(lx(i)) &
        + (1.0_r_def - abs(unit_vec(1, i))) * x1h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x2h(ly(i)) &
        + (1.0_r_def - abs(unit_vec(2, i))) * x1h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x2v(lz(i)) &
        + (1.0_r_def - abs(unit_vec(3, i))) * x1v(lz(i))

        basis_order(1, i) = (k_h + 1) - int(abs(unit_vec(1, i)))
        basis_order(2, i) = (k_h + 1) - int(abs(unit_vec(2, i)))
        basis_order(3, i) = (k_v + 1) - int(abs(unit_vec(3, i)))

        basis_x(:, 1, i) = abs(unit_vec(1, i)) * x2h(:) &
                         + (1.0_r_def - abs(unit_vec(1, i))) * x1h(:)

        basis_x(:, 2, i) = abs(unit_vec(2, i)) * x2h(:) &
                         + (1.0_r_def - abs(unit_vec(2, i))) * x1h(:)

        basis_z(:, i)    = abs(unit_vec(3, i)) * x2v(:) &
                         + (1.0_r_def - abs(unit_vec(3, i))) * x1v(:)

        do j1 = 1, size(basis_vector, 1)
          basis_vector(j1, i) = unit_vec(j1, i)
        end do

      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)

    case(W2, W2broken)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of Hdiv/discontinuous Hdiv spaces
      !-------------------------------------------------------------------------

      do idx = 1, ndof_cell
        do i = 1, 3
          unit_vec(i, idx) = 0.0_r_def
        end do
      end do

      idx = 1
      ! dofs in volume
      ! u components
      do jz = 1, k_v + 1
        do jy = 1, k_h + 1
          do jx = 2, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_normal_to_face(W, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do
      ! v components
      do jz = 1, k_v + 1
        do jy = 2, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_normal_to_face(S, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do
      ! w components
      do jz = 2, k_v + 1
        do jy = 1, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_normal_to_face(B, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      ! dofs on faces
      do i = 1, number_faces
        ! For horizontal face loop over vertical then horizontal orders
        if (i <= number_faces_h) then
          k_switch = k_v
        ! For vertical face loop over horizontal order twice
        else
          k_switch = k_h
        end if

        do j1 = 1, k_switch + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_normal_to_face(i, unit_vec(:, idx))
            if (i == number_faces - 1) dof_on_vert_boundary(idx, 1) = 0
            if (i == number_faces)    dof_on_vert_boundary(idx, 2) = 0
            ! Label face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell

        nodal_coords(1, i) = abs(unit_vec(1, i)) * x1h(lx(i)) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x1h(ly(i)) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x1v(lz(i)) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(lz(i))

        basis_order(1, i) = (k_h + 1) &
                          - int(1.0_r_def - abs(unit_vec(1, i)), i_def)

        basis_order(2, i) = (k_h + 1) &
                          - int(1.0_r_def - abs(unit_vec(2, i)), i_def)

        basis_order(3, i) = (k_v + 1) &
                          - int(1.0_r_def - abs(unit_vec(3, i)), i_def)

        basis_x(:, 1, i) = abs(unit_vec(1, i)) * x1h(:) &
                         + (1.0_r_def - abs(unit_vec(1, i))) * x2h(:)

        basis_x(:, 2, i) = abs(unit_vec(2, i)) * x1h(:) &
                         + (1.0_r_def - abs(unit_vec(2, i))) * x2h(:)

        basis_z(:, i)    = abs(unit_vec(3, i)) * x1v(:) &
                         + (1.0_r_def - abs(unit_vec(3, i))) * x2v(:)

        do j1 = 1, size(basis_vector, 1)
          basis_vector(j1, i) = unit_vec(j1, i)
        end do

      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)

    case(W2trace)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of Hdiv trace spaces
      !-------------------------------------------------------------------------

      ! Compute indices of functions
      idx = 1

      ! ===============================
      ! dofs on faces
      ! ===============================
      do i = 1, number_faces
        ! For horizontal face loop over vertical then horizontal orders
        if (i <= number_faces_h) then
          k_switch = k_v
        ! For vertical face loop over horizontal order twice
        else
          k_switch = k_h
        end if

        do j1 = 1, k_switch + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))

            ! Gather normals corresponding to each face
            call reference_element%get_outward_normal_to_face(i, &
                                                              unit_vec(:, idx))
            ! Label face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell
        nodal_coords(1, i) = abs(unit_vec(1, i)) * x1h(lx(i)) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x1h(ly(i)) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x1v(lz(i)) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(lz(i))

        basis_order(1, i) = k_h * int(1.0_r_def          &
                          - abs(unit_vec(1, i)), i_def)  &
                          + int(abs(unit_vec(1, i)), i_def)

        basis_order(2, i) = k_h * int(1.0_r_def          &
                          - abs(unit_vec(2, i)), i_def)  &
                          + int(abs(unit_vec(2, i)), i_def)

        basis_order(3, i) = k_v * int(1.0_r_def          &
                          - abs(unit_vec(3, i)), i_def)  &
                          + int(abs(unit_vec(3, i)), i_def)

        basis_x(:, 1, i)  = abs(unit_vec(1, i)) * x1h(:) &
                          + (1.0_r_def - abs(unit_vec(1, i))) * x2h(:)

        basis_x(:, 2, i)  = abs(unit_vec(2, i)) * x1h(:) &
                          + (1.0_r_def - abs(unit_vec(2, i))) * x2h(:)

        basis_z(:, i)     = abs(unit_vec(3, i)) * x1v(:) &
                          + (1.0_r_def - abs(unit_vec(3, i))) * x2v(:)
      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)
      basis_vector(:,:) = 1.0_r_def

    case(W3)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of DG spaces
      !-------------------------------------------------------------------------

      ! compute indices of functions
      idx = 1

      ! dofs in volume
      do jz = 1, k_v + 1
        do jy = 1, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell
        nodal_coords(1, i) = x2h(lx(i))
        nodal_coords(2, i) = x2h(ly(i))
        nodal_coords(3, i) = x2v(lz(i))
        basis_x(:, 1, i) = x2h(:)
        basis_x(:, 2, i) = x2h(:)
        basis_z(:, i)    = x2v(:)
      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)
      basis_vector(1,:) = 1.0_r_def
      basis_order(1,:) = k_h
      basis_order(2,:) = k_h
      basis_order(3,:) = k_v

    case (WTHETA)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of theta spaces
      !-------------------------------------------------------------------------

      idx = 1
      ! dofs in volume - (w only)
      ! w components
      do jz = 2, k_v + 1
        do jy = 1, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do


      ! dofs on faces
      do i = number_faces - 1, number_faces
        ! Loop on faces dependent on k_h only
        do j1 = 1, k_h + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            if (i == number_faces - 1) dof_on_vert_boundary(idx, 1) = 0
            if (i == number_faces)     dof_on_vert_boundary(idx, 2) = 0
            ! Label top and bottom face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell
        nodal_coords(1, i) = x2h(lx(i))
        nodal_coords(2, i) = x2h(ly(i))
        nodal_coords(3, i) = x1v(lz(i))

        basis_order(1, i) = k_h
        basis_order(2, i) = k_h
        basis_order(3, i) = k_v + 1

        basis_x(:, 1, i) = x2h(:)
        basis_x(:, 2, i) = x2h(:)
        basis_z(:, i)    = x1v(:)
      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)
      basis_vector(:,:) = 1.0_r_def

    case (W2V)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of W2V space
      !-------------------------------------------------------------------------

      do idx = 1, ndof_cell
        do i = 1, 3
          unit_vec(i, idx) = 0.0_r_def
        end do
      end do

      idx = 1
      ! dofs in volume - (w only)
      ! w components
      do jz = 2, k_v + 1
        do jy = 1, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_normal_to_face(B, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do


      ! dofs on faces
      do i = number_faces - 1, number_faces
        ! Loop on faces dependent on k_h only
        do j1 = 1, k_h + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_normal_to_face(i, unit_vec(:, idx))
            if (i == number_faces - 1) dof_on_vert_boundary(idx, 1) = 0
            if (i == number_faces)     dof_on_vert_boundary(idx, 2) = 0
            ! Label top and bottom face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell

        nodal_coords(1, i) = abs(unit_vec(1, i)) * x1h(lx(i)) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x1h(ly(i)) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x1v(lz(i)) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(lz(i))

        basis_order(1, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(1, i)), i_def)

        basis_order(2, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(2, i)), i_def)

        basis_order(3, i)  = (k_v + 1) - int(1.0_r_def &
                           - abs(unit_vec(3, i)), i_def)

        basis_x(:, 1, i)   = abs(unit_vec(1, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(:)

        basis_x(:, 2, i)   = abs(unit_vec(2, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(:)

        basis_z(:, i)      = abs(unit_vec(3, i)) * x1v(:) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(:)

        do j1 = 1, size(basis_vector, 1)
          basis_vector(j1, i) = unit_vec(j1, i)
        end do

      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)

    case (W2Vtrace)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of W2Vtrace space
      !-------------------------------------------------------------------------

      do idx = 1, ndof_cell
        do i = 1, 3
          unit_vec(i, idx) = 0.0_r_def
        end do
      end do

      idx = 1
      ! dofs on faces
      do i = number_faces - 1, number_faces
        ! Loop on faces dependent on k_h only
        do j1 = 1, k_h + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_normal_to_face(i, unit_vec(:, idx))
            if (i == number_faces - 1) dof_on_vert_boundary(idx, 1) = 0
            if (i == number_faces)     dof_on_vert_boundary(idx, 2) = 0
            ! Label top and bottom face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell

        nodal_coords(1, i) = abs(unit_vec(1, i)) * x1h(lx(i)) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x1h(ly(i)) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x1v(lz(i)) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(lz(i))

        basis_order(1, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(1, i)), i_def)

        basis_order(2, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(2, i)), i_def)

        basis_order(3, i)  = (k_v + 1) - int(1.0_r_def &
                           - abs(unit_vec(3, i)), i_def)

        basis_x(:, 1, i)   = abs(unit_vec(1, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(:)

        basis_x(:, 2, i)   = abs(unit_vec(2, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(:)

        basis_z(:, i)      = abs(unit_vec(3, i)) * x1v(:) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(:)

        do j1 = 1, size(basis_vector, 1)
          basis_vector(j1, i) = unit_vec(j1, i)
        end do

      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)

    case (W2H, W2Hbroken)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of W2H space
      !-------------------------------------------------------------------------

      do idx = 1, ndof_cell
        do i = 1, 3
          unit_vec(i, idx) = 0.0_r_def
        end do
      end do

      idx = 1

      !============================================
      ! dofs in volume - (u and v only)
      !============================================
      ! u components
      do jz = 1, k_v + 1
        do jy = 1, k_h + 1
          do jx = 2, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_normal_to_face(W, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do
      ! v components
      do jz = 1, k_v + 1
        do jy = 2, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            call reference_element%get_normal_to_face(S, unit_vec(:, idx))
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      !============================================
      ! dofs on faces
      !============================================
      do i = 1, number_faces - 2
        ! No vertical faces considered so one horizontal and one vertical loop
        do j1 = 1, k_v + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_normal_to_face(i, unit_vec(:, idx))
            ! Label horizontal face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell
        nodal_coords(1, i) = abs(unit_vec(1, i)) * x1h(lx(i)) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x1h(ly(i)) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x1v(lz(i)) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(lz(i))

        basis_order(1, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(1, i)), i_def)

        basis_order(2, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(2, i)), i_def)

        basis_order(3, i)  = (k_v + 1) - int(1.0_r_def &
                           - abs(unit_vec(3, i)), i_def)

        basis_x(:, 1, i)   = abs(unit_vec(1, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(:)

        basis_x(:, 2, i)   = abs(unit_vec(2, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(:)

        basis_z(:, i)      = abs(unit_vec(3, i)) * x1v(:) &
                           + (1.0_r_def - abs(unit_vec(  3, i))) * x2v(:  )

        do j1 = 1, size(basis_vector, 1)
          basis_vector(j1, i) = unit_vec(j1, i)
        end do

      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)

    case (W2Htrace)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of W2Htrace space
      !-------------------------------------------------------------------------

      do idx = 1, ndof_cell
        do i = 1, 3
          unit_vec(i, idx) = 0.0_r_def
        end do
      end do

      idx = 1
      !============================================
      ! dofs on faces
      !============================================
      do i = 1, number_faces - 2
        ! No vertical faces considered so one horizontal and one vertical loop
        do j1 = 1, k_v + 1
          do j2 = 1, k_h + 1
            j(1) = j1
            j(2) = j2
            j(3) = face_idx(i)
            lx(idx) = j(j2l_face(i, 1))
            ly(idx) = j(j2l_face(i, 2))
            lz(idx) = j(j2l_face(i, 3))
            call reference_element%get_normal_to_face(i, unit_vec(:, idx))
            ! Label horizontal face degrees of freedom
            entity_dofs(idx) = reference_element%get_face_entity(i)
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell
        nodal_coords(1, i) = abs(unit_vec(1, i)) * x1h(lx(i)) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(lx(i))

        nodal_coords(2, i) = abs(unit_vec(2, i)) * x1h(ly(i)) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(ly(i))

        nodal_coords(3, i) = abs(unit_vec(3, i)) * x1v(lz(i)) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(lz(i))

        basis_order(1, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(1, i)), i_def)

        basis_order(2, i)  = (k_h + 1) - int(1.0_r_def &
                           - abs(unit_vec(2, i)), i_def)

        basis_order(3, i)  = (k_v + 1) - int(1.0_r_def &
                           - abs(unit_vec(3, i)), i_def)

        basis_x(:, 1, i)   = abs(unit_vec(1, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(1, i))) * x2h(:)

        basis_x(:, 2, i)   = abs(unit_vec(2, i)) * x1h(:) &
                           + (1.0_r_def - abs(unit_vec(2, i))) * x2h(:)

        basis_z(:, i)      = abs(unit_vec(3, i)) * x1v(:) &
                           + (1.0_r_def - abs(unit_vec(3, i))) * x2v(:)

        do j1 = 1, size(basis_vector, 1)
          basis_vector(j1, i) = unit_vec(j1, i)
        end do

      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)

    case(WCHI)
      !-------------------------------------------------------------------------
      ! Section for test/trial functions of DG spaces
      !-------------------------------------------------------------------------

      ! compute indices of functions
      idx = 1

      ! dofs in volume
      do jz = 1, k_v + 1
        do jy = 1, k_h + 1
          do jx = 1, k_h + 1
            lx(idx) = jx
            ly(idx) = jy
            lz(idx) = jz
            ! Label volume degrees of freedom
            entity_dofs(idx) = V
            idx = idx + 1
          end do
        end do
      end do

      do i = 1, ndof_cell
        nodal_coords(1, i) = x2h(lx(i))
        nodal_coords(2, i) = x2h(ly(i))
        nodal_coords(3, i) = x2v(lz(i))
        basis_x(:, 1, i) = x2h(:)
        basis_x(:, 2, i) = x2h(:)
        basis_z(:, i)    = x2v(:)
      end do

      basis_index(1,:) = lx(1:ndof_cell)
      basis_index(2,:) = ly(1:ndof_cell)
      basis_index(3,:) = lz(1:ndof_cell)
      basis_vector(1,:) = 1.0_r_def
      basis_order(1,:) = k_h
      basis_order(2,:) = k_h
      basis_order(3,:) = k_v

    end select

    deallocate(lx)
    deallocate(ly)
    deallocate(lz)


    ! Allocate arrays to allow on the fly evaluation of basis functions
    select case (gungho_fs)
    case (W1, W2, W2H, W2V, W2broken, W2Hbroken, W2trace, W2Vtrace, W2Htrace)
      deallocate(unit_vec)
    end select

  end subroutine basis_setup

  !-----------------------------------------------------------------------------
  !> @brief Sets up dofmaps for the function space.
  !>
  !> @details Creates dofmap data for function space generation. This
  !>          subroutine computes the dofmaps for the function space and
  !>          stores them in a master dofmap object. The master dofmap is the
  !>          same as a stencil dofmap of a single dof.
  !>
  !> @param[in] mesh                   Mesh to define the function space on.
  !> @param[in] gungho_fs              Enumeration of the function space.
  !> @param[in] element_order_h        Polynomial order of the function space in
  !>                                   horizontal direction.
  !> @param[in] element_order_v        Polynomial order of the function space in
  !>                                   vertical direction.
  !> @param[in] ndata                  The number of data values to be held
  !>                                   at each dof location
  !> @param[in] ndata_first            Flag for ndata or nlayer first data
  !>                                   layout
  !> @param[in] ncells_2d_with_ghost   Number of 2d cells with ghost cells.
  !> @param[in] ndof_vert              Number of dofs on vertices.
  !> @param[in] ndof_edge_h            Number of dofs on horizontal edges.
  !> @param[in] ndof_edge_v            Number of dofs on vertical edges.
  !> @param[in] ndof_face_h            Number of dofs on horizontal faces.
  !> @param[in] ndof_face_v            Number of dofs on vertical faces.
  !> @param[in] ndof_vol               Number of dofs in volumes.
  !> @param[in] ndof_cell              Number of dofs associated with a cell.
  !> @param[out] last_dof_owned        Index of last owned dof for the
  !>                                   partition.
  !> @param[out] last_dof_annexed      Index of last annexed dof for the
  !>                                   partition.
  !> @param[out] last_dof_halo         Index of last halo dof for the
  !>                                   partition.
  !> @param[out] dofmap                Array containing the dofmap indexed by
  !>                                   cells.
  !> @param[out] global_dof_id         Global id of dofs.
  !> @param[out] global_cell_dof_id_2d Global id of cell dofs on the 2D
  !>                                   horizontal domain
  !> @param[out] global_edge_dof_id_2d Global id of edge dofs on the 2D
  !>                                   horizontal domain
  !> @param[out] global_vert_dof_id_2d Global id of vertex dofs on the 2D
  !>                                   horizontal domain
  !
  !     .+---B--+      In the following an edge is called vertical if it is
  !   .' |    .'|      normal to the horizontal plane (such as edge A), and
  !  +---+--+'  A      horizontal if it is parallel to it (such as edge B).
  !  | P |  |   |
  !  |  ,+--+---+      A face will be called horizontal if it is normal to
  !  |.'  Q | .'       the horizontal plane (such as face P) and vertical if it
  !  +------+'         is parallel to it (such as face Q).
  !
  !                    These are chosen to agree with the naming of W2H and
  !                    W2V.

  subroutine dofmap_setup( mesh, gungho_fs, element_order_h, element_order_v,  &
                           ndata, ndata_first, ncells_2d_with_ghost, ndof_vert,&
                           ndof_edge_h, ndof_edge_v, ndof_face_h, ndof_face_v, &
                           ndof_vol, ndof_cell, last_dof_owned,                &
                           last_dof_annexed, last_dof_halo, dofmap,            &
                           global_dof_id, global_cell_dof_id_2d,               &
                           global_edge_dof_id_2d, global_vert_dof_id_2d )
    implicit none

    ! Input
    type(mesh_type), intent(in), pointer :: mesh
    integer(i_def),  intent(in) :: gungho_fs
    integer(i_def),  intent(in) :: element_order_h
    integer(i_def),  intent(in) :: element_order_v
    integer(i_def),  intent(in) :: ndata

    logical(l_def),  intent(in) :: ndata_first

    integer(i_def),  intent(in) :: ncells_2d_with_ghost
    integer(i_def),  intent(in) :: ndof_vert
    integer(i_def),  intent(in) :: ndof_edge_h
    integer(i_def),  intent(in) :: ndof_edge_v
    integer(i_def),  intent(in) :: ndof_face_h
    integer(i_def),  intent(in) :: ndof_face_v
    integer(i_def),  intent(in) :: ndof_vol
    integer(i_def),  intent(in) :: ndof_cell

    ! Output
    integer(i_def), intent(out) :: last_dof_owned
    integer(i_def), intent(out) :: last_dof_annexed
    integer(i_def), intent(out) :: last_dof_halo(0:)

    integer(i_def), intent(out) :: dofmap(ndof_cell, 0:ncells_2d_with_ghost)

    integer(i_halo_index), intent(out) :: global_dof_id(:)

    integer(i_def), intent(out) :: global_cell_dof_id_2d(:)
    integer(i_def), intent(out) :: global_edge_dof_id_2d(:)
    integer(i_def), intent(out) :: global_vert_dof_id_2d(:)

    ! Local variables

    class(reference_element_type), pointer :: reference_element => null()

    integer(i_def) :: number_faces    ! Number of faces per cell
    integer(i_def) :: number_edges    ! Number of edges per cell
    integer(i_def) :: number_vertices ! Number of vertices per cell

    integer(i_def) :: number_horizontal_faces ! Number of horizontal faces per
                                              ! cell
    integer(i_def) :: number_horizontal_edges ! Number of horizontal edges per
                                              ! cell
    integer(i_def) :: number_2d_vertices      ! Number of vertices of 2d cell
                                              ! entity


    integer(i_def) :: ncells ! Number of cells in the rank (including ghosts)

    ! Loop counters
    integer(i_def) :: icell, iface, iedge, ivert, idof, idepth, k, m

    ! Loop upper bound for ndof loops on vertical or horizontal edges
    integer(i_def) :: ndof_stop

    ! Number of layers
    integer(i_def) :: nlayers

    ! Indices into the dofmap
    integer(i_def) :: id_owned, id_halo, id0, dof_idx
    integer(i_def) :: face_id, edge_id, vert_id
    integer(i_def) :: bottom_edge_id, top_edge_id, side_edge_id
    integer(i_def) :: bottom_vert_id, top_vert_id

    ! Number of entities for a single layer
    integer(i_def) :: nvert_layer, nedge_layer, nface_layer

    ! Start and end points of the cell indices to loop over
    integer(i_def) :: start, finish

    ! Entity dofmaps
    integer(i_def), allocatable :: dofmap_d0(:,:), &
                                   dofmap_d1(:,:), &
                                   dofmap_d2(:,:), &
                                   dofmap_d3(:,:)

    ! dof column heights for entities
    integer(i_def), allocatable :: dof_column_height_d0(:,:), &
                                   dof_column_height_d1(:,:), &
                                   dof_column_height_d2(:,:), &
                                   dof_column_height_d3(:,:)

    ! Cell that owns the dofs on entities
    integer(i_def), allocatable :: dof_cell_owner_d0(:,:), &
                                   dof_cell_owner_d1(:,:), &
                                   dof_cell_owner_d2(:,:), &
                                   dof_cell_owner_d3(:,:)

    ! dof column heights for whole space
    integer(i_def), allocatable :: dof_column_height(:,:)

    ! Owning cell of each entry in the dofamp
    integer(i_def), allocatable :: dof_cell_owner(:,:)

    ! Cell id in global index space
    integer(i_def) :: global_cell_id

    integer(i_def) :: dofmap_size(0:3)

    ! Number of cells in all the inner halos added together
    integer(i_def) :: tot_num_inner

    type(local_mesh_type), pointer :: local_mesh => null()

    type(select_entity_type), target :: select_entity_all,   &
                                        select_entity_theta, &
                                        select_entity_w2h,   &
                                        select_entity_w2v
    type(select_entity_type), pointer :: select_entity => null()

    integer(i_halo_index) :: num_layers, num_dofs, num_ndata

    integer(i_def) :: ndata_offset

    !===========================================================================

    local_mesh => mesh%get_local_mesh()
    reference_element => mesh%get_reference_element()

    number_faces    = reference_element%get_number_faces()
    number_edges    = reference_element%get_number_edges()
    number_vertices = reference_element%get_number_vertices()

    number_horizontal_faces = reference_element%get_number_horizontal_faces()
    number_horizontal_edges = reference_element%get_number_2d_edges()
    number_2d_vertices      = reference_element%get_number_2d_vertices()

    ncells = ncells_2d_with_ghost

    ! Offset for multidata fields with continuous vertical components
    ! If ndata_first we need to add ndata to the dof value
    !    => dof on top entity (face, edge, vert) = dof on bottom entity + ndata
    ! If nlayer_first we need to add 1 to the dof value
    !    => dof on top entity (face, edge, vert) = dof on bottom entity + 1
    if ( ndata_first ) then
      ndata_offset = ndata
    else
      ndata_offset = 1
    end if

    ! dofmaps for a 3D horizontal layer
    nlayers = mesh%get_nlayers()
    nvert_layer = 2 * mesh%get_nverts_2d()
    nedge_layer = 2 * mesh%get_nedges_2d() &
                +     mesh%get_nverts_2d()
    nface_layer =     mesh%get_nedges_2d() &
                + 2 * ncells

    dofmap_size(:) = 1
    dofmap_size(0) = max(dofmap_size(0), ndof_vert)
    dofmap_size(1) = max(dofmap_size(1), ndof_edge_h, ndof_edge_v)
    dofmap_size(2) = max(dofmap_size(2), ndof_face_h, ndof_face_v)
    dofmap_size(3) = max(dofmap_size(3), ndof_vol)

    allocate( dof_column_height (ndof_cell, 0:ncells))
    allocate( dof_cell_owner    (ndof_cell, 0:ncells))

    allocate( dofmap_d0             (dofmap_size(0), nvert_layer) )
    allocate( dof_column_height_d0  (dofmap_size(0), nvert_layer) )
    allocate( dof_cell_owner_d0     (dofmap_size(0), nvert_layer) )

    allocate( dofmap_d1             (dofmap_size(1), nedge_layer) )
    allocate( dof_column_height_d1  (dofmap_size(1), nedge_layer) )
    allocate( dof_cell_owner_d1     (dofmap_size(1), nedge_layer) )

    allocate( dofmap_d2             (dofmap_size(2), nface_layer) )
    allocate( dof_column_height_d2  (dofmap_size(2), nface_layer) )
    allocate( dof_cell_owner_d2     (dofmap_size(2), nface_layer) )

    allocate( dofmap_d3             (dofmap_size(3), ncells) )
    allocate( dof_column_height_d3  (dofmap_size(3), ncells) )
    allocate( dof_cell_owner_d3     (dofmap_size(3), ncells) )

    call setup_select_entities( mesh,                &
                                select_entity_all,   &
                                select_entity_theta, &
                                select_entity_w2h,   &
                                select_entity_w2v )

    ! Initialise entity dofmaps
    dofmap_d0(:,:) = 0
    dofmap_d1(:,:) = 0
    dofmap_d2(:,:) = 0
    dofmap_d3(:,:) = 0

    dofmap_d0             (:,:) = 0
    dof_column_height_d0  (:,:) = 0
    dof_cell_owner_d0     (:,:) = 0

    dofmap_d1             (:,:) = 0
    dof_column_height_d1  (:,:) = 0
    dof_cell_owner_d1     (:,:) = 0

    dofmap_d2             (:,:) = 0
    dof_column_height_d2  (:,:) = 0
    dof_cell_owner_d2     (:,:) = 0

    dofmap_d3             (:,:) = 0
    dof_column_height_d3  (:,:) = 0
    dof_cell_owner_d3     (:,:) = 0

    ! Sum the number of cells in all the inner halos
    tot_num_inner = 0
    do idepth = 1, mesh%get_inner_depth()
      tot_num_inner = tot_num_inner + &
      mesh%get_num_cells_inner(idepth)
    end do

    ! Assume we have all possible global connectivity information
    ! in practice this requires connectivity
    ! (3,2) -> faces on cells
    ! (3,1) -> edges on cells
    ! (3,0) -> vertices on cells

    id_owned = 1
    id_halo = -1

    ! loop over 3 entities (cells) starting with core + inner halos + edge
    ! + first depth halo then proceding with further halo depths as required
    start = 1
    finish = tot_num_inner              &
           + mesh%get_num_cells_edge()  &
           + mesh%get_num_cells_halo(1)

    select case (gungho_fs)
    case(W0, W1, W2, W2broken, W2trace, W3, WCHI)
      select_entity => select_entity_all
    case(WTHETA)
      select_entity => select_entity_theta
    case(W2H, W2Htrace, W2Hbroken)
      select_entity => select_entity_w2h
    case(W2V, W2Vtrace)
      select_entity => select_entity_w2v
    end select

    halo_loop : do idepth = 1, mesh%get_halo_depth() + 1
      cell_loop : do icell = start, finish

        ! Assign dofs for connectivity (3,3) (dofs in cell)
        !---------------------------------------------------------
        if (mesh%is_cell_owned(icell)) then
          do idof = 1, ndof_vol
            dofmap_d3            (idof, icell) = id_owned
            dof_column_height_d3 (idof, icell) = nlayers
            dof_cell_owner_d3    (idof, icell) = icell
            id_owned = id_owned + (ndata * nlayers)
          end do
        else
          do idof = 1, ndof_vol
            dofmap_d3             (idof, icell) = id_halo
            dof_column_height_d3  (idof, icell) = nlayers
            dof_cell_owner_d3     (idof, icell) = icell
            id_halo = id_halo - (ndata * nlayers)
          end do
        end if

        ! Assign dofs for connectivity (3,2) (dofs on faces)
        !---------------------------------------------------------

        ! Horizontal faces
        do iface = 1, number_horizontal_faces
          if (any(select_entity%faces == iface)) then
            face_id = mesh%get_face_on_cell(iface, icell)
            if (mesh%is_edge_owned(iface, icell)) then
              if (dofmap_d2(1, face_id) == 0) then
                do idof = 1, ndof_face_h
                  dofmap_d2(idof, face_id) = id_owned
                  dof_column_height_d2(idof, face_id) = nlayers
                  dof_cell_owner_d2(idof, face_id) = &
                  mesh%get_edge_cell_owner(iface, icell)

                  id_owned = id_owned + (ndata * nlayers)
                end do
              end if
            else
              if (dofmap_d2(1, face_id) == 0) then
                do idof = 1, ndof_face_h
                  dofmap_d2(idof, face_id) = id_halo
                  dof_column_height_d2(idof, face_id) = nlayers
                  dof_cell_owner_d2(idof, face_id) = &
                  mesh%get_edge_cell_owner(iface, icell)

                  id_halo = id_halo - (ndata * nlayers)
                end do
              end if
            end if
          end if ! select_entity
        end do
        ! Vertical faces
        if (mesh%is_cell_owned(icell)) then
          id0 = id_owned
          do iface = number_horizontal_faces + 1, number_faces
            if (any(select_entity%faces==iface)) then
              face_id = mesh%get_face_on_cell(iface, icell)
              if (dofmap_d2(1, face_id) == 0) then
                do idof = 1, ndof_face_v
                  dofmap_d2(idof, face_id) = id_owned
                  if (iface == number_horizontal_faces + 1) then
                    dof_column_height_d2(idof, face_id) = nlayers + 1
                  else
                    dof_column_height_d2(idof, face_id) = 0
                  end if
                  dof_cell_owner_d2(idof, face_id) = icell
                  id_owned = id_owned + (ndata * (nlayers + 1))
                end do
              end if

              if (iface == number_horizontal_faces + 1) then
                id_owned = id0 + ndata_offset
              else
                id_owned = id_owned - ndata_offset
              end if

            end if ! select_entity
          end do
        else
          id0 = id_halo
          do iface = number_horizontal_faces + 1, number_faces
            if (any(select_entity%faces == iface)) then
              face_id = mesh%get_face_on_cell(iface, icell)
              if (dofmap_d2(1, face_id) == 0) then
                do idof = 1, ndof_face_v
                  dofmap_d2(idof, face_id) = id_halo
                  if (iface == number_horizontal_faces + 1) then
                    dof_column_height_d2(idof, face_id) = nlayers + 1
                  else
                    dof_column_height_d2(idof, face_id) = 0
                  end if
                  dof_cell_owner_d2(idof, face_id) = icell
                  id_halo = id_halo - (ndata * (nlayers + 1))
                end do
              end if
              if (iface == number_horizontal_faces + 1) then
                id_halo = id0 - ndata_offset
              else
                id_halo = id_halo + ndata_offset
              end if
            end if ! select_entity
          end do
        end if ! is cell owned

        ! assign dofs for connectivity (3,1) (dofs on edges)

        ! Horizontal edges
        do iedge = 1, number_horizontal_edges
          bottom_edge_id = mesh%get_edge_on_cell(iedge, icell)
          top_edge_id    = mesh%get_edge_on_cell(iedge + number_edges       &
                                                 - number_horizontal_edges, &
                                                 icell)
          if (mesh%is_edge_owned(iedge, icell)) then
            if (dofmap_d1(1, bottom_edge_id) == 0) then
              do idof = 1, ndof_edge_h
                dofmap_d1(idof, bottom_edge_id) = id_owned
                dofmap_d1(idof, top_edge_id) = id_owned + ndata_offset
                dof_column_height_d1(idof, bottom_edge_id) = nlayers + 1
                dof_column_height_d1(idof, top_edge_id) = 0
                dof_cell_owner_d1(idof, bottom_edge_id) = &
                mesh%get_edge_cell_owner(iedge, icell)

                dof_cell_owner_d1(idof, top_edge_id) = &
                mesh%get_edge_cell_owner(iedge, icell)

                id_owned = id_owned + (ndata * (nlayers + 1))
              end do
            end if
          else
            if (dofmap_d1(1, bottom_edge_id) == 0) then
              do idof = 1, ndof_edge_h
                dofmap_d1(idof, bottom_edge_id) = id_halo
                dofmap_d1(idof, top_edge_id) = id_halo - ndata_offset
                dof_column_height_d1(idof, bottom_edge_id) = nlayers + 1
                dof_column_height_d1(idof, top_edge_id) = 0
                dof_cell_owner_d1(idof, bottom_edge_id) = &
                mesh%get_edge_cell_owner(iedge, icell)

                dof_cell_owner_d1(idof, top_edge_id) = &
                mesh%get_edge_cell_owner(iedge, icell)

                id_halo = id_halo - (ndata * (nlayers + 1))
              end do
            end if
          end if
        end do
        ! Vertical edges
        do iedge = number_horizontal_edges + 1, number_edges &
                                                - number_horizontal_edges
          side_edge_id = mesh%get_edge_on_cell(iedge, icell)
          if (mesh%is_vertex_owned(iedge - number_horizontal_edges, icell)) then
            if (dofmap_d1(1, side_edge_id) == 0) then
              do idof = 1, ndof_edge_v
                dofmap_d1(idof, side_edge_id) = id_owned
                dof_column_height_d1(idof, side_edge_id) = nlayers
                dof_cell_owner_d1(idof, side_edge_id) = &
                mesh%get_vertex_cell_owner(iedge - number_horizontal_edges, &
                                           icell)

                id_owned = id_owned + (nlayers * ndata)
              end do
            end if
          else
            if (dofmap_d1(1, side_edge_id) == 0) then
              do idof = 1, ndof_edge_v
                dofmap_d1(idof, side_edge_id) = id_halo
                dof_column_height_d1(idof, side_edge_id) = nlayers
                dof_cell_owner_d1(idof, side_edge_id) = &
                mesh%get_vertex_cell_owner(iedge - number_horizontal_edges, &
                                           icell)

                id_halo = id_halo - (nlayers * ndata)
              end do
            end if
          end if
        end do


        ! Assign dofs for connectivity (3,0) (dofs on verts)
        !---------------------------------------------------------
        do ivert = 1, number_2d_vertices
          bottom_vert_id = mesh%get_vert_on_cell(ivert, icell)
          top_vert_id = mesh%get_vert_on_cell(ivert + number_2d_vertices, icell)
          if (mesh%is_vertex_owned(ivert, icell)) then
            if (dofmap_d0(1, bottom_vert_id) == 0) then
              do idof = 1, ndof_vert
                dofmap_d0(idof, bottom_vert_id) = id_owned
                dofmap_d0(idof, top_vert_id) = id_owned + ndata_offset
                dof_column_height_d0(idof, bottom_vert_id) = nlayers + 1
                dof_column_height_d0(idof, top_vert_id) = 0
                dof_cell_owner_d0(idof, bottom_vert_id) = &
                mesh%get_vertex_cell_owner(ivert, icell)

                dof_cell_owner_d0(idof, top_vert_id) = &
                mesh%get_vertex_cell_owner(ivert, icell)

                id_owned = id_owned + (ndata * (nlayers + 1))
              end do
            end if
          else
            if (dofmap_d0(1, bottom_vert_id) == 0) then
              do idof = 1, ndof_vert
                dofmap_d0(idof, bottom_vert_id) = id_halo
                dofmap_d0(idof, top_vert_id) = id_halo - ndata_offset
                dof_column_height_d0(idof, bottom_vert_id) = nlayers + 1
                dof_column_height_d0(idof, top_vert_id) = 0
                dof_cell_owner_d0(idof, bottom_vert_id) = &
                mesh%get_vertex_cell_owner(ivert, icell)

                dof_cell_owner_d0(idof, top_vert_id) = &
                mesh%get_vertex_cell_owner(ivert, icell)

                id_halo = id_halo - (ndata * (nlayers + 1))
              end do
            end if
          end if
        end do

        if (icell == tot_num_inner + mesh%get_num_cells_edge()) then
          last_dof_owned = id_owned - 1
          last_dof_annexed = id_owned - id_halo - 2
        end if

      end do cell_loop

      if (idepth <= mesh%get_halo_depth()) then
        last_dof_halo(idepth) = id_owned - id_halo - 2
      end if

      start = finish + 1
      if (idepth < mesh%get_halo_depth()) then
        finish = start + mesh%get_num_cells_halo(idepth + 1) - 1
      else
        finish = start + mesh%get_num_cells_ghost() - 1
      end if

    end do halo_loop

    ! The zeroth depth halo contains no dofs, so set the last dof to be the
    ! same as the last dof before it in memory - i.e. the last annexed dof
    last_dof_halo(0) = last_dof_annexed

    ! Copy from the dofmap_dn arrays into one dofmap array
    dof_column_height(:,:) = -999
    dof_cell_owner(:,:) = -999
    dofmap(:,:) = -999

    do icell = 1, ncells

      dof_idx = 1

      ! dofs in volumes
      !----------------------------------------
      do idof = 1, ndof_vol
        if (dofmap_d3(idof, icell) /= 0) then

          if (dofmap_d3(idof, icell) > 0) then
            dofmap(dof_idx, icell) = dofmap_d3(idof, icell)
          else if (dofmap_d3(idof, icell) < 0) then
            dofmap(dof_idx, icell) = id_owned - (dofmap_d3(idof, icell) + 1)
          end if

          dof_column_height(dof_idx, icell) = dof_column_height_d3(idof, icell)
          dof_cell_owner(dof_idx, icell) = dof_cell_owner_d3(idof, icell)
          dof_idx = dof_idx + 1

        end if
      end do

      ! dofs on faces
      !----------------------------------------
      do iface = 1, number_faces
        face_id = mesh%get_face_on_cell(iface, icell)
        if (iface <= number_horizontal_faces) then
          ndof_stop = ndof_face_h ! Horizontal faces
        else
          ndof_stop = ndof_face_v ! Vertical faces
        end if

        do idof = 1, ndof_stop
          if (dofmap_d2(idof, face_id) /= 0) then
            if (dofmap_d2(idof, face_id) > 0) then
              dofmap(dof_idx, icell) = dofmap_d2(idof, face_id)
            else if (dofmap_d2(idof, face_id) < 0) then
              dofmap(dof_idx, icell) = id_owned - (dofmap_d2(idof, face_id) + 1)
            end if

            dof_column_height(dof_idx, icell) = dof_column_height_d2(idof, &
                                                                     face_id)
            dof_cell_owner(dof_idx, icell) = dof_cell_owner_d2(idof, face_id)
            dof_idx = dof_idx + 1

          end if
        end do
      end do

      ! dofs on edges
      !----------------------------------------
      do iedge = 1, number_edges
        edge_id = mesh%get_edge_on_cell(iedge, icell)
        if ((iedge <= number_horizontal_edges) .or. &
            (iedge > number_edges - number_horizontal_edges)) then
          ndof_stop = ndof_edge_h ! Horizontal edges
        else
          ndof_stop = ndof_edge_v ! Vertical edges
        end if

        do idof = 1, ndof_stop
          if (dofmap_d1(idof, edge_id) /= 0) then
            if (dofmap_d1(idof, edge_id) > 0) then
              dofmap(dof_idx, icell) = dofmap_d1(idof, edge_id)
            else if (dofmap_d1(idof, edge_id) < 0) then
              dofmap(dof_idx, icell) = id_owned - (dofmap_d1(idof, edge_id) + 1)
            end if

            dof_column_height(dof_idx, icell) = dof_column_height_d1(idof, &
                                                                     edge_id)
            dof_cell_owner(dof_idx, icell) = dof_cell_owner_d1(idof, edge_id)
            dof_idx = dof_idx + 1
          end if
        end do
      end do

      ! dofs on vertices
      !----------------------------------------
      do ivert = 1, number_vertices
        vert_id = mesh%get_vert_on_cell(ivert, icell)
        do idof = 1, ndof_vert
          if (dofmap_d0(idof, vert_id) /= 0) then
            if (dofmap_d0(idof, vert_id) > 0) then
              dofmap(dof_idx, icell) = dofmap_d0(idof, vert_id)
            else if (dofmap_d0(idof, vert_id) < 0) then
              dofmap(dof_idx, icell) = id_owned - (dofmap_d0(idof, vert_id) + 1)
            end if

            dof_column_height(dof_idx, icell) = dof_column_height_d0(idof, &
                                                                     vert_id)
            dof_cell_owner(dof_idx, icell) = dof_cell_owner_d0(idof, vert_id)
            dof_idx = dof_idx + 1
          end if
        end do
      end do

    end do

    dofmap(:, 0) = 0

    if (allocated(dofmap_d0)) deallocate(dofmap_d0)
    if (allocated(dofmap_d1)) deallocate(dofmap_d1)
    if (allocated(dofmap_d2)) deallocate(dofmap_d2)
    if (allocated(dofmap_d3)) deallocate(dofmap_d3)

    if (allocated(dof_column_height_d0)) deallocate(dof_column_height_d0)
    if (allocated(dof_column_height_d1)) deallocate(dof_column_height_d1)
    if (allocated(dof_column_height_d2)) deallocate(dof_column_height_d2)
    if (allocated(dof_column_height_d3)) deallocate(dof_column_height_d3)

    if (allocated(dof_cell_owner_d0)) deallocate(dof_cell_owner_d0)
    if (allocated(dof_cell_owner_d1)) deallocate(dof_cell_owner_d1)
    if (allocated(dof_cell_owner_d2)) deallocate(dof_cell_owner_d2)
    if (allocated(dof_cell_owner_d3)) deallocate(dof_cell_owner_d3)

    ! Special cases for lowest order w3 and wtheta. These allow global_dof_id
    ! to have an index space with no gaps in it for these specific funct spaces
    num_layers = int(nlayers, i_halo_index) + 1_i_halo_index
    num_dofs = int(ndof_cell, i_halo_index)
    num_ndata = int(ndata, i_halo_index)
    if( element_order_h == 0 .and. element_order_v == 0 ) then
      if (gungho_fs == W3) then
        num_layers = int(nlayers, i_halo_index)
      else if( gungho_fs == WTHETA ) then
        num_dofs = 1_i_halo_index
      end if
    end if

    ! Calculate a globally unique id for each dof, such that each partition
    ! that needs access to that dof will calculate the same id
    global_dof_id(:) = 0_i_halo_index
    do icell = 1, ncells
      global_cell_id = mesh%get_gid_from_lid(icell)
      do idof = 1, ndof_cell
        if (icell == dof_cell_owner(idof, icell)) then
          do k = 1, dof_column_height(idof, icell)
            do m = 1, ndata
              ! The following line is very confused by the casting that is
              ! required, but it is actually calculating the global id as being:
              !      (global_cell_id-1) * num_dofs * ndata * num_layers
              !      + (idof-1) * ndata * num_layers
              !      + (k - 1) * ndata
              !      + (m - 1)
              global_dof_id(dofmap(idof, icell) + (k - 1) * ndata + (m - 1)) = &
                  (int(global_cell_id, i_halo_index) - 1_i_halo_index)         &
                    * num_dofs * num_ndata * num_layers                        &
                  + (int(idof, i_halo_index) - 1_i_halo_index)                 &
                    * num_ndata * num_layers                                   &
                  + (int(k, i_halo_index) - 1_i_halo_index) * num_ndata        &
                  + (int(m, i_halo_index) - 1_i_halo_index)
            end do
          end do
        end if
      end do
    end do

    ! Calculate a globally unique id for the dofs in the volume of each cell
    ! in the 2D horizontal part of the local domain. This uses cell lookups, so
    ! will work for all function spaces - even if they don't have cell vol dofs

    ! loop over local cells
    do icell = 1, mesh%get_last_edge_cell()
      global_cell_id = mesh%get_gid_from_lid(icell)
      do m = 1, ndata
        ! The global ids must be 0 based
        global_cell_dof_id_2d((icell - 1) * ndata + m) = (global_cell_id - 1)  &
                                                         * ndata               &
                                                       + m - 1
      end do
    end do

    ! Calculate a globally unique id for the dofs on the edges of each cell
    ! in the 2D horizontal part of the local domain - only possible for
    ! function spaces that (appear to) have 2d edge dofs
    ! (for the moment, using W2H as an example of such a function space
    ! - the 2d layer at the half levels appears to have edge dofs).
    if (element_order_h == 0 .and.                                             &
        element_order_v == 0 .and.                                             &
        gungho_fs == W2H) then
      ! loop over local cells
      do icell = 1, mesh%get_last_edge_cell()
        ! loop over 2d edges within a cell
        do iedge = 1, mesh%get_nedges_per_cell_2d()
          if(mesh%is_edge_owned(iedge, icell))then
            do m = 1, ndata
              global_edge_dof_id_2d(                                           &
                    ((dofmap(iedge, icell) - 1) / (nlayers * ndata)) + 1 )     &
                = (local_mesh%get_edge_gid_on_cell(iedge, icell) - 1) * ndata + m - 1
            end do
          end if
        end do
      end do
    else
      global_edge_dof_id_2d(:) = -1
    end if

    ! Calculate a globally unique id for the dofs on the vertices of each cell
    ! in the 2D horizontal part of the local domain - only possible for
    ! function spaces that have vertex dofs.
    ! (for the moment, using W0 as an example of such a function space).
    if (element_order_h == 0 .and.                                             &
        element_order_v == 0 .and.                                             &
        gungho_fs == W0) then
      ! loop over local cells
      do icell = 1, mesh%get_last_edge_cell()
        ! loop over 2d vertices within a cell
        do ivert = 1, mesh%get_nverts_per_cell_2d()
          if(mesh%is_vertex_owned(ivert, icell))then
            do m = 1, ndata
              global_vert_dof_id_2d(                                             &
                    ((dofmap(ivert, icell) - 1) / ((nlayers + 1) * ndata)) + 1 ) &
                = (local_mesh%get_vert_gid_on_cell(ivert, icell) - 1) * ndata + m - 1
            end do
          end if
        end do
      end do
    else
      global_vert_dof_id_2d(:) = -1
    end if

    if (allocated(dof_column_height)) deallocate(dof_column_height)
    if (allocated(dof_cell_owner))    deallocate(dof_cell_owner)

    if (allocated(select_entity_all%faces)) deallocate(select_entity_all%faces)
    if (allocated(select_entity_all%edges)) deallocate(select_entity_all%edges)
    if (allocated(select_entity_all%verts)) deallocate(select_entity_all%verts)
    if (allocated(select_entity_theta%faces)) deallocate(select_entity_theta%faces)
    if (allocated(select_entity_theta%edges)) deallocate(select_entity_theta%edges)
    if (allocated(select_entity_theta%verts)) deallocate(select_entity_theta%verts)
    if (allocated(select_entity_w2v%faces)) deallocate(select_entity_w2v%faces)
    if (allocated(select_entity_w2v%edges)) deallocate(select_entity_w2v%edges)
    if (allocated(select_entity_w2v%verts)) deallocate(select_entity_w2v%verts)
    if (allocated(select_entity_w2h%faces)) deallocate(select_entity_w2h%faces)
    if (allocated(select_entity_w2h%edges)) deallocate(select_entity_w2h%edges)
    if (allocated(select_entity_w2h%verts)) deallocate(select_entity_w2h%verts)

  end subroutine dofmap_setup

  !-----------------------------------------------------------------------------
  !> @brief Creates an array of unique fractional levels.
  !>
  !> Creates an array of fractional (dof) levels for output.
  !>
  !> @param[in]  mesh         Mesh to operate on.
  !> @param[in]  nlayers      Number of layers.
  !> @param[in]  ndof_cell    Number of dofs per cell of the function space.
  !> @param[in]  nodal_coords Vector of nodal points of basis functions at each
  !>                          dof of a cell.
  !> @param[out] levels       Array of fractional levels.
  !>
  subroutine levels_setup(mesh, nlayers, ndof_cell, nodal_coords, levels)

    implicit none

    type(mesh_type), intent(in) :: mesh

    integer(i_def), intent(in) :: nlayers
    integer(i_def), intent(in) :: ndof_cell
    real(r_def),    intent(in) :: nodal_coords(3, ndof_cell)

    real(r_def), intent(out), allocatable :: levels(:)

    ! Variable to hold the number of levels we found
    integer(i_def) :: idx
    ! working array to hold fractional levels
    real(r_def), allocatable :: tmp_levs(:)

    call compute_levels( nlayers,      &
                         ndof_cell,    &
                         nodal_coords, &
                         tmp_levs,     &
                         idx )

    ! Allocate the out array to be the size of the number of levels we found
    ! and copy in the data from the temp array
    allocate(levels(size(tmp_levs(1:(idx - 1)))))
    levels = tmp_levs(1:(idx - 1))

  end subroutine levels_setup

  !---------------------------------------------------------------------------
  ! Compute the fractional levels.
  !
  ! A private routine to Compute fractional (dof) levels for output. Takes
  ! information from the data reference element combined with the number of
  ! layers to compute a unique, ordered list of fractional levels that fields
  ! on this function space are to be output on.
  !
  ! [in]  nlayers       Number of layers in the atmosphere.
  ! [in]  ndof_cell     Number of dofs per cell.
  ! [in]  coords_array  Coordinates of nodal points of basis functions at each
  !                     dof
  ! [out] tmp_levs      Allocated and filled with the fractional levels.
  ! [out] idx           Tally of items added to tmp_levs.
  !
  subroutine compute_levels( nlayers,      &
                             ndof_cell,    &
                             coords_array, &
                             tmp_levs,     &
                             idx )

    implicit none

    integer(i_def),  intent(in)               :: nlayers
    integer(i_def),  intent(in)               :: ndof_cell
    real(r_def),     intent(in)               :: coords_array(:,:)
    real(r_def),     intent(out), allocatable :: tmp_levs(:)
    integer(i_def),  intent(out)              :: idx

    ! Local variables for computation
    real(r_def) :: l
    integer(i_def) :: ilayer, idof

    ! Allocate temp levels array to be the maximum possible size
    allocate(tmp_levs(ndof_cell * nlayers))
    tmp_levs = 999.0_r_def
    idx = 1

    do ilayer = 0, (nlayers - 1)
      do idof = 1, ndof_cell
        l = real(ilayer, r_def) + coords_array(3, idof)
        if (.not.(any(tmp_levs == l))) then
          tmp_levs(idx) = l
          ! keep track of how many items we added
          idx = idx + 1
        end if
      end do
    end do

  end subroutine compute_levels

  !> @brief Generate a unique integer id for a function space
  !> @param[in] lfric_fs        Function space continuity flag
  !> @param[in] element_order_h Polynomial order of the space in the horizontal
  !> @param[in] element_order_v Polynomial order of the space in the vertical
  !> @param[in] mesh_id         Id of the mesh to build the function space on
  !> @param[in] ndata           Number of multidata points
  !> @param[in] ndata_first     ndata of layer first layout of multidata array
  !> @result    fs_id           Unique id for the function space
  function generate_fs_id(lfric_fs, element_order_h, element_order_v, mesh_id, &
                          ndata, ndata_first) result(fs_id)

    implicit none

    integer(i_def), intent(in) :: lfric_fs
    integer(i_def), intent(in) :: element_order_h
    integer(i_def), intent(in) :: element_order_v
    integer(i_def), intent(in) :: mesh_id
    integer(i_def), intent(in) :: ndata
    logical(l_def), intent(in) :: ndata_first

    integer(i_def) :: fs_id
    integer(i_def) :: ndata_first_int

    if ( ndata_first ) then
      ndata_first_int = 1
    else
      ndata_first_int = 2
    end if

    fs_id = ndata + 1000_i_def*element_order_h + 10000_i_def*element_order_v &
          + 100000_i_def*lfric_fs                                            &
          + 10000000_i_def*mesh_id + 1000000000_i_def*ndata_first_int

  end function generate_fs_id

end module function_space_constructor_helper_functions_mod
