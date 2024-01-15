

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Unit reference elements.
!>
!> @details Includes ordering of topological entities and lookups for dofs.
!>          Note that the naming of entities (e.g. edges, faces) as horizontal
!>          or vertical stems from how the entities are looped over.
!>          "Horizontal" entities are those visited when looping is done in
!>          \f[(x-y)\f] plane while "vertical" entities are those visited when
!>          looping is done in \f[(x-z)\f] and/or \f[(y-z)\f] plane.
!>
!> @todo Currently also contains geometry information which it probably
!>       shouldn't.
!>
module reference_element_mod

  use constants_mod, only : i_def, r_def

  implicit none

  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Describes a unit reference element.
  !>
  type, abstract, public :: reference_element_type
    private

    ! Geometric information about the reference element
    integer(i_def) :: number_vertices,         &
                      number_faces,            &
                      number_edges
    integer(i_def) :: number_horizontal_faces, &
                      number_horizontal_edges, &
                      number_vertical_faces,   &
                      number_vertical_edges
    integer(i_def) :: number_2d_vertices,      &
                      number_2d_edges

    ! The numbers of vertices per edge and faces per edge
    ! are invariant and will never change no matter what
    ! shape element is used
    integer(i_def) :: number_verts_per_edge = 2
    integer(i_def) :: number_faces_per_edge = 2

    ! Vertex coodinates
    real(r_def), allocatable :: vertex_coords(:,:)
    ! Coodinates of the centre of each edge.
    real(r_def), allocatable :: edge_coords(:,:)
    ! Coodinates of the centre of each face.
    real(r_def), allocatable :: face_coords(:,:)
    ! Coodinates of the centre of each volume.
    real(r_def), allocatable :: volume_coords(:,:)

    ! Incidence relationships
    integer(i_def), allocatable :: vert_on_face(:,:)
    integer(i_def), allocatable :: vert_on_edge(:,:)
    integer(i_def), allocatable :: edge_on_face(:,:)
    integer(i_def), allocatable :: edge_on_vert(:,:)
    integer(i_def), allocatable :: face_on_edge(:,:)

    ! Vector directions
    real(r_def), allocatable :: normals_to_faces(:,:)
    real(r_def), allocatable :: normals_to_horizontal_faces(:,:)
    real(r_def), allocatable :: normals_to_vertical_faces(:,:)
    real(r_def), allocatable :: tangent_to_edge(:,:)
    real(r_def), allocatable :: outward_normals_to_faces(:,:)
    real(r_def), allocatable :: outward_normals_to_horizontal_faces(:,:)
    real(r_def), allocatable :: outward_normals_to_vertical_faces(:,:)

    ! Geometric entities
    integer(i_def), allocatable :: vertex_entities(:)
    integer(i_def), allocatable :: edge_entities(:)
    integer(i_def), allocatable :: face_entities(:)

  contains

    private

    procedure :: reference_element_init
    procedure :: reference_element_final
    procedure, public :: get_number_2d_vertices
    procedure, public :: get_number_2d_edges
    procedure, public :: get_number_vertices
    procedure, public :: get_number_edges
    procedure, public :: get_number_horizontal_edges
    procedure, public :: get_number_vertical_edges
    procedure, public :: get_number_faces
    procedure, public :: get_number_horizontal_faces
    procedure, public :: get_number_vertical_faces
    procedure, public :: get_number_verts_per_edge
    procedure, public :: get_edge_on_face
    procedure, public :: get_vertex
    procedure, public :: get_vertex_coordinates
    procedure, public :: get_edge_centre_coordinates
    procedure, public :: get_face_centre_coordinates
    procedure, public :: get_volume_centre_coordinates
    procedure, public :: get_normal_to_face
    procedure, public :: get_normals_to_faces
    procedure, public :: get_normals_to_horizontal_faces
    procedure, public :: get_normals_to_vertical_faces
    procedure, public :: get_tangent_to_edge
    procedure, public :: get_outward_normal_to_face
    procedure, public :: get_outward_normals_to_faces
    procedure, public :: get_outward_normals_to_horizontal_faces
    procedure, public :: get_outward_normals_to_vertical_faces
    procedure, public :: get_face_entity
    procedure, public :: get_edge_entity
    procedure, public :: get_vertex_entity
    procedure(entities_iface),          deferred :: populate_entity_labels
    procedure(vertices_iface),          deferred :: populate_vertices
    procedure(entity_centre_iface),     deferred :: populate_entity_centres
    procedure(vertices_on_faces_iface), deferred :: populate_vertices_on_faces
    procedure(vertices_on_edges_iface), deferred :: populate_vertices_on_edges
    procedure(edge_on_face_iface),      deferred :: populate_edge_on_face
    procedure(edge_on_vertex_iface),    deferred :: populate_edge_on_vertex
    procedure(face_on_edge_iface),      deferred :: populate_face_on_edge
    procedure(normals_to_faces_iface),  deferred :: populate_normals_to_faces
    procedure(normals_to_faces_iface),  deferred :: populate_normals_to_horizontal_faces
    procedure(normals_to_faces_iface),  deferred :: populate_normals_to_vertical_faces
    procedure(tangent_to_edge_iface),   deferred :: populate_tangent_to_edge
    procedure(normals_to_faces_iface),  deferred :: populate_outward_normals_to_faces
    procedure(normals_to_faces_iface),  deferred :: populate_outward_normals_to_horizontal_faces
    procedure(normals_to_faces_iface),  deferred :: populate_outward_normals_to_vertical_faces
  end type reference_element_type

  abstract interface
    !> @brief Fills arrays with entity labels. These arrays map
    !> entity index to the geometric entity on the reference
    !> cell.
    !>
    !> @param[out] vertex_entities Maps vertex index to geometric entity.
    !> @param[out] edge_entities   Maps edge index to geometric entity.
    !> @param[out] face_entities   Maps face index to geometric entity.
    !>
    subroutine entities_iface( this, vertex_entities, edge_entities, &
                               face_entities )
      import reference_element_type, i_def
      class(reference_element_type), intent(in) :: this
      integer(i_def), intent(out) :: vertex_entities(:)
      integer(i_def), intent(out) :: edge_entities(:)
      integer(i_def), intent(out) :: face_entities(:)
    end subroutine entities_iface
    !>
    !> @brief Fills an array with vertex coordinates.
    !>
    !> @param[out] vertex Holds n times 3 coordinate values.
    !>
    subroutine vertices_iface( this, vertex )
      import reference_element_type, r_def
      class(reference_element_type), intent(in) :: this
      real(r_def), intent(out) :: vertex(:,:)
    end subroutine vertices_iface
    !>
    !> @brief Fills arrays with entity centre point coordinates.
    !>
    !> @param[out] edges    Holds n times 3 coordinate values.
    !> @param[out] faces    Holds n times 3 coordinate values.
    !> @param[out] volumes  Holds n times 3 coordinate values.
    !>
    subroutine entity_centre_iface( this, edges, faces, volumes )
      import reference_element_type, r_def
      class(reference_element_type), intent(in) :: this
      real(r_def), intent(out) :: edges(:,:)
      real(r_def), intent(out) :: faces(:,:)
      real(r_def), intent(out) :: volumes(:,:)
    end subroutine entity_centre_iface
    !>
    !> @brief Fills an array with vertex coordinates on faces.
    !>
    !> @param[out] vertex_on_face  Holds n times 3 coordinate values.
    !>
    subroutine vertices_on_faces_iface( this, vertex_on_face )
      import reference_element_type, i_def
      class(reference_element_type), intent(in) :: this
      integer(i_def), intent(out) :: vertex_on_face(:,:)
    end subroutine vertices_on_faces_iface
    !>
    !> @brief Fills an array with vertex coordinates on edges.
    !>
    !> @param[out] vertex_on_edge  Holds n times 3 coordinate values.
    !>
    subroutine vertices_on_edges_iface( this, vertex_on_edge )
      import reference_element_type, i_def
      class(reference_element_type), intent(in) :: this
      integer(i_def), intent(out) :: vertex_on_edge(:,:)
    end subroutine vertices_on_edges_iface
    !>
    !> @brief Fills an array with IDs of edges around each face.
    !>
    !> @param[out] edge_on_face  Holds face by edge.
    !>
    subroutine edge_on_face_iface( this, edge_on_face )
      import reference_element_type, i_def
      class(reference_element_type), intent(in) :: this
      integer(i_def), intent(out) :: edge_on_face(:,:)
    end subroutine edge_on_face_iface
    !>
    !> @brief Fills an array with IDs of edges around a vertex.
    !>
    !> @param[out] edge_on_vertex  Holds vertex by edge.
    !>
    subroutine edge_on_vertex_iface( this, edge_on_vertex )
      import reference_element_type, i_def
      class(reference_element_type), intent(in) :: this
      integer(i_def), intent(out) :: edge_on_vertex(:,:)
    end subroutine edge_on_vertex_iface
    !>
    !> @brief Fills an array with IDs of faces around an edge.
    !>
    !> @param[out] edges  Holds edge by face.
    !>
    subroutine face_on_edge_iface( this, edges )
      import reference_element_type, i_def
      class(reference_element_type), intent(in) :: this
      integer(i_def), intent(out) :: edges(:,:)
    end subroutine face_on_edge_iface
    !>
    !> @brief Fills an array with vector coordinates for normals to faces.
    !>
    !> @param[out] normals  Holds 3 by n vector values.
    !>
    subroutine normals_to_faces_iface( this, normals )
      import reference_element_type, r_def
      class(reference_element_type), intent(in) :: this
      real(r_def), intent(out) :: normals(:,:)
    end subroutine normals_to_faces_iface
    !>
    !> @brief Fills an array with vector coordinates for tangents to edges.
    !>
    !> @param[out] tangents  Holds n by 3 vector values.
    !>
    subroutine tangent_to_edge_iface( this, tangents )
      import reference_element_type, r_def
      class(reference_element_type), intent(in) :: this
      real(r_def), intent(out) :: tangents(:,:)
    end subroutine tangent_to_edge_iface
  end interface

  ! It is possible that we may end up having to calculate these from the
  ! arguments if we can, but for the moment they are the same for
  ! all our children.
  integer(i_def), parameter :: vert_per_face   = 4
  integer(i_def), parameter :: edge_per_face   = 4
  integer(i_def), parameter :: edge_per_vertex = 3

  ! Useful constants.
  !
  ! Short cut to the value of (root 3) over 2
  real(r_def), parameter :: RT3OV2 = sqrt(3.0_r_def) / 2.0_r_def
  ! Short cut to the value of (root 3) over 4
  real(r_def), parameter :: RT3OV4 = sqrt(3.0_r_def) / 4.0_r_def

  ! Normal and tangential vectors.
  !
  ! Unit vector in the positive k-direction.
  real(r_def), parameter :: K_VEC(3) = (/ 0.0_r_def, &
                                          0.0_r_def, &
                                          1.0_r_def /)
  ! Unit vector in the negative k-direction.
  real(r_def), parameter :: MINUS_K_VEC(3) = (/  0.0_r_def, &
                                                 0.0_r_def, &
                                                -1.0_r_def /)

  !> @name Entities of a reference element.
  !> @{
  integer(i_def), parameter, public :: V = 1 !< Enumerates the centre of the cell (volume).
  !> @}

  ! @name Offset parameters used to generate a unique identifier for each geometric entity.
  !> @{
  integer(i_def), parameter, public :: VERTEX_OFFSET = 3000
  integer(i_def), parameter, public :: EDGE_OFFSET   = 2000
  integer(i_def), parameter, public :: FACE_OFFSET   = 1000
  !> @}

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Describes a unit cube reference element.
  !>
  !> @details Entity naming convention for reference cube:
  !>
  !> <pre>
  !>         NWT-\--NT-\--NET
  !>         /|           /|
  !>        WT|          ET|
  !>       /  |         /  |             T  N
  !>     SWT-\--ST-\--SET  |             | /
  !>      |   |        |   |             |/
  !>      |  NWB-\--NB-|--NEB      W -\--\--\--\-- E
  !>      |  /         |  /             /|
  !>      | WB         | EB            / |
  !>      |/           |/             S  B
  !>     SWB-\--SB-\--SEB
  !> </pre>
  !>
  !> "Horizontal" entities are visited when looping in W ("west"), S ("south"),
  !> E ("east") and N ("north") directions while "vertical" entities are visited
  !> when looping in B ("bottom") and T ("top") directions.
  !>
  type, extends(reference_element_type), public :: reference_cube_type
    private
  contains
    ! Does not compile with nvfortran
    ! private
    procedure :: populate_entity_labels     => cube_populate_entity_labels
    procedure :: populate_vertices          => cube_populate_vertices
    procedure :: populate_entity_centres    => cube_populate_entity_centres
    procedure :: populate_vertices_on_faces => cube_populate_vertices_on_faces
    procedure :: populate_vertices_on_edges => cube_populate_vertices_on_edges
    procedure :: populate_edge_on_face      => cube_populate_edge_on_face
    procedure :: populate_edge_on_vertex    => cube_populate_edge_on_vertex
    procedure :: populate_face_on_edge      => cube_populate_face_on_edge
    procedure :: populate_normals_to_faces  => cube_populate_normals_to_faces
    procedure :: populate_normals_to_horizontal_faces &
                                            => cube_populate_normals_to_horizontal_faces
    procedure :: populate_normals_to_vertical_faces &
                                            => cube_populate_normals_to_vertical_faces
    procedure :: populate_tangent_to_edge   => cube_populate_tangent_to_edge
    procedure :: populate_outward_normals_to_faces &
                                            => cube_populate_outward_normals_to_faces
    procedure :: populate_outward_normals_to_horizontal_faces &
                                            => cube_populate_outward_normals_to_horizontal_faces
    procedure :: populate_outward_normals_to_vertical_faces &
                                            => cube_populate_outward_normals_to_vertical_faces
    final :: reference_cube_destructor
  end type reference_cube_type

  interface reference_cube_type
    module procedure reference_cube_constructor
  end interface reference_cube_type

  !> @name Faces of the cube.
  !> @{
  integer(i_def), parameter, public :: W=1 !< "West" face of the cell.
  integer(i_def), parameter, public :: S=2 !< "South" face of the cell.
  integer(i_def), parameter, public :: E=3 !< "East" face of the cell.
  integer(i_def), parameter, public :: N=4 !< "North" face of the cell.
  integer(i_def), parameter, public :: B=5 !< "Bottom" face of the cell.
  integer(i_def), parameter, public :: T=6 !< "Top" face of the cell.
  !> @}
  !>
  !> @name Vertices of the cube.
  !> @{
  integer(i_def), parameter, public :: SWB=1 !< "South west bottom" corner of the cell.
  integer(i_def), parameter, public :: SEB=2 !< "South east bottom" corner of the cell.
  integer(i_def), parameter, public :: NEB=3 !< "North east bottom" corner of the cell
  integer(i_def), parameter, public :: NWB=4 !< "North west bottom" corner of the cell
  integer(i_def), parameter, public :: SWT=5 !< "South west top" corner of the cell.
  integer(i_def), parameter, public :: SET=6 !< "South east top" corner of the cell.
  integer(i_def), parameter, public :: NET=7 !< "North east top" corner of the cell.
  integer(i_def), parameter, public :: NWT=8 !< "North west top" corner of the cell.
  !> @}
  !>
  !> @name Edges of the cube.
  !> @{
  integer(i_def), parameter, public :: WB=1 !< "West bottom" edge of the cell.
  integer(i_def), parameter, public :: SB=2 !< "South bottom" edge of the cell.
  integer(i_def), parameter, public :: EB=3 !< "East bottom" edge of the cell.
  integer(i_def), parameter, public :: NB=4 !< "North bottom" edge of the cell.
  integer(i_def), parameter, public :: SW=5 !< "South west" edge of the cell.
  integer(i_def), parameter, public :: SE=6 !< "South east" edge of the cell.
  integer(i_def), parameter, public :: NE=7 !< "North east" edge of the cell.
  integer(i_def), parameter, public :: NW=8 !< "North west" edge of the cell.
  integer(i_def), parameter, public :: WT=9 !< "West top" edge of the cell.
  integer(i_def), parameter, public :: ST=10 !< "South top" edge of the cell.
  integer(i_def), parameter, public :: ET=11 !< "East top" edge of the cell.
  integer(i_def), parameter, public :: NT=12 !< "North top" edge of the cell.
  !> @}

  ! Normal and tangential vectors on the cube.
  !
  ! Unit vector in the positive i-direction.
  real(r_def), parameter :: I_VEC(3) = (/ 1.0_r_def, &
                                          0.0_r_def, &
                                          0.0_r_def /)
  ! Unit vector in the negative i-direction.
  real(r_def), parameter :: MINUS_I_VEC(3) = (/ -1.0_r_def, &
                                                 0.0_r_def, &
                                                 0.0_r_def /)
  ! Unit vector in the positive j-direction.
  real(r_def), parameter :: J_VEC(3) = (/ 0.0_r_def, &
                                          1.0_r_def, &
                                          0.0_r_def /)
  ! Unit vector in the negative j-direction.
  real(r_def), parameter :: MINUS_J_VEC(3) = (/  0.0_r_def, &
                                                -1.0_r_def, &
                                                 0.0_r_def /)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Describes a unit triangular prism reference element.
  !>
  !> @details Entity naming convention for reference prism:
  !>
  !> <pre>
  !>               QRU
  !>               /|\  !>              / | \  !>            QU  |  RU


  !>            /   QR  \  !>           /    |    \  !>         PQU-\--PU-\--PRU              Q  U  R


  !>          |     |      |                \\ | /
  !>          |    QRL     |                 \\|/
  !>          |    / \\    |                   -
  !>         PQ   /   \\   PR                  /|
  !>          | QL     RL  |                 / |
  !>          | /       \\ |                P  L
  !>          |/         \\|
  !>         PQL--\-PL-\--PRL
  !>
  !>
  !>      PQU,      Face P      PRU,
  !>      PQ, +---------------+ PR,
  !>      PQL  \             /  PRL
  !>            \    Top    /
  !>             \   Face  /
  !>       Face Q \       / Face R
  !>               \     /
  !>                \   /
  !>                 \ /
  !>                  +
  !>              QRU,QR,QRL
  !>
  !> </pre>
  !>
  !> "Horizontal" entities are visited when looping in P, Q and R directions
  !>  while "vertical" entities are visited when looping in L and U directions.
  !>
  type, extends(reference_element_type), public :: reference_prism_type
    private
  contains
    ! Does not compile with nvfortran
    ! private
    procedure :: populate_entity_labels     => prism_populate_entity_labels
    procedure :: populate_vertices          => prism_populate_vertices
    procedure :: populate_entity_centres    => prism_populate_entity_centres
    procedure :: populate_vertices_on_faces => prism_populate_vertices_on_faces
    procedure :: populate_vertices_on_edges => prism_populate_vertices_on_edges
    procedure :: populate_edge_on_face      => prism_populate_edge_on_face
    procedure :: populate_edge_on_vertex    => prism_populate_edge_on_vertex
    procedure :: populate_face_on_edge      => prism_populate_face_on_edge
    procedure :: populate_normals_to_faces  => prism_populate_normals_to_faces
    procedure :: populate_normals_to_horizontal_faces &
                                            => prism_populate_normals_to_horizontal_faces
    procedure :: populate_normals_to_vertical_faces &
                                            => prism_populate_normals_to_vertical_faces
    procedure :: populate_tangent_to_edge   => prism_populate_tangent_to_edge
    procedure :: populate_outward_normals_to_faces &
                                            => prism_populate_outward_normals_to_faces
    procedure :: populate_outward_normals_to_horizontal_faces &
                                            => prism_populate_outward_normals_to_horizontal_faces
    procedure :: populate_outward_normals_to_vertical_faces &
                                            => prism_populate_outward_normals_to_vertical_faces
    final :: reference_prism_destructor
  end type reference_prism_type

  interface reference_prism_type
    module procedure reference_prism_constructor
  end interface reference_prism_type

  !> @name Faces of the prism.
  !> @{
  integer(i_def), parameter, public :: P=1
  !< Vertical face that when prism is viewed from above,
  !< appears as a line (0,0)->(1,0).
  !> Vertical face that when prism is viewed from above,
  !> appears as a line (1,0)->(0.5,srqt(3)/2).
  integer(i_def), parameter, public :: Q=2
  !> Vertical face that when prism is viewed from above,
  !> appears as a line (0.5,srqt(3)/2)->(0,0).
  integer(i_def), parameter, public :: R=3
  !> Face on the "lower" side of the prism.
  integer(i_def), parameter, public :: L=4
  !> Face on the "upper" of the prism.
  integer(i_def), parameter, public :: U=5
  !> @}
  !>
  !> @name Vertices of the prism.
  !> @{
  integer(i_def), parameter, public :: PRL=1 !< "Lower" corner between faces P and R.
  integer(i_def), parameter, public :: PQL=2 !< "Lower" corner between faces P and Q.
  integer(i_def), parameter, public :: QRL=3 !< "Lower" corner between faces Q and R.
  integer(i_def), parameter, public :: PRU=4 !< "Upper" corner between faces P and R.
  integer(i_def), parameter, public :: PQU=5 !< "Upper" corner between faces P and Q.
  integer(i_def), parameter, public :: QRU=6 !< "Upper" corner between faces Q and R.
  !> @}
  !>
  !> @name Edges of the prism.
  !> @{
  integer(i_def), parameter, public :: PL=1 !< Lower edge of the P face.
  integer(i_def), parameter, public :: QL=2 !< Lower edge of the Q face.
  integer(i_def), parameter, public :: RL=3 !< Lower edge of the R face.
  integer(i_def), parameter, public :: PR=4 !< Edge between the P and R faces.
  integer(i_def), parameter, public :: PQ=5 !< Edge between the P and Q faces.
  integer(i_def), parameter, public :: QR=6 !< Edge between the Q and R faces.
  integer(i_def), parameter, public :: PU=7 !< Upper edge of the P face.
  integer(i_def), parameter, public :: QU=8 !< Upper edge of the Q face.
  integer(i_def), parameter, public :: RU=9 !< Upper edge of the R face.
  !> @}

  ! Normal and tangential vectors on the prism.
  !
  ! Unit vector that is normal to the Q face in the triangular prism.
  real(r_def), parameter :: Q_NORM_VEC(3) = (/  RT3OV2,    &
                                                0.5_r_def, &
                                                0.0_r_def /)
  ! Unit vector that is normal to the R face in the triangular prism.
  real(r_def), parameter :: R_NORM_VEC(3) = (/ -RT3OV2,    &
                                                0.5_r_def, &
                                                0.0_r_def /)
  ! Unit vector that is tangential to the lower and upper edges of the Q face
  ! in a triangular prism.
  real(r_def), parameter :: Q_TANG_VEC(3) = (/ -0.5_r_def, &
                                                RT3OV2,    &
                                                0.0_r_def /)
  ! Unit vector that is tangential to the lower and upper edges of the R face
  ! in a triangular prism.
  real(r_def), parameter :: R_TANG_VEC(3) = (/ -0.5_r_def, &
                                               -RT3OV2,    &
                                                0.0_r_def /)

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Base reference element methods.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! @brief Initialises the common parameters of a 3D reference element by
  !        extruding a 2D reference shape.
  !
  ! [in] nvertices_2d  Vertices of the horizontal (2D) reference shape.
  ! [in] nedges_2d     Edges of the horizontal (2D) reference shape.
  !
  subroutine reference_element_init( this,         &
                                     nvertices_2d, &
                                     nedges_2d )
    implicit none

    class(reference_element_type), intent(inout) :: this
    integer(i_def), intent(in) :: nvertices_2d
    integer(i_def), intent(in) :: nedges_2d

    integer(i_def), parameter :: number_of_volumes = 1

    ! 2D cell information
    this%number_2d_vertices = nvertices_2d
    this%number_2d_edges    = nedges_2d

    ! Horizontal entities of the 3D reference element:
    !  - Horizontal edges are the two sets of 2D edges enclosing the bottom/lower
    !    and top/upper faces and lie in the horizontal (x-y) plane;
    !  - Horizontal faces are extruded from the edges of the 2D reference shape
    !    (2D edges lie in the horizontal plane).
    this%number_horizontal_edges = 2 * this%number_2d_edges
    this%number_horizontal_faces = this%number_2d_edges

    ! Vertical entities of the 3D reference element:
    ! - Vertical edges are extruded from the vertices of the 2D reference shape
    !   and lie in the vertical (x-z) and (y-z) planes;
    this%number_vertical_edges = this%number_2d_vertices
    ! - Vertical faces are bottom/lower and and top/upper faces (for both cubes
    !   and prisms there will only ever be 2).
    this%number_vertical_faces = 2_i_def

    ! All entities of the 3D reference element:
    ! - 3D vertices consist of two sets of 2D vertices;
    this%number_vertices       = 2 * this%number_2d_vertices
    ! - 3D edges consist of horizontal and vertical edges;
    this%number_edges          = this%number_horizontal_edges &
                               + this%number_vertical_edges
    ! - 3D faces consist of horizontal and vertical faces.
    this%number_faces          = this%number_horizontal_faces &
                               + this%number_vertical_faces

    ! Allocate and populate arrays
    allocate( this%vertex_entities(this%number_vertices) )
    allocate( this%edge_entities(this%number_edges) )
    allocate( this%face_entities(this%number_faces) )
    call this%populate_entity_labels( this%vertex_entities, &
                                      this%edge_entities,   &
                                      this%face_entities )

    allocate( this%vertex_coords(this%number_vertices, 3) )
    call this%populate_vertices( this%vertex_coords )

    allocate ( this%edge_coords(this%number_edges, 3) )
    allocate ( this%face_coords(this%number_faces, 3) )
    allocate ( this%volume_coords(number_of_volumes, 3) )
    call this%populate_entity_centres( this%edge_coords, &
                                       this%face_coords, &
                                       this%volume_coords )

    allocate( this%vert_on_face(this%number_faces, vert_per_face) )
    call this%populate_vertices_on_faces( this%vert_on_face )

    allocate( this%vert_on_edge(this%number_edges, this%number_verts_per_edge) )
    call this%populate_vertices_on_edges( this%vert_on_edge )

    allocate( this%edge_on_face(this%number_faces, edge_per_face) )
    call this%populate_edge_on_face( this%edge_on_face )

    allocate( this%edge_on_vert(this%number_vertices, edge_per_vertex) )
    call this%populate_edge_on_vertex( this%edge_on_vert )

    allocate( this%face_on_edge(this%number_edges, this%number_faces_per_edge) )
    call this%populate_face_on_edge( this%face_on_edge )

    allocate( this%normals_to_faces(3, this%number_faces))
    call this%populate_normals_to_faces( this%normals_to_faces )

    allocate( this%normals_to_horizontal_faces(3, this%number_horizontal_faces))
    call this%populate_normals_to_horizontal_faces( this%normals_to_horizontal_faces )

    allocate( this%normals_to_vertical_faces(3, this%number_vertical_faces))
    call this%populate_normals_to_vertical_faces( this%normals_to_vertical_faces )

    allocate( this%tangent_to_edge(this%number_edges,3) )
    call this%populate_tangent_to_edge( this%tangent_to_edge )

    allocate( this%outward_normals_to_faces(3, this%number_faces) )
    call this%populate_outward_normals_to_faces( this%outward_normals_to_faces )

    allocate( this%outward_normals_to_horizontal_faces(3, this%number_horizontal_faces) )
    call this%populate_outward_normals_to_horizontal_faces( this%outward_normals_to_horizontal_faces )

    allocate( this%outward_normals_to_vertical_faces(3, this%number_vertical_faces) )
    call this%populate_outward_normals_to_vertical_faces( this%outward_normals_to_vertical_faces )

  end subroutine reference_element_init

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Tidies up the common reference element parameters.
  !
  subroutine reference_element_final( this )

    implicit none

    class(reference_element_type), intent(inout) :: this

    if (allocated( this%vertex_entities )) deallocate( this%vertex_entities )
    if (allocated( this%edge_entities )) deallocate( this%edge_entities )
    if (allocated( this%face_entities )) deallocate( this%face_entities )
    if (allocated( this%vertex_coords )) deallocate( this%vertex_coords )
    if (allocated( this%vert_on_face )) deallocate( this%vert_on_face )
    if (allocated( this%vert_on_edge )) deallocate( this%vert_on_edge )
    if (allocated( this%edge_on_face )) deallocate( this%edge_on_face )
    if (allocated( this%edge_on_vert )) deallocate( this%edge_on_vert )
    if (allocated( this%face_on_edge )) deallocate( this%face_on_edge )
    if (allocated( this%normals_to_faces )) deallocate( this%normals_to_faces )
    if (allocated( this%normals_to_horizontal_faces )) deallocate( this%normals_to_horizontal_faces )
    if (allocated( this%normals_to_vertical_faces )) deallocate( this%normals_to_vertical_faces )
    if (allocated( this%tangent_to_edge )) deallocate( this%tangent_to_edge )
    if (allocated( this%outward_normals_to_faces )) deallocate( this%outward_normals_to_faces )
    if (allocated( this%outward_normals_to_horizontal_faces )) deallocate( this%outward_normals_to_horizontal_faces )
    if (allocated( this%outward_normals_to_vertical_faces )) deallocate( this%outward_normals_to_vertical_faces )

  end subroutine reference_element_final

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of vertices of the 2D reference shape.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_2d_vertices( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_2d_vertices

    get_number_2d_vertices = this%number_2d_vertices

  end function get_number_2d_vertices

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of edges of the 2D reference shape.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_2d_edges( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_2d_edges

    get_number_2d_edges = this%number_2d_edges

  end function get_number_2d_edges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of vertices in the reference element.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_vertices( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_vertices

    get_number_vertices = this%number_vertices

  end function get_number_vertices

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of edges in the reference element.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_edges( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_edges

    get_number_edges = this%number_edges

  end function get_number_edges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of edges in the horizontal, these are edges with
  !>        tangents in the horizontal directions.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_horizontal_edges( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_horizontal_edges

    get_number_horizontal_edges = this%number_horizontal_edges

  end function get_number_horizontal_edges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of edges in the vertical, thes are edges with
  !>        tangents in the vertical directions.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_vertical_edges( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_vertical_edges

    get_number_vertical_edges = this%number_vertical_edges

  end function get_number_vertical_edges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of faces in the reference element.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_faces( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_faces

    get_number_faces = this%number_faces

  end function get_number_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of faces in the horizontal, these are faces whose
  !>        normals are in the horizontal directions.
  !>
  !> @return Positive integer.
  !>
  pure function get_number_horizontal_faces( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_horizontal_faces

    get_number_horizontal_faces = this%number_horizontal_faces

  end function get_number_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of faces in the vertical, these are faces whose
  !>        normals are in the vertical direction.
  !>
  !> @return Positive integer (always 2).
  !>
  pure function get_number_vertical_faces( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_vertical_faces

    get_number_vertical_faces = this%number_vertical_faces

  end function get_number_vertical_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of vertices per edge on the reference element.
  !>
  !> @return Positive integer (always 2).
  !>
  pure function get_number_verts_per_edge( this )

    implicit none

    class(reference_element_type), intent(in) :: this
    integer(i_def) :: get_number_verts_per_edge

    get_number_verts_per_edge = this%number_verts_per_edge

  end function get_number_verts_per_edge

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the entity label corresponding to a vertex index.
  !>
  !> @param[in]  vertex_index  Vertex enumerator.
  !> @returns The geometric entity label.
  !>
  pure function get_vertex_entity( this, vertex_index )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: vertex_index
    integer(i_def)                             :: get_vertex_entity

    get_vertex_entity = this%vertex_entities(vertex_index) + VERTEX_OFFSET

  end function get_vertex_entity

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the unique entity label corresponding to an edge index.
  !>
  !> @param[in]  edge_index  Edge enumerator.
  !> @returns The geometric entity label.
  !>
  pure function get_edge_entity( this, edge_index )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: edge_index
    integer(i_def)                             :: get_edge_entity

    get_edge_entity = this%edge_entities(edge_index) + EDGE_OFFSET

  end function get_edge_entity

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the unique entity label corresponding to a face index.
  !>
  !> @param[in]  face_index  Face enumerator.
  !> @returns The geometric entity label.
  !>
  pure function get_face_entity( this, face_index )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: face_index
    integer(i_def)                             :: get_face_entity

    get_face_entity = this%face_entities(face_index) + FACE_OFFSET

  end function get_face_entity

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the co-ordinates of a vertex.
  !>
  !> @param[in] vertex_index  Positive integer index.
  !>
  !> @return Three element coordinate array.
  !>
  pure function get_vertex( this, vertex_index )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: vertex_index
    real(r_def) :: get_vertex(3)

    get_vertex = this%vertex_coords(vertex_index, :)

  end function get_vertex

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets an array of all the vertices.
  !>
  !> @param[out] vertices  Allocates and fills an array of coordinate
  !>                       triples.
  !>
  subroutine get_vertex_coordinates( this, vertices )

    implicit none

    class(reference_element_type), intent(in) :: this
    real(r_def), intent(out), allocatable :: vertices(:,:)

    allocate( vertices, source=this%vertex_coords )

  end subroutine get_vertex_coordinates

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets coordinates of the centre of each edge on the reference
  !>        element.
  !>
  !> @param[out] edges  Allocates and fills an array of coordinate per edge.
  !>
  subroutine get_edge_centre_coordinates( this, edges )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: edges(:,:)

    allocate( edges, source=this%edge_coords )

  end subroutine get_edge_centre_coordinates

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets coordinates of the centre of each face on the reference
  !>        element.
  !>
  !> @param[out] faces  Allocates and fills an array of coordinate per face.
  !>
  subroutine get_face_centre_coordinates( this, faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: faces(:,:)

    allocate( faces, source=this%face_coords )

  end subroutine get_face_centre_coordinates

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets coordinates of the centre of the volume of the reference
  !>        element.
  !>
  !> @param[out] volume  Allocates and fills an array of coordinate per
  !>                     volume.
  !>
  subroutine get_volume_centre_coordinates( this, volume )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: volume(:,:)

    allocate( volume, source=this%volume_coords )

  end subroutine get_volume_centre_coordinates

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the edges around a face.
  !>
  !> @param[in]  face_index  Face enumerator.
  !> @param[out] edges  Filled with list of edge enumerators.
  !>
  pure subroutine get_edge_on_face( this, face_index, edges )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: face_index
    integer(i_def),                intent(out) :: edges(edge_per_face)

    edges = this%edge_on_face(face_index, :)

  end subroutine get_edge_on_face

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the vector representing the normal to a face.
  !>
  !> @param[in]  face_index  Face enumerator.
  !> @param[out] normal  Vector triple.
  !>
  pure subroutine get_normal_to_face( this, face_index, normal )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: face_index
    real(r_def),                   intent(out) :: normal(3)

    normal = this%normals_to_faces(:, face_index)

  end subroutine get_normal_to_face

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the array of vectors normal to all faces.
  !>
  !> @param[out] normals_to_faces  Allocates and fills an array of vector
  !>                               triple per face.
  !>
  subroutine get_normals_to_faces( this, normals_to_faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals_to_faces(:,:)

    allocate( normals_to_faces, source=this%normals_to_faces )

  end subroutine get_normals_to_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the array of vectors normal to horizontal faces (faces whose
  !>        normals lie in the horizontal plane).
  !>
  !> @param[out] normals_to_horizontal_faces  Allocates and fills an array of vector
  !>                                          triple per horizontal face.
  !>
  subroutine get_normals_to_horizontal_faces( this, normals_to_horizontal_faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals_to_horizontal_faces(:,:)

    allocate( normals_to_horizontal_faces, source=this%normals_to_horizontal_faces )

  end subroutine get_normals_to_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the array of vectors normal to vertical faces (faces whose
  !>        normals lie in the vertical plane).
  !>
  !> @param[out] normals_to_vertical_faces  Allocates and fills an array of vector
  !>                                        triple per vertical face.
  !>
  subroutine get_normals_to_vertical_faces( this, normals_to_vertical_faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals_to_vertical_faces(:,:)

    allocate( normals_to_vertical_faces, source=this%normals_to_vertical_faces )

  end subroutine get_normals_to_vertical_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the vector representing the tangent to an edge.
  !>
  !> @param[in] edge_index  Edge enumerator.
  !> @param[out] tangent  Vector triple.
  !>
  pure subroutine get_tangent_to_edge( this, edge_index, tangent )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: edge_index
    real(r_def),                   intent(out) :: tangent(3)

    tangent = this%tangent_to_edge(edge_index, :)

  end subroutine get_tangent_to_edge

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the vector representing the outward normal to a face.
  !>
  !> @param[in]  face_index  Face enumerator.
  !> @param[out] normal  Vector triple.
  !>
  pure subroutine get_outward_normal_to_face( this, face_index, normal )

    implicit none

    class(reference_element_type), intent(in)  :: this
    integer(i_def),                intent(in)  :: face_index
    real(r_def),                   intent(out) :: normal(3)

    normal = this%outward_normals_to_faces(:, face_index)

  end subroutine get_outward_normal_to_face

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the array of vectors normal to all "outward faces".
  !>
  !> @param[out] outward_normals_to_faces  Allocates and fills an array
  !>                                       vector triple per face.
  !>
  subroutine get_outward_normals_to_faces( this, outward_normals_to_faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: outward_normals_to_faces(:,:)

    allocate( outward_normals_to_faces, source=this%outward_normals_to_faces )

  end subroutine get_outward_normals_to_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the array of vectors normal to horizontal "outward faces"
  !>        (faces whose normals lie in the horizontal plane).
  !>
  !> @param[out] outward_normals_to_horizontal_faces  Allocates and fills an array
  !>                                                  vector triple per horizontal face.
  !>
  subroutine get_outward_normals_to_horizontal_faces( this, outward_normals_to_horizontal_faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: outward_normals_to_horizontal_faces(:,:)

    allocate( outward_normals_to_horizontal_faces, source=this%outward_normals_to_horizontal_faces )

  end subroutine get_outward_normals_to_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the array of vectors normal to vertical "outward faces"
  !>        (faces whose normals lie in the vertical plane).
  !>
  !> @param[out] outward_normals_to_vertical_faces  Allocates and fills an array
  !>                                                vector triple per vertical face.
  !>
  subroutine get_outward_normals_to_vertical_faces( this, outward_normals_to_vertical_faces )

    implicit none

    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: outward_normals_to_vertical_faces(:,:)

    allocate( outward_normals_to_vertical_faces, source=this%outward_normals_to_vertical_faces )

  end subroutine get_outward_normals_to_vertical_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reference cube methods
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Constructs a reference cube by extruding a 2D reference square.
  !>
  !> @return The new instance.
  !>
  function reference_cube_constructor() result(new_cube)

    implicit none

    type(reference_cube_type) :: new_cube

    call new_cube%reference_element_init( nvertices_2d = 4, &
                                          nedges_2d = 4 )

  end function reference_cube_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destroys a reference cube.
  !>
  subroutine reference_cube_destructor( this )

    implicit none

    type(reference_cube_type), intent(inout) :: this

    call this%reference_element_final()

  end subroutine reference_cube_destructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills entity arrays with the corresponding geometric labels on
  ! a unit-cube.
  !
  ! [out] vertex_entities Maps vertex index to geometric entity.
  ! [out] edge_entities   Maps edge index to geometric entity.
  ! [out] face_entities   Maps face index to geometric entity.
  !
  subroutine cube_populate_entity_labels( this, vertex_entities, &
                                          edge_entities, face_entities )

    implicit none

    class(reference_cube_type), intent(in)  :: this
    integer(i_def),             intent(out) :: vertex_entities(:)
    integer(i_def),             intent(out) :: edge_entities(:)
    integer(i_def),             intent(out) :: face_entities(:)

    ! Vertex entities defining the closure of each edge.
    vertex_entities(1) = SWB
    vertex_entities(2) = SEB
    vertex_entities(3) = NEB
    vertex_entities(4) = NWB
    vertex_entities(5) = SWT
    vertex_entities(6) = SET
    vertex_entities(7) = NET
    vertex_entities(8) = NWT

    ! Edge entities defining the closure of each face.
    edge_entities(1)  = WB
    edge_entities(2)  = SB
    edge_entities(3)  = EB
    edge_entities(4)  = NB
    edge_entities(5)  = SW
    edge_entities(6)  = SE
    edge_entities(7)  = NE
    edge_entities(8)  = NW
    edge_entities(9)  = WT
    edge_entities(10) = ST
    edge_entities(11) = ET
    edge_entities(12) = NT

    ! Face entities on the reference cube
    face_entities(1) = W
    face_entities(2) = S
    face_entities(3) = E
    face_entities(4) = N
    face_entities(5) = B
    face_entities(6) = T

  end subroutine cube_populate_entity_labels

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the coordinates of a unit-cube.
  !
  ! [out] vertex  Vertex by 3 array of coordinates.
  !
  subroutine cube_populate_vertices( this, vertex )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: vertex(:,:)

    vertex(SWB,:) = (/ 0.0_r_def, 0.0_r_def, 0.0_r_def /)
    vertex(SEB,:) = (/ 1.0_r_def, 0.0_r_def, 0.0_r_def /)
    vertex(NEB,:) = (/ 1.0_r_def, 1.0_r_def, 0.0_r_def /)
    vertex(NWB,:) = (/ 0.0_r_def, 1.0_r_def, 0.0_r_def /)
    vertex(SWT,:) = (/ 0.0_r_def, 0.0_r_def, 1.0_r_def /)
    vertex(SET,:) = (/ 1.0_r_def, 0.0_r_def, 1.0_r_def /)
    vertex(NET,:) = (/ 1.0_r_def, 1.0_r_def, 1.0_r_def /)
    vertex(NWT,:) = (/ 0.0_r_def, 1.0_r_def, 1.0_r_def /)

  end subroutine cube_populate_vertices

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills arrays with mid points of mesh entities on a unit-cube.
  !
  ! [out] edges    Edge by 3 coordinate array.
  ! [out] faces    Face by 3 coordinate array.
  ! [out] volumes  Volume by 3 coordinate array.
  !
  subroutine cube_populate_entity_centres( this, edges, faces, volumes )

    implicit none

    class(reference_cube_type), intent(in) :: this

    real(r_def), intent(out) :: edges(:,:)
    real(r_def), intent(out) :: faces(:,:)
    real(r_def), intent(out) :: volumes(:,:)

    edges(WB,:) = (/ 0.0_r_def, 0.5_r_def, 0.0_r_def /)
    edges(SB,:) = (/ 0.5_r_def, 0.0_r_def, 0.0_r_def /)
    edges(EB,:) = (/ 1.0_r_def, 0.5_r_def, 0.0_r_def /)
    edges(NB,:) = (/ 0.5_r_def, 1.0_r_def, 0.0_r_def /)

    edges(SW,:) = (/ 0.0_r_def, 0.0_r_def, 0.5_r_def /)
    edges(SE,:) = (/ 1.0_r_def, 0.0_r_def, 0.5_r_def /)
    edges(NE,:) = (/ 1.0_r_def, 1.0_r_def, 0.5_r_def /)
    edges(NW,:) = (/ 0.0_r_def, 1.0_r_def, 0.5_r_def /)

    edges(WT,:) = (/ 0.0_r_def, 0.5_r_def, 1.0_r_def /)
    edges(ST,:) = (/ 0.5_r_def, 0.0_r_def, 1.0_r_def /)
    edges(ET,:) = (/ 1.0_r_def, 0.5_r_def, 1.0_r_def /)
    edges(NT,:) = (/ 0.5_r_def, 1.0_r_def, 1.0_r_def /)

    faces(W,:) = (/ 0.0_r_def, 0.5_r_def, 0.5_r_def /)
    faces(S,:) = (/ 0.5_r_def, 0.0_r_def, 0.5_r_def /)
    faces(E,:) = (/ 1.0_r_def, 0.5_r_def, 0.5_r_def /)
    faces(N,:) = (/ 0.5_r_def, 1.0_r_def, 0.5_r_def /)

    faces(B,:) = (/ 0.5_r_def, 0.5_r_def, 0.0_r_def /)
    faces(T,:) = (/ 0.5_r_def, 0.5_r_def, 1.0_r_def /)

    volumes(V,:) = (/ 0.5_r_def, 0.5_r_def, 0.5_r_def /)

  end subroutine cube_populate_entity_centres

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of vertices around a face.
  !
  ! [out] vertex_on_face  Face by n array of vertices
  !
  subroutine cube_populate_vertices_on_faces( this, vertex_on_face )

    implicit none

    class(reference_cube_type), intent(in) :: this
    integer(i_def), intent(out) :: vertex_on_face(:,:)

    ! Vertices on each face - anticlockwise ordering
    vertex_on_face(W,:) = (/ SWB, NWB, NWT, SWT /)
    vertex_on_face(S,:) = (/ SWB, SEB, SET, SWT /)
    vertex_on_face(E,:) = (/ SEB, NEB, NET, SET /)
    vertex_on_face(N,:) = (/ NEB, NWB, NWT, NET /)
    vertex_on_face(B,:) = (/ SWB, SEB, NEB, NWB /)
    vertex_on_face(T,:) = (/ SWT, SET, NET, NWT /)

  end subroutine cube_populate_vertices_on_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of vertices on an edge.
  !
  ! [out] vertex_on_edge  Edge by n array of vertices.
  !
  subroutine cube_populate_vertices_on_edges( this, vertex_on_edge )

    implicit none

    class(reference_cube_type), intent(in) :: this
    integer(i_def), intent(out) :: vertex_on_edge(:,:)

    ! Vertices at the end of each edge
    vertex_on_edge(WB,:) = (/ NWB, SWB /)
    vertex_on_edge(SB,:) = (/ SWB, SEB /)
    vertex_on_edge(EB,:) = (/ SEB, NEB /)
    vertex_on_edge(NB,:) = (/ NEB, NWB /)
    vertex_on_edge(SW,:) = (/ SWB, SWT /)
    vertex_on_edge(SE,:) = (/ SEB, SET /)
    vertex_on_edge(NE,:) = (/ NEB, NET /)
    vertex_on_edge(NW,:) = (/ NWB, NWT /)
    vertex_on_edge(WT,:) = (/ NWT, SWT /)
    vertex_on_edge(ST,:) = (/ SWT, SET /)
    vertex_on_edge(ET,:) = (/ SET, NET /)
    vertex_on_edge(NT,:) = (/ NET, NWT /)

  end subroutine cube_populate_vertices_on_edges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of edges around a face.
  !
  ! [out] edge_on_face  Face by n array of edges.
  !
  subroutine cube_populate_edge_on_face( this, edge_on_face )

    implicit none

    class(reference_cube_type), intent(in) :: this
    integer(i_def), intent(out) :: edge_on_face(:,:)

    ! Edges on each face
    edge_on_face(W,:) = (/ WB, SW, WT, NW /)
    edge_on_face(S,:) = (/ SB, SE, ST, SW /)
    edge_on_face(E,:) = (/ EB, NE, ET, SE /)
    edge_on_face(N,:) = (/ NB, NE, NT, NW /)
    edge_on_face(B,:) = (/ WB, SB, EB, NB /)
    edge_on_face(T,:) = (/ WT, ST, ET, NT /)

  end subroutine cube_populate_edge_on_face

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of edges attached to a vertex.
  !
  ! [out] edge_on_vertex  Vertex by n array of edges.
  !
  subroutine cube_populate_edge_on_vertex( this, edge_on_vertex )

    implicit none

    class(reference_cube_type), intent(in) :: this
    integer(i_def), intent(out) :: edge_on_vertex(:,:)

    ! Edges on each vertex
    edge_on_vertex(SWB,:) = (/ WB, SB, SW /)
    edge_on_vertex(SEB,:) = (/ SB, EB, SE /)
    edge_on_vertex(NEB,:) = (/ EB, NB, NE /)
    edge_on_vertex(NWB,:) = (/ NB, WB, NW /)
    edge_on_vertex(SWT,:) = (/ SW, WT, ST /)
    edge_on_vertex(SET,:) = (/ SE, ST, ET /)
    edge_on_vertex(NET,:) = (/ NE, ET, NT /)
    edge_on_vertex(NWT,:) = (/ NW, NT, WT /)

  end subroutine cube_populate_edge_on_vertex

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of faces on an edge.
  !
  ! [out] edges  Edge by n array of faces.
  !
  subroutine cube_populate_face_on_edge( this, edges )

    implicit none

    class(reference_cube_type), intent(in) :: this
    integer(i_def), intent(out) :: edges(:,:)

    ! Faces either side of each edge
    edges(WB,:) = (/ W, B /)
    edges(SB,:) = (/ S, B /)
    edges(EB,:) = (/ E, B /)
    edges(NB,:) = (/ N, B /)
    edges(SW,:) = (/ W, S /)
    edges(SE,:) = (/ S, E /)
    edges(NE,:) = (/ E, N /)
    edges(NW,:) = (/ N, W /)
    edges(WT,:) = (/ W, T /)
    edges(ST,:) = (/ S, T /)
    edges(ET,:) = (/ E, T /)
    edges(NT,:) = (/ N, T /)

  end subroutine cube_populate_face_on_edge

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each face.
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine cube_populate_normals_to_faces( this, normals )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Unit normal vector to each face
    normals(:,W) = I_VEC
    normals(:,S) = MINUS_J_VEC
    normals(:,E) = I_VEC
    normals(:,N) = MINUS_J_VEC
    normals(:,B) = K_VEC
    normals(:,T) = K_VEC

  end subroutine cube_populate_normals_to_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each horizontal (W, S, E, N) face.
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine cube_populate_normals_to_horizontal_faces( this, normals )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Unit normal vector to each horizontal face
    normals(:,1) = this%normals_to_faces(:,W)
    normals(:,2) = this%normals_to_faces(:,S)
    normals(:,3) = this%normals_to_faces(:,E)
    normals(:,4) = this%normals_to_faces(:,N)

  end subroutine cube_populate_normals_to_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each vertical (B, T) face.
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine cube_populate_normals_to_vertical_faces( this, normals )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Unit normal vector to each vertical face
    normals(:,1) = this%normals_to_faces(:,B)
    normals(:,2) = this%normals_to_faces(:,T)

  end subroutine cube_populate_normals_to_vertical_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with tangent vectors to each edge.
  !
  ! [out] tangents  Edge by 3 array of vectors.
  !
  subroutine cube_populate_tangent_to_edge( this, tangents )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: tangents(:,:)

    ! Tangent vectors to each edge.
    ! Convention is that vector points along edge in positive xi direction.
    tangents(WB,:) = J_VEC
    tangents(SB,:) = I_VEC
    tangents(EB,:) = J_VEC
    tangents(NB,:) = I_VEC
    tangents(SW,:) = K_VEC
    tangents(SE,:) = K_VEC
    tangents(NE,:) = K_VEC
    tangents(NW,:) = K_VEC
    tangents(WT,:) = J_VEC
    tangents(ST,:) = I_VEC
    tangents(ET,:) = J_VEC
    tangents(NT,:) = I_VEC

  end subroutine cube_populate_tangent_to_edge

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each "outward face".
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine cube_populate_outward_normals_to_faces( this, normals )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Outward unit normal vector to each face
    normals(:,W) = MINUS_I_VEC
    normals(:,S) = MINUS_J_VEC
    normals(:,E) = I_VEC
    normals(:,N) = J_VEC
    normals(:,B) = MINUS_K_VEC
    normals(:,T) = K_VEC

  end subroutine cube_populate_outward_normals_to_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each horizontal (W, S, E, N) "outward face".
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine cube_populate_outward_normals_to_horizontal_faces( this, normals )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Outward unit normal vector to each horizontal face
    normals(:,1) = this%outward_normals_to_faces(:,W)
    normals(:,2) = this%outward_normals_to_faces(:,S)
    normals(:,3) = this%outward_normals_to_faces(:,E)
    normals(:,4) = this%outward_normals_to_faces(:,N)

  end subroutine cube_populate_outward_normals_to_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each vertical (B, T) "outward face".
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine cube_populate_outward_normals_to_vertical_faces( this, normals )

    implicit none

    class(reference_cube_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Outward unit normal vector to each vertical face
    normals(:,1) = this%outward_normals_to_faces(:,B)
    normals(:,2) = this%outward_normals_to_faces(:,T)

  end subroutine cube_populate_outward_normals_to_vertical_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reference prism methods.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Constructs a reference prism by extruding a 2D reference triangle.
  !>
  !> @return The new instance.
  !>
  function reference_prism_constructor() result(new_prism)

    implicit none

    type(reference_prism_type) :: new_prism

    call new_prism%reference_element_init( nvertices_2d = 3, &
                                           nedges_2d = 3 )

  end function reference_prism_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destroys a reference prism.
  !>
  subroutine reference_prism_destructor( this )

    implicit none

    type(reference_prism_type), intent(inout) :: this

    call this%reference_element_final()

  end subroutine reference_prism_destructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills entity arrays with the corresponding geometric labels on
  ! a reference prism.
  !
  ! [out] vertex_entities Maps vertex index to geometric entity.
  ! [out] edge_entities   Maps edge index to geometric entity.
  ! [out] face_entities   Maps face index to geometric entity.
  !
  subroutine prism_populate_entity_labels( this, vertex_entities, &
                                           edge_entities, face_entities )

    implicit none

    class(reference_prism_type), intent(in)  :: this
    integer(i_def),              intent(out) :: vertex_entities(:)
    integer(i_def),              intent(out) :: edge_entities(:)
    integer(i_def),              intent(out) :: face_entities(:)

    ! Vertex entities defining the closure of each edge.
    vertex_entities(1) = PRL
    vertex_entities(2) = PQL
    vertex_entities(3) = QRL
    vertex_entities(4) = PRU
    vertex_entities(5) = PQU
    vertex_entities(6) = QRU

    ! Edge entities defining the closure of each face.
    edge_entities(1) = PL
    edge_entities(2) = QL
    edge_entities(3) = RL
    edge_entities(4) = PR
    edge_entities(5) = PQ
    edge_entities(6) = QR
    edge_entities(7) = PU
    edge_entities(8) = QU
    edge_entities(9) = RU

    ! Face entities on the reference prism.
    face_entities(1) = P
    face_entities(2) = Q
    face_entities(3) = R
    face_entities(4) = L
    face_entities(5) = U

  end subroutine prism_populate_entity_labels

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the coordinates of a unit-prism.
  !
  ! [out] vertex  Vertex by 3 array of coordinates.
  !
  subroutine prism_populate_vertices( this, vertex )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: vertex(:,:)

    ! vertex coordinates in unit reference space
    vertex(PRL,:) = (/ 0.0_r_def, 0.0_r_def, 0.0_r_def /)
    vertex(PQL,:) = (/ 1.0_r_def, 0.0_r_def, 0.0_r_def /)
    vertex(QRL,:) = (/ 0.5_r_def, RT3OV2,    0.0_r_def /)
    vertex(PRU,:) = (/ 0.0_r_def, 0.0_r_def, 1.0_r_def /)
    vertex(PQU,:) = (/ 1.0_r_def, 0.0_r_def, 1.0_r_def /)
    vertex(QRU,:) = (/ 0.5_r_def, RT3OV2,    1.0_r_def /)

  end subroutine prism_populate_vertices

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills arrays with mid points of mesh entities on a unit-prism.
  !
  ! [out] edges    Edge by 3 coordinate array.
  ! [out] faces    Face by 3 coordinate array.
  ! [out] volumes  Volume by 3 coordinate array.
  !
  subroutine prism_populate_entity_centres( this, edges, faces, volumes )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def),                 intent(out) :: edges(:,:)
    real(r_def),                 intent(out) :: faces(:,:)
    real(r_def),                 intent(out) :: volumes(:,:)

    edges(QL,:) = (/ 0.75_r_def, RT3OV4,    0.0_r_def /)
    edges(RL,:) = (/ 0.5_r_def,  RT3OV4,    0.0_r_def /)
    edges(PL,:) = (/ 0.5_r_def,  0.0_r_def, 0.0_r_def /)

    edges(QU,:) = (/ 0.75_r_def, RT3OV4,   1.0_r_def /)
    edges(RU,:) = (/ 0.5_r_def,  RT3OV4,   1.0_r_def /)
    edges(PU,:) = (/ 0.5_r_def,  0.0_r_def, 1.0_r_def /)

    edges(PQ,:) = (/ 1.0_r_def, 0.0_r_def, 0.5_r_def /)
    edges(QR,:) = (/ 0.5_r_def, RT3OV2,    0.5_r_def /)
    edges(PR,:) = (/ 0.0_r_def, 0.0_r_def, 0.5_r_def /)

    faces(Q,:) = (/ 0.75_r_def, RT3OV4,    0.5_r_def /)
    faces(R,:) = (/ 0.5_r_def,  RT3OV4,    0.5_r_def /)
    faces(P,:) = (/ 0.5_r_def,  0.0_r_def, 0.5_r_def /)

    faces(U,:) = (/ 0.5_r_def, 0.5_r_def, 1.0_r_def /)
    faces(L,:) = (/ 0.5_r_def, 0.5_r_def, 0.0_r_def /)

    volumes(V,:) = (/ 0.5_r_def, 0.5_r_def, 0.5_r_def /)

  end subroutine prism_populate_entity_centres

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of vertices around a face.
  !
  ! [out] vertex_on_face  Face by n array of vertices.
  !
  subroutine prism_populate_vertices_on_faces( this, vertex_on_face )

    implicit none

    class(reference_prism_type), intent(in) :: this
    integer(i_def), intent(out) :: vertex_on_face(:,:)

    ! Vertices on each face - anticlockwise ordering
    vertex_on_face(P,:) = (/ PRL, PQL, PQU, PRU/)
    vertex_on_face(Q,:) = (/ PQL, QRL, QRU, PQU /)
    vertex_on_face(R,:) = (/ QRL, QRU, PRU, PRL /)
    vertex_on_face(L,:) = (/ PRL, PQL, QRL, 0 /)
    vertex_on_face(U,:) = (/ PRU, PQU, QRU, 0 /)

  end subroutine prism_populate_vertices_on_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of vertices on an edge.
  !
  ! [out] vertex_on_edge  Edge by n array of vertices.
  !
  subroutine prism_populate_vertices_on_edges( this, vertex_on_edge )

    implicit none

    class(reference_prism_type), intent(in) :: this
    integer(i_def), intent(out) :: vertex_on_edge(:,:)

    ! Vertices at the end of each edge
    vertex_on_edge(PL ,:) = (/ PRL, PQL /)
    vertex_on_edge(QL ,:) = (/ PQL, QRL /)
    vertex_on_edge(RL ,:) = (/ QRL, PRL /)
    vertex_on_edge(PR ,:) = (/ PRL, PRU /)
    vertex_on_edge(PQ ,:) = (/ PQL, PQU/)
    vertex_on_edge(QR ,:) = (/ QRL, QRU /)
    vertex_on_edge(PU ,:) = (/ PRU, PQU /)
    vertex_on_edge(QU ,:) = (/ PQU, QRU /)
    vertex_on_edge(RU ,:) = (/ QRU, PRU /)

  end subroutine prism_populate_vertices_on_edges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of edges around a face.
  !
  ! [out] edge_on_face  Face by n array of edges.
  !
  subroutine prism_populate_edge_on_face( this, edge_on_face )

    implicit none

    class(reference_prism_type), intent(in) :: this
    integer(i_def), intent(out) :: edge_on_face(:,:)

    ! Edges on each face
    edge_on_face(P,:) = (/ PL, PQ, PU, PR /)
    edge_on_face(Q,:) = (/ QL, QR, QU, PQ /)
    edge_on_face(R,:) = (/ QR, RU, PR, RL /)
    edge_on_face(L,:) = (/ PL, QL, RL, 0 /)
    edge_on_face(U,:) = (/ PU, QU, RU, 0 /)

  end subroutine prism_populate_edge_on_face

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of edges attached to a vertex.
  !
  ! [out] edge_on_vertex  Vertex by n array of edges.
  !
  subroutine prism_populate_edge_on_vertex( this, edge_on_vertex )

    implicit none

    class(reference_prism_type), intent(in) :: this
    integer(i_def), intent(out) :: edge_on_vertex(:,:)

    ! Edges on each vertex
    edge_on_vertex(PRL,:) = (/ PL, PR, RL /)
    edge_on_vertex(PQL,:) = (/ PL, QL, PQ /)
    edge_on_vertex(QRL,:) = (/ QL, RL, QR /)
    edge_on_vertex(PRU,:) = (/ PR, PU, RU /)
    edge_on_vertex(PQU,:) = (/ PQ, PU, QU /)
    edge_on_vertex(QRU,:) = (/ QR, QU, RU /)

  end subroutine prism_populate_edge_on_vertex

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with the indices of faces on an edge.
  !
  ! [out] edges  Edge by n array of faces.
  !
  subroutine prism_populate_face_on_edge( this, edges )

    implicit none

    class(reference_prism_type), intent(in) :: this
    integer(i_def), intent(out) :: edges(:,:)

    ! Faces either side of each edge
    edges(PL ,:) = (/ P, L /)
    edges(QL ,:) = (/ Q, L /)
    edges(RL ,:) = (/ R, L /)
    edges(PR ,:) = (/ R, P /)
    edges(PQ ,:) = (/ P, Q /)
    edges(QR ,:) = (/ Q, R /)
    edges(PU ,:) = (/ P, U /)
    edges(QU ,:) = (/ Q, U /)
    edges(RU ,:) = (/ R, U /)

  end subroutine prism_populate_face_on_edge

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each face.
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine prism_populate_normals_to_faces( this, normals )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Unit normal vector to each face
    normals(:,P) = MINUS_J_VEC
    normals(:,Q) = Q_NORM_VEC
    normals(:,R) = R_NORM_VEC
    normals(:,L) = MINUS_K_VEC
    normals(:,U) = K_VEC

  end subroutine prism_populate_normals_to_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each horizontal (P, Q, R) face.
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine prism_populate_normals_to_horizontal_faces( this, normals )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Unit normal vector to each horizontal face
    normals(:,1) = this%normals_to_faces(:,P)
    normals(:,2) = this%normals_to_faces(:,Q)
    normals(:,3) = this%normals_to_faces(:,R)

  end subroutine prism_populate_normals_to_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each vertical (L, U) face.
  !
  ! [out] normals 3 by face array of vectors.
  !
  subroutine prism_populate_normals_to_vertical_faces( this, normals )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Unit normal vector to each vertical face
    normals(:,1) = this%normals_to_faces(:,L)
    normals(:,2) = this%normals_to_faces(:,U)

  end subroutine prism_populate_normals_to_vertical_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with tangent vectors to each edge.
  !
  ! [out] tangents  Edge by 3 array of vectors.
  !
  subroutine prism_populate_tangent_to_edge( this, tangents )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: tangents(:,:)

    ! Tangent vectors to each edge
    ! Convention is that vector points from vert_on_edge(i,1) > vert_on_edge(i,2)
    tangents(PL ,:) = I_VEC
    tangents(QL ,:) = Q_TANG_VEC
    tangents(RL ,:) = R_TANG_VEC
    tangents(PR ,:) = K_VEC
    tangents(PQ ,:) = K_VEC
    tangents(QR ,:) = K_VEC
    tangents(PU ,:) = I_VEC
    tangents(QU ,:) = Q_TANG_VEC
    tangents(RU ,:) = R_TANG_VEC

  end subroutine prism_populate_tangent_to_edge

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each "outward face".
  !
  ! [out] normals  3 by face array of vectors.
  !
  subroutine prism_populate_outward_normals_to_faces( this, normals )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Outward unit normal vectors to each face
    normals(:,P) = MINUS_J_VEC
    normals(:,Q) = Q_NORM_VEC
    normals(:,R) = R_NORM_VEC
    normals(:,L) = MINUS_K_VEC
    normals(:,U) = K_VEC

  end subroutine prism_populate_outward_normals_to_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each horizontal (P, Q, R) "outward face".
  !
  ! [out] normals  3 by face array of vectors.
  !
  subroutine prism_populate_outward_normals_to_horizontal_faces( this, normals )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Outward unit normal vectors to each horizontal face
    normals(:,1) = this%outward_normals_to_faces(:,P)
    normals(:,2) = this%outward_normals_to_faces(:,Q)
    normals(:,3) = this%outward_normals_to_faces(:,R)

  end subroutine prism_populate_outward_normals_to_horizontal_faces

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Fills an array with normal vectors to each vertical (L, U) "outward face".
  !
  ! [out] normals  3 by face array of vectors.
  !
  subroutine prism_populate_outward_normals_to_vertical_faces( this, normals )

    implicit none

    class(reference_prism_type), intent(in) :: this
    real(r_def), intent(out) :: normals(:,:)

    ! Outward unit normal vectors to each vertical face
    normals(:,1) = this%outward_normals_to_faces(:,L)
    normals(:,2) = this%outward_normals_to_faces(:,U)

  end subroutine prism_populate_outward_normals_to_vertical_faces

end module reference_element_mod
