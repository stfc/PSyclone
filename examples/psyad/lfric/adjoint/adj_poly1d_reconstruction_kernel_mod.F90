module adj_poly1d_reconstruction_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, any_discontinuous_space_2, &
       any_discontinuous_space_3, arg_type, cell_column, cross, func_type, gh_field, &
       gh_integer, gh_read, gh_real, gh_scalar, gh_write, reference_element_data_type, &
       stencil, gh_readwrite
  use constants_mod, only : i_def, r_tran
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly1d_reconstruction_kernel_type
  type(ARG_TYPE) :: META_ARGS(5) = (/ &
    arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_1, stencil(cross)), &
    arg_type(gh_field, gh_real, gh_readwrite, any_discontinuous_space_2), &
    arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_3, stencil(cross)), &
    arg_type(gh_scalar, gh_integer, gh_read), &
    arg_type(gh_scalar, gh_integer, gh_read)/)
  INTEGER :: OPERATES_ON = cell_column
  CONTAINS
    PROCEDURE, NOPASS :: adj_poly1d_reconstruction_code
  END TYPE adj_poly1d_reconstruction_kernel_type
  
  private

  public :: adj_poly1d_reconstruction_code

  contains
    subroutine adj_poly1d_reconstruction_code( &
         nlayers, reconstruction, stencil_size, stencil_map, tracer, &
         coeff, cstencil_size, cstencil_map, ndata, order, &
         ndf_md, undf_md, map_md, ndf_ws, undf_ws, map_ws, ndf_c, undf_c, map_c)
    integer(kind=i_def), parameter :: nfaces = 4
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_ws
    integer(kind=i_def), intent(in) :: undf_ws
    integer(kind=i_def), dimension(ndf_ws), intent(in) :: map_ws
    integer(kind=i_def), intent(in) :: ndf_md
    integer(kind=i_def), intent(in) :: undf_md
    integer(kind=i_def), dimension(ndf_md), intent(in) :: map_md
    integer(kind=i_def), intent(in) :: ndf_c
    integer(kind=i_def), intent(in) :: undf_c
    integer(kind=i_def), dimension(ndf_c), intent(in) :: map_c
    integer(kind=i_def), intent(in) :: ndata
    integer(kind=i_def), intent(in) :: order
    integer(kind=i_def), intent(in) :: stencil_size, cstencil_size
    real(kind=r_tran), dimension(undf_md), intent(in) :: reconstruction
    real(kind=r_tran), dimension(undf_ws), intent(inout) :: tracer
    real(kind=r_tran), dimension(undf_c), intent(in) :: coeff
    integer(kind=i_def), dimension(ndf_ws,stencil_size), intent(in) :: stencil_map
    integer(kind=i_def), dimension(ndf_c,cstencil_size), intent(in) :: cstencil_map
    integer(kind=i_def) :: f, face, face_mod, stencil_depth
    integer(kind=i_def) :: p
    integer(kind=i_def) :: stencil, depth
    integer(kind=i_def) :: nl
    INTEGER(KIND = i_def), DIMENSION(order + 1, nfaces) :: map1d, adj_map1d

    nl = ndf_ws + nlayers - 2
    stencil_depth = order / 2
    map1d(1,:) = 1
    do face = 1, nfaces, 1
      depth = 1
      face_mod = stencil_depth * MOD(face + 1, 2)
      do stencil = 2, stencil_depth + 1, 1
        map1d(stencil + depth - 1,face) = face_mod + stencil
        map1d(stencil + depth,face) = face_mod + order + stencil
        depth = depth + 1
      enddo
    enddo

    ! The key principle is that if the forward code is
    !   a_i = c_i ^ {i + 1 } b_{i + 1}

    ! then the adjoint code is
    !   b_{i} = c_{i-1} ^ {i-1 + 1} a_{i-1}

    ! In the forward we loop over faces and for face 1 we use c at the
    ! centre-cell for the face +1 (the ijp addition) and b for the
    ! next-cell for face 1.

    ! In the adjoint, we loop over faces, and for face 1 we use c at the
    ! next-cell for face -1 but for the face +1 (the ijp addition), and b
    ! for the next-cell for face -1. So this means that we can loop over
    ! the faces in the same order as the forward but use a different map
    ! (that I've called adj_map1d), and then use the same formula for ijp.
    ! i.e. use almost the same looping as the forward, but the only
    ! difference is that the map is pointing to the cells on the opposite
    ! side of the centre cell.
    adj_map1d(1,:) = map1d(1,:)
    adj_map1d(2,:) = map1d(3,:)
    adj_map1d(3,:) = map1d(2,:)

    do p = 1, order + 1, 1
       do f = 1, nfaces, 1

          if(map1d(p,f) /= 1)then
             ! Off-diagonal term. Indices to 'reconstruction'/'coeff' are swapped with those
             ! for 'tracer' with the change that 'map_md(1)' is taken to mean the 'owned cell'
             ! of the multi-data field and thus corresponds to 'map_ws(1)' for the 'tracer'
             ! field.
             ! Stencil access is now on read-only 'reconstruction' and 'coeff' fields.
            call adj_compute_reconstruction(p, f, order,                                    &
                                            reconstruction, stencil_map(1, adj_map1d(p,f)), &
                                            coeff, cstencil_map(1, adj_map1d(p,f)),         &
                                            tracer, map_ws(1), nl)
         else
            ! Diagonal term (update owned cell only)
            call adj_compute_reconstruction(p, f, order,                                &
                                            reconstruction, stencil_map(1, map1d(p,f)), &
                                            coeff, map_c(1),                            &
                                            ! We want the owned cell and for tracer that is
                                            ! given by map_ws(1)
                                            tracer, map_ws(1), nl)

         end if
      enddo
    enddo

  end subroutine adj_poly1d_reconstruction_code
  subroutine adj_compute_reconstruction(p, f, order, reconstruction, recon_cell,   &
                                        coeff, coeff_cell, tracer, tracer_cell, nl)
    integer, intent(in) :: p
    integer, intent(in) :: f
    integer, intent(in) :: nl
    integer, intent(in) :: order
    integer, intent(in) :: recon_cell
    integer, intent(in) :: coeff_cell
    integer, intent(in) :: tracer_cell
    real(kind=r_tran), dimension(:), intent(in) :: reconstruction
    real(kind=r_tran), dimension(:), intent(inout) :: tracer
    real(kind=r_tran), dimension(:), intent(in) :: coeff
    integer :: k
    integer :: ijp
    integer :: df

    df = f * nl + f - nl + recon_cell - 1
    ijp = coeff_cell + f * order + f - order + p - 2
    do k = nl, 0, -1
       tracer(k + tracer_cell) = tracer(k + tracer_cell) + &
            coeff(ijp) * reconstruction(df + k)
    enddo

  end subroutine adj_compute_reconstruction

end module adj_poly1d_reconstruction_kernel_mod
