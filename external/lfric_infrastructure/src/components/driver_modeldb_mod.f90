!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Container for everything that makes up the model state
!!
!> @details This module holds all the information required to run an instance
!>          of the model (such as an ensemble member or adjoint etc.). That
!>          includes all the scientific and technical state. Nothing
!>          should be held in global space, unless it is truely global
!!
module driver_modeldb_mod

  use calendar_mod,              only: calendar_type
  use driver_model_data_mod,     only: model_data_type
  use key_value_collection_mod,  only: key_value_collection_type
  use lfric_mpi_mod,             only: lfric_mpi_type
  use model_clock_mod,           only: model_clock_type
  use namelist_collection_mod,   only: namelist_collection_type
  use io_context_collection_mod, only: io_context_collection_type

  implicit none

  private

  !> Holds the technical and scientific model state for a model run
  !>
  type :: modeldb_type

    private

    !> Configuration namelist collection
    type(namelist_collection_type), public :: configuration

    !> Stores all the fields used by the model
    type( model_data_type ), public :: fields

    !> Stores all non-field data used by the model.
    !>
    type(key_value_collection_type), public :: values

    !> Stores IO contexts used by the model
    !>
    type(io_context_collection_type), public :: io_contexts

    !> Tracks time in the model
    type(model_clock_type), public, allocatable :: clock
    class(calendar_type),   public, allocatable :: calendar

    !> MPI object that contains all the functionality to perform MPI tasks
    !> on the MPI communicator for this model instance.
    !> @todo  Currently, this is juat a pointer to the global MPI object, as
    !>        this is the only MPI object that PSyclone can use at the moment
    !>        When PSyclone is updated, this can be the actual MPI object.
    type(lfric_mpi_type), pointer, public :: mpi

    contains

  end type modeldb_type

  public modeldb_type

contains

end module driver_modeldb_mod
