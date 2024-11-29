##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
# Various things specific to the NVIDIA Fortran compiler including flags to
# build with OpenACC offload and OpenMP multicore enabled.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
F_MOD_DESTINATION_ARG = -module$(SPACE)

FFLAGS_COMPILER           =
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2
FFLAGS_RISKY_OPTIMISATION = -O4
FFLAGS_DEBUG              = -g -traceback
FFLAGS_RUNTIME            = -Mchkptr -Mchkstk
# Option for checking code meets Fortran standard (not available for nvfortran)
FFLAGS_FORTRAN_STANDARD   =

# Flags for OpenMP threading / OpenMP offloading / OpenACC Offloading
# The LFRIC_OFFLOAD_DIRECTIVES env_variable is also queried in the PSyclone
# script to generate matching directives
ifeq ("$(LFRIC_OFFLOAD_DIRECTIVES)", "omp")
	OPENMP_ARG = -mp=gpu -gpu=mem:managed
	LDFLAGS_COMPILER = -mp=gpu -gpu=mem:managed -cuda
else ifeq ("$(LFRIC_OFFLOAD_DIRECTIVES)", "acc")
	OPENMP_ARG = -acc=gpu -gpu=mem:managed -mp=multicore
	LDFLAGS_COMPILER = -acc=gpu -gpu=mem:managed -mp=multicore -cuda
else
	OPENMP_ARG = -mp
	LDFLAGS_COMPILER = -mp
endif

FPP = cpp -traditional-cpp
FPPFLAGS = -P
FC = mpif90

# nvbug 4648082
science/src/um/src/atmosphere/large_scale_precipitation/ls_ppnc.o: private FFLAGS_RUNTIME = -Mchkstk

# FS#35751
mesh/create_mesh_mod.o: private FFLAGS_RUNTIME = -Mchkstk

# 24.3
science/src/socrates/src/cosp_github/subsample_and_optics_example/optics/quickbeam_optics/optics_lib.o: private FFLAGS_SAFE_OPTIMISATION = -O1
science/src/socrates/src/cosp_github/subsample_and_optics_example/optics/quickbeam_optics/optics_lib.o: private FFLAGS_RISKY_OPTIMISATION = -O1
