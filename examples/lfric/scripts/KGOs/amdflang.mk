##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE which you
# should have received as part of this distribution.
##############################################################################
# Various things specific to the GNU Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
$(info ** Chosen AMD Flang

F_MOD_DESTINATION_ARG     = -J
F_MOD_SOURCE_ARG          = -I

FFLAGS_OPENMP  = -fopenmp
LDFLAGS_OPENMP = -fopenmp

FFLAGS_COMPILER           = 
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2
FFLAGS_RISKY_OPTIMISATION = -Ofast
FFLAGS_DEBUG              = -g
FFLAGS_WARNINGS           = 
FFLAGS_UNIT_WARNINGS      = 
FFLAGS_INIT               =
FFLAGS_RUNTIME            =
# fast-debug flags set separately as Intel compiler needs platform-specific control on them
FFLAGS_FASTD_INIT         = $(FFLAGS_INIT)
FFLAGS_FASTD_RUNTIME      = $(FFLAGS_RUNTIME)

# Option for checking code meets Fortran standard (flang only supports 2018)
FFLAGS_FORTRAN_STANDARD   = -std=f2018

LDFLAGS_COMPILER =

FPPFLAGS = -P
