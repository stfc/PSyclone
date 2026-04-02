# This Makefile compiles all source files. It ignores the files
# in gol_lib (which are used in the PSyclone implementation later).
# The 'test' target does nothing (since no transformation is applied),
# using 'test_run' will compile and execute the code, and compare the
# output with the expected result.

EXE = gol
API = gocean

# Don't include gol-lib at link time, it is not required
IGNORE_GOL_LIB=yes
include ../common.mk

SRC = gol.f90 read_config_mod.f90 time_step_mod.f90 compute_die_mod.f90 \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90     \
	  output_field_mod.f90
OBJ = $(SRC:%.f90=%.o)

test transform:
	# Nothing to do here, the only possible testing requires compilation
	# (which happens with the test_run target).

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o
