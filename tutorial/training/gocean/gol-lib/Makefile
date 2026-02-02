API = gocean
default: libgol.a

# This flag is required to disable some rules in common.mk
# that would otherwise cause a recursion
BUILDING_GOL_LIB = 1	

include ../common.mk

# We add a dummy time_step_alg_mod file. This file will be created
# in the various hands-on exercises, but is required to compile
# gol.f90. The dummy file only declares the subroutine so it can be called.
# It has a different name to avoid triggering PSyclone (in common.mk).
SRC = gol.f90 read_config_mod.f90 compute_die_mod.f90               \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90 \
	  output_field_mod.f90 time_step_alg_dummy_mod.f90

OBJ = $(SRC:%.f90=%.o)

libgol.a: $(INF_LIB) $(OBJ)
	$(AR) $(ARFLAGS) libgol.a $(OBJ)

gol.o: read_config_mod.o time_step_alg_dummy_mod.o

clean:
	@# We cannot call the clean target in common.mk, since this would delete
	@# time_step_alg_mod*, which we need to compile gol.f
	rm -f *.o *.mod libgol.a
