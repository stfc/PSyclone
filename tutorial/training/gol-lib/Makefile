API = gocean1.0
default: libgol.a

include ../common.mk

SRC = gol.f90 read_config_mod.f90 compute_die_mod.f90               \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90 \
	  output_field_mod.f90 time_step_alg_mod.f90 time_step_alg_mod_psy.f90

OBJ = $(SRC:%.f90=%.o)

libgol.a: $(INF_LIB) $(OBJ)
	$(AR) $(ARFLAGS) libgol.a $(OBJ)
	# Remove the alg/psy layer files. They are required for compiling
	# the libraries, but the main examples will provide their own
	# version which must be used instead.
	$(AR) d libgol.a time_step_alg_mod.o time_step_alg_mod_psy.o

gol.o: read_config_mod.o time_step_alg_mod.o
time_step_alg_mod.o:time_step_alg_mod_psy.o output_field_mod.o
time_step_alg_mod_psy.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o

clean:
	# We can't call the clean target in common.mk, since this would delete
	# time_step_alg_mod*, which we need to compile gol.f
	rm -f *.o *.mod libgol.a
