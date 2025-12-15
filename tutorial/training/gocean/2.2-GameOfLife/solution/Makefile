EXE = gol
API = gocean

# Don't include gol-lib at link time, it is not required
IGNORE_GOL_LIB=yes

SRC = gol.f90 read_config_mod.f90 time_step_mod.f90 compute_die_mod.f90 \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90     \
	  output_field_mod.f90
OBJ = $(SRC:%.f90=%.o)

include ../../common.mk

test transform:
	# Nothing to do here, testing requires compilation

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o
