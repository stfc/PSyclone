
F90 ?= gfortran
F90FLAGS ?= -Wall -g -O0
ROOT_DIR := $(abspath $(dir $(this_file))../../..)
SHARED_DIR ?= $(ROOT_DIR)/external
INF_DIR ?= $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a

SRC = gol.f90 read_config_mod.f90 time_step_mod.f90 compute_die_mod.f90 \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90     \
	  output_field_mod.f90
OBJ = $(SRC:%.f90=%.o)
EXE = gol
LDFLAGS ?=

.PHONY: clean dl_esm_inf

INCL=-I$(INF_INC)
LIBS=-L$(INF_INC) -l_fd

$(EXE): dl_esm_inf $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LIBS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

# Dependencies:
# -------------
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o

# External libs
# -------------
dl_esm_inf:
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)

# Compilation rules
# -----------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $(INCL) $<

# Clean
# -----

clean:
	rm -f $(OBJ) $(EXE) *.mod

allclean: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean