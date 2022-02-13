
F90 ?= gfortran
F90FLAGS ?= -Wall -g -Ofast -fopenmp
ROOT_DIR ?= ../../..
INF_DIR ?= $(ROOT_DIR)/external/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a

SRC = gol.f90 read_config_mod.f90 compute_die_mod.f90               \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90 \
	  output_field_mod.f90
PSY_SRC = time_step_alg_mod.x90 

OBJ = $(SRC:%.f90=%.o) $(PSY_SRC:%.x90=%.o) $(PSY_SRC:%.x90=%_psy.o)
EXE = gol
LDFLAGS ?=

.PHONY: clean dl_esm_inf

INCL=-I$(INF_INC)
LIBS=-L$(INF_INC) -l_fd

# Limit output to 132 charactere
PSYCLONE=psyclone --config $(ROOT_DIR)/config/psyclone.cfg \
		-l output -nodm

$(EXE): dl_esm_inf $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LIBS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

# Dependencies:
# -------------
gol.o: read_config_mod.o time_step_alg_mod.o
time_step_alg_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o time_step_alg_mod_psy.o
time_step_alg_mod_psy.o: compute_born_mod.o

# External libs
# -------------
dl_esm_inf:
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)

# Compilation rules
# -----------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $(INCL) $<

.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

%.f90: %.x90 openmp.py Makefile
	$(PSYCLONE) -s ./openmp.py -api gocean1.0 -oalg $*.f90 -opsy $*_psy.f90 $<
	#$(PSYCLONE) -s ./task.py -api gocean1.0 -oalg $*.f90 -opsy $*_psy.f90 $<

# Clean
# -----

clean:
	rm -f $(OBJ) $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90

allclean: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean