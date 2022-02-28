EXE = gol
API = gocean

include ../common.mk

PSY_DATA_DIR = $(ROOT_DIR)/lib/profiling/simple_timing
PSY_DATA_LIB = $(PSY_DATA_DIR)/libsimple_timing.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

F90FLAGS += $(PSY_DATA_INCLUDE)

SRC = gol.f90
PSY_SRC = time_step_alg_mod.x90 

OBJ = $(SRC:%.f90=%.o) $(PSY_SRC:%.x90=%.o) $(PSY_SRC:%.x90=%_psy.o)
EXE = gol

.PHONY: run test clean

LDFLAGS += $(PSY_DATA_LIB)

$(EXE): $(INF_LIB) $(PSY_DATA_LIB) $(GOL_LIB) $(OBJ)
	echo OBJ $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	./$(EXE) config.glider | tail -n 18  | head -n 12| diff - ./glider.correct

# Dependencies:
# -------------
gol.o: time_step_alg_mod.o

time_step_alg_mod.o:time_step_alg_mod_psy.o

# External libs
# -------------
$(PSY_DATA_LIB):
	$(MAKE) -C $(PSY_DATA_DIR)

.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

%.f90: %.x90 Makefile
	$(PSYCLONE) --profile invokes -api gocean1.0 -oalg $*.f90 \
		-opsy $*_psy.f90 $<

# Clean
# -----

clean:
	rm -f $(OBJ) $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90
