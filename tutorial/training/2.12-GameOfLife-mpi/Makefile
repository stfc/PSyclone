EXE = gol
API = gocean1.0
MPI = yes
F90 = mpif90

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

include ../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	mpirun -np 4 ./$(EXE) ../gol-lib/config.glider

# Call the default test (which checks the output file), but do a second test to make
# sure we get a 2x2 domain decomposition, otherwise the job was not running properly
# with MPI
test: test-default
	@bash -c "if [[ $$(make run | grep -c 'go_decompose: using grid of   2x  2') != 1 ]]; then \
		echo 'Job is not running properly with MPI'; \
	fi"


# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90
	$(PSYCLONE) -oalg $*.f90 -opsy $*_psy.f90 $<
