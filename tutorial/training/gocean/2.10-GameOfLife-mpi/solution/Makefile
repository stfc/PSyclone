EXE = gol
API = gocean
MPI?= no
F90 = mpif90

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

include ../../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	mpirun -np 4 ./$(EXE) ../../gol-lib/config.glider

# By default, MPI is disabled (to pass github CI). It needs to
# be enabled using the environment variable `MPI=yes`.

ifeq ($(MPI), yes)
# ****************

# Call the default test (which checks the output file), but do a second test to make
# sure we get a 2x2 domain decomposition, otherwise the job was not running properly
# with MPI
TS=time_step_alg_mod
test: test-default
	bash -c "if [[ $$(make run | grep -c 'go_decompose: using grid of   2x  2') != 1 ]]; then \
		echo 'Job is not running properly with MPI'; \
	fi"

	$(PSYCLONE) -s ./openmp.py -oalg $(TS).f90 -opsy $(TS)_psy.f90 $(TS).x90
	$(PSYCLONE) -s ./openmp_combined.py -oalg $(TS).f90 -opsy $(TS)_psy.f90 $(TS).x90

else
# **

# We can't simply use the default test, since it will require mpif90 for
# compilation. If compilation is to be tested, `MPI=yes make test`
TS=time_step_alg_mod
test:
	$(PSYCLONE) -s ./openmp.py -oalg $(TS).f90 -opsy $(TS)_psy.f90 $(TS).x90

endif
# ***

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90 Makefile
	$(PSYCLONE) -s ./openmp.py -oalg $*.f90 -opsy $*_psy.f90 $<

