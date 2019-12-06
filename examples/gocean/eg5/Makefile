F90 ?= gfortran
F90FLAGS ?=

DL_DIR = ../../../external/dl_esm_inf/finite_difference/src
PROFILE_DIR = ../../../lib/profiling/template
KERNELS = init_field_mod.o update_field_mod.o
EXEC = profile_test
all:	$(EXEC)

$(EXEC):	$(KERNELS) alg.o psy.o
	$(F90) $^ -o $(EXEC) -L$(PROFILE_DIR) -ldummy -L $(DL_DIR) -l_fd

alg.f90 psy.f90: test.x90
	psyclone --profile invokes -l -api "gocean1.0" \
			 --config ../../../config/psyclone.cfg \
		     -opsy psy.f90 -oalg alg.f90 test.x90

alg.o:	psy.o 

psy.o:	$(KERNELS)

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(DL_DIR) -I $(PROFILE_DIR) -c $<

clean:
	rm -f *.o *.mod alg.f90 psy.f90 $(EXEC)