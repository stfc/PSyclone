# Location of the infrastucture code
SHARED_DIR = ${HOME}/PSycloneBench/shared
INF_DIR = ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/dl_esm_inf_fd.a

.PHONY: all kernels clean allclean

all: single.exe

single.exe: kernels psy.o alg_gen.o
	${F90} ${LDFLAGS} inc_field_*mod.o psy.o alg_gen.o ${INF_LIB} -o single.exe

alg_gen.f90 psy.f90: alg.f90
	psyclone -l -api "gocean1.0" -s ./acc_transform.py -opsy psy.f90 -oalg alg_gen.f90 alg.f90

psy.f90: alg_gen.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -I${INF_INC} -c $<

# Gnu Make caches the contents of the current working directory at
# start-up. Therefore wildcards do not pick-up any files generated
# during execution.
kernels: alg_gen.f90
	${MAKE} inc_field_mod.o inc_field_0_mod.o

clean:
	rm -f *.o *.mod *~

# Delete generated Fortran and exe
allclean: clean
	rm -f inc_field_*_mod.f90
	rm -f psy.f90 alg_gen.f90 single.exe
