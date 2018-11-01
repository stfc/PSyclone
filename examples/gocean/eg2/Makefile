# Location of the infrastucture code
SHARED_DIR = ${HOME}/PSycloneBench/shared
INF_DIR = ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/dl_esm_inf_fd.a

all: single.exe

single.exe: inc_field_mod.o psy.o alg_gen.o
	${F90} ${LDFLAGS} inc_field_mod.o psy.o alg_gen.o ${INF_LIB} -o single.exe

alg_gen.f90: alg.f90
	psyclone -l -api "gocean1.0" -opsy psy.f90 -oalg alg_gen.f90 alg.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -I${INF_INC} -c $<

clean:
	rm -f *.o *.mod
