F90=ifort
FFLAGS=-g
all: types.o kern.o psy.o alg.o top.o
	${F90} -o top ${FFLAGS} *.o
parse:
	../psygen.py alg.f90 > psy.f90
top.o : top.f90
	${F90} -c ${FFLAGS} top.f90
alg.o : alg.f90
	${F90} -c ${FFLAGS} alg.f90
psy.o :
	make parse
	${F90} -c ${FFLAGS} psy.f90
kern.o : kern.f90
	${F90} -c ${FFLAGS} kern.f90
types.o : types.f90
	${F90} -c ${FFLAGS} types.f90
inf.o : inf.f90
	${F90} -c ${FFLAGS} inf.f90
inf_checks.o : inf_checks.f90
	${F90} -c ${FFLAGS} inf_checks.f90
clean:
	rm -f *.o *.mod top fparser.log
cleanall:
	make clean
	rm -f psy.f90
