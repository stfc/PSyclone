F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = main_psy.o main_alg.o testkern_w0_mod.o

EXEC = example
LFRIC_PATH = ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

F90FLAGS += -I$(LFRIC_PATH)

default: $(EXEC)

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

# Dependencies
main_psy.o:	 testkern_w0_mod.o
main_alg.o:	main_psy.o

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90

%_psy.f90:	%.x90
	psyclone --config ../../../config/psyclone.cfg -opsy $*_psy.f90 -oalg $*_alg.f90 $<

clean:
	rm -f *.o *.mod  $(EXEC) main_alg.f90 main_psy.f90
