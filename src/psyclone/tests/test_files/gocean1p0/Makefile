

# Test files to see if the source files work with dl_esm_info:
TEST_FILES = continuity_mod.f90 model_mod.f90 momentum_mod.f90     \
             boundary_conditions_ne_offset_mod.f90                 \
             kernel_no_fld_args.f90 kernel_requires_grid_props.f90 \
             kernel_scalar_float.f90 kernel_scalar_int.f90

TEST_OBJ = $(patsubst %.f90, %.o, ${TEST_FILES})

F90 = gfortran

.PHONY: tests default

default:
	make -C infrastructure
	make tests

tests: $(TEST_OBJ)

clean:
	rm -f *.o *.mod

allclean:
	make -C infrastructure clean
	make clean

%.o: %.f90
	$(F90) $(F90FLAGS) -I infrastructure -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I infrastructure -c $<
