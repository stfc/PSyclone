# All files for dl_esm_inf that are required to run and compile PSyclone
# created gocean code.
MODULES = argument_mod.o field_mod.o global_parameters_mod.o  \
          gocean_mod.o grid_mod.o  halo_mod.o kernel_mod.o    \
          kind_params_mod.o parallel_common_mod.o             \
          parallel_stub_mod.o region_mod.o subdomain_mod.o    \
          tile_mod.o

# Test files to see if the source files work with dl_esm_info:
TEST_FILES = continuity_mod.f90 model_mod.f90 momentum_mod.f90     \
             boundary_conditions_ne_offset_mod.f90                 \
             kernel_no_fld_args.f90 kernel_requires_grid_props.f90 \
             kernel_scalar_float.f90 kernel_scalar_int.f90

API_LIB = libdl_esm_inf.a
F90 = gfortran

all: ${API_LIB}

${API_LIB}: ${MODULES}
	${AR} ${ARFLAGS} ${API_LIB} ${MODULES}

clean:
	rm -f *.o *.mod *.a

# Testing other source files:
# ---------------------------
TEST_OBJ = $(patsubst %.f90, %.o, ${TEST_FILES})

.PHONY: test

test: $(TEST_OBJ)

clean-test:
	rm  ${TEST_OBJ}

# Inter-module dependencies, alphabetical order
argument_mod.o: global_parameters_mod.o

field_mod.o: grid_mod.o halo_mod.o kind_params_mod.o \
             region_mod.o gocean_mod.o subdomain_mod.o tile_mod.o

gocean_mod.o: kind_params_mod.o parallel_stub_mod.o

grid_mod.o: global_parameters_mod.o gocean_mod.o kind_params_mod.o \
            parallel_stub_mod.o region_mod.o subdomain_mod.o

halo_mod.o: region_mod.o

kernel_mod.o: argument_mod.o global_parameters_mod.o

parallel_stub_mod.o: parallel_common_mod.o

subdomain_mod.o: parallel_stub_mod.o region_mod.o

tile_mod.o: region_mod.o


%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<
