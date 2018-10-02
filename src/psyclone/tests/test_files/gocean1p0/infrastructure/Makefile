# All files for dl_esm_inf that are required to run and compile PSyclone
# created gocean code.
MODULES = argument_mod.o field_mod.o global_parameters_mod.o  \
          gocean_mod.o grid_mod.o  halo_mod.o kernel_mod.o    \
          kind_params_mod.o parallel_common_mod.o             \
          parallel_stub_mod.o region_mod.o subdomain_mod.o    \
          tile_mod.o

API_LIB = libdl_esm_inf.a
F90 = gfortran

all: ${API_LIB}

${API_LIB}: ${MODULES}
	${AR} ${ARFLAGS} ${API_LIB} ${MODULES}

clean:
	rm -f *.o *.mod *.a

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
