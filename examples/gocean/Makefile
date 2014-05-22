# Makefile.include is a symbolic link to the desired
# Makefile.include.<compiler> file.
include Makefile.include

# add -mpi=mpilog to generate mpe logging
LDFLAGS += -lnetcdff -lnetcdf

#EXECS = join_files shallow_base shallow_parallel
EXECS = shallow_base

MODULES = kind_params.o physical_params.o field.o model.o shallow_IO.o \
          initial_conditions.o compute_cu_mod.o compute_cv_mod.o \
          compute_z.o compute_h.o manual_invoke_compute_new_fields_mod.o \
          compute_unew.o compute_vnew.o compute_pnew.o \
          manual_invoke_apply_bcs_mod.o \
          apply_bcs_cf_mod.o apply_bcs_ct_mod.o \
          apply_bcs_cu_mod.o apply_bcs_cv_mod.o \
          timing.o time_smooth.o intel_timer_mod.o mesh.o          

# My local copies of GungHo modules. Avoids attempting to build
# the GungHo code which stretches some compilers...
GUNGHO_MODULES = global_parameters.o argument.o kernel.o

all: $(EXECS)

shallow_base.o: $(MODULES)

timing.o: intel_timer_mod.o

model.o: field.o timing.o shallow_IO.o mesh.o time_smooth.o

time_smooth.o: $(GUNGHO_MODULES)

initial_conditions.o: model.o

manual_invoke_compute_new_fields_mod.o: compute_unew.o compute_vnew.o compute_pnew.o
manual_invoke_apply_bcs_mod.o: apply_bcs_cf_mod.o apply_bcs_ct_mod.o \
          apply_bcs_cu_mod.o apply_bcs_cv_mod.o

%: %.o 
	$(F90) -o $@ $< $(GUNGHO_MODULES) $(MODULES) $(LDFLAGS)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

clean: 
	rm -f *.o *.mod *.MOD $(EXECS) *.f90~

docs:
	doxygen shallow.doxy.config
