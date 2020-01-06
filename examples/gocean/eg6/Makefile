F90 ?= gfortran
F90FLAGS ?= -g -O0

DL_DIR = ../../../external/dl_esm_inf/finite_difference/src
EXTRACT_DIR = ../../../lib/extract/netcdf
NETCDF_DIR = /opt/local/
F90FLAGS += -I$(NETCDF_DIR)/include -I$(DL_DIR)
LIB_NAME = lib_kernel_data_netcdf.a


KERNELS = init_field_mod.o update_field_mod.o
EXEC = extract_test
DRIVER_INIT = driver-init_field_mod-init_field_code

all:	$(EXEC) $(DRIVER_INIT)

test:	$(DRIVER_INIT)

$(DRIVER_INIT):	$(KERNELS) $(DRIVER_INIT).o main.o init_field_mod.o $(EXTRACT_DIR)/lib_kernel_data_netcdf.a
	$(F90) $^ -o $(DRIVER_INIT)            \
	-L$(EXTRACT_DIR) -l_kernel_data_netcdf \
	-L /opt/local/lib64/ -lnetcdff -lnetcdf


$(EXEC):	$(KERNELS) alg.o psy.o $(EXTRACT_DIR)/lib_kernel_data_netcdf.a
	$(F90) $^ -o $(EXEC)                   \
	-L$(EXTRACT_DIR) -l_kernel_data_netcdf \
	-L $(DL_DIR) -l_fd                     \
	-L /opt/local/lib64/ -lnetcdff -lnetcdf
	

alg.f90 psy.f90: test.x90 extract_transform.py
	psyclone -nodm -l -api "gocean1.0" -s ./extract_transform.py\
			 --config ../../../config/psyclone.cfg \
		     -opsy psy.f90 -oalg alg.f90 test.x90

alg.o:	psy.o 

psy.o:	$(KERNELS)

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(DL_DIR) -I $(EXTRACT_DIR) -c $<

clean:
	rm -f *.o *.mod alg.f90 psy.f90 $(EXEC) $(DRiVER_INIT)