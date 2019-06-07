
# NOTE: It requieres dl_esm_inf/opencl_fix branch installed.
INF_DIR = ${HOME}/workspace/euroexa/PSycloneBench/shared/dl_esm_inf
INF_INC = ${INF_DIR}/finite_difference/src
INF_LIB = ${INF_DIR}/finite_difference/src/dl_esm_inf_fd.a
FORTCL_INC = ${INF_DIR}/external/FortCL/src
FORTCL_LIB = ${INF_DIR}/external/FortCL/src/libFortCL.a

FFLAGS = -O2 -ffree-line-length-none

baseline:
	psyclone -oalg psyalgbaseline.f90 -opsy psylayerbaseline.f90 \
		-api "gocean1.0" alg.f90
	sed -i s/subdomain%internal/simulation_domain/ psylayerbaseline.f90
	gfortran -I${INF_INC} compute_h_mod.f90 ${FFLAGS} -c
	gfortran -I${INF_INC} compute_z_mod.f90 ${FFLAGS} -c
	gfortran -I${INF_INC} compute_cu_mod.f90 ${FFLAGS} -c
	gfortran -I${INF_INC} compute_cv_mod.f90 ${FFLAGS} -c
	gfortran -I${INF_INC} psylayerbaseline.f90 ${FFLAGS} -c
	gfortran  -I${INF_INC} compute_h_mod.o compute_z_mod.o compute_cu_mod.o \
		compute_cv_mod.o psylayerbaseline.o psyalgbaseline.f90 ${INF_LIB} \
		-o baseline.exe

opencl:
	psyclone -oalg psyalgocl.f90 -opsy psylayerocl.f90 \
		-api "gocean1.0" -s ./ocl_trans.py alg.f90
	sed -i s/subdomain%internal/simulation_domain/ psylayerocl.f90
	ioc64 -cmd=build -device=cpu -input=compute_h_0.cl -spirv64=compute_h.spirv \
		-bo="-cl-std=CL1.2"
	ioc64 -cmd=build -device=cpu -input=compute_z_0.cl -spirv64=compute_z.spirv \
		-bo="-cl-std=CL1.2"
	ioc64 -cmd=build -device=cpu -input=compute_cu_0.cl -spirv64=compute_cu.spirv \
		-bo="-cl-std=CL1.2"
	ioc64 -cmd=build -device=cpu -input=compute_cv_0.cl -spirv64=compute_cv.spirv \
		-bo="-cl-std=CL1.2"
	gfortran -I${INF_INC} -I${FORTCL_INC} psylayerocl.f90 ${FFLAGS} -c
	gfortran  -I${INF_INC} -I psylayerocl.o psyalgocl.f90 ${INF_LIB} -o opencl.exe

clean:
	rm -f *.cl *.o *.spirv *.mod opencl.exe baseline.exe
	rm psyalgbaseline.f90 psylayerbaseline.f90 psyalgocl.f90 psylayerocl.f90

