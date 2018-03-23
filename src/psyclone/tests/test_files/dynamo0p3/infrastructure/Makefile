# Simple Makefile to generate .xmod files for the LFRic stub
# infrastructure.  These are required by CLAW when analysing LFRic
# kernels.
F90 = xmpf90
F90FLAGS = --stop-frontend

MODULES = constants_mod \
          linked_list_data_mod \
          argument_mod \
          kernel_mod \
          partition_mod \
          mesh_mod \
          stencil_dofmap_mod \
          function_space_mod \
          field_mod \
          abstract_quadrature_mod \
          quadrature_rule_mod \
          quadrature_xyz_mod \
          quadrature_xyoz_mod \
          quadrature_xoyoz_mod \
          quadrature_mod \
          operator_mod

%.xmod: %.[fF]90
	${F90} ${F90FLAGS} $<

all: ${MODULES:_mod=_mod.xmod}

clean:
	rm *.xmod
