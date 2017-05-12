''' Module containing configuration required to build code generated
for the Dynamo0p3 API '''

import os

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
# Stub infrastructure modules that allow us to compile the generated
# PSy code.  These must be listed in the order in which they must
# be compiled.
INFRASTRUCTURE_PATH = os.path.join(BASE_PATH, "infrastructure")
INFRASTRUCTURE_MODULES = ["constants_mod.f90",
                          "linked_list_data_mod.f90",
                          "partition_mod.f90",
                          "mesh_mod.f90",
                          "stencil_dofmap_mod.f90",
                          "function_space_mod.f90",
                          "field_mod.f90",
                          "quadrature_mod.f90",
                          "operator_mod.f90"]
