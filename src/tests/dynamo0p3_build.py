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
INFRASTRUCTURE_MODULES = ["constants_mod",
                          "linked_list_data_mod",
                          "argument_mod",
                          "kernel_mod",
                          "partition_mod",
                          "mesh_mod",
                          "stencil_dofmap_mod",
                          "function_space_mod",
                          "field_mod",
                          "quadrature_mod",
                          "operator_mod"]
