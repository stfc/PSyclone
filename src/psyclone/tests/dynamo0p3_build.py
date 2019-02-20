# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors: A. Porter and R. Ford, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology

''' Module containing configuration required to build code generated
for the Dynamo0p3 API '''

from __future__ import absolute_import
import os
from psyclone_test_utils import CompileError, Compile


class Dynamo0p3Build(Compile):
    '''Build class for compiling test files for the Dyanmo0.3 api.
    using the wrapper library contained in the test_files.
    '''

    _infrastructure_built = False
    INFRASTRUCTURE_MODULES = ["constants_mod",
                              "linked_list_data_mod",
                              "argument_mod",
                              "kernel_mod",
                              "partition_mod",
                              "mesh_map_mod",
                              "mesh_mod",
                              "stencil_dofmap_mod",
                              "function_space_mod",
                              "field_mod",
                              "abstract_quadrature_mod",
                              "quadrature_rule_mod",
                              "quadrature_xyz_mod",
                              "quadrature_xyoz_mod",
                              "quadrature_xoyoz_mod",
                              "quadrature_mod",
                              "operator_mod"]

    def __init__(self, f90, f90flags, tmpdir):
        super(Dynamo0p3Build, self).__init__(f90, f90flags, tmpdir)

        base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3")
        self.base_path = base_path
        self._infrastructure_path = os.path.join(base_path, "infrastructure")
        if not Dynamo0p3Build._infrastructure_built:
            self._build_infrastructure()

    def get_infrastructure_flags(self):
        '''Returns the required flag to use the infrastructure wrapper
        files for dynamo0p3. Each parameter must be a separate entry
        in the list, e.g.: ["-I", "/some/path"] and not ["-I /some/path"].
        :returns: A list of strings with the compiler flags required.
        :rtpe: list
        '''
        return ["-I", self._infrastructure_path]

    def _build_infrastructure(self):
        '''Compiles the Dynamo0.3 wrapper infrastructure files so that
        compilation tests can be done.
        '''

        old_pwd = os.getcwd()
        os.chdir(self._infrastructure_path)

        try:
            # Compile each infrastructure file
            for fort_file in Dynamo0p3Build.INFRASTRUCTURE_MODULES:
                name = self.find_fortran_file([self._infrastructure_path],
                                              fort_file)

                # Abort if there should be a problem
                if not self.compile_file(name):
                    break
            Dynamo0p3Build._infrastructure_built = True

        except (CompileError, IOError):
            # Failed to compile one of the files
            Dynamo0p3Build._infrastructure_built = False

        finally:
            os.chdir(old_pwd)

        return
