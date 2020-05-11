# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and R. W. Ford, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic, Met Office

''' Module containing configuration required to build code generated
for the LFRic domain. '''

from __future__ import absolute_import
from psyclone.tests.utilities import CompileError, Compile


class LFRicBuild(Compile):
    '''Build class for compilation of test files for the LFRic api.
    It uses the wrapper library from test_files/dynamo0p3/infrastructure
    and will automatically compile those files once per process.
    '''

    # A class variable to make sure we compile the infrastructure
    # file only once per process.
    _infrastructure_built = False

    # The temporary path in which the compiled infrastructure files
    # (.o and .mod) are stored for this process.
    _compilation_path = ""

    # The list of infrastructure files that must be compiled. The
    # order can be important, they will be compiled in the order
    # specified here.
    INFRASTRUCTURE_MODULES = ["constants_mod",
                              "fs_continuity_mod",
                              "linked_list_data_mod",
                              "argument_mod",
                              "kernel_mod",
                              "partition_mod",
                              "reference_element_mod",
                              "mesh_map_mod",
                              "mesh_mod",
                              "stencil_dofmap_mod",
                              "function_space_mod",
                              "field_mod",
                              "quadrature_mod",
                              "quadrature_xyz_mod",
                              "quadrature_xyoz_mod",
                              "quadrature_xoyoz_mod",
                              "quadrature_edge_mod",
                              "quadrature_face_mod",
                              "operator_mod",
                              "flux_direction_mod",
                              "log_mod"]

    def __init__(self, tmpdir):
        '''Constructor for the LFRic-specific compilation class.
        The very first time the constructor is called it will compile
        the infrastructure library in a temporary, process-specific
        location. These files will be used by all test compilations.
        :param tmpdir: Temporary directory to be used for output files.
        :type tmpdir: :py:class:`LocalPath`
        '''
        super(LFRicBuild, self).__init__(tmpdir)

        import os
        base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3")
        self.base_path = base_path
        self._infrastructure_path = os.path.join(base_path, "infrastructure")
        # On first instantiation (triggered by conftest.infra_compile)
        # compile the infrastructure library files.
        if not LFRicBuild._infrastructure_built:
            self._build_infrastructure()

    def get_infrastructure_flags(self):
        '''Returns the required flag to use the infrastructure wrapper
        files for dynamo0p3. Each parameter must be a separate entry
        in the list, e.g.: ["-I", "/some/path"] and not ["-I /some/path"].
        :returns: A list of strings with the compiler flags required.
        :rtpe: list
        '''
        return ["-I", LFRicBuild._compilation_path]

    def _build_infrastructure(self):
        '''Compiles the LFRic wrapper infrastructure files so that
        compilation tests can be done.
        '''
        old_pwd = self._tmpdir.chdir()
        # Store the temporary path so that the compiled infrastructure
        # files can be used by all test compilations later.
        LFRicBuild._compilation_path = str(self._tmpdir)

        try:
            # Compile each infrastructure file
            for fort_file in LFRicBuild.INFRASTRUCTURE_MODULES:
                name = self.find_fortran_file([self._infrastructure_path],
                                              fort_file)
                self.compile_file(name)
            LFRicBuild._infrastructure_built = True

        except (CompileError, IOError) as err:
            # Failed to compile one of the files
            LFRicBuild._infrastructure_built = False
            raise CompileError("Could not compile LFRic wrapper. "
                               "Error: {0}".format(str(err)))

        finally:
            old_pwd.chdir()
