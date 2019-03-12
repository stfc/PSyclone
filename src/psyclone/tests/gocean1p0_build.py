
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
# Authors: J. Henrichs, Bureau of Meteorology


''' Module containing configuration required to build code generated
for the Dynamo0p3 API '''

from __future__ import absolute_import, print_function
import os
from psyclone_test_utils import Compile, CompileError


class GOcean1p0Build(Compile):
    '''Build class for compiling test files for the GOcean1.0 api. It
    uses dl_esm_inf as included in the PSyclone distribution.
    '''

    _infrastructure_built = False
    _infrastructure_path = ""

    def __init__(self, tmpdir):
        super(GOcean1p0Build, self).__init__(tmpdir)

        base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "gocean1p0")
        self.base_path = base_path

        # On first instantiation (triggered by conftest.infra_compile)
        # compile the infrastructure library files.
        if Compile.TEST_COMPILE and not GOcean1p0Build._infrastructure_built:
            print("XXX")
            self._build_infrastructure()
            GOcean1p0Build._infrastructure_path = str(tmpdir)

    def get_infrastructure_flags(self):
        '''Returns the required flag to use the infrastructure library dl_esm_inf
        for gocean1p0. Each parameter must be a separate entry
        in the list, e.g.: ["-I", "/some/path"] and not ["-I /some/path"].
        :returns: A list of strings with the compiler flags required.
        :rtpe: list
        '''
        return ["-I", self._infrastructure_path]

    def _build_infrastructure(self):
        '''Compile dl_esm_inf.
        '''

        old_pwd = self._tmpdir.chdir()

        import subprocess
        dl_esm_inf_path = \
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../../../external/dl_esm_inf/finite_difference/src")

        arg_list = ["make", "F90={0}".format(self._f90),
                    "F90FLAGS={0}".format(self._f90flags),
                    "-f", "{0}/Makefile".format(dl_esm_inf_path)]
        try:
            build = subprocess.Popen(arg_list,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT)
            (output, error) = build.communicate()
            GOcean1p0Build._infrastructure_built = True
        except OSError as err:
            print("Failed to run: {0}: ".format(" ".join(arg_list)))
            print("Error was: ", str(err))
            GOcean1p0Build._infrastructure_built = False
            raise CompileError(str(err))
        finally:
            old_pwd.chdir()

        # Check the return code
        stat = build.returncode
        if stat != 0:
            print(output)
            if error:
                print("=========")
                print(error)
            raise CompileError(output)
        else:
            return True


# =============================================================================
class GOcean1p0OpenCLBuild(GOcean1p0Build):
    '''A simple class based on the GOcean1p0 compilation object, that will
    only compile OpenCL code.
    '''

    def code_compiles(self, psy):
        '''Attempts to build the OpenCL Fortran code supplied as an AST of
        f2pygen objects. Returns True for success, False otherwise.
        If no Fortran compiler is available then returns True. All files
        produced are deleted.

        :param psy_ast: The AST of the generated PSy layer
        :type psy_ast: Instance of :py:class:`psyGen.PSy`
        :return: True if generated code compiles, False otherwise
        :rtype: bool
        '''

        if not Compile.TEST_COMPILE_OPENCL:
            return True

        # Call the internal _code_compile function, since it does
        # not
        return super(GOcean1p0OpenCLBuild, self)._code_compiles(psy)
