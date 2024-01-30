# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council
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
# Author: J. Henrichs, Bureau of Meteorology


''' Module containing configuration required to build code generated
for the GOcean1.0 API '''

from __future__ import absolute_import, print_function

import os
import subprocess
import sys

from psyclone.tests.utilities import change_dir, Compile, CompileError


class GOceanBuild(Compile):
    '''Build class for compiling test files for the GOcean1.0 api. It
    uses dl_esm_inf as included in the PSyclone distribution (as a
    git submodule). The very first time the constructor is called it will
    compile the infrastructure library in a temporary, process-specific
    location. These files will be used by all test compilations.

    :param tmpdir: temporary directory, defaults to os.getcwd()
    :type tmpdir: Optional[:py:class:`LocalPath`]

    '''
    # A class variable to make sure we compile the infrastructure
    # file only once per process.
    _infrastructure_built = False

    # The temporary path in which the compiled infrastructure files
    # (.o and .mod) are stored for this process.
    _compilation_path = ""

    # Define the 'make' command to use. Having this as an attribute
    # allows testing to modify this to trigger exceptions.
    _make_command = "make"

    def __init__(self, tmpdir=None):
        super().__init__(tmpdir)

        base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "gocean1p0")
        self.base_path = base_path

        # On first instantiation (triggered by conftest.infra_compile)
        # compile the infrastructure library files.
        if (Compile.TEST_COMPILE or Compile.TEST_COMPILE_OPENCL) and \
                not GOceanBuild._infrastructure_built:
            self._build_infrastructure()

    def get_infrastructure_flags(self):
        '''Returns the required flag to use the infrastructure library
        dl_esm_inf for gocean1p0. Each parameter must be a separate entry
        in the list, e.g.: ["-I", "/some/path"] and not ["-I /some/path"].

        :returns: a list of strings with the compiler flags required.
        :rtype: list

        '''
        return ["-I", self._compilation_path]

    def _build_infrastructure(self):
        '''Compiles dl_esm_inf.
        :raises CompileError: If the compilation of dl_esm_inf fails.
        '''

        with change_dir(self._tmpdir):
            # Store the temporary path so that the compiled infrastructure
            # files can be used by all test compilations later.
            GOceanBuild._compilation_path = str(self._tmpdir)

            dl_esm_inf_path = \
                os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "../../../external/dl_esm_inf/"
                             "finite_difference/src")

            fortcl_path = \
                os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "../../../external/FortCL/src")

            arg_list = [GOceanBuild._make_command, f"F90={self._f90}",
                        f"F90FLAGS={self._f90flags}",
                        "-f", f"{dl_esm_inf_path}/Makefile"]

            arg_list_fortcl = [GOceanBuild._make_command, f"F90={self._f90}",
                               f"F90FLAGS={self._f90flags}",
                               "-f", f"{fortcl_path}/Makefile"]
            try:
                with subprocess.Popen(arg_list,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT) as build:
                    # stderr is redirected to stdout, so ignore stderr
                    (output, _) = build.communicate()
                if Compile.TEST_COMPILE_OPENCL:
                    with subprocess.Popen(arg_list_fortcl,
                                          stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT) as build:
                        # stderr is redirected to stdout, so ignore stderr
                        (output, _) = build.communicate()

            except OSError as err:
                print(f"Failed to run: {' '.join(arg_list)}: ",
                      file=sys.stderr)
                print(f"Error was: {str(err)}", file=sys.stderr)
                GOceanBuild._infrastructure_built = False
                raise CompileError(str(err)) from err

        # Check the return code
        stat = build.returncode
        if stat != 0:
            print(output, file=sys.stderr)
            raise CompileError(output)

        GOceanBuild._infrastructure_built = True


# =============================================================================
class GOceanOpenCLBuild(GOceanBuild):
    '''A simple class based on the GOcean compilation object, that will
    only compile OpenCL code.
    '''

    def code_compiles(self, psy_ast, dependencies=None):
        '''Attempts to build the OpenCL Fortran code supplied as an AST of
        f2pygen objects. Returns True for success, False otherwise.
        If no Fortran compiler is available then returns True. All files
        produced are deleted.

        :param psy_ast: the AST of the generated PSy layer.
        :type psy_ast: instance of :py:class:`psyclone.psyGen.PSy`

        :param dependencies: optional module- or file-names on which \
                    one or more of the kernels/PSy-layer depend (and \
                    that are not part of the GOcean infrastructure, \
                    dl_esm_inf).  These dependencies will be built in \
                    the order they occur in this list.
        :type dependencies: list of str or NoneType

        :return: True if generated code compiles, False otherwise.
        :rtype: bool

        '''
        if not Compile.TEST_COMPILE_OPENCL:
            return True

        # Don't call the base class code_compile() function, since it
        # will only work if --compile was specified. Call the internal
        # function instead that does the actual compilation.
        return super()._code_compiles(psy_ast, dependencies)
