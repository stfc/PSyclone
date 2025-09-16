# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council
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

import os
from pathlib import Path
import subprocess
import sys
from typing import Optional

from psyclone.psyGen import PSy
from psyclone.tests.utilities import (change_dir, Compile, CompileError,
                                      get_base_path, get_infrastructure_path)


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
    _infrastructure_built: Optional[bool] = None

    # The temporary path in which the compiled infrastructure files
    # (.o and .mod) are stored for this process.
    _compilation_path = ""

    # Define the 'make' command to use. Having this as an attribute
    # allows testing to modify this to trigger exceptions.
    _make_command = "make"

    # The path to the infrastructure source files.
    _infrastructure_path: Path

    def __init__(self, tmpdir=None) -> None:
        super().__init__(tmpdir)

        self.base_path = get_base_path("gocean")
        GOceanBuild._infrastructure_path = \
            Path(get_infrastructure_path("gocean"))

        # On first instantiation (triggered by conftest.infra_compile)
        # compile the infrastructure library files.
        if GOceanBuild._infrastructure_built is None:
            GOceanBuild._infrastructure_built = False
        if (Compile.TEST_COMPILE or Compile.TEST_COMPILE_OPENCL) and \
                not GOceanBuild._infrastructure_built:
            self._build_infrastructure()

    def get_infrastructure_flags(self) -> list[str]:
        '''Returns the required flag to use the infrastructure library
        dl_esm_inf for gocean1p0. Each parameter must be a separate entry
        in the list, e.g.: ["-I", "/some/path"] and not ["-I /some/path"].

        :returns: a list of strings with the compiler flags required.

        '''
        if Compile.TEST_COMPILE:
            # If we are compiling, point to the compilation path, which
            # contain the compiled mod files.
            root = GOceanBuild._compilation_path
        else:
            # If we are not compiling, point to the external infrastructure
            # directory, which allows tests (that uses the flags) to pass
            # even when compilation is disabled (and it will pick up if
            # the infrastructure should change as well).
            root = str(self._infrastructure_path)

        return ["-I", str(root)]

    def _build_infrastructure(self) -> None:
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

    def code_compiles(self, psy_ast: PSy,
                      dependencies: Optional[list[str]] = None) -> bool:
        '''
        Use the given GOcean PSy class to generate the necessary PSyKAl
        components to compile the OpenCL version of the psy-layer. Returns True
        for success, False otherwise.
        If no Fortran compiler is available then returns True. All files
        produced are deleted.

        :param psy_ast: the AST of the generated PSy layer.

        :param dependencies: optional module- or file-names on which one or
            more of the kernels/PSy-layer depend (and that are not part of the
            GOcean infrastructure, dl_esm_inf).  These dependencies will be
            built in the order they occur in this list.

        :return: True if generated code compiles, False otherwise.

        '''
        if not Compile.TEST_COMPILE_OPENCL:
            return True

        # Don't call the base class code_compile() function, since it
        # will only work if --compile was specified. Call the internal
        # function instead that does the actual compilation.
        return super()._code_compiles(psy_ast, dependencies)
