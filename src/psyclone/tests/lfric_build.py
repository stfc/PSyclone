# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
#          J. Henrichs, Bureau of Meteorology,
#          I. Kavcic, Met Office

''' Module containing configuration required to build code generated
for the LFRic domain. '''

import os
import subprocess
import sys


from psyclone.tests.utilities import change_dir, CompileError, Compile


class LFRicBuild(Compile):
    '''Build class for compilation of test files for the LFRic api.
    It uses the wrapper library from test_files/dynamo0p3/infrastructure.
    The very first time the constructor is called it will automatically
    compile the infrastructure library in a temporary, process-specific
    location. These files will be used by all test compilations of this
    process.

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

    def __init__(self, tmpdir):
        super().__init__(tmpdir)

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

        :returns: the required compiler flags.
        :rtype: List[str]

        '''
        all_flags = []
        for entry in os.scandir(self._infrastructure_path):
            if not entry.name.startswith('.') and entry.is_dir():
                path = os.path.join(LFRicBuild._compilation_path, entry.name)
                all_flags.extend(["-I", path])
        return all_flags

    def _build_infrastructure(self):
        '''Compiles the LFRic wrapper infrastructure files so that
        compilation tests can be done.
        '''
        if not Compile.TEST_COMPILE:
            return

        with change_dir(self._tmpdir):
            # Store the temporary path so that the compiled infrastructure
            # files can be used by all test compilations later.
            LFRicBuild._compilation_path = str(self._tmpdir)
            makefile = os.path.join(self._infrastructure_path, "Makefile")
            arg_list = [LFRicBuild._make_command, f"F90={self._f90}",
                        f"F90FLAGS={self._f90flags}", "-f", makefile]
            try:
                with subprocess.Popen(arg_list, stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT) as build:
                    # stderr = stdout, so ignore stderr result:
                    (output, _) = build.communicate()
            except OSError as err:
                print(f"Failed to run: {' '.join(arg_list)}: ",
                      file=sys.stderr)
                raise CompileError(str(err)) from err

        # Check the return code
        stat = build.returncode
        if stat != 0:
            print(f"Build command: {' '.join(arg_list)}", file=sys.stderr)
            print(output.decode("utf-8"), file=sys.stderr)
            raise CompileError(output.decode("utf-8"))

        LFRicBuild._infrastructure_built = True
