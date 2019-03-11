# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

''' Test utilities including support for testing that code compiles. '''

from __future__ import absolute_import, print_function
import abc
import os
import pytest

# The various file suffixes we recognise as being Fortran
FORTRAN_SUFFIXES = ["f90", "F90", "x90"]


class CompileError(Exception):
    '''
    Exception raised when compilation of a Fortran source file
    fails.

    :param value: description of the error condition.
    :type value: str or :py:class:`bytes`

    '''
    def __init__(self, value):
        # pylint: disable=super-init-not-called
        self.value = "Compile error: " + str(value)

    def __str__(self):
        return repr(self.value)


def line_number(root, string_name):
    '''helper routine which returns the first index of the supplied
    string or -1 if it is not found'''
    lines = str(root).splitlines()
    for idx, line in enumerate(lines):
        if string_name in line:
            return idx
    return -1


def count_lines(root, string_name):
    '''helper routine which returns the number of lines that contain the
    supplied string'''
    count = 0
    lines = str(root).splitlines()
    for line in lines:
        if string_name in line:
            count += 1
    return count


def print_diffs(expected, actual):
    '''
    Pretty-print the diff between the two, possibly multi-line, strings

    :param str expected: Multi-line string
    :param str actual: Multi-line string
    '''
    import difflib
    from pprint import pprint
    expected_lines = expected.splitlines()
    actual_lines = actual.splitlines()
    diff = difflib.Differ()
    diff_list = list(diff.compare(expected_lines, actual_lines))
    pprint(diff_list)


class Compile(object):
    '''This class provides compile functionality to the testing framework.
    It stores the name of the compiler, compiler flags, and a temporary
    directory used for test compiles.
    API-specific classes are derived from this class to manage handling
    of the corresponding infrastructure library.
    '''

    # Whether or not we run tests requiring code compilation is picked-up
    # from a command-line flag. (This is set-up in conftest.py.)
    # pylint: disable=no-member
    TEST_COMPILE = pytest.config.getoption("--compile")
    # pylint: enable=no-member
    # The following allows us to mark a test with @utils.COMPILE if it is
    # only to be run when the --compile option is passed to py.test
    COMPILE = pytest.mark.skipif(not TEST_COMPILE,
                                 reason="Need --compile option to run")

    # pylint: disable=no-member
    TEST_COMPILE_OPENCL = pytest.config.getoption("--compileopencl")
    # pylint: enable=no-member
    COMPILE_OPENCL = pytest.mark.skipif(not TEST_COMPILE_OPENCL,
                                        reason="Need --compileopencl "
                                               "option to run")

    def __init__(self, tmpdir=None):
        '''
        Constructor for the Compile base class. Thie comiler, flags
        and tmpdir specified here will be used as required by all
        other methods.
        :param f90: Command to invoke Fortran compiler
        :type f90: string
        :param f90flags: Flags to pass to the compiler
        :type f90flags: string
        :param tmpdir: py.test-supplied temporary directory
        :type tmpdir: :py:class:`LocalPath`
        '''
        self._tmpdir = tmpdir
        # pylint: disable=no-member
        self._f90 = pytest.config.getoption("--f90")
        self._f90flags = pytest.config.getoption("--f90flags")
        # pylint: enable=no-member
        self._base_path = None

    @property
    def base_path(self):
        '''Returns the base path of all test files for the API. Needs to
        be set by each API-specific compile class.
        :returns: A string with the base path of all API specific files.
        :rtype: str
        '''
        return self._base_path

    @base_path.setter
    def base_path(self, base_path):
        '''Sets the base path of all test files for the API. Needs to
        be called by each API-specific compile class.
        :param str base_path: A string with the base path of all
               API-specific files.
        '''
        self._base_path = base_path

    @abc.abstractmethod
    def get_infrastructure_flags(self):
        '''Returns a list with the required flag to use the required
        infrastructure library. This is typically ["-I", some_path] so that
        the module files of the infrastructure can be found.
        :returns: A list of strings with the compiler flags required.
        :rtpe: str
        '''

    @staticmethod
    def find_fortran_file(search_paths, root_name):
        ''' Returns the full path to a Fortran source file. Searches for
        files with suffixes defined in FORTRAN_SUFFIXES. Raises IOError
        if no matching file is found.

        :param list search_paths: List of locations to search for Fortran file
        :param root_name: Base name of the Fortran file to look for
        :type path: string
        :type root_name: string
        :return: Full path to a Fortran source file
        :rtype: string '''
        for path in search_paths:
            name = os.path.join(path, root_name)
            for suffix in FORTRAN_SUFFIXES:
                if os.path.isfile(str(name)+"."+suffix):
                    name += "." + suffix
                    return name
        raise IOError("Cannot find a Fortran file '{0}' with suffix in {1}".
                      format(name, FORTRAN_SUFFIXES))

    def compile_file(self, filename, link=False):
        ''' Compiles the specified Fortran file into an object file (in
        the current working directory) using the specified Fortran compiler
        and flags. Raises a CompileError if the compilation fails.

        :param filename: Full path to the Fortran file to compile
        :type filename: string
        :param bool link: If true will also try to compile and link the file.
            Used in testing
        :return: True if compilation succeeds
        '''

        if not Compile.TEST_COMPILE and not Compile.TEST_COMPILE_OPENCL:
            # Compilation testing is not enabled
            return True

        # Build the command to execute. Note that the f90 flags are a string
        # and so must be split into individual parts for popen (otherwise
        # "-I /some/path" will result in the compiler trying to compile the
        # file "-I /some/path").
        arg_list = [self._f90, filename] + self._f90flags.split()
        if not link:
            arg_list.append("-c")
        if self.get_infrastructure_flags():
            arg_list += self.get_infrastructure_flags()

        # Attempt to execute it using subprocess
        import subprocess
        try:
            build = subprocess.Popen(arg_list,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT)
            (output, error) = build.communicate()
        except OSError as err:
            print("Failed to run: {0}: ".format(" ".join(arg_list)))
            print("Error was: ", str(err))
            raise CompileError(str(err))

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

    def _code_compiles(self, psy_ast):
        '''Attempts to build the Fortran code supplied as an AST of
        f2pygen objects. Returns True for success, False otherwise.
        It is meant for internal test uses only, and must only be
        called when compilation is actually enabled (use code_compiles
        otherwse). All files produced are deleted.

        :param psy_ast: The AST of the generated PSy layer
        :type psy_ast: Instance of :py:class:`psyGen.PSy`
        :return: True if generated code compiles, False otherwise
        :rtype: bool
        '''

        kernel_modules = set()
        # Get the names of the modules associated with the kernels.
        # By definition, built-ins do not have associated Fortran modules.
        for invoke in psy_ast.invokes.invoke_list:
            for call in invoke.schedule.kern_calls():
                kernel_modules.add(call.module_name)

        # Change to the temporary directory passed in to us from
        # pytest. (This is a LocalPath object.)
        old_pwd = self._tmpdir.chdir()

        # Create a file containing our generated PSy layer.
        psy_filename = "psy.f90"
        with open(psy_filename, 'w') as psy_file:
            # We limit the line lengths of the generated code so that
            # we don't trip over compiler limits.
            from psyclone.line_length import FortLineLength
            fll = FortLineLength()
            psy_file.write(fll.process(str(psy_ast.gen)))

        success = False

        try:
            # Build the kernels. We allow kernels to also be located in
            # the temporary directory that we have been passed.
            for fort_file in kernel_modules:
                name = self.find_fortran_file([self.base_path,
                                               str(self._tmpdir)], fort_file)
                success = self.compile_file(name)

            # Finally, we can build the psy file we have generated
            success = self.compile_file(psy_filename)
        except CompileError:
            # Failed to compile one of the files
            success = False

        finally:
            # Clean-up - delete all generated files. This permits this routine
            # to be called multiple times from within the same test.
            os.chdir(str(old_pwd))
            for ofile in self._tmpdir.listdir():
                ofile.remove()

        return success

    def code_compiles(self, psy_ast):
        '''Attempts to build the Fortran code supplied as an AST of
        f2pygen objects. Returns True for success, False otherwise.
        If compilation is not enabled returns true. Uses _code_compiles
        for the actual compilation. All files produced are deleted.

        :param psy_ast: The AST of the generated PSy layer
        :type psy_ast: Instance of :py:class:`psyGen.PSy`
        :return: True if generated code compiles, False otherwise
        :rtype: bool
        '''
        if not Compile.TEST_COMPILE:
            # Compilation testing is not enabled
            return True

        return self._code_compiles(psy_ast)

    def string_compiles(self, code):
        '''
        Attempts to build the Fortran code supplied as a string.
        Returns True for success, False otherwise.
        If no Fortran compiler is available or compilation testing is not
        enabled then it returns True. All files produced are deleted.

        :param str code: The code to compile. Must have no external
               dependencies.
        :return: True if generated code compiles, False otherwise
        :rtype: bool

        '''
        if not Compile.TEST_COMPILE:
            # Compilation testing (--compile flag to py.test) is not enabled
            # so we just return True.
            return True

        # Change to the temporary directory passed in to us from
        # pytest. (This is a LocalPath object.)
        old_pwd = self._tmpdir.chdir()

        filename = "generated.f90"
        with open(filename, 'w') as test_file:
            test_file.write(code)

        try:
            success = self.compile_file(filename)
        except CompileError:
            # Failed to compile the file
            success = False
        finally:
            # Clean-up - delete all generated files. This permits this routine
            # to be called multiple times from within the same test.
            os.chdir(str(old_pwd))
            for ofile in self._tmpdir.listdir():
                ofile.remove()

        return success


# =============================================================================
def get_invoke(algfile, api, idx=None, name=None):
    '''
    Utility method to get the idx'th or named invoke from the algorithm
    in the specified file.
    :param str algfile: name of the Algorithm source file (Fortran)
    :param str api: which PSyclone API this Algorithm uses
    :param int idx: the index of the invoke from the Algorithm to return
                    or None if name is specified
    :param str name: the name of the required invoke or None if an index
                     is supplied
    :returns: (psy object, invoke object)
    :rtype: 2-tuple containing :py:class:`psyclone.psyGen.PSy` and
            :py:class:`psyclone.psyGen.Invoke` objects.
    :raises RuntimeError: if neither idx or name are supplied or if
                          both are supplied
    :raises RuntimeError: if the supplied name does not match an invoke in
                          the Algorithm
    '''
    from psyclone.parse import parse
    from psyclone.psyGen import PSyFactory

    if (idx is None and not name) or (idx is not None and name):
        raise RuntimeError("Either the index or the name of the "
                           "requested invoke must be specified")

    # Set up a mapping of supported APIs and corresponding directories
    api_2_path = {"dynamo0.1": "dynamo0p1",
                  "dynamo0.3": "dynamo0p3",
                  "gocean1.0": "gocean1p0",
                  "gocean0.1": "gocean0p1"}
    try:
        dir_name = api_2_path[api]
    except KeyError:
        raise RuntimeError("The API '{0}' is not supported by get_invoke. "
                           "Supported types are {1}.".
                           format(api, api_2_path.keys()))

    _, info = parse(os.path.
                    join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", dir_name, algfile),
                    api=api)
    psy = PSyFactory(api).create(info)
    if name:
        invoke = psy.invokes.get(name)
    else:
        invoke = psy.invokes.invoke_list[idx]
    return psy, invoke
