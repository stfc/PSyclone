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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

''' Test utilities including support for testing that code compiles. '''

import difflib
from contextlib import contextmanager
import os
from pprint import pprint
import subprocess
import sys

import pytest

from fparser import api as fpapi
from psyclone.line_length import FortLineLength
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.errors import PSycloneError
from psyclone.psyir.nodes import ScopingNode

# The various file suffixes we recognise as being Fortran
FORTRAN_SUFFIXES = ["f90", "F90", "x90"]


class CompileError(PSycloneError):
    '''
    Exception raised when compilation of a Fortran source file fails.

    :param value: description of the error condition.
    :type value: str or :py:class:`bytes`

    '''
    def __init__(self, value):
        # pylint: disable=super-init-not-called
        PSycloneError.value = "Compile error: " + str(value)


# =============================================================================
def line_number(root, string_name):
    '''Helper routine which returns the first index of the supplied
    name in the root object, when it is converted into a string, or
    -1 if it is not found.

    :param root: the supplied object, which is converted into a list of \
        strings using `str(root).splitlines()`
    :type root: Any
    :param str string_name: the string to search for.

    :returns: first index in the converted list of strings, or -1 if it \
        is not found
    :rtype: int

    '''
    lines = str(root).splitlines()
    for idx, line in enumerate(lines):
        if string_name in line:
            return idx
    return -1


# =============================================================================
def count_lines(root, string_name):
    '''Helper routine which returns the number of lines that contain the
    supplied string.

    :param root: the supplied object, which is converted into a list of \
        strings using `str(root).splitlines()`
    :type root: Any
    :param str string_name: the string to search for.

    :returns: the number of lines that contain the specified string.
    :rtype: int

    '''
    count = 0
    lines = str(root).splitlines()
    for line in lines:
        if string_name in line:
            count += 1
    return count


# =============================================================================
def print_diffs(expected, actual):
    '''
    Pretty-print the diff between the two, possibly multi-line, strings

    :param str expected: Multi-line string
    :param str actual: Multi-line string
    '''
    expected_lines = expected.splitlines()
    actual_lines = actual.splitlines()
    diff = difflib.Differ()
    diff_list = list(diff.compare(expected_lines, actual_lines))
    pprint(diff_list)


# =============================================================================
@contextmanager
def change_dir(new_dir):
    '''This is a small context manager that changes the current working
    directory, and will automatically switch back. Usage:

    >>> with change_dir("/tmp"):
    ...     print(f"CWD is {os.getcwd()}")
    CWD is /tmp

    '''
    prev_dir = os.getcwd()
    os.chdir(os.path.expanduser(new_dir))
    try:
        yield
    finally:
        os.chdir(prev_dir)


# =============================================================================
class Compile():
    '''This class provides compile functionality to the testing framework.
    It stores the name of the compiler, compiler flags, and a temporary
    directory used for test compiles (defaults to the current working
    directory). The temporary directory will be defined per test case, so a
    new instance must be created for each test function.
    API-specific classes are derived from this class to manage handling
    of the corresponding infrastructure library.

    :param tmpdir: temporary directory, defaults to os.getcwd()
    :type tmpdir: Optional[:py:class:`LocalPath`]

    '''
    # Class variable to store whether compilation is enabled (--compile).
    TEST_COMPILE = False

    # Class variable to store whether opencl compilation is enabled
    # (--compileopencl).
    TEST_COMPILE_OPENCL = False

    # Class variable to store the chosen Fortran compiler (--f90, default
    # gfortran).
    F90 = "gfortran"

    # Class variable to store the chosen f90 compiler flags (--f90flags).
    F90FLAGS = ""

    @staticmethod
    def store_compilation_flags(config):
        '''This function is called from the conftest session fixture
        infra_compile. It sets the class variables related to
        compilation based on the command-line options.

        :param config: The config object from pytest.
        :type config: :py:class:`pytest.config`.

        '''
        # Whether or not we run tests requiring code compilation is picked-up
        # from a command-line flag. (This is set-up in conftest.py.)
        Compile.TEST_COMPILE = config.getoption("--compile")
        Compile.TEST_COMPILE_OPENCL = config.getoption("--compileopencl")
        Compile.F90 = config.getoption("--f90")
        Compile.F90FLAGS = config.getoption("--f90flags")

    def __init__(self, tmpdir=None):
        if tmpdir:
            self._tmpdir = tmpdir
        else:
            self._tmpdir = os.getcwd()
        # Take the compiler and compile flags from the static variables.
        # This allows a specific instance to use different compiler options
        # which is used in some of the compilation tests.
        self._f90 = Compile.F90
        self._f90flags = Compile.F90FLAGS
        self._base_path = None

    @property
    def base_path(self):
        '''Returns the directory of all Fortran test files for the API,
        i.e. <PSYCLONEHOME>/src/psyclone/tests/test_files/<API>.
        Needs to be set by each API-specific compile class.

        :returns: A string with the base path of all API specific files.
        :rtype: str

        '''
        return self._base_path

    @base_path.setter
    def base_path(self, base_path):
        '''Sets the base path of all test files for the API., i.e.
        <PSYCLONEHOME>/src/psyclone/tests/test_files/<API>. Needs to be called
        by each API-specific compile class.
        :param str base_path: A string with the base path of all
               API-specific files.
        '''
        self._base_path = base_path

    def get_infrastructure_flags(self):
        '''Returns a list with the required flags to use the required
        infrastructure library. This is typically ["-I", some_path] so that
        the module files of the infrastructure can be found.

        :returns: A list of strings with the compiler flags required.
        :rtype: List[str]

        '''
        return []

    @staticmethod
    def skip_if_compilation_disabled():
        '''This function is used in all tests that should only run
        if compilation is enabled. It calls pytest.skip if compilation
        is not enabled.'''
        if not Compile.TEST_COMPILE:
            pytest.skip("Need --compile option to run")

    @staticmethod
    def skip_if_opencl_compilation_disabled():
        '''This function is used in all tests that should only run
        if opencl compilation is enabled. It calls pytest.skip if
        opencl compilation is not enabled.'''

        if not Compile.TEST_COMPILE_OPENCL:
            pytest.skip("Need --compileopencl option to run")

    @staticmethod
    def find_fortran_file(search_paths, root_name):
        '''Returns the full path to a Fortran source file. Searches for
        files with suffixes defined in FORTRAN_SUFFIXES.

        :param search_paths: List of locations to search for Fortran file.
        :type search_paths: List[str]
        :param str root_name: Base name of the Fortran file to look \
            for. If it ends with a recognised Fortran suffix then this \
            is stripped before performing the search.

        :return: Full path to a Fortran source file.
        :rtype: str

        :raises IOError: Raises IOError if no matching file is found.

        '''
        base_name = root_name[:]
        for suffix in FORTRAN_SUFFIXES:
            if root_name.endswith("."+suffix):
                base_name = base_name[:-(len(suffix)+1)]
                break
        for path in search_paths:
            name = os.path.join(path, base_name)
            for suffix in FORTRAN_SUFFIXES:
                if os.path.isfile(str(name)+"."+suffix):
                    name += "." + suffix
                    return name
        raise IOError(f"Cannot find a Fortran file '{base_name}' with "
                      f"suffix in {FORTRAN_SUFFIXES}")

    def compile_file(self, filename, link=False):
        ''' Compiles the specified Fortran file into an object file (in
        the current working directory). The compiler to be used (default
        'gfortran') and compiler flags (default none) can be specified on
        the command line using --f90 and --f90flags.

        :param str filename: Full path to the Fortran file to compile.
        :param bool link: If true will also try to link the file.
            Used in testing.

        :raises CompileError: if the compilation fails.

        '''
        if not Compile.TEST_COMPILE and not Compile.TEST_COMPILE_OPENCL:
            # Compilation testing is not enabled
            return

        # Build the command to execute. Note that the f90 flags are a string
        # and so must be split into individual parts for popen (otherwise
        # "-I /some/path" will result in the compiler trying to compile the
        # file "-I /some/path").
        arg_list = [self._f90, filename] + self._f90flags.split() + \
            self.get_infrastructure_flags()
        if not link:
            arg_list.append("-c")

        # Attempt to execute it using subprocess

        # Change to the temporary directory passed in to us from
        # pytest. (This is a LocalPath object.)
        with change_dir(self._tmpdir):
            try:
                with subprocess.Popen(arg_list,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT) as build:
                    # stderr = stdout, so ignore empty stderr in result:
                    (output, _) = build.communicate()
            except OSError as err:
                print(f"Failed to run: {' '.join(arg_list)}: ",
                      file=sys.stderr)
                raise CompileError(str(err)) from err

        # Check the return code
        stat = build.returncode
        if stat != 0:
            print(f"Compiling: {' '.join(arg_list)}", file=sys.stderr)
            print(output.decode("utf-8"), file=sys.stderr)
            raise CompileError(output)

    def _code_compiles(self, psy_ast, dependencies=None):
        '''Attempts to build the Fortran code supplied as an AST of
        f2pygen objects. Returns True for success, False otherwise.
        It is meant for internal test uses only, and must only be
        called when compilation is actually enabled (use code_compiles
        otherwse). All files produced are deleted.

        :param psy_ast: The AST of the generated PSy layer.
        :type psy_ast: :py:class:`psyclone.psyGen.PSy`
        :param dependencies: optional module- or file-names on which \
                    one or more of the kernels/PSy-layer depend (and \
                    that are not part of e.g. the GOcean or LFRic \
                    infrastructure).  These dependencies will be built \
                    in the order they occur in this list.
        :type dependencies: List[str]

        :return: True if generated code compiles, False otherwise.
        :rtype: bool

        '''
        modules = set()
        # Get the names of all the imported modules as these are dependencies
        # that will need to be compiled first
        for invoke in psy_ast.invokes.invoke_list:
            # Get any module that is imported in the PSyIR tree
            for scope in invoke.schedule.root.walk(ScopingNode):
                for symbol in scope.symbol_table.containersymbols:
                    modules.add(symbol.name)

            # Not everything is captured by PSyIR yet (some API PSy-layers are
            # fully or partially f2pygen), in these cases we still need to
            # import the kernel modules used in these PSy-layers.
            # By definition, built-ins do not have associated Fortran modules.
            for call in invoke.schedule.coded_kernels():
                modules.add(call.module_name)

        # Change to the temporary directory passed in to us from
        # pytest. (This is a LocalPath object.)
        with change_dir(self._tmpdir):
            # Create a file containing our generated PSy layer.
            psy_filename = "psy.f90"
            with open(psy_filename, 'w', encoding="utf-8") as psy_file:
                # We limit the line lengths of the generated code so that
                # we don't trip over compiler limits.
                fll = FortLineLength()
                psy_file.write(fll.process(str(psy_ast.gen)))

            success = True

            build_list = []
            # We must ensure that we build any dependencies first and in
            # the order supplied.
            if dependencies:
                build_list.extend(dependencies)
            # Then add the modules we found on the tree
            for module in modules:
                if module not in build_list:
                    build_list.append(module)

            # Build the dependencies and then the kernels. We allow kernels
            # to also be located in the temporary directory that we have
            # been passed.
            for fort_file in build_list:

                # Skip file if it is not Fortran. TODO #372: Add support
                # for C/OpenCL compiling as part of the test suite.
                if fort_file.endswith(".cl"):
                    continue

                try:
                    name = self.find_fortran_file([self.base_path,
                                                   str(self._tmpdir)],
                                                  fort_file)
                    self.compile_file(name)
                except IOError:
                    # Not all modules need to be found, for example API
                    # infrastructure modules will be provided already built.
                    print(f"File {fort_file} not found for compilation.")
                    paths = [self.base_path, str(self._tmpdir)]
                    print(f"It was searched in: {paths}")
                except CompileError:
                    # Failed to compile one of the files
                    success = False

            # Finally, we can build the psy file we have generated
            try:
                self.compile_file(psy_filename)
            except CompileError:
                # Failed to compile one of the files
                success = False

        return success

    def code_compiles(self, psy_ast, dependencies=None):
        '''Attempts to build the Fortran code supplied as an AST of
        f2pygen objects. Returns True for success, False otherwise.
        If compilation is not enabled returns true. Uses _code_compiles
        for the actual compilation. All files produced are deleted.

        :param psy_ast: The AST of the generated PSy layer.
        :type psy_ast: :py:class:`psyclone.psyGen.PSy`
        :param dependencies: optional module- or file-names on which \
                    one or more of the kernels/PSy-layer depend (and \
                    that are not part of e.g. the GOcean or LFRic \
                    infrastructure).  These dependencies will be built \
                    in the order they occur in this list.
        :type dependencies: List[str]

        :return: True if generated code compiles, False otherwise
        :rtype: bool

        '''
        if not Compile.TEST_COMPILE and not Compile.TEST_COMPILE_OPENCL:
            # Compilation testing is not enabled
            return True

        return self._code_compiles(psy_ast, dependencies)

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
        if not Compile.TEST_COMPILE and not Compile.TEST_COMPILE_OPENCL:
            # Compilation testing (--compile flag to py.test) is not enabled
            # so we just return True.
            return True

        # Change to the temporary directory passed in to us from
        # pytest. (This is a LocalPath object.)
        with change_dir(self._tmpdir):
            # Add a object-specific hash-code to the file name so that all
            # files created in the same test have different names and can
            # easily be inspected in case of errors.
            filename = f"generated-{hash(self)}.F90"
            with open(filename, 'w', encoding="utf-8") as test_file:
                test_file.write(code)

            success = True
            try:
                self.compile_file(filename)
            except CompileError:
                # Failed to compile the file
                success = False

        return success


# =============================================================================
def get_base_path(api):
    '''Get the absolute base path for the specified API relative to the
    'tests/test_files' directory, i.e. the directory in which all
    Fortran test files are stored.

    :param str api: name of the API.

    :returns: the base path for the API.
    :rtype: str

    :raises RuntimeError: if the supplied API name is invalid.

    '''
    # Define the mapping of supported APIs to Fortran directories
    # Note that the nemo files are outside of the default tests/test_files
    # directory, they are in tests/nemo/test_files
    api_2_path = {"dynamo0.3": "dynamo0p3",
                  "nemo": "../nemo/test_files",
                  "gocean1.0": "gocean1p0"}
    try:
        dir_name = api_2_path[api]
    except KeyError as err:
        raise RuntimeError(f"The API '{api}' is not supported. "
                           f"Supported types are {api_2_path.keys()}.") \
                           from err
    return os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        "test_files", dir_name)


# =============================================================================
def get_invoke(algfile, api, idx=None, name=None, dist_mem=None):
    '''
    Utility method to get the idx'th or named invoke from the algorithm
    in the specified file.

    :param str algfile: name of the Algorithm source file (Fortran).
    :param str api: which PSyclone API this Algorithm uses.
    :param int idx: the index of the invoke from the Algorithm to return
                    or None if name is specified.
    :param str name: the name of the required invoke or None if an index
                     is supplied.
    :param bool dist_mem: if the psy instance should be created with or \
                          without distributed memory support.

    :returns: (psy object, invoke object)
    :rtype: Tuple[:py:class:`psyclone.psyGen.PSy`, \
                  :py:class:`psyclone.psyGen.Invoke`]

    :raises RuntimeError: if neither idx or name are supplied or if
                          both are supplied
    :raises RuntimeError: if the supplied name does not match an invoke in
                          the Algorithm
    '''

    if (idx is None and not name) or (idx is not None and name):
        raise RuntimeError("Either the index or the name of the "
                           "requested invoke must be specified")

    _, info = parse(os.path.join(get_base_path(api), algfile), api=api)
    psy = PSyFactory(api, distributed_memory=dist_mem).create(info)
    if name:
        invoke = psy.invokes.get(name)
    else:
        invoke = psy.invokes.invoke_list[idx]
    return psy, invoke


# =============================================================================
def get_ast(api, filename):
    '''Returns the fparser1 parse tree for a filename that is stored in the
    test files for the specified API.

    :param str api: the API to use, which determines the directory \
        where files are stored.
    :param str filename: the file name to parse.

    :returns: the parse tree for the specified Fortran source file.
    :rtype: :py:class:`fparser.api.BeginSource`

    '''
    ast = fpapi.parse(os.path.join(get_base_path(api), filename),
                      ignore_comments=False)
    return ast


# =============================================================================
def check_links(parent, children):
    '''Utilitiy routine to check that the parent node has children as its
    children in the order specified and that the children have parent
    as their parent. Also check that the parent does not have any
    additional children that are not provided in the children
    argument.

    :param parent: the parent node that should have the child \
        nodes as its children.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param children: the child nodes that should have the parent \
        node as their parent.
    :type parent: List[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    assert len(parent.children) == len(children)
    for index, child in enumerate(children):
        assert child.parent is parent
        assert parent.children[index] is child
