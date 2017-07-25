# --------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# --------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' test utilities '''

import os
import pytest

# The various file suffixes we recognise as being Fortran
FORTRAN_SUFFIXES = ["f90", "F90", "x90"]

# Whether or not we run tests requiring code compilation is picked-up
# from a command-line flag. (This is set-up in conftest.py.)
compile = pytest.mark.skipif(
    not pytest.config.getoption("--compile"),
    reason="Need --compile option to run")


class CompileError(Exception):
    ''' Exception raised when compilation of a Fortran source file
    fails '''
    def __init__(self, value):
        self.value = "Compile error: " + value

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


def walk(parent, my_type):
    ''' recurse through a tree of objects and return those that are
    instances of mytype. Assumes that the children of any given node
    are held in the list node.children '''
    local_list = []
    for child in parent.children:
        if isinstance(child, my_type):
            local_list.append(child)
        local_list += walk(child, my_type)
    return local_list


def find_fortran_file(path, root_name):
    ''' Returns the full path to a Fortran source file. Searches for
    files with suffixes defined in FORTRAN_SUFFIXES. Raises IOError
    if no matching file is found. '''
    name = os.path.join(path, root_name)
    for suffix in FORTRAN_SUFFIXES:
        if os.path.isfile(str(name)+"."+suffix):
            name += "." + suffix
            return name
    raise IOError("Cannot find a Fortran file '{0}' with suffix in {1}".
                  format(name, FORTRAN_SUFFIXES))


def compile_file(filename, f90, f90flags):
    ''' Compiles the specified Fortran file into an object file (in
    the current working directory) using the specified Fortran compiler
    and flags. Raises a CompileError if the compilation fails. '''

    # Build the command to execute
    if f90flags:
        arg_list = [f90, f90flags, '-c', filename]
    else:
        arg_list = [f90, '-c', filename]

    # Attempt to execute it using subprocess
    import subprocess
    try:
        build = subprocess.Popen(arg_list,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT)
        (output, error) = build.communicate()
    except OSError as err:
        print "Failed to run: {0}: ".format(" ".join(arg_list))
        print "Error was: ", str(err)
        raise CompileError(str(err))

    # Check the return code
    stat = build.returncode
    if stat != 0:
        print output
        if error:
            print "========="
            print error
        raise CompileError(output)
    else:
        return True


def code_compiles(api, psy_ast, tmpdir, f90, f90flags):
    '''Attempts to build the Fortran code supplied as an AST of
    f2pygen objects. Returns True for success, False otherwise.
    If no Fortran compiler is available then returns True. All files
    produced are deleted. '''
    from psyclone import f2pygen

    # API-specific set-up - where to find infrastructure source files
    # and which ones to build
    supported_apis = ["dynamo0.3"]
    if api not in supported_apis:
        raise CompileError("Unsupported API in code_compiles. Got {0} but "
                           "only support {1}".format(api, supported_apis))

    if api == "dynamo0.3":
        base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3")
        from dynamo0p3_build import INFRASTRUCTURE_MODULES as module_files

    kernel_modules = []
    # Get the list of Use statements in the generated code
    use_stmts = walk(psy_ast.psy_module, f2pygen.UseGen)
    # Those that aren't in our list of infrastructure modules must be
    # kernels. Note that we don't use this method to find which infrastructure
    # modules to compile because we don't have any way of knowing which
    # ones are required by the individual kernels.
    for stmt in use_stmts:
        if stmt.root.name not in module_files:
            kernel_modules.append(stmt.root.name)

    # Change to the temporary directory passed in to us from
    # pytest. (This is a LocalPath object.)
    old_pwd = tmpdir.chdir()

    # Create a file containing our generated PSy layer.
    psy_filename = "psy.f90"
    with open(psy_filename, 'w') as psy_file:
        # We limit the line lengths of the generated code so that
        # we don't trip over compiler limits.
        from psyclone.line_length import FortLineLength
        fll = FortLineLength()
        psy_file.write(fll.process(str(psy_ast.gen)))

    # Infrastructure modules are in the 'infrastructure' directory
    module_path = os.path.join(base_path, "infrastructure")
    kernel_path = base_path

    success = False
    try:
        # First build the infrastructure modules
        for fort_file in module_files:
            name = find_fortran_file(module_path, fort_file)
            # We don't have to copy the source file - just compile it in the
            # current working directory.
            success = compile_file(name, f90, f90flags)

        # Next, build the kernels
        for fort_file in kernel_modules:
            name = find_fortran_file(kernel_path, fort_file)
            success = compile_file(name, f90, f90flags)

        # Finally, we can build the psy file we have generated
        success = compile_file(psy_filename, f90, f90flags)

    except CompileError:
        # Failed to compile one of the files
        success = False

    finally:
        # Clean-up - delete all generated files. This permits this routine
        # to be called multiple times from within the same test.
        os.chdir(str(old_pwd))
        for ofile in tmpdir.listdir():
            ofile.remove()

    return success
