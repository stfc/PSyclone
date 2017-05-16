# --------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# --------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' test utilities '''

import os

# The Fortran compiler to use is picked up from the F90
# environment variable
F90_COMPILER = os.environ.get('F90')
F90_FLAGS = os.environ.get('F90FLAGS')
# The various file suffixes we recognise as being Fortran
FORTRAN_SUFFIXES = ["f90", "F90", "x90"]


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


def find_fortran_file(path, root_name):
    ''' Returns the full path to a Fortran source file. Searches for
    files with suffixes defined in FORTRAN_SUFFIXES. Raises IOError
    if no matching file is found. '''
    name = os.path.join(path, root_name)
    for suffix in FORTRAN_SUFFIXES:
        if os.path.isfile(str(name)+"."+suffix):
            name += "." + suffix
            return name
    raise IOError("Cannot find a Fortran file {0} with suffix in {1}".
                  format(name), FORTRAN_SUFFIXES)


def compile_file(filename):
    ''' Compiles the specified Fortran file using the compiler
    previously picked-up from the F90 environment variable. '''
    import subprocess

    if F90_FLAGS:
        arg_list = [F90_COMPILER, F90_FLAGS, '-c', filename]
    else:
        arg_list = [F90_COMPILER, '-c', filename]
    build = subprocess.Popen(arg_list,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
    (output, error) = build.communicate()
    stat = build.returncode
    if stat != 0:
        print output
        print "========="
        print error
        return False
    else:
        return True


def code_compiles(base_path, module_files, psy, tmpdir):
    '''Attempts to build the supplied Fortran code. Returns True for
    success, False otherwise. If no Fortran compiler is available
    then returns True. '''

    if not F90_COMPILER:
        # TODO Log the fact that we have no Fortran compiler setup?
        # If no Fortran compiler is set-up then we quietly skip this test
        return True

    import f2pygen
    kernel_modules = []
    # Get the list of Use statements in the generated code
    use_stmts = psy.psy_module.walk(psy.psy_module.children, f2pygen.UseGen)
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
    # We limit the line lengths of the generated code so that
    # we don't trip over compiler limits.
    from line_length import FortLineLength
    fll = FortLineLength()
    psy_filename = "psy.f90"
    psy_file = open(psy_filename, 'w')
    psy_file.write(fll.process(str(psy.gen)))
    psy_file.close()

    # Infrastructure modules are in the 'infrastructure' directory
    module_path = os.path.join(base_path, "infrastructure")
    kernel_path = base_path

    # First build the infrastructure modules
    for fort_file in module_files:
        name = find_fortran_file(module_path, fort_file)
        # We don't have to copy the source file - just compile it in the
        # current working directory.
        success = compile_file(name)
        if not success:
            # Build failed so break-out of this routine early
            return False

    # Next, build the kernels
    for fort_file in kernel_modules:
        name = find_fortran_file(kernel_path, fort_file)
        success = compile_file(name)
        if not success:
            # Build failed so break-out of this routine early
            return False

    # Finally, we can build the psy file we have generated
    success = compile_file(psy_filename)

    # Clean-up - delete all generated files. This permits this routine
    # to be called multiple times from within the same test.
    os.chdir(str(old_pwd))
    for ofile in tmpdir.listdir():
        ofile.remove()

    return success
