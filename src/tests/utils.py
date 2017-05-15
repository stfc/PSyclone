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


def compile_file(filename):
    import subprocess

    build = subprocess.Popen([F90_COMPILER, '-c', filename],
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
        return True

    import f2pygen
    kernel_modules = []
    # Get the list of Use statements in the generated code
    use_stmts = psy.psy_module.walk(psy._psy_module.children, f2pygen.UseGen)
    # Those that aren't in our list of infrastructure modules must be
    # kernels
    for stmt in use_stmts:
        if stmt.root.name not in module_files:
            kernel_modules.append(stmt.root.name)

    # Change to the temporary directory passed in to us from
    # pytest. (This is a LocalPath object.)
    old_pwd = tmpdir.chdir()

    # Create a file containing our generated PSy layer
    filename = "psy.f90"
    psy_file = open(filename, 'w')
    psy_file.write(str(psy.gen))
    psy_file.close()

    # Infrastructure modules are in the 'infrastructure' directory
    module_path = os.path.join(base_path, "infrastructure")
    kernel_path = base_path

    # First build the modules
    for file in module_files:
        name = os.path.join(module_path, file)
        if os.path.isfile(str(name)+".f90"):
            name += ".f90"
        elif os.path.isfile(str(name)+".F90"):
            name += ".F90"
        else:
            raise IOError("Cannot find infrastructure module {0}.F/f90".
                          format(name))
        # We don't have to copy the source file - just compile it in the
        # current working directory.
        success = compile_file(name)
        if not success:
            return False
    # Then build the kernels
    for file in kernel_modules:
        name = os.path.join(kernel_path, file)
        if os.path.isfile(str(name)+".f90"):
            name += ".f90"
        elif os.path.isfile(str(name)+".F90"):
            name += ".F90"
        else:
            raise IOError("Cannot find kernel module {0}.F/f90".
                          format(name))
        success = compile_file(name)
        if not success:
            return False

    # Finally, we can build the psy file
    success = compile_file(filename)

    # Clean-up - delete all generated files. This permits this routine
    # to be called multiple times from within the same test.
    os.chdir(str(old_pwd))
    for file in tmpdir.listdir():
        file.remove()

    return success
