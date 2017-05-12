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


def code_compiles(module_path, module_files, code, tmpdir):
    '''Attempts to build the supplied Fortran code. Returns True for
    success, False otherwise. If no Fortran compiler is available
    then returns True. '''
    import shutil

    if not F90_COMPILER:
        # TODO Log the fact that we have no Fortran compiler set-up?
        return True

    # Create a temporary working directory using the object passed to
    # us from pytest
    cwd = str(tmpdir.mkdir("tmp"))
    os.chdir(cwd)

    filename = "psy.f90"
    psy_file = open(filename, 'w')
    psy_file.write(code)
    psy_file.close()

    for file in module_files:
        shutil.copy(os.path.join(module_path, file), cwd)

        success = compile_file(file)
        if not success:
            return False

    # Finally, we can build the psy file
    success = compile_file(filename)
    return success
