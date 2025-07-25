# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council
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

''' PSyclone compiler wrapper implementation. '''

import sys
import os
import subprocess
from pathlib import Path
from psyclone.generator import main

FORTRAN_EXTENSIONS = ('.f90', '.f', '.F90', '.F')


def compiler_wrapper(arguments):
    '''
    PSyclone compiler wrapper.

    This utility intercepts compilation commands and applies the psyclone
    transformation tool to any encountered Fortran file before passing them
    to the target compiler. The goal is to provide an easy way to inject
    psyclone into build systems without needing to modify them.

    The target compiler can be selected with the PSYCLONE_COMPILER environment
    variable. Setting it is mandatory.

    Other psyclone options (e.g. which transformation recipe to apply) can
    be set with the optional PSYCLONE_OPTS environment variable.

    For example, to intercept the compiler calls from a Makefile that would
    normally be invoked as:

    FC=gfortran make

    We can use:

    FC=psyclonefc PSYCLONE_COMPILER=gfortran PSYCLONE_OTPS="-s recipe.py" make

    This will, for each fortran file:

    1) Apply the C-preprocessing step as the target compiler would.
    2) Apply the psyclone transformation
    3) Provide the resulting transformed file to the target compiler.

    Note that this wrapper can be compounded with other compiler wrappers, such
    as:

    FC=psyclonefc PSYCLONE_COMPILER=mpifort OMPI_FC=gfortran make

    '''
    fortran_compiler = os.getenv("PSYCLONE_COMPILER", default=None)
    psyclone_options = os.getenv("PSYCLONE_OPTS", default="").split(' ')

    # Validate mandatory PSYCLONE_COMPILER
    if fortran_compiler is None:
        sys.exit(
            'psyclonefc error: PSYCLONE_COMPILER environment variable not '
            'found! This environment variable must be set to the Fortran '
            'compiler to use.')
    # Remove empty strings from the list (caused by the default empty envvar or
    # multi-spaces gaps)
    while "" in psyclone_options:
        psyclone_options.remove("")

    # Capture the dirctory where the .mod files are written because this is
    # also where we want to place the output psyclone file (so following
    # -I search_path finds them).
    # output_dir = Path.cwd()
    # for idx, argument in enumerate(arguments):
    #     if argument.endswith("-J"):
    #         mod_path = argument[2:]
    #         if not mod_path:
    #             mod_path = arguments[idx + 1]
    #         output_dir = Path(output_dir, mod_path)
    # TODO #3012: output_dir is currently ignored, this is not a problem for
    # NEMO because it all goes to the same directory (BLD/tmp), the following
    # psyclone commands will find the modules due to the implied "-I ."

    # The new compiler command will start with the selected compiler
    new_compiler_command = [fortran_compiler]

    # And it will be followed by each of the original arguments ...
    for argument in arguments:
        # ... but for each fortran file:
        if argument.endswith(FORTRAN_EXTENSIONS):
            # 1) Run the preprocessor
            # TODO #3012: preprocessing is currently ignored, this is not a
            # problem for NEMO because the build system does the proprocessor
            # in a separate step

            # 2) Run psyclone
            stem = Path(argument).stem
            suffix = Path(argument).suffix
            output = f"{stem}.psycloned{suffix}"
            psyclone_args = psyclone_options + ['-o', output, argument]
            print("psyclone " + ' '.join(psyclone_args))
            main(psyclone_args)

            # 3) Replace the fortran file in the compiler command with the
            # fortran file produced by psyclone
            new_compiler_command.append(output)
        else:
            new_compiler_command.append(argument)

    # Run the new compiler command and propagate error code
    print(' '.join(new_compiler_command))
    result = subprocess.run(new_compiler_command, check=False)
    sys.exit(result.returncode)
