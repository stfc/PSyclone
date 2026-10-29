# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' Module which provides a fixture to setup a test environment for
the module manager.
'''

import os

import pytest


# ----------------------------------------------------------------------------
@pytest.fixture(scope='function')
def mod_man_test_setup_directories():
    '''Sets up a directory and file structure for several of the following
    tests. The following structure is created - note that each Fortran file
    declares a module of the same name as the basename of the file:
    tmp/d1/a_mod.f90       : no dependencies
    tmp/d1/d3/b_mod.F90    : no dependencies
    tmp/d1/d3/c_mod.x90    : depends on a_mod/b_mod
    tmp/d2/d_mod.X90       : depends on c_mod
    tmp/d2/d4/e_mod.F90    : depends on netcdf
    tmp/d2/d4/f_mod.ignore
    tmp/d2/g_mod.F90       : no dependencies, contains an interface
    tmp/d2/error_mod.F90   : invalid Fortran, not parseable
    '''

    os.makedirs("d1/d3")
    os.makedirs("d2/d4")
    for (name, path, dependencies) in [("a_mod.f90", "d1", []),
                                       ("b_mod.F90", "d1/d3", []),
                                       ("c_mod.x90", "d1/d3", ["a_mod",
                                                               "b_mod"]),
                                       ("d_mod.X90", "d2", ["c_mod"]),
                                       ("e_mod.F90", "d2/d4", ["netcdf"]),
                                       ("f_mod.ignore", "d2/d4", [])]:
        base, _ = os.path.splitext(name)
        # Create a list of "use a_mod, only: a_mod_symbol" statements
        uses = "\n".join(f"use {dep}, only: {dep}_symbol"
                         for dep in dependencies)
        with open(os.path.join(path, name),
                  "w", encoding="utf-8") as f_out:
            f_out.write(f"module {base}\n{uses}\nend module {base}")

    # g_mod contains a generic interface
    # ----------------------------------
    module_code = '''
    module g_mod
      integer :: module_var, module_var_1, module_var_2
    interface myfunc
        procedure myfunc1
        procedure myfunc2
    end interface myfunc
    contains
        subroutine myfunc1()
            integer :: a, p
            ! Read module_var
            a = p + module_var_1 + module_var
        end subroutine myfunc1
        subroutine myfunc2()
            integer :: a, p
            ! Write module_var
            module_var = p + module_var_2
        end subroutine myfunc2
    end module g_mod
    '''
    with open("d2/g_mod.F90", "w", encoding="utf-8") as f_out:
        f_out.write(module_code)

    # error_mod contains invalid Fortran
    # ----------------------------------
    module_code = '''
    module error_mod
    contains
       ERROR
    end module error_mod
    '''
    with open("d2/error_mod.F90", "w", encoding="utf-8") as f_out:
        f_out.write(module_code)
