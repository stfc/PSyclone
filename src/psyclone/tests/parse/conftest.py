# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology


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
        # Create a list of "use a_mod, only: a_mod_symbol" statements
        uses = "\n".join(f"use {dep}, only: {dep}_symbol"
                         for dep in dependencies)
        base, _ = os.path.splitext(name)
        with open(os.path.join(path, name), "w", encoding="utf-8") as f_out:
            f_out.write(f"module {base}\n{uses}\nend module {base}")
