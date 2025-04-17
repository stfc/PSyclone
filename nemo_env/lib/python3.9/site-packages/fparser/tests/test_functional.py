# Copyright (c) 2017 Science and Technology Facilities Council

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

""" Module containing functional tests - i.e. tests of specific functionality
    within the context of parsing a piece of compilable Fortran """


def test_procedure_interface():
    """Test that parser copes with a procedure declaration in
    a subroutine"""
    from fparser import api

    source_str = """  subroutine proc_interface_test()
    use field_mod,                   only: field_type, write_interface
    use fs_continuity_mod,           only: W3
    use io_mod,                      only: write_field
    implicit none
    type(field_type)                      :: divergence
    procedure (write_interface), pointer  :: tmp_ptr

    divergence = field_type( vector_space = &
                    function_space_collection%get_fs(0, 0, W3) )
    tmp_ptr => write_field
    call divergence%set_write_field_behaviour(write_field)
  end subroutine proc_interface_test
"""
    tree = api.parse(source_str, isfree=True, isstrict=False, ignore_comments=False)
    gen_code = str(tree)
    assert "PROCEDURE (write_interface) , POINTER :: tmp_ptr" in gen_code
