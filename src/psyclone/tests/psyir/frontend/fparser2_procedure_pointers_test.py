# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the fparser2 PSyIR front-end support for
    procedure pointers. '''

from psyclone.psyir.nodes import CodeBlock
from psyclone.psyir.symbols import Symbol


def test_procedure_declaration_pointers(fortran_reader, fortran_writer):

    code = """
    module procedures
        use other
        procedure(my_proc_interface) :: proc_ptr_private
        procedure(my_proc_interface) :: proc_ptr_public
        public proc_ptr_public
        private proc_ptr_private
        implicit none
        contains
        subroutine test
            procedure(my_proc_interface), pointer :: proc_ptr, &
                proc_ptr_2 => null()

            save proc_ptr

            ! They can be assinged to
            proc_ptr => my_func
            ! Called
            call proc_ptr(10.0, 5.0)
            ! Or passed as arguments
            call other_func(proc_ptr)

        end subroutine test
    end module procedures
    """
    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)
    print(output)

    # This code must not have CodeBlocks
    assert len(psyir.walk(CodeBlock)) == 0

    # Check that the module proc_delcs exist and have the correct visibility
    module = psyir.children[0]
    priv = Symbol.Visibility.PRIVATE
    pub = Symbol.Visibility.PUBLIC
    assert module.symbol_table.lookup("proc_ptr_private").visibility == priv
    assert module.symbol_table.lookup("proc_ptr_public").visibility == pub

    # Check that the routine proc_decls exist and have the correct attributes
    routine = module.children[0]
    assert routine.symbol_table.lookup("proc_ptr").is_static
    assert not routine.symbol_table.lookup("proc_ptr_2").is_static
