# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the fparser2 PSyIR front-end support for
    procedure pointers. '''

from psyclone.psyir.nodes import CodeBlock
from psyclone.psyir.symbols import Symbol, UnsupportedFortranType, DataSymbol


def test_procedure_declaration_pointers(fortran_reader):
    """
    Test that the fparser2 reader handles procedures and procedure pointer
    declarations (currently by leaving them as UnsupportedFortranType)

    Also that accessibility and storage statements interact properly with
    the created procedure symbols.
    """

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

    # This code must not have CodeBlocks
    assert len(psyir.walk(CodeBlock)) == 0

    # Lookup the symbols
    module = psyir.children[0]
    proc_ptr_private = module.symbol_table.lookup("proc_ptr_private")
    proc_ptr_public = module.symbol_table.lookup("proc_ptr_public")
    routine = module.children[0]
    proc_ptr = routine.symbol_table.lookup("proc_ptr")
    proc_ptr_2 = routine.symbol_table.lookup("proc_ptr_2")

    # Make sure they are all DataSymbols with UnsupportedFortranType
    assert isinstance(proc_ptr_private, DataSymbol)
    assert isinstance(proc_ptr_private.datatype, UnsupportedFortranType)
    assert (proc_ptr_private.datatype.declaration ==
            "PROCEDURE(my_proc_interface)  :: proc_ptr_private")
    assert isinstance(proc_ptr_public, DataSymbol)
    assert isinstance(proc_ptr_public.datatype, UnsupportedFortranType)
    assert (proc_ptr_public.datatype.declaration ==
            "PROCEDURE(my_proc_interface)  :: proc_ptr_public")
    # The following two come from the same statements, but the declaration
    # should have been splited
    assert isinstance(proc_ptr, DataSymbol)
    assert isinstance(proc_ptr.datatype, UnsupportedFortranType)
    assert (proc_ptr.datatype.declaration ==
            "PROCEDURE(my_proc_interface), POINTER :: proc_ptr")
    assert isinstance(proc_ptr_2, DataSymbol)
    assert isinstance(proc_ptr_2.datatype, UnsupportedFortranType)
    assert (proc_ptr_2.datatype.declaration ==
            "PROCEDURE(my_proc_interface), POINTER :: proc_ptr_2 => null()")

    # Check that the module declarations have the correct visibility
    priv = Symbol.Visibility.PRIVATE
    pub = Symbol.Visibility.PUBLIC
    assert proc_ptr_private.visibility == priv
    assert proc_ptr_public.visibility == pub

    # Check that the routine declarations have the correct interface
    assert proc_ptr.is_static
    assert not proc_ptr_2.is_static
