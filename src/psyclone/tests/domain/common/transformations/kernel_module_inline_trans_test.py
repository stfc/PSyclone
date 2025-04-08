# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
#          C.M. Maynard, Met Office / University of Reading
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab

''' Tests of the KernelModuleInlineTrans PSyIR transformation. '''

import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.configuration import Config
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.errors import InternalError
from psyclone.psyGen import CodedKern, Kern
from psyclone.psyir.nodes import (
    Container, Routine, CodeBlock, Call, IntrinsicCall)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, ImportInterface, RoutineSymbol, REAL_TYPE,
    Symbol, SymbolError, SymbolTable)
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import Compile, count_lines, get_invoke


def test_module_inline_constructor_and_str():
    ''' Test that the transformation can be created and stringified. '''
    inline_trans = KernelModuleInlineTrans()
    assert (str(inline_trans) == "Copy the routine associated with a (Kernel) "
            "call into the Container of the call site.")


def test_validate_inline_error_if_not_kernel(fortran_reader):
    ''' Test that the inline transformation fails if the object being
    passed is not a kernel or a Call or if it is an IntrinsicCall.'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(kern_call)
    assert ("Target of a KernelModuleInlineTrans must be a sub-class of "
            "psyGen.CodedKern or psyir.nodes.Call but got 'GOLoop'" in
            str(err.value))
    # Test when it is an IntrinsicCall.
    psyir = fortran_reader.psyir_from_source('''\
module my_mod
  contains
subroutine my_sub
  real :: a, b
  a = sin(b)
end subroutine my_sub
end module my_mod
''')
    call = psyir.walk(Call)[0]
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(call)
    assert ("Cannot module-inline a call to an intrinsic (got 'SIN(b)')"
            in str(err.value))


def test_validate_with_imported_subroutine_call():
    ''' Test that the module inline transformation supports kernels with
    call nodes that reference an imported symbol. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.walk(CodedKern)[0]
    # Create a call to made up subroutine and module symbols
    kern_schedule = kern_call.get_kernel_schedule()
    mymod = kern_schedule.symbol_table.new_symbol(
            "mymod",
            symbol_type=ContainerSymbol)
    myfunc = kern_schedule.symbol_table.new_symbol(
            "myfunc",
            symbol_type=RoutineSymbol,
            interface=ImportInterface(mymod))
    kern_schedule.addchild(Call.create(myfunc))

    # The validate should succeed
    inline_trans = KernelModuleInlineTrans()
    inline_trans.validate(kern_call)


def test_validate_invalid_get_kernel_schedule(monkeypatch):
    '''Check that the validate method in the class KernelTrans raises an
    exception if the kernel code can not be retrieved.

    '''
    kernel_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel = kernels[0]

    def raise_symbol_error():
        '''Simple function that raises SymbolError.'''
        raise SymbolError("error")
    monkeypatch.setattr(kernel, "get_kernel_schedule", raise_symbol_error)
    with pytest.raises(TransformationError) as err:
        kernel_trans.apply(kernel)
    assert ("KernelModuleInlineTrans failed to retrieve PSyIR for Kernel "
            "'kernel_with_global_code' due to: " in str(err.value))


def test_validate_no_inline_global_var(parser):
    ''' Check that we refuse to in-line a kernel that accesses a global
    variable. '''
    inline_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(kernels[0])
    assert ("Kernel 'kernel_with_global_code' contains accesses to 'alpha' "
            "which is declared in the callee module scope. Cannot inline such "
            "a Kernel." in str(err.value))

    # Check that the issue is also reported if the symbol is inside a
    # Codeblock
    reader = FortranStringReader('''
    subroutine mytest
        alpha = alpha + 1
    end subroutine mytest''')
    stmt = parser(reader).children[0].children[1]
    block = CodeBlock([stmt], CodeBlock.Structure.STATEMENT)
    kernels[0].get_kernel_schedule().pop_all_children()
    kernels[0].get_kernel_schedule().addchild(block)

    with pytest.raises(TransformationError) as err:
        inline_trans.validate(kernels[0])
    assert ("'kernel_with_global_code' contains accesses to 'alpha' which is "
            "declared in the callee module scope. Cannot "
            "inline such a Kernel." in str(err.value))

    # Check that a symbol of unknown origin within a CodeBlock is caught.
    reader = FortranStringReader('''
    subroutine mytest
        unknown = unknown + 1
    end subroutine mytest''')
    stmt = parser(reader).children[0].children[1]
    block = CodeBlock([stmt], CodeBlock.Structure.STATEMENT)
    kernels[0].get_kernel_schedule().pop_all_children()
    kernels[0].get_kernel_schedule().addchild(block)
    table = kernels[0].get_kernel_schedule().symbol_table
    # Remove symbols that refer to 'go_wp' in outer scope.
    table._symbols.pop("field_old")
    table._symbols.pop("field_new")
    table._symbols.pop("field")
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(kernels[0])
    assert ("Kernel 'kernel_with_global_code' contains accesses to 'unknown' "
            "but the origin of this signature is unknown" in
            str(err.value))

    # But make sure that an IntrinsicCall routine name is not considered
    # a global symbol, as they are implicitly declared everywhere
    kernels[0].get_kernel_schedule().pop_all_children()
    kernels[0].get_kernel_schedule().addchild(
        IntrinsicCall.create(IntrinsicCall.Intrinsic.DATE_AND_TIME, []))
    inline_trans.validate(kernels[0])


def test_validate_name_clashes():
    ''' Test that if the module-inline transformation finds the kernel name
    already used in the Container scope, it raises the appropriate error'''
    # Use LFRic example with a repeated CodedKern
    psy, _ = get_invoke("4.6_multikernel_invokes.f90", "lfric", idx=0,
                        dist_mem=False)
    schedule = psy.invokes.invoke_list[0].schedule
    coded_kern = schedule.children[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()

    # Check that name clashes which are not subroutines are detected
    schedule.symbol_table.add(DataSymbol("ru_code", REAL_TYPE))
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(coded_kern)
    assert ("Cannot module-inline Kernel 'ru_code' because symbol "
            "'ru_code: DataSymbol<Scalar<REAL, UNDEFINED>, Automatic>' with "
            "the same name already exists and changing the name of "
            "module-inlined subroutines is not supported yet."
            in str(err.value))

    # TODO # 898. Manually force removal of previous imported symbol
    # symbol_table.remove() is not implemented yet.
    schedule.symbol_table._symbols.pop("ru_code")

    # Check that if a subroutine with the same name already exists and it is
    # not identical, it fails.
    schedule.parent.addchild(Routine.create("ru_code"))
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(coded_kern)
    assert ("Cannot inline subroutine 'ru_code' because another, different, "
            "subroutine with the same name already exists and versioning of "
            "module-inlined subroutines is not implemented "
            "yet.") in str(err.value)


def test_validate_unsupported_symbol_shadowing(fortran_reader, monkeypatch):
    ''' Test that the validate method refuses to transform a kernel which
    contains local variables that shadow a module name that would need to
    be brought into the subroutine scope.'''

    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[1].loop_body[0].loop_body[0]

    # Manually set the kernel to the desired problematic code
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod, only: r_def
        contains
        subroutine compute_cv_code()
            real :: external_mod
            real(kind=r_def) :: a
            a = external_mod + 1
        end subroutine compute_cv_code
    end module my_mod
    ''')
    routine = psyir.walk(Routine)[0]
    monkeypatch.setattr(kern_call, "_kern_schedule", routine)

    # and try to apply the transformation
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(kern_call)
    assert ("Kernel 'compute_cv_code' cannot be module-inlined because the "
            "subroutine shadows the symbol name of the module container "
            "'external_mod'." in str(err.value))

    # Repeat the same with a wildcard imports
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod
        contains
        subroutine compute_cv_code()
            real :: external_mod
            real(kind=r_def) :: a
            a = external_mod + 1
        end subroutine compute_cv_code
    end module my_mod
    ''')
    routine = psyir.walk(Routine)[0]
    monkeypatch.setattr(kern_call, "_kern_schedule", routine)

    # and try to apply the transformation
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(kern_call)
    assert ("Kernel 'compute_cv_code' cannot be module-inlined because the "
            "subroutine shadows the symbol name of the module container "
            "'external_mod'." in str(err.value))

    # But it is fine if it shadows itself
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod
        contains
        subroutine compute_cv_code()
            use external_mod
            real(kind=r_def) :: a
            a = external_mod + 1
        end subroutine compute_cv_code
    end module my_mod
    ''')
    routine = psyir.walk(Routine)[0]
    monkeypatch.setattr(kern_call, "_kern_schedule", routine)

    container = kern_call.ancestor(Container)
    assert "compute_cv_code" not in container.symbol_table

    inline_trans.apply(kern_call)

    # A RoutineSymbol should have been added to the Container symbol table.
    rsym = container.symbol_table.lookup("compute_cv_code")
    assert isinstance(rsym, RoutineSymbol)
    assert rsym.visibility == Symbol.Visibility.PRIVATE


def test_validate_local_routine(fortran_reader):
    '''Test that validate rejects a call to a routine that is already present
    in the current Container.'''
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        integer, parameter :: r_def = kind(1.0d0)
        contains
        subroutine compute_cv_code()
            real(kind=r_def) :: a
            call do_something(a)
        end subroutine compute_cv_code
        subroutine do_something(arg)
          real(kind=r_def), intent(inout) :: arg
          arg = arg + 3.14592_r_def
        end subroutine do_something
    end module my_mod
    ''')
    call = psyir.walk(Call)[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("routine 'do_something' cannot be module inlined into Container "
            "'my_mod' because there is no explicit import of it ('USE ..., "
            "ONLY: do_something' in Fortran) and a Routine with that name is "
            "already present in the Container." in str(err.value))


def test_validate_fail_to_get_psyir(fortran_reader):
    '''
    Test that the validate() method raises the expected error if the
    PSyIR for the called routine cannot be found.

    '''
    intrans = KernelModuleInlineTrans()
    code = '''\
    module a_mod
      use my_mod
      use other_mod
    contains
      subroutine a_sub()
        real, dimension(10) :: a
        call my_sub(a)
      end subroutine a_sub
    end module a_mod
    '''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(TransformationError) as err:
        intrans.validate(call)
    assert ("failed to retrieve PSyIR for routine 'my_sub' due to: Failed to "
            "find the source code of the unresolved routine 'my_sub' - looked "
            "at any routines in the same source file and attempted to resolve "
            "the wildcard imports from ['my_mod', 'other_mod']. However, "
            "failed to find the source for ['my_mod', 'other_mod']. The module"
            " search path is set to []. Searching for external routines that "
            "are only resolved at link time is not supported."
            in str(err.value))


def test_validate_nested_scopes(fortran_reader, monkeypatch):
    '''
    Test that validate() works correctly when two symbols in nested scopes
    have a name clash.

    TODO #2424 - this xfails at the moment because VariablesAccessInfo does not
    support nested scopes.

    '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[1].loop_body[0].loop_body[0]

    # Manually set the kernel to the desired problematic code
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod
        contains
        subroutine compute_cv_code()
            integer :: i
            do i = 1, 10
              a(i) = i
            end do
        end subroutine compute_cv_code
    end module my_mod
    ''')
    routine = psyir.walk(Routine)[0]
    # Move the definition of 'a' into the table associated with the loop.
    sym = routine.symbol_table.lookup("a")
    routine[0].loop_body.symbol_table.add(sym)
    del routine.symbol_table._symbols["a"]
    # Put a new, different symbol (with the same name) into the table of the
    # parent Container.
    routine.parent.scope.symbol_table.add(DataSymbol("a", REAL_TYPE))
    monkeypatch.setattr(kern_call, "_kern_schedule", routine)

    # The transformation should succeed (because the symbol named 'a' is
    # actually local to the routine. However, the dependence analysis thinks it
    # clashes with the symbol of the same name in the module scope.
    intrans = KernelModuleInlineTrans()
    try:
        intrans.validate(kern_call)
    except TransformationError:
        pytest.xfail(reason="TODO #2424 - VariablesAccessInfo does not support"
                     " nested scopes")


def test_module_inline_apply_transformation(tmpdir, fortran_writer):
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module using a transformation '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                             idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply the inline transformation
    kern_call = schedule.children[1].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)

    # The new inlined routine must now exist and be private.
    routine_sym = kern_call.ancestor(Container).symbol_table.lookup(
        "compute_cv_code")
    assert routine_sym
    assert routine_sym.visibility == Symbol.Visibility.PRIVATE
    assert kern_call.ancestor(Container).children[1].name == "compute_cv_code"
    assert (kern_call.ancestor(Container).symbol_table.
            lookup("compute_cv_code").is_modulevar)

    # We should see it in the output of both:
    # - the backend
    code = fortran_writer(schedule.root)
    assert 'subroutine compute_cv_code(i, j, cv, p, v)' in code

    # - the gen_code
    gen = str(psy.gen)
    assert 'SUBROUTINE compute_cv_code(i, j, cv, p, v)' in gen

    # And the import has been remove from both
    # check that the associated use no longer exists
    assert 'use compute_cv_mod, only: compute_cv_code' not in code
    assert 'USE compute_cv_mod, ONLY: compute_cv_code' not in gen

    # Do the gen_code check again because repeating the call resets some
    # aspects and we need to see if the second call still works as expected
    gen = str(psy.gen)
    assert 'SUBROUTINE compute_cv_code(i, j, cv, p, v)' in gen
    assert 'USE compute_cv_mod, ONLY: compute_cv_code' not in gen
    assert gen.count("SUBROUTINE compute_cv_code(") == 1

    # And it is valid code
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_module_inline_apply_kernel_in_multiple_invokes(tmpdir):
    ''' Check that module-inline works as expected when the same kernel
    is provided in different invokes'''
    # Use LFRic example with the kernel 'testkern_qr_mod' repeated once in
    # the first invoke and 3 times in the second invoke.
    psy, _ = get_invoke("3.1_multi_functions_multi_invokes.f90", "lfric",
                        idx=0, dist_mem=False)

    # By default the kernel is imported once per invoke
    gen = str(psy.gen)
    assert gen.count("USE testkern_qr_mod, ONLY: testkern_qr_code") == 2
    assert gen.count("END SUBROUTINE testkern_qr_code") == 0

    # Module inline kernel in invoke 1
    inline_trans = KernelModuleInlineTrans()
    schedule1 = psy.invokes.invoke_list[0].schedule
    for coded_kern in schedule1.walk(CodedKern):
        if coded_kern.name == "testkern_qr_code":
            inline_trans.apply(coded_kern)
    gen = str(psy.gen)

    # After this, one invoke uses the inlined top-level subroutine
    # and the other imports it (shadowing the top-level symbol)
    assert gen.count("USE testkern_qr_mod, ONLY: testkern_qr_code") == 1
    assert gen.count("END SUBROUTINE testkern_qr_code") == 1

    # Module inline kernel in invoke 2
    schedule1 = psy.invokes.invoke_list[1].schedule
    for coded_kern in schedule1.walk(CodedKern):
        if coded_kern.name == "testkern_qr_code":
            inline_trans.apply(coded_kern)
    gen = str(psy.gen)
    # After this, no imports are remaining and both use the same
    # top-level implementation
    assert gen.count("USE testkern_qr_mod, ONLY: testkern_qr_code") == 0
    assert gen.count("END SUBROUTINE testkern_qr_code") == 1

    # And it is valid code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_module_inline_apply_with_sub_use(tmpdir):
    ''' Test that we can module inline a kernel subroutine which
    contains a use statement'''
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", "gocean",
                             idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE bc_ssh_code(ji, jj, istep, ssha, tmask)' in gen
    # check that the use within the subroutine exists
    assert 'USE grid_mod' in gen
    # check that the associated psy use does not exist
    assert 'USE bc_ssh_mod, ONLY: bc_ssh_code' not in gen
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_module_inline_apply_same_kernel(tmpdir):
    '''Tests that correct results are obtained when an invoke that uses
    the same kernel subroutine more than once has that kernel
    inlined'''
    psy, invoke = get_invoke("test14_module_inline_same_kernel.f90",
                             "gocean", idx=0)
    schedule = invoke.schedule
    kern_call = schedule.coded_kernels()[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE compute_cu_code(' in gen
    # check that the associated psy "use" does not exist
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' not in gen
    # check that the subroutine has only been inlined once
    count = count_lines(gen, "SUBROUTINE compute_cu_code(")
    assert count == 1, "Expecting subroutine to be inlined once"
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_module_inline_apply_bring_in_non_local_symbols(
        fortran_reader, fortran_writer):
    ''' Test that when the inlined routine uses non local symbols, it brings
    them inside the subroutine when feasible. '''

    inline_trans = KernelModuleInlineTrans()

    # Bring all imports when we can't guarantee where symbols come from
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1
        use external_mod2
        use not_needed, only: not_used
        implicit none
        contains
        subroutine code()
            a = b + c
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1" in result
    assert "use external_mod2" in result
    assert "not_needed" not in result
    assert "not_used" not in result

    # Also if they are imports with 'only' and '=>' keywords
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: a
        use external_mod2, only: b => var1, c => var2
        use not_needed, only: not_used
        implicit none
        contains
        subroutine code()
            a = b + c
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : a" in result
    assert "use external_mod2, only : b=>var1, c=>var2" in result
    assert "not_needed" not in result
    assert "not_used" not in result

    # Same but now with some pre-existing module clashes
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: a
        use external_mod2, only: b => var1, c => var2
        use not_needed, only: not_used
        implicit none
        contains
        subroutine code()
             use external_mod1, only : d
             use external_mod2, only : var1
            a = b + c
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : a, d" in result
    assert "use external_mod2, only : b=>var1, c=>var2, var1" in result
    assert "not_needed" not in result
    assert "not_used" not in result

    # Also, if they are in datatype precision expressions
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: r_def
        use external_mod2, only: my_user_type
        use not_needed
        implicit none
        contains
        subroutine code()
            real(kind=r_def) :: a,b
            type(my_user_type) :: x
            a = b + x%data
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : r_def" in result
    assert "use external_mod2, only : my_user_type" in result
    assert "use not_needed" not in result

    # Also, if they are literal precision expressions
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: r_def
        use not_needed
        implicit none
        contains
        subroutine code()
            real :: a,b
            a = b + 1.0_r_def
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : r_def" in result
    assert "use not_needed" not in result

    # Also, if they are routine names
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: my_sub
        implicit none
        contains
        subroutine code()
            real :: a
            call random_number(a) !intrinsic
            call my_sub(a)
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : my_sub" in result

    # Also, if they are inside CodeBlocks
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: a, b
        implicit none
        contains
        subroutine code()
            write(*,*) a + b
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : a, b" in result

    # Check that symbol shadowing is respected (in this example
    # only 'c' must be brought into the subroutine)
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod1, only: a,b,c
        use not_needed
        implicit none
        contains
        subroutine code()
            real :: a,b
            a = b + c
        end subroutine code
    end module my_mod
    ''')

    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod1, only : c" in result

    # Another shadowing example where the local module should be
    # promoted to a wildcard import
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod
        contains
        subroutine code()
            use external_mod, only : r_def
            real(kind=r_def) :: a
            a = another_unresolved + 1
        end subroutine code
    end module my_mod
    ''')
    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod\n" in result
    assert "use external_mod, only : r_def" not in result

    # Routine References (in Calls) are also brought into the subroutine
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
        use external_mod, only: a
        contains
        subroutine code()
            call a()
        end subroutine code
    end module my_mod
    ''')
    routine = psyir.walk(Routine)[0]
    inline_trans._prepare_code_to_inline(routine)
    result = fortran_writer(routine)
    assert "use external_mod, only : a" in result


def test_module_inline_lfric(tmpdir, monkeypatch, annexed, dist_mem):
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the LFRic API. All previous tests
    use GOcean for testing.

    We also test when annexed is False and True as it affects how many halo
    exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("lfric")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", "lfric",
                             name="invoke_0", dist_mem=dist_mem)
    kern_call = invoke.schedule.walk(CodedKern)[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE ru_code(' in gen
    # check that the associated psy "use" does not exist
    assert 'USE ru_kernel_mod, only : ru_code' not in gen

    # And it is valid code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_module_inline_with_interfaces(tmpdir):
    ''' Test the module-inlining when the kernel points to an interface, we
    use an LFRic mixed-precision kernel as an example of this.

    '''
    psy, invoke = get_invoke("26.8_mixed_precision_args.f90", "lfric",
                             name="invoke_0", dist_mem=False)
    kern_call = invoke.schedule.walk(CodedKern)[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # Both the caller and the callee are in the file and use the specialized
    # implementation name.
    assert "CALL mixed_code_64(" in gen
    assert "SUBROUTINE mixed_code_64(" in gen

    # And it is valid code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_get_psyir_to_inline(monkeypatch):
    '''
    Test that _get_psyir_to_inline() raises the expected error if more than
    one potential routine implementation is found.

    '''
    sym = RoutineSymbol("my_sym")
    rout = Routine.create("my_sym", SymbolTable(), [])
    node = Call.create(sym)
    # For simplicity we just monkeypatch Call.get_callees() so that it appears
    # to return more than one Routine.
    monkeypatch.setattr(node, "get_callees", lambda: [rout, rout])
    with pytest.raises(TransformationError) as err:
        KernelModuleInlineTrans._get_psyir_to_inline(node)
    # The duplicated symbol name below is purely a result of the monkeypatch
    # - in reality these names will come from a generic interface and be
    # different.
    assert ("The target of the call to 'my_sym' cannot be inserted because "
            "multiple implementations were found: ['my_sym', 'my_sym']." in
            str(err.value))


@pytest.mark.parametrize("mod_use, sub_use",
                         [("use my_mod, only: my_sub, my_other_sub", ""),
                          ("", "use my_mod, only: my_sub, my_other_sub")])
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_psyir_mod_inline(fortran_reader, fortran_writer, tmpdir,
                          monkeypatch, mod_use, sub_use):
    '''
    Test module inlining a subroutine in generic PSyIR when the Call is
    within a Routine in a Container.

    '''
    intrans = KernelModuleInlineTrans()
    code = f'''\
    module a_mod
      {mod_use}
    contains
      subroutine a_sub()
        {sub_use}
        real, dimension(10) :: a
        call my_sub(a)
        call my_other_sub(a)
      end subroutine a_sub
    end module a_mod
    '''
    # Create the module containing the subroutine definition, write it to
    # file and set the search path so that PSyclone can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), '_include_paths', [path])

    with open(os.path.join(path, "my_mod.f90"),
              "w", encoding="utf-8") as mfile:
        mfile.write('''\
    module my_mod
    contains
      subroutine my_sub(arg)
        real, dimension(10), intent(inout) :: arg
        arg(1:10) = 1.0
      end subroutine my_sub
      subroutine my_other_sub(arg)
        real, dimension(10), intent(inout) :: arg
        arg(1:10) = 1.0
      end subroutine my_other_sub
    end module my_mod
    ''')
    psyir = fortran_reader.psyir_from_source(code)
    container = psyir.children[0]
    calls = psyir.walk(Call)
    intrans.apply(calls[0])

    routines = container.walk(Routine)
    assert len(routines) == 2
    assert routines[0].name in ["a_sub", "my_sub"]
    assert routines[1].name in ["a_sub", "my_sub"]
    # Local copy of routine must be private and in Container symbol table.
    rsym = container.symbol_table.lookup("my_sub")
    assert rsym.visibility == Symbol.Visibility.PRIVATE
    output = fortran_writer(psyir)
    assert "subroutine a_sub" in output
    assert "subroutine my_sub" in output
    assert "use my_mod, only : my_other_sub\n" in output
    # We can't test the compilation of this code because of the 'use my_mod.'

    # Check that we raise the expected error if the name of the obtained
    # subroutine doesn't match that of the caller. It is not
    # possible to create this situation (because _get_psyir_to_inline
    # will raise an exception) so we monkeypatch.
    # TODO #924 - ultimately we should be able to support this.
    def fake_get_code(node):
        '''
        :param node: the routine for which to get PSyIR.
        :type node: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        :returns: a fake routine name and the routine PSyIR.
        :rtype: Tuple(str, :py:class:`psyclone.psyir.nodes.Routine`)
        '''
        code_to_inline = node.get_callees()[0]
        return "broken", code_to_inline

    monkeypatch.setattr(KernelModuleInlineTrans, "_get_psyir_to_inline",
                        fake_get_code)
    with pytest.raises(InternalError) as err:
        intrans.apply(calls[1])
    assert ("Cannot module-inline call to 'broken' because its name does not "
            "match that of the callee: 'my_other_sub'. TODO #924."
            in str(err.value))


def test_mod_inline_no_container(fortran_reader, fortran_writer, tmpdir,
                                 monkeypatch):
    '''
    Test that the transformation works when the Call is within a Program (i.e.
    without an enclosing module).

    '''
    # Create the module containing the subroutine definition, write it to
    # file and set the search path so that PSyclone can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), '_include_paths', [path])

    with open(os.path.join(path, "my_mod.f90"),
              "w", encoding="utf-8") as mfile:
        mfile.write('''\
    module my_mod
    contains
      subroutine my_sub(arg)
        real, dimension(10), intent(inout) :: arg
        arg(1:10) = 1.0
      end subroutine my_sub
    end module my_mod
    ''')

    intrans = KernelModuleInlineTrans()
    code = '''\
    program my_prog
      use my_mod, only: my_sub
      real, dimension(10) :: a
      call my_sub(a)
    end program my_prog
    '''
    prog_psyir = fortran_reader.psyir_from_source(code)
    assert len(prog_psyir.children) == 1
    call = prog_psyir.walk(Call)[0]

    intrans.apply(call)

    assert len(prog_psyir.children) == 2
    assert set(child.name for child in prog_psyir.children) == {"my_sub",
                                                                "my_prog"}
    # Now that we've 'privatised' the target of the call, the code can be
    # compiled standalone.
    output = fortran_writer(prog_psyir)
    assert Compile(tmpdir).string_compiles(output)
