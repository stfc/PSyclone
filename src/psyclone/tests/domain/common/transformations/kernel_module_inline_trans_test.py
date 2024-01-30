# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.configuration import Config
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyGen import CodedKern, Kern
from psyclone.psyir.nodes import (
    Container, Routine, CodeBlock, Call, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, RoutineSymbol, REAL_TYPE, SymbolError, ContainerSymbol,
    ImportInterface)
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import count_lines, get_invoke


def test_module_inline_constructor_and_str():
    ''' Test that the transformation can be created and stringified. '''
    inline_trans = KernelModuleInlineTrans()
    assert str(inline_trans) == \
        "Inline a kernel subroutine into the PSy module"


def test_validate_inline_error_if_not_kernel():
    ''' Test that the inline transformation fails if the object being
    passed is not a kernel'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(kern_call)
    assert ("Target of a KernelModuleInlineTrans must be a sub-class of "
            "psyGen.CodedKern but got 'GOLoop'" in str(err.value))


def test_validate_with_imported_subroutine_call():
    ''' Test that the module inline transformation supports kernels with
    call nodes that reference and imported symbol. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
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
    kern_schedule.addchild(Call(myfunc))

    # The validate should succeed
    inline_trans = KernelModuleInlineTrans()
    inline_trans.validate(kern_call)


def test_validate_invalid_get_kernel_schedule(monkeypatch):
    '''Check that the validate method in the class KernelTrans raises an
    exception if the kernel code can not be retrieved.

    '''
    kernel_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel = kernels[0]

    def raise_symbol_error():
        '''Simple function that raises SymbolError.'''
        raise SymbolError("error")
    monkeypatch.setattr(kernel, "get_kernel_schedule", raise_symbol_error)
    with pytest.raises(TransformationError) as err:
        kernel_trans.apply(kernel)
    assert ("KernelModuleInlineTrans failed to retrieve PSyIR for kernel "
            "'kernel_with_global_code' using the 'get_kernel_schedule' "
            "method due to" in str(err.value))


def test_validate_no_inline_global_var(parser):
    ''' Check that we refuse to in-line a kernel that accesses a global
    variable. '''
    inline_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(kernels[0])
    assert ("'kernel_with_global_code' contains accesses to 'alpha' which is "
            "declared in the same module scope. Cannot inline such a kernel."
            in str(err.value))

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
    assert ("'kernel_with_global_code' contains accesses to 'alpha' in a "
            "CodeBlock that is declared in the same module scope. Cannot "
            "inline such a kernel." in str(err.value))

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
    psy, _ = get_invoke("4.6_multikernel_invokes.f90", "dynamo0.3", idx=0,
                        dist_mem=False)
    schedule = psy.invokes.invoke_list[0].schedule
    coded_kern = schedule.children[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()

    # Check that name clashes which are not subroutines are detected
    schedule.symbol_table.add(DataSymbol("ru_code", REAL_TYPE))
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(coded_kern)
    assert ("Cannot module-inline subroutine 'ru_code' because symbol "
            "'ru_code: DataSymbol<Scalar<REAL, UNDEFINED>, Automatic>' with "
            "the same name already exists and changing the name of "
            "module-inlined subroutines is not supported yet."
            in str(err.value))

    # TODO # 898. Manually force removal of previous imported symbol
    # symbol_table.remove() is not implemented yet.
    schedule.symbol_table._symbols.pop("ru_code")

    # Check that if a subroutine with the same name already exists and it is
    # not identical, it fails.
    new_symbol = RoutineSymbol("ru_code")
    schedule.parent.symbol_table.add(new_symbol)
    schedule.parent.addchild(Routine(new_symbol.name))
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

    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
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
    inline_trans = KernelModuleInlineTrans()
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

    inline_trans.apply(kern_call)


def test_module_inline_apply_transformation(tmpdir, fortran_writer):
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module using a transformation '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
                             idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply the inline transformation
    kern_call = schedule.children[1].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)

    # The new inlined routine must now exist
    assert kern_call.ancestor(Container).symbol_table.lookup("compute_cv_code")
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
    psy, _ = get_invoke("3.1_multi_functions_multi_invokes.f90", "dynamo0.3",
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
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", "gocean1.0",
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
                             "gocean1.0", idx=0)
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
            a => b
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
    into the psy-layer in the LFRic (dynamo0.3) API. All previous tests
    use GOcean for testing.

    We also test when annexed is False and True as it affects how many halo
    exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", "dynamo0.3",
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
    psy, invoke = get_invoke("26.8_mixed_precision_args.f90", "dynamo0.3",
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
