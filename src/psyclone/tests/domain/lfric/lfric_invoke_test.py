import pytest


from psyclone.configuration import Config
from psyclone.domain.lfric import FunctionSpace, LFRicConstants
from psyclone.domain.lfric.lfric_global_reduction import (
    LFRicGlobalMax, LFRicGlobalSum)
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.errors import GenerationError, InternalError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import LFRicOMPLoopTrans

TEST_API = "lfric"


def test_lfricinvoke_first_access():
    ''' Tests that we raise an error if LFRicInvoke.first_access(name) is
    called for an argument name that doesn't exist '''
    _, invoke = get_invoke("1.7_single_invoke_3scalar.f90",
                           api=TEST_API, dist_mem=True, idx=0)
    with pytest.raises(GenerationError) as excinfo:
        invoke.first_access("not_an_arg")
    assert ("Failed to find any kernel argument with name"
            in str(excinfo.value))


def test_lfricinvoke_arg_for_fs():
    ''' Test that LFRicInvoke.arg_for_funcspace() raises an error if
    passed an invalid or unused function space.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", api=TEST_API, idx=0,
                           dist_mem=True)
    with pytest.raises(InternalError) as err:
        _ = invoke.arg_for_funcspace(FunctionSpace("waah", "waah"))
    const = LFRicConstants()
    assert (f"Unrecognised function space 'waah'. The supported spaces are "
            f"{const.VALID_FUNCTION_SPACE_NAMES}" in str(err.value))
    with pytest.raises(GenerationError) as excinfo:
        invoke.arg_for_funcspace(FunctionSpace("wtheta", None))
    assert "No argument found on 'wtheta' space" in str(excinfo.value)


def test_lfricinvoke_uniq_declns_intent_inv_argtype():
    ''' Tests that we raise an error when LFRicInvoke.unique_declns_by_intent()
    is called with at least one invalid argument type. '''
    _, invoke = get_invoke("1.7_single_invoke_3scalar.f90",
                           api=TEST_API, dist_mem=True, idx=0)
    with pytest.raises(InternalError) as excinfo:
        invoke.unique_declns_by_intent(["gh_invalid"])
    const = LFRicConstants()
    assert (f"Invoke.unique_declns_by_intent() called with at least one "
            f"invalid argument type. Expected one of "
            f"{const.VALID_ARG_TYPE_NAMES} but found ['gh_invalid']."
            in str(excinfo.value))


def test_lfricinvoke_uniq_declns_intent_invalid_intrinsic():
    ''' Tests that we raise an error when Invoke.unique_declns_by_intent()
    is called for an invalid intrinsic type. '''
    _, invoke = get_invoke("1.7_single_invoke_3scalar.f90", idx=0,
                           api=TEST_API, dist_mem=True)
    with pytest.raises(InternalError) as excinfo:
        invoke.unique_declns_by_intent(["gh_scalar"], intrinsic_type="triple")
    const = LFRicConstants()
    assert (f"Invoke.unique_declns_by_intent() called with an invalid "
            f"intrinsic argument data type. Expected one of "
            f"{const.VALID_INTRINSIC_TYPES} but found 'triple'."
            in str(excinfo.value))


def test_lfricinvoke_uniq_declns_intent_ops(tmp_path):
    ''' Tests that LFRicInvoke.unique_declns_by_intent() returns the correct
    list of arguments for operator arguments. '''
    psy, invoke = get_invoke("4.4_multikernel_invokes.f90", idx=0,
                             api=TEST_API, dist_mem=True)
    args = invoke.unique_declns_by_intent(["gh_operator"])
    assert args['inout'] == []
    args_out = [arg.declaration_name for arg in args['out']]
    assert args_out == ['op']
    assert args['in'] == []

    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricinvoke_uniq_declns_intent_cma_ops(tmp_path):
    ''' Tests that LFRicInvoke.unique_declns_by_intent() returns the correct
    list of arguments for columnwise operator arguments. '''
    psy, invoke = get_invoke("20.5_multi_cma_invoke.f90", idx=0,
                             api=TEST_API, dist_mem=True)
    args = invoke.unique_declns_by_intent(["gh_columnwise_operator"])
    args_out = [arg.declaration_name for arg in args['out']]
    assert args_out == ['cma_op1']
    args_inout = [arg.declaration_name for arg in args['inout']]
    assert args_inout == ['cma_opc']
    args_in = [arg.declaration_name for arg in args['in']]
    assert args_in == ['cma_opb']

    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricinvoke_global_reductions():
    '''
    Check the construction of an LFRicInvoke containing a GlobalSum.
    '''
    _, invoke = get_invoke("15.9.2_X_innerproduct_X_builtin.f90", idx=0,
                           api=TEST_API, dist_mem=True)
    assert isinstance(invoke.schedule[1], LFRicGlobalSum)
    _, invoke = get_invoke("15.10.9_min_max_X_builtin.f90", idx=0,
                           api=TEST_API, dist_mem=True)
    assert isinstance(invoke.schedule[4], LFRicGlobalMax)


def test_lfricinvoke_setup_psy_layer_symbols(monkeypatch, dist_mem):
    '''
    Tests for the setup_psy_layer_symbols() method.
    '''
    config = Config.get()
    monkeypatch.setattr(config, "_reproducible_reductions", True)
    _, invoke = get_invoke("15.9.2_X_innerproduct_X_builtin.f90", idx=0,
                           api=TEST_API, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = LFRicOMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": True})
    # Apply an OpenMP Parallel for all loops
    rtrans.apply(schedule.children[0:2])
    # Check that setup_psy_layer_symbols() populates the symbol table.
    assert "f1_proxy" not in invoke.schedule.symbol_table
    invoke.setup_psy_layer_symbols()
    assert "f1_proxy" in invoke.schedule.symbol_table
    assert invoke.schedule.symbol_table.lookup_with_tag("omp_num_threads")
    assert "omp_get_max_threads" in invoke.schedule.symbol_table


def test_lfricinvoke_invalid_reduction(monkeypatch):
    '''
    Check that the LFRicInvoke constructor raises the expected error if it
    encounters an unknown type of reduction.

    '''
    # This is not easy to trigger so we resort to monkeypatching the definition
    # of one of the kernels to give it an invalid reduction type.
    from psyclone.domain.lfric.lfric_builtins import LFRicMaxvalXKern
    monkeypatch.setattr(LFRicMaxvalXKern, "_reduction_type", "wrong")

    with pytest.raises(InternalError) as err:
        _ = get_invoke("15.10.9_min_max_X_builtin.f90", idx=0,
                       api=TEST_API, dist_mem=True)
    assert ("Unrecognised reduction 'wrong' found for kernel 'maxval_x'"
            in str(err.value))


def test_lfricinvoke_halo_depths():
    '''
    Test that the construction of an LFRicInvoke sets up the symbols
    holding the various halo depths.
    '''
    _, invoke = get_invoke("1.4_into_halos_invoke.f90", idx=0,
                           api=TEST_API, dist_mem=True)
    assert invoke._alg_unique_halo_depth_args == ["hdepth"]
