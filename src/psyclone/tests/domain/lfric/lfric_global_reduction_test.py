import os
from pathlib import Path
import pytest

from psyclone.core import AccessType
from psyclone.domain.common.psylayer.global_reduction import ReductionOp
from psyclone.domain.lfric.lfric_global_reduction import LFRicGlobalReduction
from psyclone.errors import GenerationError, InternalError
from psyclone.tests.utilities import get_invoke

BASE_PATH = Path(os.path.dirname(os.path.abspath(__file__)))
BASE_PATH = BASE_PATH / ".." / ".." / "test_files" / "lfric"

TEST_API = "lfric"


def test_lfricglobalsum_unsupported_argument():
    ''' Check that an instance of the LFRicGlobalSum class raises an
    exception for an unsupported argument type. '''
    # Get an instance of a non-scalar argument
    _, invoke = get_invoke("1.6.1_single_invoke_1_int_scalar.f90", TEST_API,
                           dist_mem=True, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(InternalError) as err:
        _ = LFRicGlobalReduction(ReductionOp.SUM, argument)
    assert ("LFRicGlobalSum.init(): A global sum argument should be a scalar "
            "but found argument of type 'gh_field'." in str(err.value))


def test_lfricglobalsum_unsupported_scalar():
    ''' Check that an instance of the LFRicGlobalSum class raises an
    exception if an unsupported scalar type is provided when distributed
    memory is enabled (dm=True).

    '''
    # Get an instance of an integer scalar
    _, invoke = get_invoke("1.6.1_single_invoke_1_int_scalar.f90",
                           TEST_API, dist_mem=True, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalReduction(ReductionOp.SUM, argument)
    assert ("LFRicGlobalSum currently only supports real scalars, but "
            "argument 'iflag' in Kernel 'testkern_one_int_scalar_code' "
            "has 'integer' intrinsic type." in str(err.value))


def test_lfricglobalsum_nodm_error():
    ''' Check that an instance of the LFRicGlobalSum class raises an
    exception if it is instantiated with no distributed memory enabled
    (dm=False).

    '''
    # Get an instance of a real scalar
    _, invoke = get_invoke("1.9_single_invoke_2_real_scalars.f90",
                           TEST_API, dist_mem=False, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalReduction(ReductionOp.SUM, argument)
    assert ("It makes no sense to create an LFRicGlobalSum object when "
            "distributed memory is not enabled (dm=False)."
            in str(err.value))


def test_globalsum_arg():
    ''' Check that the globalsum argument is defined as gh_readwrite and
    points to the GlobalSum node '''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    glob_sum_arg = glob_sum.operand
    assert glob_sum_arg.access == AccessType.READWRITE
    assert glob_sum_arg.call == glob_sum


def test_globalsum_args():
    '''Test that the globalsum class args method returns the appropriate
    argument '''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert len(global_sum.args) == 1
    assert global_sum.args[0] == global_sum.operand
