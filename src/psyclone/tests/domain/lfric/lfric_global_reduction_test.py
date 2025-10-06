import os
from pathlib import Path
import pytest

from psyclone.domain.common.psylayer.global_reduction import GlobalReduction
from psyclone.domain.lfric.lfric_global_reduction import LFRicGlobalReduction
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import Arg, parse
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
        _ = LFRicGlobalReduction(GlobalReduction.Reduction.SUM,
                                 argument)
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
        _ = LFRicGlobalReduction(GlobalReduction.Reduction.SUM,
                                 argument)
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
        _ = LFRicGlobalReduction(GlobalReduction.Reduction.SUM, argument)
    assert ("It makes no sense to create an LFRicGlobalSum object when "
            "distributed memory is not enabled (dm=False)."
            in str(err.value))
