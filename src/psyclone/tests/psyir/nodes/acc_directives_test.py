# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenACC PSyIR Directive nodes. '''

from __future__ import absolute_import
import os
import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import colored, Directive, ACCEnterDataDirective, \
    ACCKernelsDirective, Schedule, Loop, ACCUpdateDirective, \
    ACCParallelDirective
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import ACCLoopTrans, ACCEnterDataTrans, \
    ACCParallelTrans, ACCKernelsTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use a new Config instance.'''
    Config._instance = None
    yield()
    Config._instance = None


def test_acc_dir_node_str():
    ''' Test the node_str() method of OpenACC directives '''

    acclt = ACCLoopTrans()
    accdt = ACCEnterDataTrans()
    accpt = ACCParallelTrans()
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0,
                           dist_mem=False)
    colour = Directive._colour
    schedule = invoke.schedule

    # Enter-data
    accdt.apply(schedule)
    out = schedule[0].node_str()
    assert out.startswith(
        colored("Directive", colour)+"[ACC enter data]")

    # Parallel region around outermost loop
    accpt.apply(schedule[1])
    out = schedule[1].node_str()
    assert out.startswith(
        colored("Directive", colour)+"[ACC Parallel]")

    # Loop directive on outermost loop
    acclt.apply(schedule[1].dir_body[0])
    out = schedule[1].dir_body[0].node_str()
    assert out.startswith(
        colored("Directive", colour)+"[ACC Loop, independent]")

    # Loop directive with collapse
    acclt.apply(schedule[1].dir_body[0].dir_body[0], {"collapse": 2})
    out = schedule[1].dir_body[0].dir_body[0].node_str()
    assert out.startswith(
        colored("Directive", colour) + "[ACC Loop, collapse=2, "
                                       "independent]")


def test_acc_dag_names():
    ''' Check that we generate the correct dag names for ACC parallel,
    ACC enter-data and ACC loop directive Nodes '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    acclt = ACCLoopTrans()
    accdt = ACCEnterDataTrans()
    accpt = ACCParallelTrans()
    # Enter-data
    accdt.apply(schedule)
    assert schedule[0].dag_name == "ACC_data_1"
    # Parallel region
    accpt.apply(schedule[1])
    assert schedule[1].dag_name == "ACC_parallel_2"
    # Base directive class
    name = super(ACCParallelDirective, schedule[1]).dag_name
    assert name == "region_directive_2"
    # Loop directive
    acclt.apply(schedule[1].dir_body[0])
    assert schedule[1].dir_body[0].dag_name == "ACC_loop_4"
    # Base standalone directive class
    name = super(ACCEnterDataDirective, schedule[0]).dag_name
    assert name == "standalone_directive_1"

# Class ACCKernelsDirective start


# (1/1) Method __init__
def test_acckernelsdirective_init():
    '''Test an ACCKernelsDirective can be created and that the optional
    arguments are set and can be set as expected.

    '''
    directive = ACCKernelsDirective()
    assert directive._default_present
    assert directive.parent is None
    assert len(directive.children) == 1
    assert isinstance(directive.children[0], Schedule)
    directive = ACCKernelsDirective(default_present=False)
    assert not directive._default_present


# (1/1) Method dag_name
def test_acckernelsdirective_dagname():
    '''Check that the dag_name method in the ACCKernelsDirective class
    behaves as expected.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    trans = ACCKernelsTrans()
    _, _ = trans.apply(sched)
    assert sched.children[0].dag_name == "ACC_kernels_1"


# (1/1) Method node_str
def test_acckernelsdirective_node_str():
    '''Check that the node_str method in the ACCKernelsDirective class behaves
    as expected.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    trans = ACCKernelsTrans()
    _, _ = trans.apply(sched)

    out = sched[0].node_str()
    assert out.startswith(
        colored("Directive", Directive._colour)+"[ACC Kernels]")
    assert colored("Loop", Loop._colour) in sched[0].dir_body[0].node_str()
    assert "CodedKern" in sched[0].dir_body[0].loop_body[0].node_str()


# (1/1) Method gen_code
@pytest.mark.parametrize("default_present", [False, True])
def test_acckernelsdirective_gencode(default_present):
    '''Check that the gen_code method in the ACCKernelsDirective class
    generates the expected code. Use the dynamo0.3 API.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    trans = ACCKernelsTrans()
    _, _ = trans.apply(sched, {"default_present": default_present})

    code = str(psy.gen)
    string = ""
    if default_present:
        string = " default(present)"
    assert (
        "      !$acc kernels{0}\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n".format(string) in code)
    assert (
        "      END DO\n"
        "      !$acc end kernels\n" in code)


# (1/1) Method update
@pytest.mark.parametrize("default_present", [False, True])
def test_acckernelsdirective_update(parser, default_present):
    '''Check that the update method in the ACCKernelsDirective class
    generates the expected code. Use the nemo API.

    '''
    reader = FortranStringReader("program implicit_loop\n"
                                 "real(kind=wp) :: sto_tmp(5,5)\n"
                                 "sto_tmp(:,:) = 0.0_wp\n"
                                 "end program implicit_loop\n")
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    kernels_trans = ACCKernelsTrans()
    kernels_trans.apply(schedule.children[0:1],
                        {"default_present": default_present})
    gen_code = str(psy.gen)
    string = ""
    if default_present:
        string = " DEFAULT(PRESENT)"
    assert ("  !$ACC KERNELS{0}\n"
            "  sto_tmp(:, :) = 0.0_wp\n"
            "  !$ACC END KERNELS\n".format(string) in gen_code)

# Class ACCKernelsDirective end

# Class ACCEnterDataDirective start


# (1/1) Method __init__
def test_acc_datadevice_virtual():
    ''' Check that we can't instantiate an instance of
    ACCEnterDataDirective. '''
    # pylint:disable=abstract-class-instantiated
    with pytest.raises(TypeError) as err:
        ACCEnterDataDirective()
    # pylint:enable=abstract-class-instantiated
    assert ("instantiate abstract class ACCEnterDataDirective with abstract "
            "methods data_on_device" in str(err.value))

# (1/1) Method node_str
# Covered in test test_acc_dir_node_str

# (1/1) Method dag_name
# Covered in test_acc_dag_names


# (1/4) Method gen_code
def test_accenterdatadirective_gencode_1():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with a single loop, raises the expected exception as there is no
    following OpenACC Parallel or OpenACC Kernels directive as at
    least one is required. This test uses the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    acc_enter_trans.apply(sched)
    with pytest.raises(GenerationError) as excinfo:
        str(psy.gen)
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel or ACCKernels directives within the "
            "region." in str(excinfo.value))

    # Test that the same error is produced by the begin_string() which is used
    # by the PSyIR backend
    sched[0].lower_to_language_level()
    with pytest.raises(GenerationError) as excinfo:
        sched[0].begin_string()
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel or ACCKernels directives within the "
            "region." in str(excinfo.value))


# (2/4) Method gen_code
def test_accenterdatadirective_gencode_2():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with multiple loops, raises the expected exception, as there is no
    following OpenACC Parallel or OpenACCKernels directive and at
    least one is required. This test uses the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0').schedule
    acc_enter_trans.apply(sched)
    with pytest.raises(GenerationError) as excinfo:
        str(psy.gen)
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel or ACCKernels directives within the "
            "region." in str(excinfo.value))


# (3/4) Method gen_code
@pytest.mark.parametrize("trans", [ACCParallelTrans, ACCKernelsTrans])
def test_accenterdatadirective_gencode_3(trans):
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with a single loop, produces the expected code (there should be
    "copy in" data as there is a following OpenACC parallel or kernels
    directive). This test uses the dynamo0.3 API.

    '''
    acc_trans = trans()
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    _ = acc_trans.apply(sched.children)
    _ = acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert (
        "      !$acc enter data copyin(nlayers,a,f1_proxy,f1_proxy%data,"
        "f2_proxy,f2_proxy%data,m1_proxy,m1_proxy%data,m2_proxy,"
        "m2_proxy%data,ndf_w1,undf_w1,map_w1,ndf_w2,undf_w2,map_w2,"
        "ndf_w3,undf_w3,map_w3)\n" in code)


# (4/4) Method gen_code
@pytest.mark.parametrize("trans1,trans2",
                         [(ACCParallelTrans, ACCParallelTrans),
                          (ACCParallelTrans, ACCKernelsTrans),
                          (ACCKernelsTrans, ACCParallelTrans),
                          (ACCKernelsTrans, ACCKernelsTrans)])
def test_accenterdatadirective_gencode_4(trans1, trans2):
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with multiple loops and multiple OpenACC parallel and/or Kernel
    directives, produces the expected code (when the same argument is
    used in multiple loops there should only be one entry). This test
    uses the dynamo0.3 API.

    '''
    acc_trans1 = trans1()
    acc_trans2 = trans2()
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0').schedule
    _ = acc_trans1.apply([sched.children[1]])
    _ = acc_trans2.apply([sched.children[0]])
    _ = acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert (
        "      !$acc enter data copyin(nlayers,a,f1_proxy,f1_proxy%data,"
        "f2_proxy,f2_proxy%data,m1_proxy,m1_proxy%data,m2_proxy,m2_proxy%data,"
        "ndf_w1,undf_w1,map_w1,ndf_w2,undf_w2,map_w2,ndf_w3,undf_w3,map_w3,"
        "f3_proxy,f3_proxy%data)\n" in code)


# Class ACCUpdateDirective

def test_accupdatedirective_init():
    ''' Test the constructor of ACCUpdateDirective node'''

    # Check argument validations
    with pytest.raises(TypeError) as err:
        _ = ACCUpdateDirective("invalid", "host")
    assert ("The ACCUpdateDirective symbol argument must be a 'DataSymbol' "
            "but found 'str'." in str(err.value))

    symbol = DataSymbol("x", REAL_TYPE)
    with pytest.raises(ValueError) as err:
        _ = ACCUpdateDirective(symbol, "invalid")
    assert ("The ACCUpdateDirective direction argument must be a string with "
            "any of the values in '('host', 'device')' but found 'invalid'."
            in str(err.value))

    # Successful init
    directive = ACCUpdateDirective(symbol, "host")
    assert directive._symbol is symbol
    assert directive._direction == "host"


def test_accupdatedirective_begin_string():
    ''' Test the begin_string method of ACCUpdateDirective'''

    symbol = DataSymbol("x", REAL_TYPE)
    directive1 = ACCUpdateDirective(symbol, "host")
    directive2 = ACCUpdateDirective(symbol, "device")

    assert directive1.begin_string() == "acc update host(x)"
    assert directive2.begin_string() == "acc update device(x)"
