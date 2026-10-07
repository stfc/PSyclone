# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

import pytest

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Call, Node
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.callee_transformation_mixin import (
    CalleeTransformationMixin)
from psyclone.tests.utilities import get_invoke


class TestTrans(Transformation, CalleeTransformationMixin):
    '''
    A test 'transformation' class that inherits from CalleeTransformationMixin.
    '''
    def validate(self, node, **kwargs):
        '''
        Just calls the CalleeTransformationMixin validation.
        '''
        self._check_callee_implementation_is_local(node)

    def apply(self, node, **kwargs):
        '''
        Do nothing.
        '''


def test_not_call_or_kern():
    '''Check that the supplied node must be a Call or CodedKern.'''
    trans = TestTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(Node())
    assert ("Attempted to apply TestTrans to 'Node' which is not a Call or a "
            "CodedKern" in str(err.value))


def test_check_codedkern():
    '''Check that a CodedKern is handled correctly.'''
    trans = TestTrans()
    mod_inline_trans = KernelModuleInlineTrans()
    psy, invoke = get_invoke("26.8_mixed_precision_args.f90",
                             api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    with pytest.raises(TransformationError) as err:
        trans.validate(kernels[0])
    assert ("Cannot transform this Kernel call to 'mixed_code' because "
            "Routine 'mixed_code' is not in the same Container "
            "('mixed_precision_psy') as the call site. Try using "
            "KernelModuleInlineTrans" in str(err.value))
    # Module-inline the kernel in order to transform it.
    mod_inline_trans.apply(kernels[0])
    trans.validate(kernels[0])


def test_check_call(fortran_reader):
    '''Check operation with a generic Call.'''
    # When the target is unresolved.
    psyir = fortran_reader.psyir_from_source('''
    program my_prog
    call my_sub
    end program''')
    call = psyir.walk(Call)[0]
    trans = TestTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(call)
    assert ("no routine or interface matching this name could be found in the"
            " same Container as the call site." in str(err.value))
    # When the target is in the same FileContainer
    psyir = fortran_reader.psyir_from_source('''
    subroutine my_sub()
    end subroutine my_sub
    program my_prog
    call my_sub
    end program''')
    call = psyir.walk(Call)[0]
    trans.validate(call)
    # No parent Container
    call.detach()
    with pytest.raises(TransformationError) as err:
        trans.validate(call)
    assert ("call to 'my_sub' because there is no ancestor Container"
            in str(err.value))
    # Call is to an interface involving unresolved routines
    psyir = fortran_reader.psyir_from_source('''
    program my_prog
      use some_mod
      implicit none
      interface my_sub
        procedure :: other_sub1, other_sub2
      end interface
      call my_sub
    end program''')
    call = psyir.walk(Call)[0]
    with pytest.raises(TransformationError) as err:
        trans.validate(call)
    assert ("'my_sub' because no routine or interface matching this name"
            in str(err.value))
    # Call is to an interface to imported routines.
    psyir = fortran_reader.psyir_from_source('''
    program my_prog
      use some_mod, only: other_sub1, other_sub2
      implicit none
      interface my_sub
        procedure :: other_sub1, other_sub2
      end interface
      call my_sub
    end program''')
    call = psyir.walk(Call)[0]
    with pytest.raises(TransformationError) as err:
        trans.validate(call)
    assert ("'my_sub' because no routine or interface matching this name"
            in str(err.value))
