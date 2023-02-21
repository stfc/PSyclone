# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
# Modified I. Kavcic and A. Coughtrie, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

'''This module implements LFRic-specific implementation of GlobalReduction
class and relevant subclasses (Global Sum).
'''

import abc

from psyclone.configuration import Config
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import AssignGen, TypeDeclGen, UseGen
from psyclone.psyGen import GlobalReduction, InvokeSchedule


class LFRicGlobalReduction(GlobalReduction):
    '''
    LFRic-specific GlobalReduction class which can be added to and
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global reduction.
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: the parent node of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the scalar is not of "real" intrinsic type.

    '''
    def __init__(self, scalar, parent=None):
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                "It makes no sense to create an LFRicGlobalReduction object "
                "when distributed memory is not enabled (dm=False).")
        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"LFRicGlobalReduction.init(): A global reduction argument "
                f"should be a scalar but found argument of type "
                f"'{scalar.argument_type}'.")
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type != "real":
            raise GenerationError(
                f"LFRicGlobalReduction currently only supports real scalars, "
                f"but argument '{scalar.name}' in Kernel '{scalar.call.name}' "
                f"has '{scalar.intrinsic_type}' intrinsic type.")
        # Initialise the parent class
        super().__init__(scalar, parent=parent)

    @abc.abstractmethod
    def gen_code(self, parent):
        ''' Must be overridden by sub class. '''


class LFRicGlobalSum(LFRicGlobalReduction):
    '''
    LFRic-specific GlobalSum class which can be added to and
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global reduction.
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: the parent node of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the scalar is not of "real" intrinsic type.

    '''
    def __init__(self, scalar, parent=None):
        # Initialise the parent class
        super().__init__(scalar, parent=parent)

    def gen_code(self, parent):
        '''
        LFRic-specific code generation for this class.

        :param parent: f2pygen node to which to add AST nodes.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        name = self._scalar.name
        # Use InvokeSchedule SymbolTable to share the same symbol for all
        # GlobalSums in the Invoke.
        sum_name = self.ancestor(InvokeSchedule).symbol_table.\
            find_or_create_tag("global_sum").name
        sum_type = self._scalar.data_type
        sum_mod = self._scalar.module_name
        parent.add(UseGen(parent, name=sum_mod, only=True,
                          funcnames=[sum_type]))
        parent.add(TypeDeclGen(parent, datatype=sum_type,
                               entity_decls=[sum_name]))
        parent.add(AssignGen(parent, lhs=sum_name+"%value", rhs=name))
        parent.add(AssignGen(parent, lhs=name, rhs=sum_name+"%get_sum()"))


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = [
    'LFRicGlobalReduction',
    'LFRicGlobalSum']
