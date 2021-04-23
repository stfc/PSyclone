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
# Author R. W. Ford STFC Daresbury Lab

'''This module contains LFRic Algorithm-layer-specific PSyIR classes.

'''
from psyclone.domain.common.algorithm import (AlgorithmInvokeCall,
                                              KernelFunctor)


class LFRicAlgorithmInvokeCall(AlgorithmInvokeCall):
    '''An invoke call from the LFRic Algorithm layer.

    :param routine: the routine that this call calls.
    :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param int index: the position of this invoke call relative to \
        other invokes in the algorithm layer.
    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param description: an optional description of the \
        LFRicAlgorithmInvokeCall. Defaults to None.
    :type description: str or NoneType

    '''
    _text_name = "LFRicAlgorithmInvokeCall"

    def __init__(self, routine, index, parent=None, description=None):
        super(LFRicAlgorithmInvokeCall, self).__init__(
            routine, index, parent=parent)
        self._description = description

    @classmethod
    def create(cls, routine, arguments, index, description=None):
        '''Create an instance of the calling class given valid instances of a
        routine symbol, a list of child nodes for its arguments and an
        optional description.

        :param routine: the routine that the instance being created \
            calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.
        :param description: a string describing the purpose of the \
            invoke or None if one is not provided. This is used to \
            create the name of the routine that replaces the \
            invoke. Defaults to None.
        :type name: str or NoneType

        :returns: an instance of the calling class.
        :rtype: \
            :py:class:`psyclone.psyir.nodes.LFRicAlgorithmInvokeCall` or a \
            subclass thereof.

        '''
        instance = super(LFRicAlgorithmInvokeCall, cls).create(
            routine, arguments, index)
        # pylint: disable=protected-access
        instance._description = description
        return instance

    def _def_routine_root_name(self):
        '''Internal function that returns the proposed PSy-layer routine
        name given the index of this invoke.

        :returns: the proposed processed routine name for this invoke.
        :rtype: str

        '''
        if self._description:
            name = self._description.lower().strip()
            if name[0] == '"' and name[-1] == '"' or \
               name[0] == "'" and name[-1] == "'":
                # fparser2 (issue #295) currently includes quotes as
                # part of a string, so strip them out.
                name = name[1:-1]
            name = name.replace(" ", "_")
        else:
            name = super(LFRicAlgorithmInvokeCall,
                         self)._def_routine_root_name()
        return name

    def node_str(self, colour=True):
        '''Construct a text representation of this node, optionally
        containing colour control codes. Specialise as this node has
        an additional description argument.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return "{0}[description=\"{1}\"]".format(self.coloured_name(colour),
                                                 self._description)


class LFRicBuiltinFunctor(KernelFunctor):
    '''Object containing an LFRic builtin call, a description of its
    required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicBuiltinFunctor"


class LFRicKernelFunctor(KernelFunctor):
    '''Object containing an LFRic kernel call, a description of its
    required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicKernelFunctor"
