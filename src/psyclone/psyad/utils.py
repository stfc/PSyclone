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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Utilities for the PSyclone Adjoint (PSyAD) functionality.

'''

from psyclone.psyir.nodes import Reference


def node_is_active(node, active_variables):
    ''' Determines whether this node contains variables that are active.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables.
    :type active_variables: list of \
        :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: True if active and False otherwise.
    :rtype: bool

    '''
    for reference in node.walk(Reference):
        if reference.symbol in active_variables:
            return True
    return False


def node_is_passive(node, active_variables):
    '''Determines whether this node contains only variables that are
    passive.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables.
    :type active_variables: list of \
        :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: True if passive and False otherwise.
    :rtype: bool

    '''
    return not node_is_active(node, active_variables)


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["node_is_active", "node_is_passive"]
