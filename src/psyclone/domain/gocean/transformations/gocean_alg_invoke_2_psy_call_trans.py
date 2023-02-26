# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Transform a PSyclone GOcean algorithm-layer-specific invoke call
into a call to the corresponding PSy-layer routine.

'''

from psyclone.core import SymbolicMaths
from psyclone.domain.common.transformations import AlgInvoke2PSyCallTrans
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Literal, Reference, ArrayReference


class GOceanAlgInvoke2PSyCallTrans(AlgInvoke2PSyCallTrans):
    '''
    Transforms a GOceanAlgorithmInvokeCall into a standard Call to a generated
    PSy-layer routine.

    '''
    def get_arguments(self, node, options=None):
        '''Creates the GOcean processed (lowered) argument list from the
        argument lists of the kernels within the invoke call and the
        kernel metadata.

        :param node: a GOcean algorithm invoke call.
        :type node: :py:class:`psyclone.domain.common.algorithm.psyir.\
            AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises InternalError: if an unexpected argument is found.

        '''
        arguments = []
        sym_maths = SymbolicMaths.get()
        for kern in node.children:
            for arg in kern.children:
                if isinstance(arg, Literal):
                    # Literals are not passed by argument.
                    pass
                elif isinstance(arg, (Reference, ArrayReference)):
                    for existing_arg in arguments:
                        if sym_maths.equal(arg, existing_arg):
                            break
                    else:
                        arguments.append(arg.copy())
                else:
                    raise InternalError(
                        f"Expected Algorithm-layer kernel arguments to be "
                        f"a literal, reference or array reference, but "
                        f"found '{type(arg).__name__}'.")
        return arguments
