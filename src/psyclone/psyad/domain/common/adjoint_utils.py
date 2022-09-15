# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

''' Provides various utilities in support of the PSyAD adjoint
    functionality. '''

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Container, FileContainer

#: The prefix we will prepend to a routine, container and metadata
#: names when generating the adjoint. If the original name contains
#: the tl prefix, then this is removed.
ADJOINT_NAME_PREFIX = "adj_"
TL_NAME_PREFIX = "tl_"


def create_adjoint_name(tl_name):
    '''Create an adjoint name from the supplied tangent linear name. This
    is done by stripping the TL_NAME_PREFIX from the name if it exists
    and then adding the ADJOINT_NAME_PREFIX. The adjoint name is also
    lower-cased.

    :param str: the tangent-linear name.

    :returns: the adjoint name.
    :rtype: str

    '''
    adj_name = tl_name.lower()
    if adj_name.startswith(TL_NAME_PREFIX):
        adj_name = adj_name[len(TL_NAME_PREFIX):]
    return ADJOINT_NAME_PREFIX + adj_name


def find_container(psyir):
    ''' Finds the first Container in the supplied PSyIR that is not a
    FileContainer. Also validates that the PSyIR contains at most one
    FileContainer which, if present, contains a Container.

    :returns: the first Container that is not a FileContainer or None if \
              there is none.
    :rtype: :py:class:`psyclone.psyir.nodes.Container` or NoneType

    :raises InternalError: if there are two Containers and the second is a \
                           FileContainer.
    :raises NotImplementedError: if there are two Containers and the first is \
                                 not a FileContainer.
    :raises NotImplementedError: if there are more than two Containers.

    '''
    containers = psyir.walk(Container)
    if not containers:
        return None

    if len(containers) == 1:
        if isinstance(containers[0], FileContainer):
            return None
        return containers[0]

    if len(containers) == 2:
        if isinstance(containers[1], FileContainer):
            raise InternalError(
                "The supplied PSyIR contains two Containers but the innermost "
                "is a FileContainer. This should not be possible.")
        if not isinstance(containers[0], FileContainer):
            raise NotImplementedError(
                "The supplied PSyIR contains two Containers and the outermost "
                "one is not a FileContainer. This is not supported.")
        return containers[1]

    raise NotImplementedError("The supplied PSyIR contains more than two "
                              "Containers. This is not supported.")


__all__ = ["create_adjoint_name", "find_container"]
