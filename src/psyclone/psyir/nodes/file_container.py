# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Modified: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the FileContainer node implementation.'''

import sys
from psyclone.alg_gen import NoInvokesError
from psyclone.psyir.nodes.container import Container


class FileContainer(Container):
    '''PSyIR node to encapsulate the scope of a source file. In the
    PSyIR, a FileContainer is identical to a Container. However, it is
    useful to distinguish this type of container for the backends,
    which can have different constraints/syntax for general
    Containers and a FileContainer. For example, a FileContainer can
    not have any symbol table entries in Fortran.

    '''
    _text_name = "FileContainer"
    _colour = "yellow"

    def __str__(self):
        return f"FileContainer[name='{self.name}']\n"

    @property
    def invokes(self):
        ''' Return the Invokes object associated to this FileContainer.
        This is for compatibility with old psyclone transformation scripts.
        Previously, the entry point was PSy, and the script had to find the
        list of InvokeSchedules, now the entry point is the root FileContainer:

        before: PSy -> Invokes -> Invoke -> InvokeSchedule
        now:                FileContainer --^

        This method creates a shortcut:
            PSy -> Invokes -> Invoke -> InvokeSchedule
                   ^--- FileContainer --^

        So that previous:
            def trans(psy):
                psy.invokes.get_invoke('name').schedule

        still work as expected. However, it exposes the PSy hierachy to
        users scripts, so this will eventually be deprecated.

        :return: the associated Invokes object.
        :rtype: :py:class:`psyclone.psyGen.Invokes`

        :raises NoInvokesError: if no InvokeSchedule was found.

        '''
        print("Deprecation warning: PSyclone script uses the legacy "
              "transformation signature 'def trans(psy)', please update the "
              "script to receive the root psyir node as argument.",
              file=sys.stderr)
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import InvokeSchedule
        invokes = self.walk(InvokeSchedule, stop_type=InvokeSchedule)
        if not invokes:
            raise NoInvokesError(
                f"No InvokeSchedule found in '{self.name}', does it come from"
                f" a PSyKAl file that conforms to the GOcean or LFRic API?")
        return invokes[0].invoke.invokes


# For AutoAPI documentation generation
__all__ = ['FileContainer']
