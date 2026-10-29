# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the FileContainer node implementation.'''

import sys

from psyclone.errors import GenerationError
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

        still work as expected. However, it exposes the PSy hierarchy to
        users scripts, so this will eventually be deprecated.

        :return: the associated Invokes object.
        :rtype: :py:class:`psyclone.psyGen.Invokes`

        :raises GenerationError: if no InvokeSchedule was found.

        '''
        print("Deprecation warning: PSyclone script uses the legacy "
              "transformation signature 'def trans(psy)', please update the "
              "script to receive the root psyir node as argument.",
              file=sys.stderr)
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import InvokeSchedule
        invokes = self.walk(InvokeSchedule, stop_type=InvokeSchedule)
        if not invokes:
            raise GenerationError(
                f"No InvokeSchedule found in '{self.name}', does it come from"
                f" a PSyKAl file that conforms to the GOcean or LFRic API?")
        return invokes[0].invoke.invokes


# For AutoAPI documentation generation
__all__ = ['FileContainer']
