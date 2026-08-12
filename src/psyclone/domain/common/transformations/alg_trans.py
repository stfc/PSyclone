# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Specialise generic PSyIR representing an algorithm layer to a
PSyclone algorithm-layer-specific PSyIR which uses specialised classes.

'''
from psyclone.domain.common.transformations import RaisePSyIR2AlgTrans
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Call, Routine, Container, CodeBlock
from psyclone.psyir.transformations import TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class AlgTrans(Transformation):
    '''Transform a generic PSyIR representation of the Algorithm layer to
    a PSyclone version with specialised domain-specific nodes.

    '''
    def __init__(self):
        self._invoke_trans = RaisePSyIR2AlgTrans()

    def validate(self, node, options=None, **kwargs):
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node argument is \
            not a Routine or a Container.
        :raises TransformationError: if the supplied node argument has \
            a parent.

        '''
        if not options:
            self.validate_options(**kwargs)

        if not isinstance(node, (Routine, Container)):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be a Routine or Container node but found "
                f"'{type(node).__name__}'.")
        if node.parent:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be the root of a PSyIR tree but this node has a "
                f"parent.")

        for cb in node.walk(CodeBlock):
            if "invoke" in cb.get_symbol_names():
                raise TransformationError(
                    f"Error in {self.name} transformation. The supplied code"
                    f"cannot be uplifted to an Algorithm layer because "
                    f"there is an unrecognised Fortran construct containing an"
                    f" invoke: {cb.debug_string()}\n You could attempt "
                    f"rewriting the algorithm file with the invoke outside "
                    f" this construct.")

    def apply(self, node, options=None, **kwargs):
        ''' Apply transformation to the supplied PSyIR node.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options=options, **kwargs)
        idx = 0
        for call in node.walk(Call, stop_type=Call):
            if call.routine.name.lower() == "invoke":
                self._invoke_trans.apply(call, idx, options=options, **kwargs)
                idx += 1


__all__ = ['AlgTrans']
