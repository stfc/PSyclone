# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
# Modified by I. Kavcic and L.Turner, Met Office
# Modified by J. Henrichs, Bureau of Meteorology

'''This module provides the LFRic-specific loop fusion transformation.
'''

from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.transformations import LoopFuseTrans, TransformationError
from psyclone.transformations import check_intergrid


class LFRicLoopFuseTrans(LoopFuseTrans):
    ''' Dynamo0.3 API specialisation of the
    :py:class:`base class <LoopFuseTrans>` in order to fuse two Dynamo
    loops after performing validity checks. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "dynamo0.3"
    >>> FILENAME = "alg.x90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
    >>> ftrans =  LFRicLoopFuseTrans()
    >>>
    >>> ftrans.apply(schedule[0], schedule[1])
    >>> print(schedule.view())

    The optional argument `same_space` can be set as

    >>> ftrans.apply(schedule[0], schedule[1], {"same_space": True})

    when applying the transformation.

    '''

    def __str__(self):
        return ("Fuse two adjacent loops together with Dynamo-specific "
                "validity checks")

    def validate(self, node1, node2, options=None):
        ''' Performs various checks to ensure that it is valid to apply
        the LFRicLoopFuseTrans transformation to the supplied loops.

        :param node1: the first Loop to fuse.
        :type node1: :py:class:`psyclone.domain.lfric.LFRicLoop`
        :param node2: the second Loop to fuse.
        :type node2: :py:class:`psyclone.domain.lfric.LFRicLoop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["same_space"]: this optional flag, set to `True`, \
            asserts that an unknown iteration space (i.e. `ANY_SPACE`) \
            matches the other iteration space. This is set at the user's own \
            risk. If both iteration spaces are discontinuous the loops can be \
            fused without having to use the `same_space` flag.

        :raises TransformationError: if either of the supplied loops contains \
                                     an inter-grid kernel.
        :raises TransformationError: if one or both function spaces have \
                                     invalid names.
        :raises TransformationError: if the `same_space` flag was set, but \
                                     does not apply because neither field \
                                     is on `ANY_SPACE` or the spaces are not \
                                     the same.
        :raises TransformationError: if one or more of the iteration spaces \
                                     is unknown (`ANY_SPACE`) and the \
                                     `same_space` flag is not set to `True`.
        :raises TransformationError: if the loops are over different spaces \
                                     that are not both discontinuous and \
                                     the loops both iterate over cells.
        :raises TransformationError: if the loops' upper bound names are \
                                     not the same.
        :raises TransformationError: if the halo-depth indices of two loops \
                                     are not the same.
        :raises TransformationError: if each loop already contains a reduction.
        :raises TransformationError: if the first loop has a reduction and \
                                     the second loop reads the result of \
                                     the reduction.
        '''
        # pylint: disable=too-many-locals,too-many-branches
        # Call the parent class validation first

        if not options:
            options = {}
        same_space = options.get("same_space", False)
        if same_space and not isinstance(same_space, bool):
            raise TransformationError(
                f"Error in {self.name} transformation: The value of the "
                f"'same_space' flag must be either bool or None type, but the "
                f"type of flag provided was '{type(same_space).__name__}'.")
        super(LFRicLoopFuseTrans, self).validate(node1, node2,
                                                 options=options)
        # Now test for Dynamo-specific constraints

        # 1) Check that we don't have an inter-grid kernel
        check_intergrid(node1)
        check_intergrid(node2)

        # 2) Check function space names
        node1_fs_name = node1.field_space.orig_name
        node2_fs_name = node2.field_space.orig_name
        # 2.1) Check that both function spaces are valid
        const = LFRicConstants()
        if not (node1_fs_name in const.VALID_FUNCTION_SPACE_NAMES and
                node2_fs_name in const.VALID_FUNCTION_SPACE_NAMES):
            raise TransformationError(
                f"Error in {self.name} transformation: One or both function "
                f"spaces '{node1_fs_name}' and '{node2_fs_name}' have invalid "
                f"names.")
        # Check whether any of the spaces is ANY_SPACE. Loop fusion over
        # ANY_SPACE is allowed only when the 'same_space' flag is set
        node_on_any_space = node1_fs_name in \
            const.VALID_ANY_SPACE_NAMES or \
            node2_fs_name in const.VALID_ANY_SPACE_NAMES
        # 2.2) If 'same_space' is true check that both function spaces are
        # the same or that at least one of the nodes is on ANY_SPACE. The
        # former case is convenient when loop fusion is applied generically.

        if same_space:
            if node1_fs_name == node2_fs_name:
                pass
            elif not node_on_any_space:
                raise TransformationError(
                    f"Error in {self.name} transformation: The 'same_space' "
                    f"flag was set, but does not apply because "
                    f"neither field is on 'ANY_SPACE'.")
        # 2.3) If 'same_space' is not True then make further checks
        else:
            # 2.3.1) Check whether one or more of the function spaces
            # is ANY_SPACE without the 'same_space' flag
            if node_on_any_space:
                raise TransformationError(
                    f"Error in {self.name} transformation: One or more of the "
                    f"iteration spaces is unknown ('ANY_SPACE') so loop fusion"
                    f" might be invalid. If you know the spaces are the same "
                    f"then please set the 'same_space' optional argument to "
                    f"'True'.")
            # 2.3.2) Check whether specific function spaces are the
            # same. If they are not, the loop fusion is still possible
            # but only when both function spaces are discontinuous
            # (w3, w2v, wtheta or any_discontinuous_space) and the upper
            # loop bounds are the same (checked further below).
            if node1_fs_name != node2_fs_name:
                if not (node1_fs_name in
                        const.VALID_DISCONTINUOUS_NAMES and
                        node2_fs_name in
                        const.VALID_DISCONTINUOUS_NAMES):
                    raise TransformationError(
                        f"Error in {self.name} transformation: Cannot fuse "
                        f"loops that are over different spaces "
                        f"'{node1_fs_name}' and '{node2_fs_name}' unless they "
                        f"are both discontinuous.")

        # 3) Check upper loop bounds
        if node1.upper_bound_name != node2.upper_bound_name:
            raise TransformationError(
                f"Error in {self.name} transformation: The upper bound names "
                f"are not the same. Found '{node1.upper_bound_name}' and "
                f"'{node2.upper_bound_name}'.")

        # 4) Check halo depths
        if node1.upper_bound_halo_depth != node2.upper_bound_halo_depth:
            raise TransformationError(
                f"Error in {self.name} transformation: The halo-depth indices "
                f"are not the same. Found '{node1.upper_bound_halo_depth}' "
                f"and '{node2.upper_bound_halo_depth}'.")

        # 5) Check for reductions
        arg_types = const.VALID_SCALAR_NAMES
        all_reductions = AccessType.get_valid_reduction_modes()
        node1_red_args = node1.args_filter(arg_types=arg_types,
                                           arg_accesses=all_reductions)
        node2_red_args = node2.args_filter(arg_types=arg_types,
                                           arg_accesses=all_reductions)

        if node1_red_args and node2_red_args:
            raise TransformationError(
                f"Error in {self.name} transformation: Cannot fuse loops "
                f"when each loop already contains a reduction.")
        if node1_red_args:
            for reduction_arg in node1_red_args:
                other_args = node2.args_filter()
                for arg in other_args:
                    if reduction_arg.name == arg.name:
                        raise TransformationError(
                            f"Error in {self.name} transformation: Cannot fuse"
                            f" loops as the first loop has a reduction and "
                            f"the second loop reads the result of the "
                            f"reduction.")


# For automatic documentation generation
__all__ = ["LFRicLoopFuseTrans"]
