# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides the LFRic-specific loop fusion transformation.
'''

from typing import Iterable

from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import LFRicConstants, LFRicLoop
from psyclone.psyGen import args_filter, InvokeSchedule, Kern
from psyclone.psyir.nodes import (
    ArrayOfStructuresReference, BinaryOperation, Call, IfBlock,
    StructureReference, Literal
)
from psyclone.psyir.symbols import ScalarType
from psyclone.psyir.transformations import LoopFuseTrans, TransformationError
from psyclone.transformations import check_intergrid
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class LFRicLoopFuseTrans(LoopFuseTrans):
    ''' LFRic API specialisation of the :py:class:`base class <LoopFuseTrans>`
    in order to fuse two LFRic loops after performing validity checks. For
    example:

    .. code-block :: python

        from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
        ftrans =  LFRicLoopFuseTrans()
        ftrans.apply(schedule[0], schedule[1])


    The input loops will be fused as long as:

    1. Neither input contains an inter-grid kernel.
    2. The loops contain valid function spaces.
    3. The function space must be the same or discontinuous or ANY_SPACE.
    4. All kernels must only write to a single field.
    5. The upper_bound_name must be the same.
    6. The halo depths must be the same.
    7. All reductions must be on the same field and the second
       loop can't read the result of a reduction in the first loop.
    8. There's no dependency between the loops that could require a halo
       exchange.
    9. If we have a non-discontinuous ANY_SPACE field, we need to be able to
       work out what the actual field space is or must be doing conditional
       fusion.
    10. The computed field space for ANY_SPACE fields must be the same space
        if not discontinuous.

    The optional argument `same_space` can be set as

    .. code-block :: python

        ftrans.apply(schedule[0], schedule[1], same_space=True)

    when applying the transformation. This overrides the need to compute the
    actual space for fields on ANY_SPACE.

    '''

    def __str__(self):
        return ("Fuse two adjacent loops together with LFRic-specific "
                "validity checks")

    def validate(self, nodes: Iterable[LFRicLoop],
                 options=None, **kwargs):
        ''' Performs various checks to ensure that it is valid to apply
        the LFRicLoopFuseTrans transformation to the supplied loops.

        :param nodes: the two loops to fuse.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if either of the supplied loops contains
                                     an inter-grid kernel.
        :raises TransformationError: if one or both function spaces have
                                     invalid names.
        :raises TypeError: if the `same_space` flag was set, but
                           does not apply because neither field
                           is on `ANY_SPACE` or the spaces are not
                           the same.
        :raises TransformationError: if the loops are over different spaces
                                     that are not both discontinuous and
                                     the loops both iterate over cells.
        :raises TransformationError: if the loops' upper bound names are
                                     not the same.
        :raises TransformationError: if the halo-depth indices of two loops
                                     are not the same.
        :raises TransformationError: if each loop already contains a reduction.
        :raises TransformationError: if the first loop has a reduction and
                                     the second loop reads the result of
                                     the reduction.
        :raises TransformationError: if either loop has more than one write
                                     access.
        :raises TransformationError: if there is a dependency between the
            loops.
        '''
        # pylint: disable=too-many-locals,too-many-branches
        # Call the parent class validation first

        # TODO #2668: Deprecate options dict.
        my_options = None
        if not options:
            self.validate_options(**kwargs)
            same_space = self.get_option("same_space", **kwargs)
        else:
            my_options = options.copy()
            my_options["force"] = True
            same_space = my_options.get("same_space", False)
        # TODO #2498: access information for LFRic kernels do not have any
        # index information for field accesses, and the loop fusion dependency
        # tests will therefore fail. To avoid this, disable the dependency test
        # in the generic loop fusion class for LFRic.
        # TODO #257: if the loop-fusion transformation is implemented to just
        # check that a variable with a stencil read-access is written, then
        # the test could be enabled for LFRic as well, so the force option
        # can be removed.
        force = True

        # TODO #2668: This can be removed when deprecating the options dict
        # as its handled by validate_options automatically.
        if same_space and not isinstance(same_space, bool):
            raise TypeError(
                f"Error in {self.name} transformation: The value of the "
                f"'same_space' flag must be either bool or None type, but the "
                f"type of flag provided was '{type(same_space).__name__}'.")
        super().validate(nodes, force=force, options=my_options)
        node1 = nodes[0]
        node2 = nodes[1]
        # Now test for LFRic-specific constraints

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
            # 2.3.1) Check whether specific function spaces are the
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

        # 3) Check that each kernel in each loop only write to one field.
        for kern in node1.kernels() + node2.kernels():
            # We use the args_filter function directly as its not defined for
            # kern.arguments
            kern_write_args = args_filter(
                kern.arguments.args,
                arg_accesses=AccessType.all_write_accesses()
            )
            if len(kern_write_args) > 1:
                raise TransformationError(
                    f"Error in {self.name}: Kernel '{kern.name}' in one of "
                    f"the input loops has {len(kern_write_args)} write "
                    f"arguments. {self.name} can only currently fuse when "
                    f"each kernel has at most one."
                )

        # 4) Check upper loop bounds
        if node1.upper_bound_name != node2.upper_bound_name:
            raise TransformationError(
                f"Error in {self.name} transformation: The upper bound names "
                f"are not the same. Found '{node1.upper_bound_name}' and "
                f"'{node2.upper_bound_name}'.")

        # 5) Check halo depths
        if node1.upper_bound_halo_depth != node2.upper_bound_halo_depth:
            node1_depth = (node1.upper_bound_halo_depth.debug_string() if
                           node1.upper_bound_halo_depth else "None")
            node2_depth = (node2.upper_bound_halo_depth.debug_string() if
                           node2.upper_bound_halo_depth else "None")
            raise TransformationError(
                f"Error in {self.name} transformation: The halo-depth indices "
                f"are not the same. Found "
                f"'{node1_depth}' and '{node2_depth}'.")

        # 6) Check for reductions
        arg_types = const.VALID_SCALAR_NAMES
        node1_red_args = node1.args_filter(arg_types=arg_types,
                                           arg_accesses=[AccessType.REDUCTION])
        node2_red_args = node2.args_filter(arg_types=arg_types,
                                           arg_accesses=[AccessType.REDUCTION])

        # Find the iteration space arguments if they exist, as they don't for
        # inputs like colour loops.
        same_field_name = False
        kern1 = node1.kernel
        args1 = kern1.arguments if kern1 else None
        arg1_field = args1.iteration_space_arg() if args1 else None
        kern2 = node2.kernel
        args2 = kern2.arguments if kern2 else None
        arg2_field = args2.iteration_space_arg() if args2 else None

        if arg1_field is not None and arg2_field is not None:
            same_field_name = arg1_field.name == arg2_field.name

        # If we have two reductions and they aren't on the same field then
        # we can't fuse them.
        if (node1_red_args and node2_red_args and not same_field_name):
            raise TransformationError(
                f"Error in {self.name} transformation: Cannot fuse loops "
                f"when each loop contains a reduction and those reductions "
                f"are to different fields.")
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

        # 7) Check for dependency between the loops. If we do then we can't
        # fuse them as we can't guarantee we won't change the result due to
        # no associativity between mathemtical operators.
        for kern in node2.kernels():
            for halo_field in (kern.ancestor(LFRicLoop).
                               unique_fields_with_halo_reads()):
                prev_arg_list = halo_field.backward_write_dependencies()
                if (prev_arg_list and
                        prev_arg_list[0].call.parent.is_descendant_of(node1)):
                    raise TransformationError(
                        f"Error in {self.name}: Cannot fuse loops as the "
                        f"loops have a dependency on the field "
                        f"{halo_field.name}."
                    )

    def apply(self, nodes: Iterable[LFRicLoop],
              options=None, same_space: bool = False,
              conditional_fusion: bool = False, **kwargs):
        ''' Applies the LFricLoopFuseTrans to the provided nodes.

        :param nodes: the two loops to fuse.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param same_space: this optional flag, set to `True`,
            asserts that an unknown iteration space (i.e. `ANY_SPACE`)
            matches the other iteration space. This is set at the user's own
            risk. If both iteration spaces are discontinuous the loops can be
            fused without having to use the `same_space` flag.
        :param conditional_fusion: if PSyclone can't be sure whether fusion is
            possible during processing (e.g. two user-defined kernels are on
            `ANY_SPACE` and PSyclone can't find other metadata to determine
            the spaces), this option tells PSyclone whether to add a runtime
            check to determine whether the loops should be fused or not.

        :raises TransformationError: if we have continuous ANY_SPACE field
            written to by one of the input loops, we can't find
            another access to the field in the invoke to compute the field, and
            same_space nor conditional_fusion are enabled.
        :raises TransformationError: if we have any continuous ANY_SPACE
            fields written to by the input loops and after resolving the space
            we determine the accesses are to fields on different spaces.
        '''
        # TODO #2668: Deprecate options dict.
        self.validate(nodes, options=options, same_space=same_space,
                      conditional_fusion=conditional_fusion, **kwargs)
        if options:
            same_space = options.get("same_space", False)
        node1 = nodes[0]
        node2 = nodes[1]
        # Get function space names
        node1_fs_name = node1.field_space.orig_name
        node2_fs_name = node2.field_space.orig_name
        # Check if either is on ANY SPACE
        const = LFRicConstants()
        node_on_any_space = (
            node1_fs_name in const.VALID_ANY_SPACE_NAMES or
            node2_fs_name in const.VALID_ANY_SPACE_NAMES
        )
        loop1_type = node1.loop_type
        loop2_type = node2.loop_type
        # If both loops are colour loops then just try to fuse them.
        if loop1_type == "colours" and loop2_type == "colours":
            # We always add force so need to make sure its not a duplicated
            # keyword argument.
            kwargs["force"] = True
            super().apply((node1, node2), same_space=same_space,
                          **kwargs)
            return

        # If same space is set and at least one of the nodes is on ANY_SPACE
        # then we try to fuse.
        if same_space and node_on_any_space:
            # We always add force so need to make sure its not a duplicated
            # keyword argument.
            kwargs["force"] = True
            super().apply((node1, node2), same_space=same_space,
                          **kwargs)
            return

        # Otherwise we need to find the iteration space arg.
        arg1_field = node1.kernel.arguments.iteration_space_arg()
        arg2_field = node2.kernel.arguments.iteration_space_arg()

        # If the iteration space argument has the same name then we can fuse.
        if arg1_field.name == arg2_field.name:
            # We always add force so need to make sure its not a duplicated
            # keyword argument.
            kwargs["force"] = True
            super().apply((node1, node2), same_space=same_space,
                          **kwargs)
            return

        # If neither field is on ANY_SPACE, then we can fuse if both fields
        # are on the same function space.
        if not node_on_any_space:
            fs1 = arg1_field.function_space.undf_name
            fs2 = arg2_field.function_space.undf_name
            if fs1 == fs2:
                # We always add force so need to make sure its not a
                # duplicated keyword argument.
                kwargs["force"] = True
                super().apply((node1, node2),
                              **kwargs)
                return

        # If both are discontinuous we can fuse.
        both_discontinuous = (
            node1_fs_name in const.VALID_DISCONTINUOUS_NAMES and
            node2_fs_name in const.VALID_DISCONTINUOUS_NAMES
        )
        if both_discontinuous:
            # We always add force so need to make sure its not a
            # duplicated keyword argument.
            kwargs["force"] = True
            super().apply((node1, node2),
                          **kwargs)
            return

        # If one or more is on any space then we need to search for the space.
        found_space1 = node1.field_space
        found_space2 = node2.field_space
        invokeschedule = node1.ancestor(InvokeSchedule)
        # Find all the Kerns in the InvokeSchedule
        kerns = invokeschedule.walk(Kern)

        if node1_fs_name in const.VALID_ANY_SPACE_NAMES:
            it_space_arg1 = node1.kernel.arguments.iteration_space_arg()
            found_space1 = None
        if node2_fs_name in const.VALID_ANY_SPACE_NAMES:
            it_space_arg2 = node2.kernel.arguments.iteration_space_arg()
            found_space2 = None

        # Check if the it_space_args appear in any of the other kernels
        for kern in kerns:
            for arg in kern.arguments.args:
                if found_space1 is None and arg.name == it_space_arg1.name:
                    # If one does, we can use it as the iteration space for
                    # the relevant arg.
                    fs = arg.function_space
                    fs_name = fs.orig_name
                    if fs_name not in const.VALID_ANY_SPACE_NAMES:
                        found_space1 = fs
                if found_space2 is None and arg.name == it_space_arg2.name:
                    # If one does, we can use it as the iteration space for
                    # the relevant arg.
                    fs = arg.function_space
                    fs_name = fs.orig_name
                    if fs_name not in const.VALID_ANY_SPACE_NAMES:
                        found_space2 = fs
                # If both are found then we can stop.
                if found_space1 is not None and found_space2 is not None:
                    break
            # If both are found then we can stop.
            if found_space1 is not None and found_space2 is not None:
                break

        if ((found_space1 is None or found_space2 is None) and
                not conditional_fusion):
            # Couldn't work out the space and aren't doing conditional fusion
            # so we should stop.
            raise TransformationError(
                f"Error in {self.name}: Couldn't lookup the field space "
                f"for one or more of the ANY_SPACE fields being operated on "
                f"and conditional fusion wasn't specified."
            )

        if found_space1 is not None and found_space2 is not None:
            if found_space1.orig_name == found_space2.orig_name:
                # They are on the same space so we can fuse them.
                # We always add force so need to make sure its not a
                # duplicated keyword argument.
                kwargs["force"] = True
                super().apply((node1, node2),
                              **kwargs)
                return
            else:
                # They're on different spaces so we can't fuse.
                raise TransformationError(
                    f"Error in {self.name}: The kernels provided are on "
                    f"different spaces so can't be fused. Computed spaces "
                    f"were '{found_space1.orig_name}' and "
                    f"'{found_space2.orig_name}'."
                )

        # Otherwise we have at least one node on any space, and have met
        # all other criteria for fusion, so we can fuse with a runtime check.
        arg1_sym = node1.scope.symbol_table.lookup(arg1_field.name)
        arg2_sym = node2.scope.symbol_table.lookup(arg2_field.name)

        # Create the test call for node1
        if arg1_field.vector_size > 1:
            call1 = Call.create(ArrayOfStructuresReference.create(
                        arg1_sym,
                        [Literal('1', ScalarType.integer_type())],
                        ["which_function_space"]))
        else:
            call1 = Call.create(StructureReference.create(
                arg1_sym, ["which_function_space"]))

        # Create the test call for node2
        if arg1_field.vector_size > 1:
            call2 = Call.create(ArrayOfStructuresReference.create(
                        arg2_sym,
                        [Literal('1', ScalarType.integer_type())],
                        ["which_function_space"]))
        else:
            call2 = Call.create(StructureReference.create(
                arg2_sym, ["which_function_space"]))
        # Create an IfBlock comparing them
        cond = BinaryOperation.create(
                BinaryOperation.Operator.EQ,
                call1, call2
        )
        # Create copies of the input nodes
        node1_copy = node1.copy()
        node2_copy = node2.copy()
        # If the fields are the same then we execute the fused loops,
        # otherwise we do the loops individually.
        ifblock = IfBlock.create(cond, [], [node1_copy, node2_copy])
        node1.replace_with(ifblock)
        node2.detach()
        ifblock.if_body.addchild(node1)
        ifblock.if_body.addchild(node2)
        # We always add force so need to make sure its not a duplicated
        # keyword argument.
        kwargs["force"] = True
        super().apply((node1, node2), same_space=same_space,
                      **kwargs)


# For automatic documentation generation
__all__ = ["LFRicLoopFuseTrans"]
