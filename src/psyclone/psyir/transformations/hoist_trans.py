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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module contains the HoistTrans transformation. HoistTrans
moves an assignment out of a parent loop if it is safe to do so. Hoist
is a name that is often used to describe this type of transformation.

'''

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Loop, Assignment, Schedule
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class HoistTrans(Transformation):
    '''This transformation takes an assignment and moves it outside of
    its parent loop if it is valid to do so. For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import HoistTrans
    >>> code = ("program test\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n,n)\\n"
    ...         "  real value\\n"
    ...         "  do i=1,n\\n"
    ...         "    value = 1.0\\n"
    ...         "    do j=1,n\\n"
    ...         "      a(i,j) = value\\n"
    ...         "    end do\\n"
    ...         "  end do\\n"
    ...         "end program\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = HoistTrans()
    >>> hoist.apply(psyir.walk(Assignment)[0])
    >>> print(FortranWriter()(psyir))
    program test
      integer :: i
      integer :: j
      integer :: n
      real, dimension(n,n) :: a
      real :: value
    <BLANKLINE>
      value = 1.0
      do i = 1, n, 1
        do j = 1, n, 1
          a(i,j) = value
        enddo
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Applies the hoist transformation to the supplied assignment node
        within a loop, moving the assignment outside of the loop if it
        is valid to do so. Issue #1387 will also look to extend this
        transformation to other types of node.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        # Find the enclosing loop (the validate() method has already
        # verified that there is one).
        loop = node.ancestor(Loop)

        # Remove the assignment node
        node.detach()

        # Place the assignment node before the loop.
        loop.parent.children.insert(loop.position, node)

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a hoist
        transformation. Dependency and checks still need to be added,
        see issue #1387.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not an \
            assignment.
        :raises TransformationError: if the assignment is not within a \
            loop.
        :raises TransformationError: if the assignment is not a direct \
            child of the the loop.

        '''
        # The node should be an assignment
        if not isinstance(node, Assignment):
            raise TransformationError(
                "The target of the HoistTrans transformation should be an "
                "assignment, but found '{0}'.".format(type(node).__name__))

        # The assignment should be within a loop.
        parent_loop = node.ancestor(Loop)
        if not parent_loop:
            raise TransformationError(
                "The supplied assignment node '{0}' should be within a loop, "
                "but no loop was found.".format(self._writer(node)))

        # The assignment should be directly within a loop i.e. no other
        # control logic inbetween.
        current = node.parent
        while current is not parent_loop:
            if not isinstance(current, Schedule):
                raise TransformationError(
                    "The supplied assignment node '{0}' should be directly "
                    "within a loop but found '{1}'."
                    "".format(self._writer(node), self._writer(current)))
            current = current.parent

        # TODO: Dependency checks, see issue #1387.
        self.validate_dependencies(node, parent_loop)

    def validate_dependencies(self, assignment, parent_loop):
        '''Checks if the variable usage allows hoistage, i.e. no dependency
        on loop variable (directly or indirectly).
        '''
        loop_var = parent_loop.variable
        loop_sig = Signature(loop_var.name)

        lhs = VariablesAccessInfo(assignment.lhs).all_signatures
        rhs = VariablesAccessInfo(assignment.rhs).all_signatures

        all_vars = VariablesAccessInfo(parent_loop)
        var_accesses = all_vars[Signature(assignment.lhs.name)]

        # Check if there is more than one write access to the variable
        # to be hoisted. That would likely create invalid code (unless
        # all assignments are the same, or the assigned value is not used)
        writes_to_var = [access for access in var_accesses
                         if access.access_type == AccessType.WRITE]
        if len(writes_to_var) > 1:
            raise TransformationError("There is more than one write to the "
                                      "variable '{0}'."
                                      .format(assignment.lhs.name))

        if loop_sig in lhs:
            raise TransformationError("The supplied assignment node '{0}' "
                                      "depends directly on the parent loop "
                                      "iterator '{1}' on the left-hand side."
                                      .format(self._writer(assignment).strip(),
                                              loop_var.name))

        if loop_sig in rhs:
            raise TransformationError("The supplied assignment node '{0}' "
                                      "depends directly on the parent loop "
                                      "iterator '{1}' on the right-hand side."
                                      .format(self._writer(assignment).strip(),
                                              loop_var.name))

        for sig in all_vars.all_signatures:
            accesses = all_vars[sig]
            if sig == Signature(assignment.lhs.name):
                # Ignore access to the assignment variable
                continue

            if sig == loop_sig:
                # Ignore access to loop variable
                continue
            if accesses.is_written() and sig in lhs:
                raise TransformationError(
                    "The supplied assignment node '{0}' "
                    "depends indirectly on the parent "
                    "loop iterator '{1}' on the "
                    "left-hand side via the variable '{2}'."
                    .format(self._writer(assignment).strip(),
                            loop_var.name,
                            str(sig)))

            if accesses.is_written() and sig in rhs:
                raise TransformationError(
                    "The supplied assignment node '{0}' "
                    "depends indirectly on the parent "
                    "loop iterator '{1}' on the "
                    "right-hand side via the variable '{2}'."
                    .format(self._writer(assignment).strip(),
                            loop_var.name,
                            str(sig)))

    def __str__(self):
        return "Hoist an assignment outside of its parent loop"


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistTrans"]
