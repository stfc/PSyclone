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
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.nodes import Loop, Assignment, Schedule, Reference


class HoistTrans(Transformation):
    '''This transformation takes an assignment and moves it outside of
    its parent loop if it is valid to do so.

    '''
    def apply(self, node, options=None):
        '''Applies the hoist transformation to an assignment node within a
        loop, moving the assignment outside of the loop if it is valid
        to do so.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        # Find the enclosing loop
        loop = node.ancestor(Loop)

        # Remove the assignment node
        node.detach()

        # Place the assignment node before the loop.
        loop.parent.children.insert(loop.position, node)

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a hoist
        transformation.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node is not an \
            assignment.
        :raises TransformationError: if the assignment is not within a \
            loop.
        :raises TransformationError: if the assignment is not a direct \
            child of the the loop.
        :raises TransformationError: if the assignment is not \
            independent of the loop.

        '''
        # TODO remove the following two lines and use the pre-existing
        # self._writer from the base class when PR #1263 adds this
        # change to master.
        from psyclone.psyir.backend.fortran import FortranWriter
        self._writer = FortranWriter()

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

        # TODO: The dependency check needs to be done properly. Below
        # is just a simple check that does not cover all cases. Talk
        # to Joerg about dependence analysis.

        # Dependency checks
        
        # 1: All accesses in the assignment should be independent of
        # the loop iterator.
        iterator = parent_loop.variable
        for reference in node.walk(Reference):
            if reference.symbol is iterator:
                raise TransformationError(
                    "The supplied assignment node '{0}' depends on the parent "
                    "loop iterator '{1}'.".format(self._writer(node), iterator.name))

        # Known limitations for this dependence analysis
        # 1: Assumes all accesses are dependent (so does not
        # take into account a(1) and a(2) being different)
        # 2: Ignores potential side-effects within calls.

        # 2: The modified reference symbol should not be read or
        # written within the loop before this assigment.
        lhs_reference = assignment.lhs
        prev_reference = lhs_reference.previous_reference
        if prev_reference.abs_position > parent_loop.abs_position:
            raise TransformationError(
                "Writer has a previous read or write within the loop")

        # 3: The reference symbols that are read should not be written
        # within the loop before this assigment.
        for reference in self.rhs.walk(Reference):
            prev_reference = reference.previous_reference(access=WRITE)
            if prev_reference.abs_position > parent_loop.abs_position:
                raise TransformationError(
                    "Reader has a previous write within the loop")

        # 4: The writer can't be read and then written in the loop afterwards
        lhs_reference = assignment.lhs
        next_reference = lhs_reference.following_reference
        if next_reference.READ and next_reference.abs_position <= after_parent_loop.abs_position:
            next_write_reference = lhs_reference.following_reference(access=WRITE)
            if next_reference.abs_position <= after_parent_loop.abs_position:
                raise TransformationError(
                    "Writer has a following read then write within the loop")
            
        # 5: The readers can't be written in the loop afterwards
        for reference in self.rhs.walk(Reference):
            next_reference = reference.following_reference(access=WRITE)
            if next_reference.abs_position <= after_parent_loop.abs_position:
                raise TransformationError(
                    "Reader has a subsequent write within the loop")
        
    @property
    def name(self):
        '''
        :returns: the name of this class.
        :rtype: str
        '''
        return self.__class__.__name__

    def __str__(self):
        return "Hoist an assignment outside of its parent loop"


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistTrans"]
