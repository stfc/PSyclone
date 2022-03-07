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
# Author R. W. Ford STFC Daresbury Lab

'''Specialise generic PSyIR representing a kernel-layer routine to
PSyclone kernel-layer-specific PSyIR which uses specialised classes.

'''
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk
from psyclone.domain.common.kernel import KernelMetadataSymbol
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import DataTypeSymbol
from psyclone.psyir.transformations import TransformationError
# from psyclone.domain.common.transformations import KernMetadataTrans


class KernTrans(Transformation):
    '''Transform a generic PSyIR representation of a kernel-layer routine
    to a PSyclone version with specialised domain-specific nodes and
    symbols. This is currently limited to the specialisation of kernel
    metadata.

    '''
    def __init__(self):
        self._metadata_name = None

    def validate(self, node, options=None):
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node argument is \
            not a Routine or a Container.
        :raises TransformationError: if the supplied node argument has \
            a parent.

        '''
        # TODO check metadata_name is set and is set to a valid value
        # that exists in this kernel.

        #if not isinstance(node, (Routine, Container)):
        #    raise TransformationError(
        #        "Error in {0} transformation. The supplied call argument "
        #        "should be a Routine or Container node but found '{1}'."
        #        "".format(self.name, type(node).__name__))
        #if node.parent:
        #    raise TransformationError(
        #        "Error in {0} transformation. The supplied node should be the "
        #        "root of a PSyIR tree but this node has a parent."
        #        "".format(self.name))

    @property
    def metadata_name(self):
        '''Returns the name of the metadata that determines what will be
        specialised to kernel-specific PSyIR.

        '''
        return self._metadata_name

    @metadata_name.setter
    def metadata_name(self, value):
        '''Sets the name of the metadata that determines what will be
        specialised to kernel-specific PSyIR.

        '''
        self._metadata_name = value

    def apply(self, psyir, options=None):
        ''' Apply transformation to the supplied PSyIR node.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(psyir, options=options)

        # Find the metadata symbol
        metadata_symbol = None
        from psyclone.psyir.nodes import Schedule
        for schedule_node in psyir.walk(Schedule):
            try:
                metadata_symbol = schedule_node.symbol_table.lookup(
                    self._metadata_name)
                break
            except KeyError:
                pass

        # TODO: Use a separate metadata transformation to do this?
        # Transform the kernel metadata
        metadata_symbol.specialise(KernelMetadataSymbol)
        metadata_symbol._setup()
        # psyir._metadata = metadata_symbol

        # Find the kernel code
        routine_name = metadata_symbol._routine_name
        for routine_node in psyir.walk(Routine):
            if routine_node.name == metadata_symbol._routine_name:
                break
        # psyir._code = routine_node
        # TODO raise generic PSyIR to GOcean-specific kernel PSyIR

    @property
    def name(self):
        '''
        :returns: a name identifying this transformation.
        :rtype: str

        '''
        return "KernTrans"


__all__ = ['KernTrans']
