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

'''Raise generic PSyIR representing a kernel-layer routine to
PSyclone kernel-layer-specific PSyIR which uses specialised classes.

'''
from psyclone.domain.gocean.kernel import KernelMetadataSymbol
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Schedule, Container, Routine
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.gocean.kernel.psyir import GOceanKernelMetadata, GOceanKernel


class KernTrans(Transformation):
    '''Raise a generic PSyIR representation of a kernel-layer routine
    to a PSyclone version with specialised domain-specific nodes and
    symbols. This is currently limited to the specialisation of kernel
    metadata.

    '''
    def __init__(self):
        super().__init__()
        self._metadata_name = None

    def validate(self, node, options=None):
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: Union[:py:class:`psyclone.psyir.node.Routine`, \
            :py:class:`psyclone.psyir.node.Container`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str: str]]

        :raises TransformationError: if the metadata name has not been \
            set or does not exist in the code.
        :raises TransformationError: if the supplied node argument is \
            not a Routine or a Container.
        :raises TransformationError: if the supplied node argument has \
            a parent.

        '''
        if not self._metadata_name:
            raise TransformationError(
                "The kern_trans transformation requires the metadata name to "
                "be set before applying the transformation.")

        metadata_symbol = None
        for schedule_node in node.walk(Schedule):
            try:
                metadata_symbol = schedule_node.symbol_table.lookup(
                    self._metadata_name)
                break
            except KeyError:
                pass
        else:
            raise TransformationError(
                f"The metadata name ({self._metadata_name}) provided to the "
                f"transformation does not correspond to a symbol in the "
                f"supplied PSyIR.")

        # Validate the metadata. This is done as part of the setup()
        # method. This method can't be called until we specialise the
        # symbol, so we specialise with a copy of the symbol.
        #tmp_metadata_symbol = metadata_symbol.copy()
        #tmp_metadata_symbol.specialise(KernelMetadataSymbol)
        #tmp_metadata_symbol.setup()

        metadata = GOceanKernelMetadata.create_from_psyir(metadata_symbol.datatype)

        if not isinstance(node, Container):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be a Container node but found "
                f"'{type(node).__name__}'.")

        if node.parent:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be the root of a PSyIR tree but this node has a "
                f"parent.")

    @property
    def metadata_name(self):
        '''
        :returns: the name of the metadata that determines what will be \
            specialised to kernel-specific PSyIR.
        :rtype: str

        '''
        return self._metadata_name

    @metadata_name.setter
    def metadata_name(self, value):
        '''
        :param str value: sets the name of the metadata that \
            determines what will be specialised to kernel-specific PSyIR.
        '''
        if not isinstance(value, str):
            raise TypeError(
                f"The kern_trans transformation requires the metadata name "
                f"to be a string, but found '{type(value).__name__}'.")

        self._metadata_name = value

    def apply(self, node, options=None):
        ''' Apply transformation to the supplied PSyIR.

        :param node: a kernel represented in generic PSyIR.
        :type node: Union[:py:class:`psyclone.psyir.node.Routine`, \
            :py:class:`psyclone.psyir.node.Container`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str: str]]

        '''

        self.validate(node, options=options)

        # Find routine.
        routines = node.walk(Routine)
        routine = routines[0]
        
        # find metadata type
        # Find the metadata symbol. No need to check it is found as
        # this is done in the validate method.
        metadata_symbol = None
        for schedule_node in node.walk(Schedule):
            try:
                metadata_symbol = schedule_node.symbol_table.lookup(
                    self._metadata_name)
                break
            except KeyError:
                pass

        # Create metadata
        metadata = GOceanKernelMetadata.create_from_psyir(metadata_symbol.datatype)
        ## Specialise routine
        if routine.return_symbol:
            routine_symbol_name = routine.return_symbol.name
        else:
            routine_symbol_name = None
        routine.symbol_table._node = None
        go_kernel = GOceanKernel.create(
            routine.name, routine.symbol_table, routine.pop_all_children(),
            routine.is_program, routine_symbol_name)
        go_kernel.metadata = metadata
        routine.replace_with(go_kernel)

        # Remove metadata type
        # How????
        # metadata_symbol.detach()


__all__ = ['KernTrans']
