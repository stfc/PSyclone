# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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

'''This module implements a class that provides a mapping from the
index of a kernel argument to the index of its associated meta_arg
kernel metadata.

'''
from psyclone.domain.lfric.metadata_to_arguments_rules import \
    MetadataToArgumentsRules

# pylint: disable=too-few-public-methods


class ArgIndexToMetadataIndex(MetadataToArgumentsRules):
    '''Provides a mapping from the index of a kernel argument to the index
    of its associated meta_arg kernel metadata. Mappings are not
    provided to arguments that do not have associated meta_arg kernel
    metadata.

    '''

    @classmethod
    def _initialise(cls, _):
        '''Initial state (for the _info variable) is not used by this subclass
        i.e. the optional info argument to the mapping method is not
        used. Instead, initial state is always set to an empty
        dictionary. Also initialises any additional state for this
        class.

        '''
        # We could use super()._initialise({}) here but it is simpler
        # to just set the value of _info directly and it avoids pylint
        # complaining.
        cls._info = {}
        # Initialise the local _index variable. This is used to keep
        # track of the current argument index.
        cls._index = 0

    @classmethod
    def _scalar(cls, meta_arg):
        '''Argument providing an LFRic scalar value.

        :param meta_arg: the metadata associated with this scalar argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.ScalarArgMetadata`

        '''
        cls._add_arg(meta_arg)

    @classmethod
    def _field(cls, meta_arg):
        '''Argument providing an LFRic field.

        :param meta_arg: the metadata associated with this field argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._add_arg(meta_arg)

    @classmethod
    def _field_vector(cls, meta_arg):
        '''Arguments providing the components of an LFRic field vector.

        :param meta_arg: the metadata associated with this field \
            vector argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldVectorArgMetadata`

        '''
        for _ in range(int(meta_arg.vector_length)):
            cls._add_arg(meta_arg)

    @classmethod
    def _operator(cls, meta_arg):
        '''Arguments providing an LMA operator.

        :param meta_arg: the metadata associated with the operator \
            arguments.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.OperatorArgMetadata`

        '''
        cls._index += 1  # ncell_3d
        cls._add_arg(meta_arg)

    @classmethod
    def _cma_operator(cls, meta_arg):
        '''Arguments providing a columnwise operator.

        :param meta_arg: the metadata associated with the CMA operator \
            arguments.
        :type meta_arg: :py:class:`psyclone.domain.lfric.kernel.\
            ColumnwiseOperatorArgMetadata`

        '''
        cls._add_arg(meta_arg)
        cls._index += 1  # nrow
        if meta_arg.function_space_to != meta_arg.function_space_from:
            cls._index += 1  # ncol
        cls._index += 5  # bandwidth, alpha, beta, gamma_m, gamma_p

    @classmethod
    def _add_arg(cls, meta_arg):
        '''Utility method to add a mapping (to the _info dictionary) from the
        current argument index (_index) to the index of the supplied
        meta_arg metadata argument.

        :param meta_arg: the current meta_arg metadata.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.CommonMetaArgMetadata`

        '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        cls._info[cls._index] = meta_arg_index
        cls._index += 1

    @classmethod
    def _cell_position(cls):
        '''A cell position argument.'''
        cls._index += 1

    @classmethod
    def _mesh_height(cls):
        '''A mesh height argument.'''
        cls._index += 1

    @classmethod
    def _mesh_ncell2d_no_halos(cls):
        '''Argument providing the number of columns in the mesh ignoring
        halos.

        '''
        cls._index += 1

    @classmethod
    def _mesh_ncell2d(cls):
        '''Argument providing the number of columns in the mesh including
        halos.

        '''
        cls._index += 1

    @classmethod
    def _cell_map(cls):
        '''Arguments providing a mapping from coarse to fine mesh for the
        current column.

        '''
        cls._index += 4

    @classmethod
    def _stencil_cross2d_extent(cls, meta_arg):
        '''The field has a stencil access of type 'cross2d' of unknown extent
        and therefore requires the extent to be passed from the
        algorithm layer.

        :param meta_arg: the metadata associated with a field argument \
            with a cross2d stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._index += 1

    # pylint: disable=unused-argument
    @classmethod
    def _stencil_cross2d2d_max_extent(cls, meta_arg):
        '''The field has a stencil access of type 'cross2d' and requires the
        maximum size of a stencil extent to be passed from the
        algorithm layer.

        :param meta_arg: the metadata associated with a field argument \
            with a cross2d stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._index += 1

    @classmethod
    def _stencil_extent(cls, meta_arg):
        '''The field has a stencil access (that is not of type 'cross2d') of
        unknown extent and therefore requires the extent to be passed
        from the algorithm layer.

        :param meta_arg: the metadata associated with a field argument \
            with a stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._index += 1

    @classmethod
    def _stencil_xory1d_direction(cls, meta_arg):
        '''The field has a stencil access of type 'xory1d' and therefore
        requires the stencil direction to be passed from the algorithm
        layer.

        :param meta_arg: the metadata associated with a field argument \
            with a xory1d stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._index += 1

    @classmethod
    def _stencil_cross2d(cls, meta_arg):
        '''Stencil information that is always passed from the algorithm layer
        if a field has a stencil access of type 'cross2d'.

        :param meta_arg: the metadata associated with a field argument \
            with a stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._index += 1

    @classmethod
    def _stencil(cls, meta_arg):
        '''Stencil information that is always passed from the algorithm layer
        if a field has a stencil access that is not of type 'cross2d'.

        :param meta_arg: the metadata associated with a field argument \
            with a stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        cls._index += 1
