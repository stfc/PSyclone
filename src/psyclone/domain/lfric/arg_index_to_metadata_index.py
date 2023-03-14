# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
        super()._initialise({})
        cls._index = 0

    def __getattr__(cls, name):
        def func(*args):
            print(name)
            exit(1)
        return func
    #        if name in ["_scalar", "_field", ...]:
    #            cls._add_arg(*args)
    #        elif name in ["_cell_position", "_mesh_height", ...]:
    #            cls._index += 1
    #        else:
    #            getattr(cls, name)(*args)
    #    return func

    #@classmethod
    #def _scalar(cls, meta_arg):
    #    ''' xxx '''
    #    cls._add_arg(meta_arg)
        
    @classmethod
    def _field(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)

    @classmethod
    def _field_vector(cls, meta_arg):
        ''' xxx '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        for idx in range(int(meta_arg.vector_length)):
            cls._add_arg(meta_arg)

    @classmethod
    def _operator(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)

    @classmethod
    def _cma_operator(cls, meta_arg):
        ''' xxx '''
        cls._add_arg(meta_arg)

    @classmethod
    def _add_arg(cls, meta_arg):
        ''' xxx '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        cls._info[cls._index] = meta_arg_index
        cls._index += 1

    @classmethod
    def _cell_position(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _mesh_height(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _mesh_ncell2d_no_halos(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _mesh_ncell2d(cls):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _cell_map(cls):
        ''' xxx '''
        cls._index += 4

    @classmethod
    def _stencil_2d_unknown_extent(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_2d_max_extent(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_unknown_extent(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_unknown_direction(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil_2d(cls, meta_arg):
        ''' xxx '''
        cls._index += 1

    @classmethod
    def _stencil(cls, meta_arg):
        ''' xxx '''
        cls._index += 1
