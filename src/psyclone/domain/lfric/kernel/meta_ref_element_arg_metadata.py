# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing the MetaRefElementArgMetadata class which
captures the argument values for the LFRic kernel
REFERENCE_ELEMENT metadata.

'''
from psyclone.domain.lfric.kernel.common_arg import CommonArg


class MetaRefElementArgMetadata(CommonArg):
    ''' xxx '''

    def __init__(self, reference_element):
        self._reference_element = reference_element

    def check_access(_):
        '''Not needed by this class '''
        pass

    def check_datatype(_):
        ''' Not needed by this class '''
        pass

    def create_from_fortran_string(fortran_string):
        ''' xxx '''
        fparser2_tree = MetaRefElementArgMetadata.create_fparser2(fortran_string)
        return MetaRefElementArgMetadata.create_from_fparser2(fparser2_tree)

    def create_from_fparser2(fparser2_tree):
        ''' xxx '''
        MetaRefElementArgMetadata.check_fparser2(
            fparser2_tree, type_name="reference_element_data_type")
        nargs = MetaRefElementArgMetadata.get_nargs(fparser2_tree)
        if nargs != 1:
            raise Exception("Must be 1")
        reference_element = MetaRefElementArgMetadata.get_arg(fparser2_tree, 0)
        return MetaRefElementArgMetadata(reference_element)
        
    def fortran_string(self):
        ''' xxx '''
        return(f"reference_element_data_type({self.reference_element})")

    @property
    def reference_element(self):
        ''' xxx '''
        return self._reference_element

    @reference_element.setter
    def reference_element(self, value):
        ''' xxx '''
        if value.lower() not in const.VALID_REF_ELEMENT_NAMES:
            raise ValueError("invalid value")
        self._reference_element = value.lower()
