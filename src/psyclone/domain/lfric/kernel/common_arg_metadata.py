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

'''Module containing the abstract CommonArgMetadata class which captures the
metadata associated with a generic LFRic argument. Supports the
creation, modification and Fortran output of such an argument.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.common_metadata import CommonMetadata
from psyclone.errors import InternalError


class CommonArgMetadata(CommonMetadata):
    '''Class to capture common LFRic kernel argument metadata.'''
 
    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Part_Ref

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from Fortran.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of this class.
        :rtype: subclass of \
            :py:class:`python.domain.lfric.kernel.CommonArgMetadata`

        '''
        fparser2_tree = cls.create_fparser2(fortran_string, cls.fparser2_class)
        return cls.create_from_fparser2(fparser2_tree)

    @staticmethod
    def check_value(value, name, valid_values):
        '''Check that the value argument is one of the values in the
        valid_values argument.

        :param str value: the value to be checked.
        :param str name: the name of the value.
        :param valid_values: a list of valid values.
        :type valid_values: List[str]

        :raises ValueError: if the value is not one of the values in \
            the valid_values list.

        '''
        if value.lower() not in valid_values:
            raise ValueError(
                f"The {name} value should be one of {valid_values}, but "
                f"found '{value}'.")

    @staticmethod
    def check_nargs(fparser2_tree, nargs):
        '''Checks that the metadata may has the number of arguments specified
        by the 'nargs' argument, otherwise an exception is raised.

        :param fparser2_tree: fparser2 tree capturing a metadata argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`
        :param int nargs: the number of expected arguments.

        :raises ValueError: if the kernel metadata does not contain \
            the expected number of arguments (nargs).

        '''
        if len(fparser2_tree.children[1].children) != nargs:
            raise ValueError(
                f"Expected kernel metadata to have {nargs} "
                f"arguments, but found "
                f"{len(fparser2_tree.children[1].children)} in "
                f"'{str(fparser2_tree)}'.")

    @staticmethod
    def check_fparser2(fparser2_tree, type_name,
                       encoding=Fortran2003.Part_Ref):
        '''Checks that the fparser2 tree is valid. The metadata will be in the
        form of a Fortran2003 Part_Ref or a Fortran2003
        Structure_Constructor.

        :param fparser2_tree: fparser2 tree capturing a metadata argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`
        :param str type_name: the name of the argument datatype.
        :param encoding: class in which the fparser2 tree should \
            be encoded. Defaults to fparser.two.Fortran2003.Part_Ref.
        :type encoding: Optional[ \
            :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`]

        :raises TypeError: if the fparser2_tree argument is not of the \
            type specified by the encoding argument.
        :raises ValueError: if the kernel metadata is not in \
            the form arg_type(...).

        '''
        if not isinstance(fparser2_tree, encoding):
            raise TypeError(
                f"Expected kernel metadata to be encoded as an "
                f"fparser2 {encoding.__name__} object but found type "
                f"'{type(fparser2_tree).__name__}' with value "
                f"'{str(fparser2_tree)}'.")
        if not fparser2_tree.children[0].tostr().lower() == type_name:
            raise ValueError(
                f"Expected kernel metadata to have the name "
                f"'{type_name}' and be in the form '{type_name}(...)', but "
                f"found '{str(fparser2_tree)}'.")

    @staticmethod
    def get_nargs(fparser2_tree):
        '''Returns the number of metadata arguments found in the fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the required metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`
        
        '''
        return len(fparser2_tree.children[1].children)

    @staticmethod
    def get_arg(fparser2_tree, index):
        '''Retrieves the metadata value found at the position specified by the
        index argument within the supplied fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the required metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param int index: the position of the metadata argument.

        :returns: the metadata value extracted from the fparser2 tree.
        :rtype: str

        '''
        return fparser2_tree.children[1].children[index].tostr()
