# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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


class CommonArgMetadata(CommonMetadata):
    '''Class to capture common LFRic kernel argument metadata.'''

    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Part_Ref

    @staticmethod
    def check_boolean(value, name):
        '''
        :param bool value: the value to validate.
        :param str name: the name of the entity being checked.

        :raises TypeError: if the provided value is not a boolean.

        '''
        if not isinstance(value, bool):
            raise TypeError(
                f"The {name} should be a boolean but found "
                f"'{type(value).__name__}'.")

    @staticmethod
    def check_nargs(fparser2_tree, nargs):
        '''Checks that the metadata has the number of arguments specified
        by the 'nargs' argument, otherwise an exception is raised.

        :param fparser2_tree: fparser2 tree capturing a metadata argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`
        :param nargs: the number of expected arguments. This can \
            either be a single value or a list containing a lower and an \
            upper value.
        :type nargs: int or Tuple[int, int]

        :raises ValueError: if the kernel metadata does not contain \
            the expected number of arguments (nargs).

        '''
        if isinstance(nargs, tuple):
            min_args = nargs[0]
            max_args = nargs[1]
            string = f"between {min_args} and {max_args}"
        else:
            min_args = nargs
            max_args = nargs
            string = f"{nargs}"

        num_args_found = len(fparser2_tree.children[1].children)
        if num_args_found < min_args or num_args_found > max_args:
            raise ValueError(
                f"Expected kernel metadata to have {string} "
                f"arguments, but found {num_args_found} in "
                f"'{str(fparser2_tree)}'.")

    @classmethod
    def check_fparser2_arg(cls, fparser2_tree, type_name):
        '''Checks that the fparser2 tree is valid. The metadata will be in the
        form of a Fortran2003 Part_Ref or a Fortran2003
        Structure_Constructor which captures a metadata argument.

        :param fparser2_tree: fparser2 tree capturing a metadata argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`
        :param str type_name: the name of the argument datatype.

        :raises ValueError: if the kernel metadata is not in \
            the form arg_type(...).

        '''
        CommonMetadata.check_fparser2(fparser2_tree, cls.fparser2_class)

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

        :returns: the metadata value extracted from the fparser2 tree \
            or None if it does not exist.
        :rtype: Optional[str]

        '''
        try:
            return fparser2_tree.children[1].children[index].tostr()
        except IndexError:
            # Metadata at the specified index does not exist.
            return None


__all__ = ["CommonArgMetadata"]
