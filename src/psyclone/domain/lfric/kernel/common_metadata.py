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

'''Module containing the CommonMetadata base class which captures the
common functionality for LFRic kernel metadata.

'''
from abc import ABC, abstractmethod

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two.utils import NoMatchError, FortranSyntaxError


# TODO issue #1886. This class and its subclasses may have
# commonalities with the GOcean metadata processing.
class CommonMetadata(ABC):
    '''Abstract class to capture common LFRic kernel metadata.'''

    # The fparser2 class that captures this metadata.
    fparser2_class = None

    @staticmethod
    def check_fparser2(fparser2_tree, encoding):
        '''Checks that the fparser2 tree is valid.

        :param fparser2_tree: fparser2 tree capturing a metadata argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Base`
        :param encoding: class in which the fparser2 tree should \
            be encoded.
        :type encoding: :py:class:`fparser.two.Fortran2003.Base`

        :raises TypeError: if the fparser2_tree argument is not of the \
            type specified by the encoding argument.

        '''
        if not isinstance(fparser2_tree, encoding):
            raise TypeError(
                f"Expected kernel metadata to be encoded as an "
                f"fparser2 {encoding.__name__} object but found type "
                f"'{type(fparser2_tree).__name__}' with value "
                f"'{str(fparser2_tree)}'.")

    @staticmethod
    def validate_scalar_value(value, valid_values, name):
        '''Check that the value argument is one of the values supplied in the
        valid_values list.

        :param str value: the value being checked.
        :param List[str] valid_values: a list of valid values.
        :param str name: the name of the metadata being checked

        :raises TypeError: if the value is not a string.
        :raises ValueError: if the supplied value is not one of the \
            values in the valid_values list.

        '''
        if not isinstance(value, str):
            raise TypeError(f"The '{name}' value should be of type str, but "
                            f"found '{type(value).__name__}'.")
        if value.lower() not in valid_values:
            raise ValueError(
                f"The '{name}' metadata should be a recognised "
                f"value (one of {valid_values}) "
                f"but found '{value}'.")

    @staticmethod
    def create_fparser2(fortran_string, encoding):
        '''Creates an fparser2 tree from a Fortran string. The resultant
        parent node of the tree will be the same type as the encoding
        argument if the string conforms to the encoding, otherwise an
        exception will be raised.

        TODO: issue #1965: relocate this method as it is not specific
        to metadata processing.

        :param str fortran_string: a string containing the metadata in \
           Fortran.
        :param encoding: the parent class with which we will encode the \
            Fortran string.
        :type encoding: subclass of :py:class:`fparser.two.Fortran2003.Base`

        :returns: an fparser2 tree containing a metadata \
            argument.
        :rtype: subclass of :py:class:`fparser.two.Fortran2003.Base`

        :raises ValueError: if the Fortran string is not in the \
            expected form.

        '''
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        match = True
        try:
            fparser2_tree = encoding(reader)
        except (NoMatchError, FortranSyntaxError):
            match = False
        if not match or not fparser2_tree:
            raise ValueError(
                f"Expected kernel metadata to be a Fortran "
                f"{encoding.__name__}, but found '{fortran_string}'.")
        return fparser2_tree

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from Fortran.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of this class.
        :rtype: subclass of \
            :py:class:`python.domain.lfric.kernel.CommonMetadata`

        '''
        fparser2_tree = cls.create_fparser2(fortran_string, cls.fparser2_class)
        return cls.create_from_fparser2(fparser2_tree)

    @staticmethod
    @abstractmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        '''


__all__ = ["CommonMetadata"]
