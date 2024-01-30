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

'''Module containing the FieldArgMetadata class which captures the metadata
associated with a field argument. Supports the creation, modification
and Fortran output of a Field argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.scalar_arg_metadata import \
    ScalarArgMetadata


class FieldArgMetadata(ScalarArgMetadata):
    '''Class to capture LFRic kernel metadata information for a field
    argument.

    :param str datatype: the datatype of this field (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this field (GH_WRITE, ...).
    :param str function_space: the function space that this field is \
        on (W0, ...).
    :param Optional[str] stencil: the type of stencil used by the \
        kernel when accessing this field.

    '''
    # The name used to specify a field argument in LFRic metadata.
    form = "gh_field"
    # The relative positions of LFRic metadata. Metadata for a field
    # argument is provided in the following format 'arg_type(form,
    # datatype, access, function_space)'. Therefore, for example, the
    # index of the form argument (form_arg_index) is 0.
    form_arg_index = 0
    datatype_arg_index = 1
    access_arg_index = 2
    function_space_arg_index = 3
    stencil_arg_index = 4
    # The name to use for any exceptions.
    check_name = "field"
    # The number of arguments in the language-level metadata (min and
    # max values).
    nargs = (4, 5)

    def __init__(self, datatype, access, function_space, stencil=None):
        super().__init__(datatype, access)
        self.function_space = function_space
        self.stencil = stencil

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected
        form (but do not check the metadata values as that is done
        separately).

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a tuple containing the datatype, access, function \
            space and stencil metadata.
        :rtype: Tuple[str, str, str, Optional[str]]

        '''
        datatype, access = super()._get_metadata(fparser2_tree)
        function_space = cls.get_arg(
            fparser2_tree, cls.function_space_arg_index)
        stencil = cls.get_stencil(fparser2_tree)
        return (datatype, access, function_space, stencil)

    @classmethod
    def get_stencil(cls, fparser2_tree):
        '''Retrieves the stencil metadata value found within the supplied
        fparser2 tree (if there is one) and checks that it is valid.

        :param fparser2_tree: fparser2 tree capturing the required metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: the stencil value extracted from the fparser2 tree \
            if there is one, or None if not.
        :rtype: Optional[str]

        :raises TypeError: if the stencil metadata is not in the \
            expected form.

        '''
        raw_stencil_text = FieldArgMetadata.get_arg(
            fparser2_tree, cls.stencil_arg_index)
        if not raw_stencil_text:
            return None
        raw_stencil_text = raw_stencil_text.strip().lower()
        if not (raw_stencil_text.startswith("stencil(") and
                raw_stencil_text.endswith(")") and len(raw_stencil_text) > 9):
            raise TypeError(f"The stencil metadata should be in the form "
                            f"'stencil(type)' but found '{raw_stencil_text}'.")
        stencil = raw_stencil_text[8:-1]
        return stencil

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        if self.stencil:
            return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                    f"{self.function_space}, stencil({self.stencil}))")
        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space})")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        FieldArgMetadata.validate_scalar_value(
            value, const.VALID_FIELD_DATA_TYPES, "datatype descriptor")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.
        '''
        const = LFRicConstants()
        FieldArgMetadata.validate_scalar_value(
            value, const.VALID_FIELD_ACCESS_TYPES, "access descriptor")

    @property
    def function_space(self):
        '''
        :returns: the function space for this field argument.
        :rtype: str
        '''
        return self._function_space

    @function_space.setter
    def function_space(self, value):
        '''
        :param str value: set the function space to the \
            specified value.
        '''
        const = LFRicConstants()
        FieldArgMetadata.validate_scalar_value(
            value, const.VALID_FUNCTION_SPACE_NAMES, "function space")
        self._function_space = value.lower()

    @property
    def stencil(self):
        '''
        :returns: the stencil for this field argument, or None if there
            isn't one.
        :rtype: Optional[str]

        '''
        return self._stencil

    @stencil.setter
    def stencil(self, value):
        '''
        :param str value: set the stencil to the specified value.
        '''
        if value is None:
            self._stencil = None
        else:
            const = LFRicConstants()
            FieldArgMetadata.validate_scalar_value(
                value, const.VALID_STENCIL_TYPES, "stencil")
            self._stencil = value.lower()


__all__ = ["FieldArgMetadata"]
