# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the abstract CommonArgMetadata class which captures the
metadata associated with a generic LFRic argument. Supports the
creation, modification and Fortran output of such an argument.

'''
from typing import Optional, Union
from fparser.two import Fortran2003
from fparser.two import utils as fp_utils

from psyclone.domain.lfric.kernel.common_metadata import CommonMetadata
from psyclone.psyir.frontend.fortran import FortranReader


class CommonArgMetadata(CommonMetadata):
    '''Class to capture common LFRic kernel argument metadata.'''

    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Structure_Constructor

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
    def check_nargs(fparser2_tree: Union[Fortran2003.Part_Ref,
                                         Fortran2003.Structure_Constructor],
                    nargs: Union[int, tuple[int, int]]) -> None:
        '''Checks that the metadata has the number of arguments specified
        by the 'nargs' argument, otherwise an exception is raised.

        :param fparser2_tree: fparser2 tree capturing a metadata argument.
        :param nargs: the number of expected arguments. This can either be
            a single value or a list containing a lower and an upper value.

        :raises ValueError: if the kernel metadata does not contain
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
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` |
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`
        :param str type_name: the name of the argument datatype.

        :raises ValueError: if the kernel metadata is not in
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

    @staticmethod
    def get_named_arg(fparser2_tree: fp_utils.Base,
                      name: str
                      ) -> Optional[str]:
        '''
        Searches the supplied metadata for 'name=value' expressions and
        returns the value corresponding to the supplied name if found.
        Otherwise returns None. If the value is a string then it is
        lower-cased and must be a valid Fortran variable name.

        :param fparser2_tree: the parse tree of the metadata.
        :param name: the name of the metadata element that we want.

        :returns: the value of the named metadata element or None if not found.

        :raises ValueError: if the value is a str but is not a number or a
                            valid Fortran name.
        '''
        for child in fp_utils.walk(fparser2_tree, Fortran2003.Component_Spec):
            if child.children[0].tostr().lower() == name:
                text = child.children[1].tostr()
                if isinstance(child.children[1],
                              Fortran2003.Char_Literal_Constant):
                    # TODO https://github.com/stfc/fparser/issues/295 -
                    # fparser keeps the quotation marks in character strings.
                    label = text[1:-1].lower()
                    if not label.isnumeric():
                        try:
                            FortranReader.validate_name(label)
                        except (ValueError, TypeError) as err:
                            raise ValueError(
                                f"A string value assigned to a named metadata "
                                f"element must be a valid Fortran name but "
                                f"'{label}' is not.") from err
                    return label
                return text
        return None

    @staticmethod
    def _validate_named_args(fparser2_tree: fp_utils.Base,
                             valid_names: list[str]) -> None:
        '''
        Checks that any named arguments in the supplied parse tree match
        with the names in `valid_names`.

        :raises ValueError: if an unsupported named argument is found in
            the supplied metadata.
        '''
        for child in fp_utils.walk(fparser2_tree, Fortran2003.Component_Spec):
            name = child.children[0].tostr().lower()
            if name not in valid_names:
                raise ValueError(
                    f"Kernel metadata contains keyword argument '{name}' "
                    f"which is not one of the valid options: {valid_names}.")


__all__ = ["CommonArgMetadata"]
