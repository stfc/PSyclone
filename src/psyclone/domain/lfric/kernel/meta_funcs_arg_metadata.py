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

'''Module containing the MetaFuncsArgMetadata class which captures
the argument values for the LFRic kernel META_FUNCS metadata.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata


class MetaFuncsArgMetadata(CommonArgMetadata):
    '''Class to capture the LFRic kernel metadata information for a
    meta_funcs argument.

    :param str function_space: the name of a function space.
    :param bool basis_function: whether a basis_function is \
        required. Defaults to False.
    :param bool diff_basis_function: whether a differential basis \
        function is required. Defaults to False.

    '''

    def __init__(self, function_space, basis_function=False,
                 diff_basis_function=False):
        self.function_space = function_space
        self.basis_function = basis_function
        self.diff_basis_function = diff_basis_function
        self._check_constraints(basis_function, diff_basis_function)

    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for a meta_funcs argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.MetaFuncsArgMetadata`

        '''
        MetaFuncsArgMetadata.check_fparser2(
            fparser2_tree, type_name="func_type")
        nargs = MetaFuncsArgMetadata.get_nargs(fparser2_tree)
        # There must be at least 2 and at most 3 arguments
        if nargs < 2:
            raise ParseError(
                f"There must be at least 2 arguments, a function_space and "
                f"one of basis_function or diff_basis_function, but found "
                f"'{nargs}' arguments.")
        if nargs > 3:
            raise ParseError(
                f"There must be at most 3 arguments, function_space, "
                f"basis_function and diff_basis_function, but found "
                f"'{nargs}'.")
        function_space = MetaFuncsArgMetadata.get_arg(fparser2_tree, 0)

        basis_function = False
        diff_basis_function = False
        arg1 = MetaFuncsArgMetadata.get_arg(fparser2_tree, 1)
        if arg1.lower() == "gh_basis":
            basis_function = True
        elif arg1.lower() == "gh_diff_basis":
            diff_basis_function = True
        else:
            raise ValueError(
                f"The basis or differential basis value should be one of "
                f"['gh_basis', 'gh_diff_basis'], but found '{arg1}'.")
        arg2 = None
        if nargs == 3:
            arg2 = MetaFuncsArgMetadata.get_arg(fparser2_tree, 2)
            if arg2.lower() == "gh_basis":
                basis_function = True
            elif arg2.lower() == "gh_diff_basis":
                diff_basis_function = True
            else:
                raise ValueError(
                    f"The basis or differential basis value should be one of "
                    f"['gh_basis', 'gh_diff_basis'], but found '{arg2}'.")
            if arg1.lower() == arg2.lower():
                raise ParseError(
                    f"The same basis or differential basis function value "
                    f"should not be repeated, but found '{arg1}' twice.")
        MetaFuncsArgMetadata._check_constraints(
            basis_function, diff_basis_function)
        return MetaFuncsArgMetadata(
            function_space, basis_function=basis_function,
            diff_basis_function=diff_basis_function)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str

        '''
        args_str_list = [self._function_space]
        if self._basis_function:
            args_str_list.append("gh_basis")
        if self._diff_basis_function:
            args_str_list.append("gh_diff_basis")
        args_str = ", ".join(args_str_list)
        return(f"func_type({args_str})")

    @property
    def function_space(self):
        '''
        :returns: the function space for this meta_funcs argument.
        :rtype: str
        '''
        return self._function_space

    @function_space.setter
    def function_space(self, value):
        '''
        :param str value: set the function space to the specified value.
        '''
        const = LFRicConstants()
        self.check_value(value, "function_space", const.VALID_FUNCTION_SPACES)
        self._function_space = value

    @property
    def basis_function(self):
        '''
        :returns: whether a basis function is required for this function \
            space, or not.
        :rtype: bool

        '''
        return self._basis_function

    @basis_function.setter
    def basis_function(self, value):
        '''
        :param bool value: set the basis function to True or False.
        '''
        self._check_boolean(value)
        self._basis_function = value

    @property
    def diff_basis_function(self):
        '''
        :returns: whether a differential basis function is required for this \
            function space, or not.
        :rtype: bool

        '''
        self._check_boolean(value)
        return self._diff_basis_function

    @diff_basis_function.setter
    def diff_basis_function(self, value):
        '''
        :param bool value: set the differential basis function to True \
            or False.

        '''
        self._check_boolean(value)
        self._diff_basis_function = value

    @staticmethod
    def _check_boolean(value):
        '''
        :param bool value: the value to validate.

        :raises ValueError: if the provided value is not a boolean.

        '''
        if not isinstance(value, bool):
            raise TypeError(
                f"The value should be a boolean but found "
                f"'{type(value).__name__}'.")

    @staticmethod
    def _check_constraints(basis_function, diff_basis_function):
        '''Check that at least one of basis_function or diff_basis_function
        have been set to True.

        :param bool basis_function: whether a basis_function is \
            required.
        :param bool diff_basis_function: whether a differential basis \
            function is required.

        :param ValueError: if neither of basis_function or \
            diff_basis_function are set to True.

        '''
        if not basis_function and not diff_basis_function:
            raise ValueError(
                "At least one of basis_function or diff_basis_function must "
                "be set to True.")
