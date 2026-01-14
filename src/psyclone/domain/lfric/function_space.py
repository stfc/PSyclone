# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
#          J. Henrichs, Bureau of Meteorology

'''This module contains the FunctionSpace object and related constants.
'''

from __future__ import annotations
from typing import Optional, TYPE_CHECKING

from psyclone.errors import InternalError, FieldNotFoundError, GenerationError
from psyclone.domain.lfric.lfric_constants import LFRicConstants
if TYPE_CHECKING:
    from psyclone.lfric import LFRicKernelArgument, LFRicKernelArguments


class FunctionSpace():
    '''
    Manages the name of a function space. If it is an any_space or
    any_discontinuous_space then its name is mangled such that it is unique
    within the scope of an Invoke.

    :param name: original name of function space.
    :param kernel_args: object encapsulating all arguments to the kernel,
                        one or more of which are on this function space.

    :raises InternalError: if an unrecognised function space is encountered.

    '''
    #: The maximum length of name to generate when combining function space
    ## and field names.
    MAX_NAME_LEN = 21

    def __init__(self, name: str, kernel_args: "LFRicKernelArguments"):
        self._orig_name = name
        self._kernel_args = kernel_args

        const = LFRicConstants()
        # Check whether the function space name is a valid name
        if self._orig_name not in const.VALID_FUNCTION_SPACE_NAMES:
            raise InternalError(
                f"Unrecognised function space '{self._orig_name}'. The "
                f"supported spaces are {const.VALID_FUNCTION_SPACE_NAMES}.")

    @property
    def orig_name(self) -> str:
        '''
        Returns the name of this function space as declared in the
        kernel meta-data.

        :returns: original name of this function space.

        '''
        return self._orig_name

    @property
    def mangled_name(self) -> str:
        '''
        Constructs the mangled version of a function-space name given a list
        of kernel arguments if the argument's function space is any_*_space
        (if the argument's function space is one of the valid LFRic function
        spaces then the mangled name is the original name, set in the class
        initialisation). The mangled name is the short name of the function
        space combined with the argument's name.

        :returns: mangled name of this function space.

        :raises FieldNotFoundError: if no kernel argument was found on
                                    the specified function space.
        '''
        # First check that the the function space is one of any_*_space
        # spaces and then proceed with name-mangling.
        const = LFRicConstants()
        if (self._orig_name not in const.VALID_ANY_SPACE_NAMES +
                const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES):
            return self._orig_name

        # List kernel arguments
        args = self._kernel_args.args
        # Mangle the function space name for any_*_space
        lorig_name = self._orig_name.lower()
        for arg in args:
            for fspace in arg.function_spaces:
                if (fspace and fspace.orig_name.lower() == lorig_name):
                    return (f"{self.short_name}_"
                            f"{self._shorten_name(arg.name)}")
        # Raise an error if there are no kernel arguments on this
        # function space
        raise FieldNotFoundError(f"No kernel argument found for function "
                                 f"space '{self._orig_name}'")

    @staticmethod
    def _shorten_name(name: str) -> str:
        '''
        If the supplied name is less than FunctionSpace.MAX_NAME_LEN characters
        then it is returned unchanged. Otherwise, shortens the provided name by
        splitting on any underscore characters and reconstructing the name
        using the beginning and ending chars from each token.

        :param name: the name to shorten.

        :returns: a shortened form of the name.

        '''
        if len(name) < FunctionSpace.MAX_NAME_LEN:
            return name
        new_parts = []
        for part in name.split("_"):
            new_name = part[0]
            if len(part) > 1:
                new_name += part[-1]
            new_parts.append(new_name)
        return "_".join(new_parts)

    @property
    def short_name(self) -> str:
        '''
        Creates a short version of the name of this function space  to be used
        for mangled names from the condensed keywords and function space IDs.

        For spaces other than any_*_spaces then the original name is just
        returned unchanged.

        :returns: short name of this function space.

        '''
        # Create a start for the short name and check whether the function
        # space is one of any_*_space spaces
        const = LFRicConstants()
        if self._orig_name in const.VALID_ANY_SPACE_NAMES:
            start = "a"
        elif self._orig_name in const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            start = "ad"
        else:
            # It's not an any_*_space so use the original name.
            return self._orig_name

        # Split name string to find any_*_space ID and create a short name as
        # "<start>" + "s" + "ID"
        return start + "s" + self._orig_name.split("_")[-1]

    @property
    def map_name(self) -> str:
        '''
        :returns: a dofmap name for the supplied FunctionSpace.
        '''
        return "map_" + self.mangled_name

    @property
    def cbanded_map_name(self) -> str:
        '''
        :returns: the name of a column-banded dofmap for this FunctionSpace.
        '''
        return "cbanded_map_" + self.mangled_name

    @property
    def cma_indirection_map_name(self) -> str:
        '''
        :returns: the name of a CMA indirection dofmap for the supplied
            FunctionSpace.
        '''
        return "cma_indirection_map_" + self.mangled_name

    @property
    def ndf_name(self) -> str:
        '''
        :returns: a ndf name for this FunctionSpace object.
        '''
        return "ndf_" + self.mangled_name

    @property
    def undf_name(self) -> str:
        '''
        :returns: a undf name for this FunctionSpace object.
        '''
        return "undf_" + self.mangled_name

    def get_basis_name(self,
                       qr_var: str = None,
                       on_space: FunctionSpace = None) -> str:
        '''
        Returns a name for the basis function on this FunctionSpace. If
        the name of an associated quadrature object is supplied then this
        is appended to the returned name. Similarly, if the function space
        at which the basis is to be evaluated is supplied then this is
        also appended to the name.

        :param qr_var: the name of the Quadrature Object for which the
                       basis functions are required
        :param on_space: the function space at which the basis functions
                         will be evaluated

        :returns: name for the Fortran array holding the basis function

        '''
        name = "_".join(["basis", self.mangled_name])
        if qr_var:
            name += "_" + qr_var
        if on_space:
            name += "_on_" + on_space.mangled_name
        return name

    def get_diff_basis_name(self,
                            qr_var: str = None,
                            on_space: FunctionSpace = None) -> str:
        '''
        Returns a name for the differential basis function on this
        FunctionSpace.  If the name of an associated quadrature object is
        supplied then this is appended to the returned name. Similarly, if the
        function space at which the basis is to be evaluated is supplied then
        this is also appended to the name.

        :param qr_var: the name of the Quadrature Object for which the
                       differential basis functions are required.
        :param on_space: the function space at which the differential basis
                         functions will be evaluated

        :returns: name for the Fortran array holding the differential basis
                  function

        '''
        name = "diff_basis_" + self.mangled_name
        if qr_var:
            name += "_" + qr_var
        if on_space:
            name += "_on_" + on_space.mangled_name
        return name

    def get_operator_name(self,
                          operator_name: str,
                          qr_var: str = None,
                          on_space: FunctionSpace = None) -> str:
        '''
        Returns the name of the specified operator (basis or differential
        basis) for this FunctionSpace.

        :param operator_name: name (type) of the operator.
        :param qr_var: the name of the Quadrature Object for which the
                       operator is required.
        :param on_space: the function space at which the operator is required.

        :returns: name for the Fortran arry holding the named operator
                  for the specified function space.

        '''
        if operator_name == "gh_basis":
            return self.get_basis_name(qr_var=qr_var, on_space=on_space)
        if operator_name == "gh_diff_basis":
            return self.get_diff_basis_name(qr_var=qr_var, on_space=on_space)

        const = LFRicConstants()
        raise GenerationError(
            f"Unsupported name '{operator_name}' found. Expected one of "
            f"{const.VALID_METAFUNC_NAMES}")

    def field_on_space(self, arguments: "LFRicKernelArguments") -> Optional[
            "LFRicKernelArgument"]:
        '''Returns the corresponding argument if the supplied list of
        arguments contains a field that exists on this space. Otherwise this
        function returns None.

        :param arguments: list of arguments to be tested.

        :returns: the argument from the supplied list of arguments that
                  contains a field that exists on this space or None.

        '''
        if self.mangled_name in arguments.unique_fs_names:
            for arg in arguments.args:
                # First, test that argument is a field as some argument
                # objects won't have function spaces, e.g. scalars
                if arg.is_field and \
                   arg.function_space.orig_name == self.orig_name:
                    return arg
        return None

    def cma_on_space(self, arguments: "LFRicKernelArguments") -> Optional[
            "LFRicKernelArgument"]:
        '''Returns the corresponding argument if the supplied list of
        arguments contains a cma operator that maps to/from this FunctionSpace.
        Otherwise this function returns None.

        :param arguments: the arguments to be tested.

        :returns: the argument from the supplied list of arguments that
                  contains a field that exists on this space or None.

        '''
        if self.mangled_name in arguments.unique_fs_names:
            for arg in arguments.args:
                # First, test that arg is a CMA op as some argument objects
                # won't have function spaces, e.g. scalars
                if arg.argument_type == "gh_columnwise_operator" and \
                   self.orig_name in [arg.function_space_to.orig_name,
                                      arg.function_space_from.orig_name]:
                    return arg
        return None

    @property
    def has_scalar_basis(self) -> bool:
        ''':returns: True if this function space has scalar basis functions.
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.SCALAR_BASIS_SPACE_NAMES

    @property
    def has_vector_basis(self) -> bool:
        ''':returns: True if this function space has vector basis functions.
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.VECTOR_BASIS_SPACE_NAMES

    @property
    def has_scalar_diff_basis(self) -> bool:
        ''':returns: True if this function space has scalar differential
            basis functions.
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.SCALAR_DIFF_BASIS_SPACE_NAMES

    @property
    def has_vector_diff_basis(self) -> bool:
        ''':returns: True if this function space has vector differential
            basis functions.
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.VECTOR_DIFF_BASIS_SPACE_NAMES
