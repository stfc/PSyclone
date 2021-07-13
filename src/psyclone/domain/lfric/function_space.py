# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
#          J. Henrichs, Bureau of Meteorology

'''This module contains the FunctionSpace object and related constants.
'''

from psyclone.errors import InternalError, FieldNotFoundError, GenerationError
from psyclone.domain.lfric.lfric_constants import LFRicConstants


class FunctionSpace(object):
    '''
    Manages the name of a function space. If it is an any_space or
    any_discontinuous_space then its name is mangled such that it is unique
    within the scope of an Invoke.

    :param str name: original name of function space to create a \
                     mangled name for.
    :param kernel_args: object encapsulating all arguments to the kernel, \
                        one or more of which are on this function space.
    :type kernel_args: :py:class:`psyclone.dynamo0p3.DynKernelArguments`

    :raises InternalError: if an unrecognised function space is encountered.

    '''

    def __init__(self, name, kernel_args):
        self._orig_name = name
        self._kernel_args = kernel_args
        self._mangled_name = None
        self._short_name = None

        const = LFRicConstants()
        # Check whether the function space name is a valid name
        if self._orig_name not in const.VALID_FUNCTION_SPACE_NAMES:
            raise InternalError(
                "Unrecognised function space '{0}'. The "
                "supported spaces are {1}."
                .format(self._orig_name,
                        const.VALID_FUNCTION_SPACE_NAMES))

        if self._orig_name not in const.VALID_ANY_SPACE_NAMES + \
                const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            # We only need to name-mangle any_space and
            # any_discontinuous_space spaces
            self._short_name = self._orig_name
            self._mangled_name = self._orig_name
        else:
            # Create short names for any_*_spaces used for mangled names
            self._short_name = self._shorten_fs_name()
            # We do not construct the name-mangled name at this point
            # as the full list of kernel arguments may still be under
            # construction.

    @property
    def orig_name(self):
        '''
        Returns the name of this function space as declared in the
        kernel meta-data.

        :returns: original name of this function space.
        :rtype: str

        '''
        return self._orig_name

    @property
    def short_name(self):
        '''
        Returns the short name of this function space (original name for a
        valid LFRic function space and condensed name for any_*_spaces).

        :returns: short name of this function space.
        :rtype: str

        '''
        return self._short_name

    @property
    def mangled_name(self):
        '''
        Returns the mangled name of this function space such that it is
        unique within the scope of an invoke. If the mangled name has not
        been generated then we do that the first time we are called.

        :returns: mangled name of this function space.
        :rtype: str

        '''
        if self._mangled_name:
            return self._mangled_name
        # Cannot use kernel_args.field_on_space(x) here because that
        # routine itself requires the mangled name in order to identify
        # whether the space is present in the kernel call.
        self._mangled_name = self._mangle_fs_name()
        return self._mangled_name

    def _mangle_fs_name(self):
        '''
        Constructs the mangled version of a function-space name given a list
        of kernel arguments if the argument's function space is any_*_space
        (if the argument's function space is one of the valid LFRic function
        spaces then the mangled name is the original name, set in the class
        initialisation). The mangled name is the short name of the function
        space combined with the argument's name.

        :returns: mangled name of this function space.
        :rtype: str

        :raises InternalError: if a function space to create the mangled \
                               name for is not one of 'any_space' or \
                               'any_discontinuous_space' spaces.
        :raises FieldNotFoundError: if no kernel argument was found on \
                                    the specified function space.

        '''
        # First check that the the function space is one of any_*_space
        # spaces and then proceed with name-mangling.
        const = LFRicConstants()
        if self._orig_name not in const.VALID_ANY_SPACE_NAMES + \
                const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            raise InternalError(
                "_mangle_fs_name: function space '{0}' is not one of "
                "{1} or {2} spaces.".
                format(self._orig_name, const.VALID_ANY_SPACE_NAMES,
                       const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES))

        # List kernel arguments
        args = self._kernel_args.args
        # Mangle the function space name for any_*_space
        for arg in args:
            for fspace in arg.function_spaces:
                if (fspace and fspace.orig_name.lower() ==
                        self._orig_name.lower()):
                    mngl_name = self._short_name + "_" + arg.name
                    return mngl_name
        # Raise an error if there are no kernel arguments on this
        # function space
        raise FieldNotFoundError("No kernel argument found for function space "
                                 "'{0}'".format(self._orig_name))

    def _shorten_fs_name(self):
        '''
        Creates short names for any_*_spaces to be used for mangled names
        from the condensed keywords and function space IDs.

        :returns: short name of this function space.
        :rtype: str

        :raises InternalError: if a function space to create the short \
                               name for is not one of 'any_space' or \
                               'any_discontinuous_space' spaces.

        '''
        # Create a start for the short name and check whether the function
        # space is one of any_*_space spaces
        const = LFRicConstants()
        if self._orig_name in const.VALID_ANY_SPACE_NAMES:
            start = "a"
        elif self._orig_name in const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            start = "ad"
        else:
            raise InternalError(
                "_shorten_fs_name: function space '{0}' is not one of "
                "{1} or {2} spaces.".
                format(self._orig_name, const.VALID_ANY_SPACE_NAMES,
                       const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES))

        # Split name string to find any_*_space ID and create a short name as
        # "<start>" + "spc" + "ID"
        fslist = self._orig_name.split("_")
        self._short_name = start + "spc" + fslist[-1]
        return self._short_name

    @property
    def map_name(self):
        ''':returns: a dofmap name for the supplied FunctionSpace.
        :rtype: str
        '''
        return "map_" + self.mangled_name

    @property
    def cbanded_map_name(self):
        ''':returns: the name of a column-banded dofmap for this FunctionSpace.
        :rtype: str
        '''
        return "cbanded_map_" + self.mangled_name

    @property
    def cma_indirection_map_name(self):
        ''':returns: the name of a CMA indirection dofmap for the supplied \
            FunctionSpace.
        :rtype: str
        '''
        return "cma_indirection_map_" + self.mangled_name

    @property
    def ndf_name(self):
        ''':returns: a ndf name for this FunctionSpace object.
        :rtype: str
        '''
        return "ndf_" + self.mangled_name

    @property
    def undf_name(self):
        ''':returns: a undf name for this FunctionSpace object.
        :rtype: str
        '''
        return "undf_" + self.mangled_name

    def get_basis_name(self, qr_var=None, on_space=None):
        '''
        Returns a name for the basis function on this FunctionSpace. If
        the name of an associated quadrature object is supplied then this
        is appended to the returned name. Similarly, if the function space
        at which the basis is to be evaluated is supplied then this is
        also appended to the name.

        :param string qr_var: the name of the Quadrature Object for which the \
                              basis functions are required
        :param on_space: the function space at which the basis functions \
                         will be evaluated
        :type on_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :returns: name for the Fortran array holding the basis function
        :rtype: str

        '''
        name = "_".join(["basis", self.mangled_name])
        if qr_var:
            name += "_" + qr_var
        if on_space:
            name += "_on_" + on_space.mangled_name
        return name

    def get_diff_basis_name(self, qr_var=None, on_space=None):
        '''
        Returns a name for the differential basis function on this
        FunctionSpace.  If the name of an associated quadrature object is
        supplied then this is appended to the returned name. Similarly, if the
        function space at which the basis is to be evaluated is supplied then
        this is also appended to the name.

        :param str qr_var: the name of the Quadrature Object for which the \
                           differential basis functions are required.
        :param on_space: the function space at which the differential basis \
                         functions will be evaluated
        :type on_space: :py:class:`psyclone.dynamo0p3.domain.lfric.\
                        FunctionSpace`
        :returns: name for the Fortran array holding the differential basis \
                  function
        :rtype: str

        '''
        name = "diff_basis_" + self.mangled_name
        if qr_var:
            name += "_" + qr_var
        if on_space:
            name += "_on_" + on_space.mangled_name
        return name

    def get_operator_name(self, operator_name, qr_var=None, on_space=None):
        '''
        Returns the name of the specified operator (basis or differential
        basis) for this FunctionSpace.

        :param str operator_name: name (type) of the operator.
        :param str qr_var: the name of the Quadrature Object for which the \
                           operator is required.
        :param on_space: the function space at which the operator is required.
        :type on_space: :py:class:`psyclone.domain.lfric.FunctionSpace`

        :returns: name for the Fortran arry holding the named operator \
                  for the specified function space.
        :rtype: str

        '''
        if operator_name == "gh_basis":
            return self.get_basis_name(qr_var=qr_var, on_space=on_space)
        if operator_name == "gh_diff_basis":
            return self.get_diff_basis_name(qr_var=qr_var, on_space=on_space)

        const = LFRicConstants()
        raise GenerationError(
            "Unsupported name '{0}' found. Expected one of {1}".
            format(operator_name, const.VALID_METAFUNC_NAMES))

    def field_on_space(self, arguments):
        '''Returns the corresponding argument if the supplied list of
        arguments contains a field that exists on this space. Otherwise this
        function returns None.

        :param arguments: list of arguments to be tested.
        :type arguments: :py:class:`psyclone.dynamo0p3.DynKernelArguments`

        :returns: the argument from the supplied list of arguments that \
                  contains a field that exists on this space or None.
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument` or None

        '''
        if self.mangled_name in arguments.unique_fs_names:
            for arg in arguments.args:
                # First, test that argument is a field as some argument
                # objects won't have function spaces, e.g. scalars
                if arg.is_field and \
                   arg.function_space.orig_name == self.orig_name:
                    return arg
        return None

    def cma_on_space(self, arguments):
        '''Returns the corresponding argument if the supplied list of
        arguments contains a cma operator that maps to/from this FunctionSpace.
        Otherwise this function returns None.

        :param arguments: list of arguments to be tested.
        :type arguments: :py:class:`psyclone.dynamo0p3.DynKernelArguments`

        :returns: the argument from the supplied list of arguments that \
                  contains a field that exists on this space or None.
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument` or None

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
    def has_scalar_basis(self):
        ''':returns: True if this function space has scalar basis functions.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.SCALAR_BASIS_SPACE_NAMES

    @property
    def has_vector_basis(self):
        ''':returns: True if this function space has vector basis functions.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.VECTOR_BASIS_SPACE_NAMES

    @property
    def has_scalar_diff_basis(self):
        ''':returns: True if this function space has scalar differential
            basis functions.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.SCALAR_DIFF_BASIS_SPACE_NAMES

    @property
    def has_vector_diff_basis(self):
        ''':returns: True if this function space has vector differential
            basis functions.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self.orig_name.lower() in const.VECTOR_DIFF_BASIS_SPACE_NAMES
