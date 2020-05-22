# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

from psyclone.errors import InternalError, FieldNotFoundError

# ---------- Function spaces (FS) ------------------------------------------- #
# Discontinuous FS
DISCONTINUOUS_FUNCTION_SPACES = ["w3", "wtheta", "w2v", "w2vtrace", "w2broken"]

# Continuous FS
# Note, any_w2 is not a space on its own. any_w2 is used as a common term for
# any vector "w2*" function space (w2, w2h, w2v, w2broken) but not w2*trace
# (spaces of scalar functions). As any_w2 stands for all vector "w2*" spaces
# it needs to a) be treated as continuous and b) have vector basis and scalar
# differential basis dimensions.
# TODO #540: resolve what W2* spaces should be included in ANY_W2 list and
# whether ANY_W2 should be in the continuous function space list.
ANY_W2_FUNCTION_SPACES = ["w2", "w2h", "w2v", "w2broken"]

CONTINUOUS_FUNCTION_SPACES = \
    ["w0", "w1", "w2", "w2trace", "w2h", "w2htrace", "any_w2"]

# Read-only FS
READ_ONLY_FUNCTION_SPACES = ["wchi"]

# Valid FS names
VALID_FUNCTION_SPACES = DISCONTINUOUS_FUNCTION_SPACES + \
    CONTINUOUS_FUNCTION_SPACES + READ_ONLY_FUNCTION_SPACES

# Valid any_space metadata (general FS, could be continuous or discontinuous)
VALID_ANY_SPACE_NAMES = ["any_space_{0}".format(x+1) for x in range(10)]

# Valid any_discontinuous_space metadata (general FS known to be discontinuous)
VALID_ANY_DISCONTINUOUS_SPACE_NAMES = \
    ["any_discontinuous_space_{0}".format(x+1) for x in range(10)]

# Valid discontinuous FS names (for optimisation purposes)
VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES = DISCONTINUOUS_FUNCTION_SPACES + \
    VALID_ANY_DISCONTINUOUS_SPACE_NAMES

# FS names consist of all valid names
VALID_FUNCTION_SPACE_NAMES = VALID_FUNCTION_SPACES + \
                             VALID_ANY_SPACE_NAMES + \
                             VALID_ANY_DISCONTINUOUS_SPACE_NAMES

# Lists of function spaces that have
# a) scalar basis functions;
SCALAR_BASIS_SPACE_NAMES = \
    ["w0", "w2trace", "w2htrace", "w2vtrace", "w3", "wtheta", "wchi"]
# b) vector basis functions;
VECTOR_BASIS_SPACE_NAMES = ["w1", "w2", "w2h", "w2v", "w2broken", "any_w2"]
# c) scalar differential basis functions;
SCALAR_DIFF_BASIS_SPACE_NAMES = ["w2", "w2h", "w2v", "w2broken", "any_w2"]


class FunctionSpace(object):
    '''
    Manages the name of a function space. If it is an any_space or
    any_discontinuous_space then its name is mangled such that it is unique
    within the scope of an Invoke.

    :param str name: original name of function space to create a \
                     mangled name for.
    :param kernel_args: object encapsulating all arguments to the kernel call.
    :type kernel_args: :py:class:`psyclone.dynamo0p3.DynKernelArguments`

    :raises InternalError: if an unrecognised function space is encountered.

    '''

    # d) vector differential basis functions.
    VECTOR_DIFF_BASIS_SPACE_NAMES = \
        ["w0", "w1", "w2trace", "w2htrace", "w2vtrace", "w3", "wtheta", "wchi"]

    def __init__(self, name, kernel_args):
        self._orig_name = name
        self._kernel_args = kernel_args
        self._mangled_name = None
        self._short_name = None

        # Check whether the function space name is a valid name
        if self._orig_name not in VALID_FUNCTION_SPACE_NAMES:
            raise InternalError("Unrecognised function space '{0}'. The "
                                "supported spaces are {1}."
                                .format(self._orig_name,
                                        VALID_FUNCTION_SPACE_NAMES))

        if self._orig_name not in VALID_ANY_SPACE_NAMES + \
           VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
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
        if self._orig_name not in VALID_ANY_SPACE_NAMES + \
           VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            raise InternalError(
                "_mangle_fs_name: function space '{0}' is not one of "
                "{1} or {2} spaces.".
                format(self._orig_name, VALID_ANY_SPACE_NAMES,
                       VALID_ANY_DISCONTINUOUS_SPACE_NAMES))

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
        if self._orig_name in VALID_ANY_SPACE_NAMES:
            start = "a"
        elif self._orig_name in VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            start = "ad"
        else:
            raise InternalError(
                "_shorten_fs_name: function space '{0}' is not one of "
                "{1} or {2} spaces.".
                format(self._orig_name, VALID_ANY_SPACE_NAMES,
                       VALID_ANY_DISCONTINUOUS_SPACE_NAMES))

        # Split name string to find any_*_space ID and create a short name as
        # "<start>" + "spc" + "ID"
        fslist = self._orig_name.split("_")
        self._short_name = start + "spc" + fslist[-1]
        return self._short_name

    @property
    def get_fs_map_name(self):
        ''':returns: a dofmap name for the supplied FunctionSpace.
        :rtype: str'''
        return "map_" + self.mangled_name

    @staticmethod
    def vector_diff_basis_space_names():
        ''':returns: list of all vector differential basis functions names.
        :rtype: list of str
        '''
        return FunctionSpace.VECTOR_DIFF_BASIS_SPACE_NAMES
