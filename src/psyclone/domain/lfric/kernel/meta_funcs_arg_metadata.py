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
from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata


class MetaFuncsArgMetadata(CommonArgMetadata):
    ''' xxx '''

    def __init__(self, function_space, basis_function=False,
                 diff_basis_function=False):
        self._function_space = function_space
        self._basis_function = basis_function
        self._diff_basis_function = diff_basis_function

    def check_access(_):
        '''Not needed by this class '''
        pass

    def check_datatype(_):
        ''' Not needed by this class '''
        pass

    def create_from_fortran_string(fortran_string):
        ''' xxx '''
        fparser2_tree = MetaFuncsArgMetadata.create_fparser2(fortran_string)
        return MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)

    def create_from_fparser2(fparser2_tree):
        ''' xxx '''
        MetaFuncsArgMetadata.check_fparser2(
            fparser2_tree, type_name="func_type")
        nargs = MetaFuncsArgMetadata.get_nargs(fparser2_tree)
        # must be at least 2 and at most 3
        if nargs < 2:
            raise Exception("Must have a function_space as first argument "
                            "and at least one of ...")
        if nargs > 3:
            raise Exception("Must have at most 3 args, function_space, "
                            "basis and diff_basis")
        function_space = MetaFuncsArgMetadata.get_arg(fparser2_tree, 0)
        from psyclone.domain.lfric import LFRicConstants
        const = LFRicConstants()
        if function_space.lower() not in const.VALID_FUNCTION_SPACES:
            raise Exception("")
        basis_function = False
        diff_basis_function = False
        arg1 = MetaFuncsArgMetadata.get_arg(fparser2_tree, 1)
        if arg1.lower() == "gh_basis":
            basis_function = True
        elif arg1.lower() == "gh_diff_basis":
            diff_basis_function = True
        else:
            raise Exception("")
        arg2 = None
        if nargs == 3:
            arg2 = MetaFuncsArgMetadata.get_arg(fparser2_tree, 2)
            if arg2.lower() == "gh_basis":
                basis_function = True
            elif arg2.lower() == "gh_diff_basis":
                diff_basis_function = True
            else:
                raise Exception(f"{arg2}")
            if arg1.lower() == arg2.lower():
                raise Exception("")
        return MetaFuncsArgMetadata(
            function_space, basis_function=basis_function,
            diff_basis_function=diff_basis_function)

    def fortran_string(self):
        ''' xxx '''
        args_str_list = [self._function_space]
        if self._basis_function:
            args_str_list.append("gh_basis")
        if self._diff_basis_function:
            args_str_list.append("gh_diff_basis")
        args_str = ", ".join(args_str_list)
        return(f"func_type({args_str})")

    @property
    def function_space(self):
        return self._function_space

    @property
    def basis_function(self):
        return self._basis_function

    @property
    def diff_basis_function(self):
        return self._diff_basis_function
