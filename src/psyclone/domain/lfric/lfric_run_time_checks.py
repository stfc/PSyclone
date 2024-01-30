# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module contians the LFRicRunTimeChecks class which handles
declarations and code generation for run-time checks. The methods
check fields' function spaces and read-only fields against kernel
function-space metadata on initialisation. The class inherits from
the LFRicCollection class.'''

# Imports
from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import LFRicCollection, LFRicConstants
from psyclone.f2pygen import CallGen, CommentGen, IfThenGen, UseGen


class LFRicRunTimeChecks(LFRicCollection):
    '''Handle declarations and code generation for run-time checks. This
    is not used in the stub generator.

    '''

    def _invoke_declarations(self, parent):
        '''Insert declarations of all data and functions required by the
        run-time checks code into the PSy layer.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if Config.get().api_conf("dynamo0.3").run_time_checks:
            # Only add if run-time checks are requested
            const = LFRicConstants()
            parent.add(
                UseGen(parent, name=const.
                       FUNCTION_SPACE_TYPE_MAP["fs_continuity"]["module"]))
            parent.add(UseGen(parent, name=const.
                              UTILITIES_MOD_MAP["logging"]["module"],
                              only=True,
                              funcnames=["log_event", "LOG_LEVEL_ERROR"]))

    def _check_field_fs(self, parent):
        '''
        Internal method that adds run-time checks to make sure that the
        field's function space is consistent with the appropriate
        kernel metadata function spaces.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        parent.add(CommentGen(
            parent, " Check field function space and kernel metadata "
            "function spaces are compatible"))

        # When issue #30 is addressed (with issue #79 helping further)
        # we may know some or all field function spaces statically. If
        # so, we should remove these from the fields to check at run
        # time (as they will have already been checked at code
        # generation time).

        const = LFRicConstants()
        existing_checks = []
        for kern_call in self._invoke.schedule.kernels():
            for arg in kern_call.arguments.args:
                if not arg.is_field:
                    # This check is limited to fields
                    continue
                fs_name = arg.function_space.orig_name
                field_name = arg.name_indexed
                if fs_name in const.VALID_ANY_SPACE_NAMES:
                    # We don't need to check validity of a field's
                    # function space if the metadata specifies
                    # any_space as this means that all spaces are
                    # valid.
                    continue
                if (fs_name, field_name) in existing_checks:
                    # This particular combination has already been
                    # checked.
                    continue
                existing_checks.append((fs_name, field_name))

                if fs_name in const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
                    # We need to check against all discontinuous
                    # function spaces
                    function_space_names = const.DISCONTINUOUS_FUNCTION_SPACES
                elif fs_name == "any_w2":
                    # We need to check against all any_w2 function
                    # spaces
                    function_space_names = const.ANY_W2_FUNCTION_SPACES
                else:
                    # We need to check against a specific function space
                    function_space_names = [fs_name]

                if_condition = " .and. ".join(
                    [f"{field_name}%which_function_space() /= {name.upper()}"
                     for name in function_space_names])
                if_then = IfThenGen(parent, if_condition)
                call_abort = CallGen(
                    if_then, "log_event(\"In alg "
                    f"'{self._invoke.invokes.psy.orig_name}' invoke "
                    f"'{self._invoke.name}', the field '{arg.name}' is passed "
                    f"to kernel '{kern_call.name}' but its function space is "
                    f"not compatible with the function space specified in the "
                    f"kernel metadata '{fs_name}'.\", LOG_LEVEL_ERROR)")
                if_then.add(call_abort)
                parent.add(if_then)

    def _check_field_ro(self, parent):
        '''
        Internal method that adds runtime checks to make sure that if the
        field is on a read-only function space then the associated
        kernel metadata does not specify that the field is modified.

        As we make use of the LFRic infrastructure halo exchange
        function, there is no need to check whether the halo of a
        read-only field is clean (which it should always be) as the
        LFric halo-exchange will raise an exception if it is called
        with a read-only field.

        Whilst the LFRic infrastructure halo exchange would also
        indirectly pick up a readonly field being modified, it would
        not be picked up where the error occured. Therefore adding
        checks here is still useful.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # When issue #30 is addressed (with issue #79 helping further)
        # we may know some or all field function spaces statically. If
        # so, we should remove these from the fields to check at run
        # time (as they will have already been checked at code
        # generation time).

        # Create a list of modified fields
        modified_fields = []
        for call in self._invoke.schedule.kernels():
            for arg in call.arguments.args:
                if (arg.text and arg.is_field and
                        arg.access != AccessType.READ and
                        not [entry for entry in modified_fields if
                             entry[0].name == arg.name]):
                    modified_fields.append((arg, call))
        if modified_fields:
            parent.add(CommentGen(
                parent, " Check that read-only fields are not modified"))
        for field, call in modified_fields:
            if_then = IfThenGen(
                parent, f"{field.proxy_name_indexed}%vspace%is_readonly()")
            call_abort = CallGen(
                if_then, "log_event(\"In alg "
                f"'{self._invoke.invokes.psy.orig_name}' invoke "
                f"'{self._invoke.name}', field '{field.name}' is on a "
                f"read-only function space but is modified by kernel "
                f"'{call.name}'.\", LOG_LEVEL_ERROR)")
            if_then.add(call_abort)
            parent.add(if_then)

    def initialise(self, parent):
        '''Add runtime checks to make sure that the arguments being passed
        from the algorithm layer are consistent with the metadata
        specified in the associated kernels. Currently checks are
        limited to ensuring that field function spaces are consistent
        with the associated kernel function-space metadata.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if not Config.get().api_conf("dynamo0.3").run_time_checks:
            # Run-time checks are not requested.
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Perform run-time checks"))
        parent.add(CommentGen(parent, ""))

        # Check that field function spaces are compatible with the
        # function spaces specified in the kernel metadata.
        self._check_field_fs(parent)

        # Check that fields on read-only function spaces are not
        # passed into a kernel where the kernel metadata specifies
        # that the field will be modified.
        self._check_field_ro(parent)

        # These checks should be expanded. Issue #768 suggests
        # extending function space checks to operators.


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicRunTimeChecks']
