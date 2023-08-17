# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (KernelType) and
    adding a new class (DynFuncDescriptor03) to capture function descriptor
    metadata and 2) specialising the required base classes in psyGen.py
    (PSy, Invokes, Invoke, InvokeSchedule, Loop, Kern, Inf, Arguments and
    Argument). '''

# Imports
from psyclone.dynamo0p3 import (DynHaloExchangeStart, DynHaloExchangeEnd,
                                HaloReadAccess, HaloWriteAccess,
                                _create_depth_list)
from psyclone.configuration import Config
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import CallGen, CommentGen, IfThenGen
from psyclone.psyGen import HaloExchange

# pylint: disable=too-many-lines
# --------------------------------------------------------------------------- #
# ========== First section : Parser specialisations and classes ============= #
# --------------------------------------------------------------------------- #
#

# ---------- Functions ------------------------------------------------------ #


class LFRicHaloExchange(HaloExchange):

    '''Dynamo specific halo exchange class which can be added to and
    manipulated in a schedule.

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param check_dirty: optional argument default True indicating \
    whether this halo exchange should be subject to a run-time check \
    for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to \
    identify which index of a vector field this halo exchange is \
    responsible for
    :type vector_index: int
    :param parent: optional PSyIRe parent node (default None) of this \
    object
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    def __init__(self, field, check_dirty=True,
                 vector_index=None, parent=None):
        HaloExchange.__init__(self, field, check_dirty=check_dirty,
                              vector_index=vector_index, parent=parent)
        # set up some defaults for this class
        self._halo_exchange_name = "halo_exchange"

    def _compute_stencil_type(self):
        '''Dynamically work out the type of stencil required for this halo
        exchange as it could change as transformations are applied to
        the schedule. If all stencil accesses are of the same type then we
        return that stencil, otherwise we return the "region" stencil
        type (as it is safe for all stencils).

        :return: the type of stencil required for this halo exchange
        :rtype: str

        '''
        # get information about stencil accesses from all read fields
        # dependent on this halo exchange
        halo_info_list = self._compute_halo_read_info()

        trial_stencil = halo_info_list[0].stencil_type
        for halo_info in halo_info_list:
            # assume that if stencil accesses are different that we
            # simply revert to region. We could be more clever in the
            # future e.g. x and y implies cross.
            if halo_info.stencil_type != trial_stencil:
                return "region"
        return trial_stencil

    def _compute_halo_depth(self):
        '''Dynamically determine the depth of the halo for this halo exchange,
        as the depth can change as transformations are applied to the
        schedule.

        :return: the halo exchange depth as a Fortran string
        :rtype: str

        '''
        # get information about reading from the halo from all read fields
        # dependent on this halo exchange
        depth_info_list = self._compute_halo_read_depth_info()

        # if there is only one entry in the list we can just return
        # the depth
        if len(depth_info_list) == 1:
            return str(depth_info_list[0])
        # the depth information can't be reduced to a single
        # expression, therefore we need to determine the maximum
        # of all expressions
        depth_str_list = [str(depth_info) for depth_info in
                          depth_info_list]
        return "max("+",".join(depth_str_list)+")"

    def _compute_halo_read_depth_info(self, ignore_hex_dep=False):
        '''Take a list of `psyclone.dynamo0p3.HaloReadAccess` objects and
        create an equivalent list of `psyclone.dynamo0p3.HaloDepth`
        objects. Whilst doing this we simplify the
        `psyclone.dynamo0p3.HaloDepth` list to remove redundant depth
        information e.g. depth=1 is not required if we have a depth=2.
        If the optional ignore_hex_dep argument is set to True then
        any read accesses contained in halo exchange nodes are
        ignored. This option can therefore be used to filter out any
        halo exchange dependencies and only return non-halo exchange
        dependencies if and when required.

        :param bool ignore_hex_dep: if True then ignore any read \
            accesses contained in halo exchanges. This is an optional \
            argument that defaults to False.

        :return: a list containing halo depth information derived from \
            all fields dependent on this halo exchange.
        :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloDepth`

        '''
        # get our halo information
        halo_info_list = self._compute_halo_read_info(ignore_hex_dep)
        # use the halo information to generate depth information
        depth_info_list = _create_depth_list(halo_info_list,
                                             self._symbol_table)
        return depth_info_list

    def _compute_halo_read_info(self, ignore_hex_dep=False):
        '''Dynamically computes all halo read dependencies and returns the
        required halo information (i.e. halo depth and stencil type)
        in a list of HaloReadAccess objects. If the optional
        ignore_hex_dep argument is set to True then any read accesses
        contained in halo exchange nodes are ignored. This option can
        therefore be used to filter out any halo exchange dependencies
        and only return non-halo exchange dependencies if and when
        required.

        :param bool ignore_hex_dep: if True then ignore any read \
            accesses contained in halo exchanges. This is an optional \
            argument that defaults to False.

        :return: a list containing halo information for each read dependency.
        :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloReadAccess`

        :raises InternalError: if there is more than one read \
            dependency associated with a halo exchange.
        :raises InternalError: if there is a read dependency \
            associated with a halo exchange and it is not the last \
            entry in the read dependency list.
        :raises GenerationError: if there is a read dependency \
            associated with an asynchronous halo exchange.
        :raises InternalError: if no read dependencies are found.

        '''
        read_dependencies = self.field.forward_read_dependencies()
        hex_deps = [dep for dep in read_dependencies
                    if isinstance(dep.call, LFRicHaloExchange)]
        if hex_deps:
            # There is a field accessed by a halo exchange that is
            # a read dependence. As ignore_hex_dep is True this should
            # be ignored so this is removed from the list.
            if any(dep for dep in hex_deps
                   if isinstance(dep.call, (DynHaloExchangeStart,
                                            DynHaloExchangeEnd))):
                raise GenerationError(
                    "Please perform redundant computation transformations "
                    "before asynchronous halo exchange transformations.")

            # There can only be one field accessed by a
            # halo exchange that is a read dependence.
            if len(hex_deps) != 1:
                raise InternalError(
                    f"There should only ever be at most one read dependency "
                    f"associated with a halo exchange in the read dependency "
                    f"list, but found {len(hex_deps)} for field "
                    f"{self.field.name}.")
            # For sanity, check that the field accessed by the halo
            # exchange is the last dependence in the list.
            if not isinstance(read_dependencies[-1].call, LFRicHaloExchange):
                raise InternalError(
                    "If there is a read dependency associated with a halo "
                    "exchange in the list of read dependencies then it should "
                    "be the last one in the list.")
            if ignore_hex_dep:
                # Remove the last entry in the list (the field accessed by
                # the halo exchange).
                del read_dependencies[-1]

        if not read_dependencies:
            raise InternalError(
                "Internal logic error. There should be at least one read "
                "dependence for a halo exchange.")
        return [HaloReadAccess(read_dependency, self._symbol_table) for
                read_dependency in read_dependencies]

    def _compute_halo_write_info(self):
        '''Determines how much of the halo has been cleaned from any previous
        redundant computation

        :return: a HaloWriteAccess object containing the required \
        information, or None if no dependence information is found.
        :rtype: :py:class:`psyclone.dynamo0p3.HaloWriteAccess` or None
        :raises GenerationError: if more than one write dependence is \
        found for this halo exchange as this should not be possible

        '''
        write_dependencies = self.field.backward_write_dependencies()
        if not write_dependencies:
            # no write dependence information
            return None
        if len(write_dependencies) > 1:
            raise GenerationError(
                f"Internal logic error. There should be at most one write "
                f"dependence for a halo exchange. Found "
                f"'{len(write_dependencies)}'")
        return HaloWriteAccess(write_dependencies[0], self._symbol_table)

    def required(self, ignore_hex_dep=False):
        '''Determines whether this halo exchange is definitely required
        ``(True, True)``, might be required ``(True, False)`` or is definitely
        not required ``(False, *)``.

        If the optional ignore_hex_dep argument is set to True then
        any read accesses contained in halo exchange nodes are
        ignored. This option can therefore be used to filter out any
        halo exchange dependencies and only consider non-halo exchange
        dependencies if and when required.

        Whilst a halo exchange is generally only ever added if it is
        required, or if it may be required, this situation can change
        if redundant computation transformations are applied. The
        first argument can be used to remove such halo exchanges if
        required.

        When the first return value is True, the second return value
        can be used to see if we need to rely on the runtime
        (set_dirty and set_clean calls) and therefore add a
        check_dirty() call around the halo exchange or whether we
        definitely know that this halo exchange is required.

        This routine assumes that a stencil size provided via a
        variable may take the value 0. If a variables value is
        constrained to be 1, or more, then the logic for deciding
        whether a halo exchange is definitely required should be
        updated. Note, the routine would still be correct as is, it
        would just return more unknown results than it should).

        :param bool ignore_hex_dep: if True then ignore any read \
            accesses contained in halo exchanges. This is an optional \
            argument that defaults to False.

        :returns: (x, y) where x specifies whether this halo \
            exchange is (or might be) required - True, or is not \
            required - False. If the first tuple item is True then the \
            second argument specifies whether we definitely know that \
            we need the HaloExchange - True, or are not sure - False.
        :rtype: (bool, bool)

        '''
        # pylint: disable=too-many-branches, too-many-return-statements
        # get *aggregated* information about halo reads
        required_clean_info = self._compute_halo_read_depth_info(
            ignore_hex_dep)
        # get information about the halo write
        clean_info = self._compute_halo_write_info()

        # no need to test whether we return at least one read
        # dependency as _compute_halo_read_depth_info() raises an
        # exception if none are found

        if Config.get().api_conf("dynamo0.3").compute_annexed_dofs and \
           len(required_clean_info) == 1 and \
           required_clean_info[0].annexed_only:
            # We definitely don't need the halo exchange as we
            # only read annexed dofs and these are always clean as
            # they are computed by default when iterating over
            # dofs and kept up-to-date by redundant computation
            # when iterating over cells.
            required = False
            known = True  # redundant information as it is always known
            return required, known

        if not clean_info:
            # this halo exchange has no previous write dependencies so
            # we do not know the initial state of the halo. This means
            # that we do not know if we need a halo exchange or not
            required = True
            known = False
            return required, known

        if clean_info.max_depth:
            if not clean_info.dirty_outer:
                # all of the halo is cleaned by redundant computation
                # so halo exchange is not required
                required = False
                known = True  # redundant information as it is always known
            else:
                # the last level halo is dirty
                if required_clean_info[0].max_depth:
                    # we know that we need to clean the outermost halo level
                    required = True
                    known = True
                else:
                    # we don't know whether the halo exchange is
                    # required or not as the reader reads the halo to
                    # a specified depth but we don't know the depth
                    # of the halo
                    required = True
                    known = False
            return required, known

        # at this point we know that clean_info.max_depth is False

        if not clean_info.literal_depth:
            # if literal_depth is 0 then the writer does not
            # redundantly compute so we definitely need the halo
            # exchange
            required = True
            known = True
            return required, known

        if clean_info.literal_depth == 1 and clean_info.dirty_outer:
            # the writer redundantly computes in the level 1 halo but
            # leaves it dirty (although annexed dofs are now clean).
            if len(required_clean_info) == 1 and \
               required_clean_info[0].annexed_only:
                # we definitely don't need the halo exchange as we
                # only read annexed dofs and these have been made
                # clean by the redundant computation
                required = False
                known = True  # redundant information as it is always known
            else:
                # we definitely need the halo exchange as the reader(s)
                # require the halo to be clean
                required = True
                known = True
            return required, known

        # At this point we know that the writer cleans the halo to a
        # known (literal) depth through redundant computation. We now
        # compute this value for use by the logic in the rest of the
        # routine.
        clean_depth = clean_info.literal_depth
        if clean_info.dirty_outer:
            # outer layer stays dirty
            clean_depth -= 1

        # If a literal value in any of the required clean halo depths
        # is greater than the cleaned depth then we definitely need
        # the halo exchange (as any additional variable depth would
        # increase the required depth value). We only look at the case
        # where we have multiple entries as the single entry case is
        # dealt with separately
        if len(required_clean_info) > 1:
            for required_clean in required_clean_info:
                if required_clean.literal_depth > clean_depth:
                    required = True
                    known = True
                    return required, known

        # The only other case where we know that a halo exchange is
        # required (or not) is where we read the halo to a known
        # literal depth. As the read information is aggregated, a known
        # literal depth will mean that there is only one
        # required_clean_info entry
        if len(required_clean_info) == 1:
            # the halo might be read to a fixed literal depth
            if required_clean_info[0].var_depth or \
               required_clean_info[0].max_depth:
                # no it isn't so we might need the halo exchange
                required = True
                known = False
            else:
                # the halo is read to a fixed literal depth.
                required_clean_depth = required_clean_info[0].literal_depth
                if clean_depth < required_clean_depth:
                    # we definitely need this halo exchange
                    required = True
                    known = True
                else:
                    # we definitely don't need this halo exchange
                    required = False
                    known = True  # redundant information as it is always known
            return required, known

        # We now know that at least one required_clean entry has a
        # variable depth and any required_clean fixed depths are less
        # than the cleaned depth so we may need a halo exchange.
        required = True
        known = False
        return required, known

    def node_str(self, colour=True):
        ''' Creates a text summary of this HaloExchange node.

        :param bool colour: whether or not to include control codes for colour.

        :returns: text summary of this node, optionally with control codes \
                  for colour highlighting.
        :rtype: str

        '''
        _, known = self.required()
        runtime_check = not known
        field_id = self._field.name
        if self.vector_index:
            field_id += f"({self.vector_index})"
        return (f"{self.coloured_name(colour)}[field='{field_id}', "
                f"type='{self._compute_stencil_type()}', "
                f"depth={self._compute_halo_depth()}, "
                f"check_dirty={runtime_check}]")

    def gen_code(self, parent):
        '''Dynamo specific code generation for this class.

        :param parent: an f2pygen object that will be the parent of \
        f2pygen objects created in this method
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        if self.vector_index:
            ref = f"({self.vector_index})"
        else:
            ref = ""
        _, known = self.required()
        if not known:
            if_then = IfThenGen(parent, self._field.proxy_name + ref +
                                "%is_dirty(depth=" +
                                self._compute_halo_depth() + ")")
            parent.add(if_then)
            halo_parent = if_then
        else:
            halo_parent = parent
        halo_parent.add(
            CallGen(
                halo_parent, name=self._field.proxy_name + ref +
                "%" + self._halo_exchange_name +
                "(depth=" + self._compute_halo_depth() + ")"))
        parent.add(CommentGen(parent, ""))


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicHaloExchange']
