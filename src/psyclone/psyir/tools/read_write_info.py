# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides the class to store information about which variables
are written, and which ones are read.'''

import logging
from typing import List, Optional, Tuple

from psyclone.core import Signature


class ReadWriteInfo:
    '''This class stores signature and container name of variables read or
    written. The container name is optional, it will default to "" if the
    signature belongs to a symbol declared in the local scope, otherwise it
    is the name of the container from which it must be imported.

    The information is stored in lists of tuples, the first element being
    the container name, the second the signature. When accessing any of these
    two lists, the getter will make sure that the lists are sorted. This
    will guarantee that, for example, the kernel extraction and driver creation
    read the variables in the same order.
    '''

    def __init__(self) -> None:
        self._read_list: list[Tuple[str, Signature]] = []
        self._write_list: list[Tuple[str, Signature]] = []
        self._sorted = True

    # -------------------------------------------------------------------------
    @property
    def read_list(self) -> List[Tuple[str, Signature]]:
        '''
        :returns: the sorted list of container_name,signature pairs that
            are read.
        :rtype: List[Tuple[str,:py:class:`psyclone.core.Signature`]]

        '''
        if not self._sorted:
            self._read_list.sort()
            self._write_list.sort()
            self._sorted = True
        return self._read_list

    # -------------------------------------------------------------------------
    @property
    def signatures_read(self) -> List[Signature]:
        '''
        :returns: the list of all signatures read.

        '''
        return [sig for _, sig in self.read_list]

    # -------------------------------------------------------------------------
    @property
    def write_list(self) -> List[Tuple[str, Signature]]:
        '''
        :returns: the sorted list of container_name,signature pairs that
            are written.

        '''
        if not self._sorted:
            self._read_list.sort()
            self._write_list.sort()
            self._sorted = True
        return self._write_list

    # -------------------------------------------------------------------------
    @property
    def signatures_written(self) -> List[Signature]:
        '''
        :returns: the list of all signatures written.

        '''
        return [sig for _, sig in self.write_list]

    # -------------------------------------------------------------------------
    @property
    def all_used_vars_list(self) -> List[Tuple[str, Signature]]:
        '''
        :returns: the sorted list of container_name,signature pairs that
            are used.

        '''
        all_vars = list(set(self._read_list) | set(self._write_list))
        all_vars.sort()
        return all_vars

    # -------------------------------------------------------------------------
    def add_read(self,
                 signature: Signature,
                 container_name: Optional[str] = None) -> None:
        '''This function adds a read access to the specified signature and
        container name. The container_name is optional and defaults to "",
        indicating that this signature is not based on importing a symbol
        from an external container (i.e. a module in Fortran).

        :param signature: the signature of the access.
        :param container_name: the container name (optional)

        '''
        if container_name:
            self._read_list.append((container_name, signature))
        else:
            self._read_list.append(("", signature))
        self._sorted = False

    # -------------------------------------------------------------------------
    def add_write(self,
                  signature: Signature,
                  container_name: Optional[str] = None) -> None:
        '''This function adds a write access to the specified signature and
        container name. The container_name is optional and defaults to "",
        indicating that this signature is not based on importing a symbol
        from an external container (i.e. a module in Fortran).

        :param signature: the signature of the access.
        :param container_name: the container name (optional)

        '''
        if container_name:
            self._write_list.append((container_name, signature))
        else:
            self._write_list.append(("", signature))
        self._sorted = False

    # -------------------------------------------------------------------------
    def is_read(self, signature: Signature) -> bool:
        '''
        Checks if the given signature is in the read list.

        :param signature: the signature to check

        :returns: whether the signature is in the read list (independent
            of the container name).

        '''
        return any(signature == sig for _, sig in self._read_list)

    # -------------------------------------------------------------------------
    def remove(self,
               signature: Signature,
               container_name: Optional[str] = "") -> None:
        '''
        This function removes a signature (if required with the corresponding
        module name specified) from the read and/or write list. If no
        `container_name` is specified, the signature is expected to be
        local (i.e. it will use "" as container name). A warning
        will be logged if the specified name is not found in any of the
        two lists.

        :param signature: the signature to remove.
        :param container_name: the optional container name if the variable is
            imported.

        '''
        var_info = (container_name, signature)
        not_found_counter = 0
        try:
            self._read_list.remove(var_info)
        except ValueError:
            # If the variable is not in that list, ignore it
            not_found_counter += 1
        try:
            self._write_list.remove(var_info)
        except ValueError:
            # If the variable is not in that list, ignore it
            not_found_counter += 1
        if not_found_counter == 2:
            logger = logging.getLogger(__name__)
            logger.warning(f"ExtractNode: Variable '{var_info[1]}' is to "
                           f"be removed, but it's neither in the list of "
                           f"read variables ({self._read_list}), nor "
                           f"in the list of write variables "
                           f"({self._write_list}).")
