# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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


class ReadWriteInfo:
    '''This class stores signature and module name of variables read or
    written. The module name is optional, it will default to "" if the
    signature belongs to a local symbol, otherwise it is the name of
    the module from which it must be imported.

    The information is stores in lists of tuples, the first element being
    the module name, the second the signature. When accessing any of these
    two lists, the getter will make sure that the lists are sorted. This
    will guarantee that for example the kernel extraction and driver creation
    read the variables in the same order.
    '''

    def __init__(self):
        self._read_list = []
        self._write_list = []
        self._sorted = True

    # -------------------------------------------------------------------------
    @property
    def read_list(self):
        ''':returns the sorted list of module_name,signature pairs that are \
            read.
        :type: List[Tuple[str,:py:class:`psyclone.core.Signature`]]

        '''
        if not self._sorted:
            self._read_list.sort()
            self._write_list.sort()
            self._sorted = True
        return self._read_list

    # -------------------------------------------------------------------------
    @property
    def write_list(self):
        ''':returns the sorted list of module_name,signature pairs that are \
            written.
        :type: List[Tuple[str,:py:class:`psyclone.core.Signature`]]

        '''
        if not self._sorted:
            self._read_list.sort()
            self._write_list.sort()
            self._sorted = True
        return self._write_list

    # -------------------------------------------------------------------------
    @property
    def set_of_all_used_vars(self):
        '''This property returns a set with all (module_name, Signature)
        tuples. Since it is a set this guarantees that each tuple is only
        listed once.
        :returns:  set with all (module_name, Signature) pairs.
        :rtype: List[Set[str, :py:class:`psyclone.core.Signature`]]

        '''
        return set(self._read_list) | set(self._write_list)

    # -------------------------------------------------------------------------
    def add_read(self, signature, module_name=None):
        '''This function adds a read access to the specified signature and
        module name. The module_name is optional and defaults to ""
        (indicating a local access).

        :param signature: the signature of the access.
        :type signature: :py:class:`psyclone.core.Signature`
        :param module_name: the module name (optional)
        :type module_name: Optional[str]

        '''
        if module_name:
            self._read_list.append((module_name, signature))
        else:
            self._read_list.append(("", signature))
        self._sorted = False

    # -------------------------------------------------------------------------
    def add_write(self, signature, module_name=None):
        '''This function adds a write access to the specified signature and
        module name. The module_name is optional and defaults to ""
        (indicating a local access).

        :param signature: the signature of the access.
        :type signature: :py:class:`psyclone.core.Signature`
        :param module_name: the module name (optional)
        :type module_name: Optional[str]

        '''
        if module_name:
            self._write_list.append((module_name, signature))
        else:
            self._write_list.append(("", signature))
        self._sorted = False

    # -------------------------------------------------------------------------
    def is_read(self, signature):
        ''':returns: whether the signature is in the read list (independent \
            of the module name).
        :rtype: bool

        '''
        return any(signature == sig for _, sig in self._read_list)
