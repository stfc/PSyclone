# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Modified by N. Nobre, STFC Daresbury Lab
# Modified by S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module provides management of variable access information.'''

from psyclone.errors import InternalError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE


# =============================================================================
class Signature:
    '''Given a variable access of the form ``a(i,j)%b(k,l)%c``, the signature
    of this access is the tuple ``(a,b,c)``. For a simple scalar variable
    ``a`` the signature would just be ``(a,)``.
    The signature is the key used in `VariablesAccessMap`. In order to make
    sure two different signature objects containing the same variable
    can be used as a key, this class implements `__hash__` and other special
    functions.
    The constructor also supports appending an existing signature to this
    new signature using the `sub_sig` argument. This is used in
    StructureReference to assemble the overall signature of a structure
    access.

    :param variable: the variable that is accessed.
    :type variable: str or tuple of str or list of str

    :param sub_sig: a signature that is to be added to this new signature.
    :type sub_sig: :py:class:`psyclone.core.Signature`

    '''
    def __init__(self, variable, sub_sig=None):
        if sub_sig:
            sub_tuple = sub_sig._signature
        else:
            # null-tuple
            sub_tuple = ()
        if isinstance(variable, str):
            self._signature = tuple(variable.split("%")) + sub_tuple
        elif isinstance(variable, tuple):
            self._signature = variable + sub_tuple
        elif isinstance(variable, list):
            self._signature = tuple(variable) + sub_tuple
        elif isinstance(variable, Signature):
            self._signature = variable._signature + sub_tuple
        else:
            raise InternalError(f"Got unexpected type "
                                f"'{type(variable).__name__}' in Signature "
                                f"constructor")

    # ------------------------------------------------------------------------
    @property
    def is_structure(self):
        ''':returns: True if this signature represents a structure.
        :rtype: bool
        '''
        return len(self._signature) > 1

    # ------------------------------------------------------------------------
    def __len__(self):
        ''':returns: the number of components of this signature.
        :rtype: int'''
        return len(self._signature)

    # ------------------------------------------------------------------------
    def __getitem__(self, indx):
        if isinstance(indx, slice):
            return Signature(self._signature[indx])
        return self._signature[indx]

    # ------------------------------------------------------------------------
    def __str__(self):
        return "%".join(self._signature)

    # ------------------------------------------------------------------------
    def __repr__(self):
        return f"Signature({str(self)})"

    # ------------------------------------------------------------------------
    def __hash__(self):
        '''This returns a hash value that is independent of the instance.
        I.e. two instances with the same signature will have the same
        hash key.
        '''
        return hash(self._signature)

    # ------------------------------------------------------------------------
    def __eq__(self, other):
        '''Required in order to use a Signature instance as a key.
        Compares two objects (one of which might not be a Signature).'''
        if not hasattr(other, "_signature"):
            return False
        return self._signature == other._signature

    # ------------------------------------------------------------------------
    def __ne__(self, other):
        '''Required for != comparisons of Signatures with python2.
        Compares two objects (one of which might not be a Signature).'''
        if not hasattr(other, "_signature"):
            return True
        return self._signature != other._signature

    # ------------------------------------------------------------------------
    def __lt__(self, other):
        '''Required to sort signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError(f"'<' not supported between instances of "
                            f"'Signature' and '{type(other).__name__}'.")
        return self._signature < other._signature

    # ------------------------------------------------------------------------
    def __le__(self, other):
        '''Required to compare signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError(f"'<=' not supported between instances of "
                            f"'Signature' and '{type(other).__name__}'.")
        return self._signature <= other._signature

    # ------------------------------------------------------------------------
    def __gt__(self, other):
        '''Required to compare signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError(f"'>' not supported between instances of "
                            f"'Signature' and '{type(other).__name__}'.")
        return self._signature > other._signature

    # ------------------------------------------------------------------------
    def __ge__(self, other):
        '''Required to compare signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError(f"'>=' not supported between instances of "
                            f"'Signature' and '{type(other).__name__}'.")
        return self._signature >= other._signature

    # ------------------------------------------------------------------------
    @property
    def var_name(self):
        ''':returns: the actual variable name, i.e. the first component of
            the signature.
        :rtype: str
        '''
        return self._signature[0]


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ["Signature"]
