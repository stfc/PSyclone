# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides management of variable access information.'''

from psyclone.errors import InternalError


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
