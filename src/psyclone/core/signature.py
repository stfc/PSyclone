# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''This module provides management of variable access information.'''

from __future__ import print_function, absolute_import

import six

from psyclone.errors import InternalError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE


# =============================================================================
class Signature(object):
    '''Given a variable access of the form ``a(i,j)%b(k,l)%c``, the signature
    of this access is the tuple ``(a,b,c)``. For a simple scalar variable
    ``a`` the signature would just be ``(a,)``.
    The signature is the key used in `VariablesAccessInfo`. In order to make
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
        if isinstance(variable, (str, six.text_type)):
            # str() required for python2 unicode support
            self._signature = (str(variable),) + sub_tuple
        elif isinstance(variable, tuple):
            self._signature = variable + sub_tuple
        elif isinstance(variable, list):
            self._signature = tuple(variable) + sub_tuple
        elif isinstance(variable, Signature):
            self._signature = variable._signature + sub_tuple
        else:
            raise InternalError("Got unexpected type '{0}' in Signature "
                                "constructor".format(type(variable).__name__))

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
        return self._signature[indx]

    # ------------------------------------------------------------------------
    def __str__(self):
        return "%".join(self._signature)

    # ------------------------------------------------------------------------
    def to_language(self, component_indices, language_writer=None):
        # pylint: disable=too-many-locals
        '''Converts this signature with the provided indices to a string
        in the selected language.

        TODO 1320 This subroutine can be removed when we stop supporting
        strings - then we can use a PSyIR writer for the ReferenceNode
        to provide the right string.

        :param component_indices: the indices for each component of \
            the signature.
        :type component_indices: \
            :py:class:`psyclone.core.component_indices.ComponentIndices`
        :param language_writer: a backend visitor to convert PSyIR \
            expressions to a representation in the selected language. \
            This is used when creating error and warning messages.
        :type language_writer: None (default is Fortran), or an \
            instance of \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :raises InternalError: if the number of components in this signature \
            is different from the number of indices in component_indices.
        '''

        # Check if number of components between self and component_indices
        # is consistent:
        if len(self._signature) != len(component_indices):
            raise InternalError("Signature '{0}' has {1} components, but "
                                "component_indices {2} has {3}."
                                .format(self, len(self._signature),
                                        component_indices,
                                        len(component_indices)))
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.fortran import FortranWriter
        from psyclone.psyir.nodes import Literal, Node, Reference

        if language_writer is None:
            writer = FortranWriter()
        else:
            writer = language_writer

        # out_list collects the string representation of the components
        # including indices
        out_list = []
        for i, component in enumerate(self._signature):
            indices = component_indices[i]
            if not indices:
                out_list.append(component)
            else:
                # If there are indices, add the "(ind1, ind2, ...)"
                # TODO 1320: since we support strings and integer, we cannot
                # simply pass the list of indices to writer.gen_indices
                # (since it only accepts PSyIR Nodes). Instead we convert each
                # string to a Reference, and each integer to a Literal
                index_list = []

                for dimension in indices:
                    if isinstance(dimension, Node):
                        index_list.append(dimension)
                    elif isinstance(dimension, int):
                        index_list.append(Literal(str(dimension),
                                                  INTEGER_TYPE))
                    else:
                        ref = Reference(DataSymbol(dimension, INTEGER_TYPE))
                        index_list.append(ref)
                dims = writer.gen_indices(index_list, component)

                parenthesis = writer.array_parenthesis
                out_list.append(component + parenthesis[0] +
                                ",".join(dims) +
                                parenthesis[1])

        # Combine the components in out_list to form the language string.
        return writer.structure_character.join(out_list)

    # ------------------------------------------------------------------------
    def __repr__(self):
        return "Signature({0})".format("%".join(self._signature))

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
            raise TypeError("'<' not supported between instances of "
                            "'Signature' and '{0}'."
                            .format(type(other).__name__))
        return self._signature < other._signature

    # ------------------------------------------------------------------------
    def __le__(self, other):
        '''Required to compare signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError("'<=' not supported between instances of "
                            "'Signature' and '{0}'."
                            .format(type(other).__name__))
        return self._signature <= other._signature

    # ------------------------------------------------------------------------
    def __gt__(self, other):
        '''Required to compare signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError("'>' not supported between instances of "
                            "'Signature' and '{0}'."
                            .format(type(other).__name__))
        return self._signature > other._signature

    # ------------------------------------------------------------------------
    def __ge__(self, other):
        '''Required to compare signatures. It just compares the tuples.'''
        if not isinstance(other, Signature):
            raise TypeError("'>=' not supported between instances of "
                            "'Signature' and '{0}'."
                            .format(type(other).__name__))
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
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ["Signature"]
