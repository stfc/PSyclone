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

'''Module containing the InterGridArg class which captures the metadata
associated with an intergrid argument. Supports the creation, modification
and Fortran output of an intergrid argument.

'''
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.field_arg import FieldArg


class InterGridArg(FieldArg):
    '''Class to capture LFRic kernel metadata information for an intergrid
    argument.

    :param Optional[str] datatype: the datatype of this InterGrid \
        argument (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        intergrid argument (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        InterGrid is on (W0, ...).
    :param Optional[str] mesh_arg: the type of mesh that this \
        InterGrid arg is on (coarse or fine).

    '''
    def __init__(self, datatype=None, access=None, function_space=None,
                 mesh_arg=None):
        super().__init__(datatype, access, function_space)
        if mesh_arg is None:
            self._mesh_arg = mesh_arg
        else:
            self.mesh_arg = mesh_arg

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for an InterGrid \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: an instance of InterGridArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.InterGridArg`

        '''
        InterGridArg.check_psyir(
            psyir, nargs=5, encoding=Fortran2003.Structure_Constructor)
        args = psyir.children[1]
        datatype = args.children[1].tostr()
        access = args.children[2].tostr()
        function_space = args.children[3].tostr()
        mesh_arg = args.children[4].children[1].tostr()
        return InterGridArg(datatype, access, function_space, mesh_arg)

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from a Fortran string.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.cls`

        '''
        structure_constructor = cls.create_psyir(
            fortran_string, encoding=Fortran2003.Structure_Constructor)
        return cls.create_from_psyir(structure_constructor)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as a \
            Fortran string.
        :rtype: str

        raises ValueError: if one or more of the datatype, access or \
            function_space values have not been set.

        '''
        if not (self.datatype and self.access and self.function_space and
                self.mesh_arg):
            raise ValueError(
                f"Values for datatype, access, function_space and mesh_arg "
                f"must be provided before calling the fortran_string method, "
                f"but found '{self.datatype}', '{self.access}', "
                f"'{self.function_space}' and '{self.mesh_arg}'.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space}, mesh_arg={self.mesh_arg})")

    @property
    def mesh_arg(self):
        '''

        :returns: the mesh type for this intergrid argument.
        :rtype: str

        '''
        return self._mesh_arg

    @mesh_arg.setter
    def mesh_arg(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            mesh_argument type (gh_coarse or gh_fine).

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_MESH_TYPES:
            raise ValueError(
                f"The fifth metadata entry for an intergrid argument should "
                f"be a recognised mesh type (one of "
                f"{const.VALID_MESH_TYPES}), but found '{value}'.")
        self._mesh_arg = value
