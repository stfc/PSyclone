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

'''This module contains a singleton class that manages LFRic types. '''


from collections import namedtuple

from psyclone.psyir.symbols import ArrayType, DataSymbol

from psyclone.errors import InternalError
from psyclone.domain.lfric import psyir


class LFRicTypes:
    '''This class implements a singleton that manages LFRic types.
    Using the 'call' interface, you can query the data type for
    LFRic types, e.g.:
    lfric_types = LFRicTypes.get()
    rvfdds = lfric_types("RealVectorFieldDataDataSymbol")

    '''

    # Class variable to store the singleton instance
    _instance = None

    # ------------------------------------------------------------------------
    @staticmethod
    def get():
        '''Static function that if necessary creates and returns the singleton
        LFricTypes instance.

        :returns: the singleton instance of LFRicTypes.
        :rtype: :py:class:`psyclone.domain.lfric.lfric_Types.LFRicTypes`

        '''
        if not LFRicTypes._instance:
            LFRicTypes._instance = LFRicTypes()
        return LFRicTypes._instance

    # ------------------------------------------------------------------------
    def __init__(self):

        if LFRicTypes._instance is not None:
            raise InternalError("You need to use 'LFRicTypes.get()' "
                                "to get the singleton instance.")
        self._name_to_class = {}

        self._create_fields()
        # Generate LFRic vector-field-data symbols as subclasses of
        # field-data symbols
        for intrinsic in ["Real", "Integer", "Logical"]:
            name = f"{intrinsic}VectorFieldDataDataSymbol"
            print("Creating now", name)
            baseclass = self(f"{intrinsic}FieldDataDataSymbol")
            self._name_to_class[name] = type(name, (baseclass, ), {})

    # ------------------------------------------------------------------------
    def __call__(self, name):
        ''':returns: the class of the required type.
        :rtype:  Class instance

        '''
        return self._name_to_class[name]

    # ------------------------------------------------------------------------
    def _create_fields(self):
        # Note, field_datatypes are no different to array_datatypes and are
        # treated in the same way. They are only separated into a different
        # list because they are used to create vector field datatypes and
        # symbols.

        # The Array namedtuple has 4 properties: the first determines the
        # names of the resultant datatype and datasymbol classes, the second
        # references the generic scalar type classes declared above, the third
        # specifies the dimensions of the array by specifying a list of scalar
        # type classes declared above, and the fourth specifies any additional
        # class properties that should be declared in the generated datasymbol
        # class.

        Array = namedtuple('Array',
                           ["name", "scalar_type", "dims", "properties"])
        field_datatypes = [
            Array("RealFieldData", psyir.LfricRealScalarDataType,
                  ["number of unique dofs"], ["fs"]),
            Array("IntegerFieldData", psyir.LfricIntegerScalarDataType,
                  ["number of unique dofs"], ["fs"]),
            Array("LogicalFieldData", psyir.LfricLogicalScalarDataType,
                  ["number of unique dofs"], ["fs"])]

        array_datatypes = [
            Array("Operator", psyir.LfricRealScalarDataType,
                  ["number of dofs", "number of dofs", "number of cells"],
                  ["fs_from", "fs_to"]),
            Array("DofMap", psyir.LfricIntegerScalarDataType,
                  ["number of dofs"], ["fs"]),
            Array("BasisFunctionQrXyoz", psyir.LfricRealScalarDataType,
                  [psyir.LfricDimension, "number of dofs",
                   "number of qr points in xy",
                   "number of qr points in z"], ["fs"]),
            Array("BasisFunctionQrFace", psyir.LfricRealScalarDataType,
                  [psyir.LfricDimension, "number of dofs",
                   "number of qr points in faces",
                   "number of faces"], ["fs"]),
            Array("BasisFunctionQrEdge", psyir.LfricRealScalarDataType,
                  [psyir.LfricDimension, "number of dofs",
                   "number of qr points in edges",
                   "number of edges"], ["fs"]),
            Array("DiffBasisFunctionQrXyoz", psyir.LfricRealScalarDataType,
                  [psyir.LfricDimension, "number of dofs",
                   "number of qr points in xy",
                   "number of qr points in z"], ["fs"]),
            Array("DiffBasisFunctionQrFace", psyir.LfricRealScalarDataType,
                  [psyir.LfricDimension, "number of dofs",
                   "number of qr points in faces",
                   "number of faces"], ["fs"]),
            Array("DiffBasisFunctionQrEdge", psyir.LfricRealScalarDataType,
                  [psyir.LfricDimension, "number of dofs",
                   "number of qr points in edges", "number of edges"], ["fs"]),
            Array("QrWeightsInXy", psyir.LfricRealScalarDataType,
                  ["number of qr points in xy"], []),
            Array("QrWeightsInZ", psyir.LfricRealScalarDataType,
                  ["number of qr points in z"], []),
            Array("QrWeightsInFaces", psyir.LfricRealScalarDataType,
                  ["number of qr points in faces"], []),
            Array("QrWeightsInEdges", psyir.LfricRealScalarDataType,
                  ["number of qr points in edges"], [])
            ]

        for array_type in array_datatypes + field_datatypes:
            name = f"{array_type.name}DataType"
            print(f"Creating {name}(ArrayType)")
            self._create_array_data_type_class(name, len(array_type.dims),
                                               array_type.scalar_type)

            my__class = self(name)
            name = f"{array_type.name}DataSymbol"
            print(f"Creating {name}(DataSymbol)")
            self._create_array_data_symbol_class(name, my__class,
                                                 array_type.properties)

    # ------------------------------------------------------------------------
    def _create_array_data_type_class(self, name, num_dims, scalar_type):

        # ---------------------------------------------------------------------
        # This is the __init__ function for the newly declared classes, which
        # will be added as an attribute for the newly created class. It parses
        # the additional positional and keyword arguments and sets them as
        # attributes.

        def __myinit__(self, dims):
            if len(dims) != self.num_dims:
                raise TypeError(f"'{type(self).__name__}' expected the number "
                                f"of supplied dimensions to be {self.num_dims}"
                                f" but found {len(dims)}.")
            ArrayType.__init__(self, self.scalar_class(), dims)

        # ---------------------------------------------------------------------
        self._name_to_class[name] = \
            type(name, (ArrayType, ),
                 {"__init__": __myinit__,
                  "scalar_class": scalar_type,
                  "num_dims": num_dims})

    # ------------------------------------------------------------------------
    def _create_array_data_symbol_class(self, name, scalar_type,
                                        array_properties):
        '''This function creates an array-data-symbol-class and adds it to
        the internal type dictionary.

        :param str name: the name of the class to be created.
        :param scalar_type: ??
        :param array_properties: the list of additional required properties \
            to be passed to the constructor.
        :type array_properties: List[str]

        '''

        # ---------------------------------------------------------------------
        # This is the __init__ function for the newly declared classes, which
        # will be added as an attribute for the newly created class. It parses
        # the additional positional and keyword arguments and sets them as
        # attributes.

        def __myinit__(self, name, dims, *args, **kwargs):
            # Set all the positional arguments as attributes:
            for i, arg in enumerate(args):
                setattr(self, self.parameters[i], arg)
            # Now handle the keyword arguments: any keyword arguments
            # that are declared as parameter will be set as attribute,
            # anything else will be passed to the constructor of the
            # base class.
            remaining_kwargs = {}
            for key, value in kwargs.items():
                # It is one of the additional parameters, set it as
                # attribute:
                if key in self.parameters:
                    setattr(self, key, value)
                else:
                    # Otherwise add it as keyword parameter for the
                    # base class constructor
                    remaining_kwargs[key] = value
            DataSymbol.__init__(self, name, self.scalar_class(dims),
                                **remaining_kwargs)
        # ----------------------------------------------------------------

        # Now create the actual class. We need to keep a copy of the parameters
        # of this class as attributes, otherwise they would be shared among the
        # several instances of the __myinit__function: this affects the
        # required arguments (array_type.properties) and scalar class:
        self._name_to_class[name] = \
            type(name, (DataSymbol, ),
                 {"__init__": __myinit__,
                  "scalar_class": scalar_type,
                  "parameters": array_properties})
