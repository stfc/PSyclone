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

'''Module containing tests for the KernelMetadataSymbol
kernel-layer-specific symbol. The tests include translation of
language-level PSyIR to PSyclone LFRic Kernel PSyIR and PSyclone LFRic
Kernel PSyIR to language-level PSyIR.

'''
import re

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk, get_child

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.field_arg import FieldArg

from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import Container


class LFRicContainer(Container):
    '''An LFRic-specific Container. This specialises the generic Container
    node and adds in any domain-specific information.

    :param str name: the name of the container.
    :param metadata: the metadata object.
    :type metadata: :py:class:`psyclone.domain.gocean.kernel.psyir.\
        LFRicKernelMetadata`
    :param parent: optional parent node of this Container in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param symbol_table: initialise the node with a given symbol table.
    :type symbol_table: \
        Optional[:py:class:`psyclone.psyir.symbols.SymbolTable`]

    '''
    def __init__(self, name, metadata, **kwargs):
        super().__init__(name, **kwargs)
        # The metadata object capturing LFRic kernel metadata.
        self._metadata = metadata

    @classmethod
    def create(cls, name, metadata, symbol_table, children):
        '''Create an LFRic Container instance given a name, metadata, a symbol
        table and a list of child nodes. An LFRic-specific kernel is
        created with the metadata describing the kernel interface for
        a single kernel routine within the container.
        :param str name: the name of the Container.
        :param symbol_table: the symbol table associated with this \
            Container.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param metadata: the metadata object.
        :type metadata: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`
        :param children: a list of PSyIR nodes contained in the \
            Container. These must be Containers or Routines.
        :type children: List[:py:class:`psyclone.psyir.nodes.Container` \
            | :py:class:`psyclone.psyir.nodes.Routine`]
        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` or subclass
            thereof
        '''
        return cls(name, metadata, children=children,
                   symbol_table=symbol_table.detach())

    @property
    def metadata(self):
        '''
        :returns the LFRic metadata object.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`
        '''
        return self._metadata

    def lower_to_language_level(self):
        '''Lower this LFRic-specific container to language level psyir.'''

        # Create metadata symbol and add it to the container symbol
        # table.
        data_symbol = self.metadata.lower_to_psyir()
        self.symbol_table.add(data_symbol)

        # Replace this LFRic container with a generic container
        children = self.pop_all_children()
        generic_container = Container.create(
            self.name, self.symbol_table.detach(), children)
        self.replace_with(generic_container)


class LFRicKernelMetadata():
    '''Contains LFRic kernel metadata. This class supports kernel
    metadata creation, modification, loading from a fortran string,
    writing to a fortran string, raising from existing language-level
    PSyIR and lowering to language-level psyir.

    :param meta_args: a list of 'meta_arg' objects which capture the \
        metadata values of the kernel arguments.

    :type meta_args: Optional[List[:py:class:`ScalarArg` | :py:class:`FieldArg` \
        | :py:class:`OperatorArg`]]

    :param meta_funcs: a list of 'meta_arg' objects which capture the \
        metadata values of the kernel arguments.

    :type meta_funcs: Optional[List[:py:class:`GridArg` | :py:class:`FieldArg` \
        | :py:class:`ScalarArg`]]

    :param meta_reference_element: a list of 'meta_arg' objects which capture the \
        metadata values of the kernel arguments.
    :type meta_reference_element: Optional[List[:py:class:`GridArg` | :py:class:`FieldArg` \
        | :py:class:`ScalarArg`]]

    :param meta_mesh: a list of 'meta_arg' objects which capture the \
        metadata values of the kernel arguments.
    :type meta_mesh: Optional[List[:py:class:`ScalarArg` | :py:class:`FieldArg` \
        | :py:class:`OperatorArg`]]


    :param shape: quadrature.
    :type shape: Optional[str]

    :param operates_on: the name of the quantity that this kernel is
        intended to iterate over.
    :type operates_on: Optional[str]

    :param procedure_name: the name of the kernel procedure to call.
    :type procedure_name: Optional[str]
    :param name: the name of the symbol to use for the metadata in \
        language-level PSyIR.
    :type name: Optional[str]

    '''
    @staticmethod
    def create_from_psyir(symbol):
        '''Create a new instance of LFRicKernelMetadata populated with
        metadata from a kernel in language-level PSyIR.
        :param symbol: the symbol in which the metadata is stored \
            in language-level PSyIR.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        :returns: an instance of LFRicKernelMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`
        :raises TypeError: if the symbol argument is not the expected \
            type.
        :raises InternalError: if the datatype of the provided symbol \
            is not the expected type.

        '''
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found a "
                f"{type(symbol).__name__}.")

        datatype = symbol.datatype

        if not isinstance(datatype, UnknownFortranType):
            raise InternalError(
                f"Expected kernel metadata to be stored in the PSyIR as "
                f"an UnknownFortranType, but found "
                f"{type(datatype).__name__}.")

        # In an UnknownFortranType, the declaration is stored as a
        # string, so use create_from_fortran_string()
        return LFRicKernelMetadata.create_from_fortran_string(
            datatype.declaration)


    @staticmethod
    def create_from_fortran_string(fortran_string):
        '''Create a new instance of GOceanKernelMetadata populated with
        metadata stored in a fortran string.
        :param str fortran_string: the metadata stored as Fortran.
        :returns: an instance of GOceanKernelMetadata.
        :rtype: :py:class:`psyclone.domain.gocean.kernel.psyir.\
            GOceanKernelMetadata`
        :raises ValueError: if the string does not contain a fortran \
            derived type.
        :raises ParseError: if the metadata has an unexpected format.

        meta_args
        meta_funcs
        meta_reference_element
        meta_mesh

        '''
        # Ensure the Fortran2003 parser is initialised.
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        try:
            spec_part = Fortran2003.Derived_Type_Def(reader)
        except Fortran2003.NoMatchError:
            # pylint: disable=raise-missing-from
            raise ValueError(
                f"Expected kernel metadata to be a Fortran derived type, but "
                f"found '{fortran_string}'.")

        kernel_metadata.name = spec_part.children[0].children[1].tostr()

        # Extract and store the required 'operates_on', 'gh_shape' and
        # 'procedure_name' properties from the parse tree

        # the value of gh_shape (gh_quadrature_XYoZ, ...))
        value = LFRicKernelMetadata._get_property(
            spec_part, "gh_shape").string
        kernel_metadata.gh_shape = value

        # the value of operates_on (cell_column, ...)
        value = LFRicKernelMetadata._get_property(
            spec_part, "operates_on").string
        kernel_metadata.operates_on = value

        # the name of the procedure that this metadata refers to.
        kernel_metadata.procedure_name = LFRicKernelMetadata._get_property(
            spec_part, "code").string

        # meta_args contains arguments which have
        # properties. Therefore create appropriate
        # instances to capture this information.
        meta_args = LFRicMetadata._get_property(
            spec_part, "meta_args")
        args = walk(meta_args, Fortran2003.Ac_Value_List)
        if not args:
            raise ParseError(
                f"meta_args should be a list, but found "
                f"'{str(meta_args)}' in '{spec_part}'.")

        for meta_arg in args[0].children:
            print( meta_arg.children[1].children[0])
            exit(1)
            #GOceanKernelMetadata.GridArg(meta_arg, kernel_metadata))
            #elif len(meta_arg.children[1].children) == 3:
            #    # scalar and field args have 3 arguments
            #    arg2 = meta_arg.children[1].children[1].string.lower()
            #    if arg2 in const.VALID_FIELD_GRID_TYPES:
            #        kernel_metadata.meta_args.append(
            #            GOceanKernelMetadata.FieldArg(
            #                meta_arg, kernel_metadata))
            #    elif arg2 in const.VALID_SCALAR_TYPES:
            #        kernel_metadata.meta_args.append(
            #            GOceanKernelMetadata.ScalarArg(
            #                meta_arg, kernel_metadata))
            #    else:
            #        raise ParseError(
            #            f"Expected a 'meta_arg' entry with 3 arguments to "
            #            f"either be a field or a scalar, but found '{arg2}' "
            #            f"as the second argument instead of "
            #            f"'{const.VALID_FIELD_GRID_TYPES}' (fields) or "
            #            f"'{const.VALID_SCALAR_TYPES}' (scalars).")
            #else:
            #    raise ParseError(
            #        f"'meta_args' should have either 2 or 3 arguments, but "
            #        f"found {len(meta_arg.children[1].children)} in "
            #        f"{str(meta_arg)}.")

        return kernel_metadata

    #general purpose kernel operates_on=CELL_COLUMN (no CMA)
    #general purpose kernel operates_on=DOMAIN
    #CMA construction kernel
    #CMA application kernel
    #CMA matrix-matrix kernel
    #inter-grid kernel

    VALID_NAME = re.compile(r'[a-zA-Z_][\w]*')

    def __init__(self, operates_on=None, gh_shape=None, meta_args=None,
                 meta_funcs=None, meta_reference_element=None,
                 meta_mesh=None, procedure_name=None, name=None):
        # Validate values using setters if they are not None
        self._operates_on = None
        if operates_on is not None:
            self.operates_on = operates_on
        self._gh_shape = None
        if gh_shape is not None:
            self.gh_shape = gh_shape
        if meta_args is None:
            self._meta_args = []
        else:
            if not isinstance(meta_args, list):
                raise TypeError(f"meta_args should be a list but found "
                                f"{type(meta_args).__name__}.")
            for entry in meta_args:
                if not isinstance(entry,
                                  (GOceanKernelMetadata.FieldArg,
                                   GOceanKernelMetadata.GridArg,
                                   GOceanKernelMetadata.ScalarArg)):
                    raise TypeError(
                        f"meta_args should be a list of FieldArg, GridArg or "
                        f"ScalarArg objects, but found "
                        f"{type(entry).__name__}.")
            self._meta_args = meta_args
        if meta_funcs is None:
            self._meta_funcs = []
        if meta_reference_element is None:
            self._meta_reference_element = []
        if meta_mesh is None:
            self._meta_mesh = []

        self._procedure_name = None
        if procedure_name is not None:
            self.procedure_name = procedure_name
        self._name = None
        if name is not None:
            self.name = name

    def lower_to_psyir(self):
        ''' Lower the metadata to language-level PSyIR.
        :returns: metadata as stored in language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        '''
        return DataTypeSymbol(
            str(self.name), UnknownFortranType(self.fortran_string()))

    @staticmethod
    def create_from_psyir(symbol):
        '''Create a new instance of GOceanKernelMetadata populated with
        metadata from a kernel in language-level PSyIR.
        :param symbol: the symbol in which the metadata is stored \
            in language-level PSyIR.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        :returns: an instance of GOceanKernelMetadata.
        :rtype: :py:class:`psyclone.domain.gocean.kernel.psyir.\
            GOceanKernelMetadata`
        :raises TypeError: if the symbol argument is not the expected \
            type.
        :raises InternalError: if the datatype of the provided symbol \
            is not the expected type.
        '''
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found a "
                f"{type(symbol).__name__}.")

        datatype = symbol.datatype

        if not isinstance(datatype, UnknownFortranType):
            raise InternalError(
                f"Expected kernel metadata to be stored in the PSyIR as "
                f"an UnknownFortranType, but found "
                f"{type(datatype).__name__}.")

        # In an UnknownFortranType, the declaration is stored as a
        # string, so use create_from_fortran_string()
        return LFRicKernelMetadata.create_from_fortran_string(
            datatype.declaration)

    @staticmethod
    def create_from_fortran_string(fortran_string):
        '''Create a new instance of GOceanKernelMetadata populated with
        metadata stored in a fortran string.
        :param str fortran_string: the metadata stored as Fortran.
        :returns: an instance of GOceanKernelMetadata.
        :rtype: :py:class:`psyclone.domain.gocean.kernel.psyir.\
            GOceanKernelMetadata`
        :raises ValueError: if the string does not contain a fortran \
            derived type.
        :raises ParseError: if the metadata has an unexpected format.
        '''
        kernel_metadata = LFRicKernelMetadata()

        # Ensure the Fortran2003 parser is initialised.
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        try:
            spec_part = Fortran2003.Derived_Type_Def(reader)
        except Fortran2003.NoMatchError:
            # pylint: disable=raise-missing-from
            raise ValueError(
                f"Expected kernel metadata to be a Fortran derived type, but "
                f"found '{fortran_string}'.")

        kernel_metadata.name = spec_part.children[0].children[1].tostr()

        # the value of operates on (CELL_COLUMN, ...)
        value = LFRicKernelMetadata._get_property(
            spec_part, "operates_on").string
        kernel_metadata.operates_on = value

        # the value of index offset (NE, ...)
        value = LFRicKernelMetadata._get_property(
            spec_part, "gh_shape").string
        kernel_metadata.gh_shape = value

        # the name of the procedure that this metadata refers to.
        kernel_metadata.procedure_name = LFRicKernelMetadata._get_property(
            spec_part, "code").string

        # meta_args contains arguments which have
        # properties. Therefore create appropriate (OperatorArg, ScalarArg
        # or FieldArg) instances to capture this information.
        kernel_metadata.meta_args = LFRicKernelMetadata._get_property(
            spec_part, "meta_args")
        args = walk(kernel_metadata.meta_args, Fortran2003.Ac_Value_List)
        if not args:
            raise ParseError(
                f"meta_args should be a list, but found "
                f"'{str(meta_args)}' in '{spec_part}'.")

        kernel_metadata.meta_args = []
        for meta_arg in args[0].children:
            form = meta_arg.children[1].children[0].tostr()
            print(type(meta_arg))
            print(meta_arg)

            exit(1)
            if form == "GH_FIELD":
                field_arg = FieldArg.create_from_psyir(meta_arg)
                kernel_metadata.meta_args.append(field_arg)
            elif "GH_FIELD" in form and "*" in form:
                kernel_metadata.meta_args.append(
                    LFRicKernelMetadata.FieldVectorArg(form, form.split("*")[1].strip()))                
            elif form == "GH_SCALAR":
                kernel_metadata.meta_args.append(
                    LFRicKernelMetadata.ScalarArg(form))
            elif form == "GH_OPERATOR":
                kernel_metadata.meta_args.append(
                    LFRicKernelMetadata.OperatorArg(form))
            else:
                raise ParseError(
                    f"Expected a 'meta_arg' entry with to "
                    f"either be a field, a scalar or an operator, but found "
                    f"'{meta_arg}'.")

        # meta_args contains arguments which have
        # properties. Therefore create appropriate (GridArg, ScalarArg
        # or FieldArg) instances to capture this information.
        meta_args = LFRicKernelMetadata._get_property(
            spec_part, "meta_funcs")
        args = walk(meta_args, Fortran2003.Ac_Value_List)
        if not args:
            raise ParseError(
                f"meta_funcs should be a list, but found "
                f"'{str(meta_funcs)}' in '{spec_part}'.")

        # meta_args contains arguments which have
        # properties.
        LFRicKernelMetadata.meta_reference_element = []
        try:
            LFRicKernelMetadata.meta_reference_element = LFRicKernelMetadata._get_property(
                spec_part, "meta_reference_element")
        except ParseError:
            pass
        args = walk(LFRicKernelMetadata.meta_reference_element, Fortran2003.Ac_Value_List)
        if not args:
            LFRicKernelMetadata.meta_reference_element = []
            #raise ParseError(
            #    f"meta_reference_element should be a list, but found "
            #    f"'{str(LFRicKernelMetadata.meta_reference_element)}' in '{spec_part}'.")

        # meta_mesh contains arguments which have
        # properties.
        try:
            meta_mesh = LFRicKernelMetadata._get_property(
                spec_part, "meta_mesh")
        except ParseError:
            # meta_mesh is not specified in the metadata
            LFRicKernelMetadata.meta_mesh = []
        finally:
            args = walk(meta_args, Fortran2003.Ac_Value_List)
            if not args:
                raise ParseError(
                    f"meta_mesh should be a list, but found "
                    f"'{str(meta_mesh)}' in '{spec_part}'.")
        return kernel_metadata

    @staticmethod
    def _get_property(spec_part, property_name):
        '''Internal utility that gets the property 'property_name' from an
        fparser2 tree capturing gocean metadata. It is assumed that
        the code property is part of a type bound procedure and that
        the other properties are part of the data declarations.
        :param spec_part: the fparser2 parse tree containing the metadata.
        :type spec_part: :py:class:`fparser.two.Fortran2003.Derived_Type_Def`
        :param str property_name: the name of the property whose value \
            is being extracted from the metadata.
        :returns: the value of the property.
        :rtype: :py:class:`fparser.two.Fortran2003.Name | \
            :py:class:`fparser.two.Fortran2003.Array_Constructor`
        :raises ParseError: if the property name is not found in the \
            metadata.
        '''
        if property_name.lower() == "code":
            # The value of 'code' should be found in a type bound
            # procedure (after the contains keyword)
            type_bound_procedure = get_child(
                spec_part, Fortran2003.Type_Bound_Procedure_Part)
            if not type_bound_procedure:
                raise ParseError(
                    f"No type-bound procedure found within a 'contains' "
                    f"section in '{spec_part}'.")
            if len(type_bound_procedure.children) != 2:
                raise ParseError(
                    f"Expecting a type-bound procedure, but found "
                    f"'{spec_part}'.")
            specific_binding = type_bound_procedure.children[1]
            if not isinstance(specific_binding, Fortran2003.Specific_Binding):
                raise ParseError(
                    f"Expecting a specific binding for the type-bound "
                    f"procedure, but found '{specific_binding}' in "
                    f"'{spec_part}'.")
            binding_name = specific_binding.children[3]
            procedure_name = specific_binding.children[4]
            if binding_name.string.lower() != "code" and procedure_name:
                raise ParseError(
                    f"Expecting the type-bound procedure binding-name to be "
                    f"'code' if there is a procedure name, but found "
                    f"'{str(binding_name)}' in '{spec_part}'.")
            if not procedure_name:
                # Support the alternative metadata format that does
                # not include 'code =>'
                procedure_name = binding_name
            return procedure_name

        # The 'property_name' will be declared within Component_Part.
        component_part = get_child(spec_part, Fortran2003.Component_Part)
        if not component_part:
            raise ParseError(
                f"No declarations were found in the kernel metadata: "
                f"'{spec_part}'.")
        # Each name/value pair will be contained within a Component_Decl
        for component_decl in walk(component_part, Fortran2003.Component_Decl):
            # Component_Decl(Name('name') ...)
            name = component_decl.children[0].string
            if name.lower() == property_name.lower():
                # The value will be contained in a Component_Initialization
                comp_init = get_child(
                    component_decl, Fortran2003.Component_Initialization)
                if not comp_init:
                    raise ParseError(
                        f"No value for property {property_name} was found "
                        f"in '{spec_part}'.")
                # Component_Initialization('=', Name('name'))
                return comp_init.children[1]
        raise ParseError(
            f"'{property_name}' was not found in {str(spec_part)}.")

    def fortran_string(self):
        '''
        :returns: the metadata represented by this instance as Fortran.
        :rtype: str
        '''
        lfric_args = []
        for lfric_arg in self.meta_args:
            lfric_args.append(lfric_arg.fortran_string())
        lfric_args_str = ", &\n".join(lfric_args)
        result = (
            f"TYPE, PUBLIC, EXTENDS(kernel_type) :: {self.name}\n"
            f"  TYPE(arg_type) :: meta_args({len(self.meta_args)}) = "
            f"(/ &\n{lfric_args_str}/)\n"
            f"  TYPE(func_type) :: meta_funcs(x) = xxx\n"
            f"  TYPE(ref_type) :: meta_ref(x) = xxx\n"
            f"  TYPE(grid_type) :: meta_grid(x) = xxx\n"
            f"  INTEGER :: OPERATES_ON = {self.operates_on}\n"
            f"  INTEGER :: GH_SHAPE = {self.gh_shape}\n"
            f"  CONTAINS\n"
            f"    PROCEDURE, NOPASS :: {self.procedure_name}\n"
            f"END TYPE {self.name}\n")
        return result

    #class FieldVectorArg():
    #    def __init__(self, arg_string, vector_size, kernel_metadata):
    #        self.form = "GH_FIELD"
    #        self.vector_size = vector_size
    #
    #    def fortran_string(self):
    #        ''' xxx '''
    #        return f"arg_type({self.form}*{self.vector_size}, ...)"

    class ScalarArg():
        def __init__(self, arg_string, kernel_metadata):
            self.form = "GH_SCALAR"

        def fortran_string(self):
            ''' xxx '''
            return f"arg_type({self.form}, ...)"

    class OperatorArg():
        def __init__(arg_string, kernel_metadata):
            self.form = "GH_OPERATOR"

        def fortran_string(self):
            ''' xxx '''
            return f"arg_type({self.form}, ...)"

    # class ColumnwiseOperatorArg():
