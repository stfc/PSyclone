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

'''Module containing the LFRicKernelMetadata
kernel-layer-specific class that captures the LFRic kernel metadata.

'''
from fparser.two import Fortran2003
from fparser.two.utils import walk, get_child

from psyclone.configuration import Config
from psyclone.domain.lfric.kernel.common_metadata import CommonMetadata
from psyclone.domain.lfric.kernel.evaluator_targets_metadata import \
    EvaluatorTargetsMetadata
from psyclone.domain.lfric.kernel.meta_args_metadata import \
    MetaArgsMetadata
from psyclone.domain.lfric.kernel.meta_funcs_metadata import \
    MetaFuncsMetadata
from psyclone.domain.lfric.kernel.meta_mesh_metadata import \
    MetaMeshMetadata
from psyclone.domain.lfric.kernel.meta_ref_element_metadata import \
    MetaRefElementMetadata
from psyclone.domain.lfric.kernel.operates_on_metadata import \
    OperatesOnMetadata
from psyclone.domain.lfric.kernel.shapes_metadata import ShapesMetadata
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, UnknownFortranType


class LFRicKernelMetadata(CommonMetadata):
    '''Contains LFRic kernel metadata. This class supports kernel
    metadata creation, modification, loading from a fortran string,
    writing to a fortran string, raising from existing language-level
    PSyIR and lowering to language-level PSyIR.

    :param operates_on: the name of the quantity that this kernel is \
        intended to iterate over.
    :type operates_on: Optional[str]
    :param shapes: if a kernel requires basis or differential-basis \
        functions then the metadata must also specify the set of points on \
        which these functions are required. This information is provided \
        by the gh_shape component of the metadata.
    :type shapes: Optional[List[str]]
    :param evaluator_targets: the function spaces on which an \
        evaluator is required.
    :type evaluator_targets: Optional[List[str]]
    :param meta_args: a list of 'meta_arg' objects which capture the \
        metadata values of the kernel arguments.
    :type meta_args: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
        CommonArgMetadata`]]
    :param meta_funcs: a list of 'meta_func' objects which capture whether \
        quadrature or evaluator data is required for a given function space.
    :type meta_funcs: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
        MetaFuncsArgMetadata`]]
    :param meta_ref_element: a kernel that requires properties \
        of the reference element in LFRic specifies those properties \
        through the meta_reference_element metadata entry.
    :type meta_ref_element: Optional[:py:class:`psyclone.domain.lfric.kernel.\
        RefElementArgMetadata`]
    :param meta_mesh: a kernel that requires properties of the LFRic \
        mesh object specifies those properties through the meta_mesh \
        metadata entry.
    :type meta_mesh: Optional[:py:class:`psyclone.domain.lfric.kernel.\
        MetaMeshArgMetadata`]
    :param procedure_name: the name of the kernel procedure to call.
    :type procedure_name: Optional[str]
    :param name: the name of the symbol to use for the metadata in \
        language-level PSyIR.
    :type name: Optional[str]

    '''
    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Derived_Type_Def

    def __init__(self, operates_on=None, shapes=None, evaluator_targets=None,
                 meta_args=None, meta_funcs=None, meta_ref_element=None,
                 meta_mesh=None, procedure_name=None, name=None):
        super().__init__()
        # Initialise internal variables
        self._operates_on = None
        self._shapes = None
        self._evaluator_targets = None
        self._meta_args = None
        self._meta_funcs = None
        self._meta_ref_element = None
        self._meta_mesh = None
        self._procedure_name = None
        self._name = None

        if operates_on is not None:
            self._operates_on = OperatesOnMetadata(operates_on)
        if shapes is not None:
            self._shapes = ShapesMetadata(shapes)
        if evaluator_targets is not None:
            self._evaluator_targets = EvaluatorTargetsMetadata(
                evaluator_targets)
        if meta_args is not None:
            self._meta_args = MetaArgsMetadata(meta_args)
        if meta_funcs is not None:
            self._meta_funcs = MetaFuncsMetadata(meta_funcs)
        if meta_ref_element is not None:
            self._meta_ref_element = MetaRefElementMetadata(
                meta_ref_element)
        if meta_mesh is not None:
            self._meta_mesh = MetaMeshMetadata(meta_mesh)
        if procedure_name is not None:
            # Validate procedure_name via setter
            self.procedure_name = procedure_name
        if name is not None:
            # Validate name via setter
            self.name = name

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
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for an LFRic Kernel.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Derived_Type_Ref`

        :returns: an instance of LFRicKernelMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`

        :raises ParseError: if one of the meta_args entries is an \
            unexpected type.
        :raises ParseError: if the metadata type does not extend kernel_type.

        '''
        LFRicKernelMetadata.check_fparser2(
            fparser2_tree, Fortran2003.Derived_Type_Def)

        kernel_metadata = LFRicKernelMetadata()

        for fparser2_node in walk(
                fparser2_tree, Fortran2003.Data_Component_Def_Stmt):
            fortran_string = str(fparser2_node).lower()
            # pylint: disable=protected-access
            if "operates_on" in fortran_string:
                # the value of operates on (CELL_COLUMN, ...)
                kernel_metadata._operates_on = OperatesOnMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_args" in fortran_string:
                kernel_metadata._meta_args = MetaArgsMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_funcs" in fortran_string:
                kernel_metadata._meta_funcs = MetaFuncsMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "gh_shape" in fortran_string:
                # the gh_shape values (gh_quadrature_XYoZ, ...)
                kernel_metadata._shapes = ShapesMetadata.create_from_fparser2(
                    fparser2_node)
            elif "gh_evaluator_targets" in fortran_string:
                # the gh_evaluator_targets values (w0, w1, ...)
                kernel_metadata._evaluator_targets = EvaluatorTargetsMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_reference_element" in fortran_string:
                kernel_metadata._meta_ref_element = MetaRefElementMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_mesh" in fortran_string:
                kernel_metadata._meta_mesh = MetaMeshMetadata.\
                    create_from_fparser2(fparser2_node)
            else:
                raise ParseError(
                    f"Found unexpected metadata declaration "
                    f"'{str(fparser2_node)}' in '{str(fparser2_tree)}'.")
            # pylint: enable=protected-access

        kernel_metadata.name = fparser2_tree.children[0].children[1].tostr()

        attribute_list = fparser2_tree.children[0].children[0]
        str_attribute_list = str(attribute_list).lower() \
            if attribute_list else ""
        if (attribute_list is None or
                "extends(kernel_type)" not in str_attribute_list):
            raise ParseError(
                f"The metadata type declaration should extend kernel_type, "
                f"but found '{fparser2_tree.children[0]}' in {fparser2_tree}.")
        kernel_metadata.procedure_name = \
            LFRicKernelMetadata._get_procedure_name(fparser2_tree)

        return kernel_metadata

    def lower_to_psyir(self):
        '''Lower the metadata to language-level PSyIR.

        :returns: metadata as stored in language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`

        '''
        return DataTypeSymbol(
            str(self.name), UnknownFortranType(self.fortran_string()))

    @staticmethod
    def _get_procedure_name(spec_part):
        '''Internal utility that extracts the procedure name from an
        fparser2 tree that captures LFRic metadata.

        TODO Issue #1946: potentially update the metadata to capture
        interface names as well as the interface itself. The procedure
        name will then no longer be optional.

        :param spec_part: the fparser2 parse tree containing the metadata.
        :type spec_part: :py:class:`fparser.two.Fortran2003.Derived_Type_Def`

        :returns: the value of the property.
        :rtype: Optional[str]

        :raises ParseError: if the metadata is invalid.

        '''
        # The value of 'code' should be found in a type bound
        # procedure (after the contains keyword)
        type_bound_procedure = get_child(
            spec_part, Fortran2003.Type_Bound_Procedure_Part)
        if not type_bound_procedure:
            return None
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
        return procedure_name.string

    def fortran_string(self):
        '''
        :returns: the metadata represented by this instance as Fortran.
        :rtype: str

        :raises ValueError: if the values for name, meta_arg, \
            operates_on and procedure_name have not been set.

        '''
        if not (self.operates_on and self._meta_args and self.name):
            raise ValueError(
                f"Values for operates_on, meta_args and name "
                f"must be provided before calling the fortran_string method, "
                f"but found '{self.operates_on}', '{self._meta_args}' "
                f"and '{self.name}' respectively.")

        operates_on = f"  {self._operates_on.fortran_string()}"
        meta_args = f"  {self._meta_args.fortran_string()}"

        shapes = ""
        if self._shapes:
            shapes = f"  {self._shapes.fortran_string()}"

        evaluator_targets = ""
        if self._evaluator_targets:
            evaluator_targets = f"  {self._evaluator_targets.fortran_string()}"

        meta_funcs = ""
        if self._meta_funcs:
            meta_funcs = f"  {self._meta_funcs.fortran_string()}"

        meta_ref_element = ""
        if self._meta_ref_element:
            meta_ref_element = f"  {self._meta_ref_element.fortran_string()}"

        meta_mesh = ""
        if self._meta_mesh:
            meta_mesh = f"  {self._meta_mesh.fortran_string()}"

        # TODO Issue #1946: potentially update the metadata to capture
        # interface names as well as the interface itself. The
        # procedure name will then no longer be optional.
        procedure = ""
        if self.procedure_name:
            procedure = (
                f"  CONTAINS\n"
                f"    PROCEDURE, NOPASS :: {self.procedure_name}\n")

        result = (
            f"TYPE, PUBLIC, EXTENDS(kernel_type) :: {self.name}\n"
            f"{meta_args}"
            f"{meta_funcs}"
            f"{meta_ref_element}"
            f"{meta_mesh}"
            f"{shapes}"
            f"{evaluator_targets}"
            f"{operates_on}"
            f"{procedure}"
            f"END TYPE {self.name}\n")
        return result

    @property
    def operates_on(self):
        '''
        :returns: the kernel operates_on property specified by the \
            metadata.
        :rtype: str
        '''
        if self._operates_on is None:
            return None
        return self._operates_on.operates_on

    @operates_on.setter
    def operates_on(self, value):
        '''
        :param str value: set the kernel operates_on property \
            in the metadata to the specified value.

        '''
        self._operates_on = OperatesOnMetadata(value)

    @property
    def shapes(self):
        '''
        :returns: a list of shape metadata values.
        :rtype: Optional[List[str]]

        '''
        if self._shapes is None:
            return None
        return self._shapes.shapes

    @shapes.setter
    def shapes(self, values):
        '''
        :param values: set the shape metadata to the \
            supplied list of values.
        :type values: List[str]

        '''
        self._shapes = ShapesMetadata(values)

    @property
    def evaluator_targets(self):
        '''
        :returns: a list of evaluator_targets metadata values.
        :rtype: Optional[List[str]]

        '''
        if self._evaluator_targets is None:
            return None
        return self._evaluator_targets.evaluator_targets

    @evaluator_targets.setter
    def evaluator_targets(self, values):
        '''
        :param values: set the evaluator_targets metadata to the \
            supplied list of values.
        :type values: List[str]

        '''
        self._evaluator_targets = EvaluatorTargetsMetadata(values)

    @property
    def meta_args(self):
        '''
        :returns: a list of 'meta_arg' objects which capture the \
            metadata values of the kernel arguments.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArg`]]

        '''
        if self._meta_args is None:
            return None
        return self._meta_args.meta_args_args

    @meta_args.setter
    def meta_args(self, values):
        '''
        :param values: set the meta_args metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArg`]

        '''
        self._meta_args = MetaArgsMetadata(values)

    @property
    def meta_funcs(self):
        '''
        :returns: a list of meta_funcs metadata values.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsArgMetadata`]]

        '''
        if self._meta_funcs is None:
            return None
        return self._meta_funcs.meta_funcs_args

    @meta_funcs.setter
    def meta_funcs(self, values):
        '''
        :param values: set the meta_funcs metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsArgMetadata`]

        '''
        self._meta_funcs = MetaFuncsMetadata(values)

    @property
    def meta_ref_element(self):
        '''
        :returns: a list of meta_reference_element metadata values.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`]]

        '''
        if self._meta_ref_element is None:
            return None
        return self._meta_ref_element.meta_ref_element_args

    @meta_ref_element.setter
    def meta_ref_element(self, values):
        '''
        :param values: set the meta_funcs metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`]

        '''
        self._meta_ref_element = MetaRefElementMetadata(values)

    @property
    def meta_mesh(self):
        '''
        :returns: a list of meta_mesh metadata values.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshArgMetadata`]]

        '''
        if self._meta_mesh is None:
            return None
        return self._meta_mesh.meta_mesh_args

    @meta_mesh.setter
    def meta_mesh(self, values):
        '''
        :param values: set the meta_mesh metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshArgMetadata`]

        '''
        self._meta_mesh = MetaMeshMetadata(values)

    @property
    def procedure_name(self):
        '''
        :returns: the kernel procedure name specified by the metadata.
        :rtype: str
        '''
        return self._procedure_name

    @procedure_name.setter
    def procedure_name(self, value):
        '''
        :param Optional[str] value: set the kernel procedure name in the \
            metadata to the specified value.

        :raises ValueError: if the metadata has an invalid value.

        '''
        if value is None:
            self._procedure_name = None
        else:
            config = Config.get()
            if not value or not config.valid_name.match(value):
                raise ValueError(
                    f"Expected procedure_name to be a valid Fortran name but "
                    f"found '{value}'.")
            self._procedure_name = value

    @property
    def name(self):
        '''
        :returns: the name of the symbol that will contain the \
            metadata when lowering.
        :rtype: str

        '''
        return self._name

    @name.setter
    def name(self, value):
        '''
        :param str value: set the name of the symbol that will contain \
            the metadata when lowering.

        :raises ValueError: if the name is not valid.

        '''
        config = Config.get()
        if not value or not config.valid_name.match(value):
            raise ValueError(
                f"Expected name to be a valid Fortran name but found "
                f"'{value}'.")
        self._name = value
