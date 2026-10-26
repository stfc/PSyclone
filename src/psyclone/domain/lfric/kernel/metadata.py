# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""
The LFRic kernel metadata is stored with the following immutable nested
dataclasses, built from language-level PSyIR:

.. code-block:: text

    LFRicKernelMetadata:
        operates_on: Optional[str]
        shapes: tuple[str, ...]
        evaluator_targets: tuple[str, ...]
        meta_args: tuple[Union[
            ScalarArgMetadata:
                datatype: str
                access: str
            ScalarArrayArgMetadata:
                datatype: str
                access: str
                array_ndims: int
            FieldArgMetadata:
                datatype: str
                access: str
                function_space: str
                stencil: Optional[str]
                nlayers: Optional[str]
                ndata: Optional[str]
            FieldVectorArgMetadata:
                datatype: str
                access: str
                function_space: str
                vector_length: str
                stencil: Optional[str]
                nlayers: Optional[str]
                ndata: Optional[str]
            InterGridArgMetadata:
                datatype: str
                access: str
                function_space: str
                mesh_arg: str
                stencil: Optional[str]
                nlayers: Optional[str]
                ndata: Optional[str]
            InterGridVectorArgMetadata:
                datatype: str
                access: str
                function_space: str
                mesh_arg: str
                vector_length: str
                stencil: Optional[str]
                nlayers: Optional[str]
                ndata: Optional[str]
            OperatorArgMetadata:
                datatype: str
                access: str
                function_space_to: str
                function_space_from: str
            ColumnwiseOperatorArgMetadata:
                datatype: str
                access: str
                function_space_to: str
                function_space_from: str
        ], ...]
        meta_funcs: tuple[MetaFuncsArgMetadata:
            function_space: str
            basis_function: bool
            diff_basis_function: bool
        , ...]
        meta_ref_element: tuple[MetaRefElementArgMetadata:
            reference_element: str
        , ...]
        meta_mesh: tuple[MetaMeshArgMetadata:
            mesh: str
        , ...]
        procedure_name: Optional[str]
        name: Optional[str]

"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import ClassVar, Optional, TYPE_CHECKING, Union

from psyclone.domain.common.kernel.metadata import (
    KernelMetadata, normalise as _normalise)
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ArrayConstructor,
    BinaryOperation,
    Call,
    Container,
    FileContainer,
    Literal,
    Node,
    Reference,
    Routine,
)
from psyclone.psyir.symbols import (
    DataTypeSymbol,
    GenericInterfaceSymbol,
    ScalarType,
)

# Metadata records naturally contain more state than behavioural classes.
# pylint: disable=too-many-instance-attributes,too-many-lines

if TYPE_CHECKING:
    from psyclone.core import AccessType


class LFRicArgMetadata:  # pylint: disable=too-few-public-methods
    """Common behaviour of all typed LFRic argument records."""

    @property
    def access_type(self) -> AccessType:
        """:returns: the generic access type for this argument."""
        return LFRicConstants().ACCESS_MAPPING[getattr(self, "access")]


def _normalise_label(value: object, description: str, default: str) -> str:
    """Normalise a layer/data-count label and ensure that it is either a
    positive integer or a valid Fortran name.

    :param value: label to validate, or ``None`` to use the default.
    :param description: description used in any error message.
    :param default: value to use when ``value`` is ``None``.

    :returns: the normalised label.

    :raises ValueError: if the value is neither a positive integer nor a
        valid Fortran name.
    """
    if value is None:
        return default
    result = _normalise(str(value), description)
    if result == default:
        return result
    if result.isnumeric():
        if int(result) > 0:
            return result
        raise ValueError(f"Expected {description} to be greater than zero.")
    try:
        FortranReader.validate_name(result)
    except (TypeError, ValueError) as err:
        raise ValueError(
            f"Expected {description} to be a positive integer or a valid "
            f"Fortran name but found '{result}'."
        ) from err
    return result


@dataclass(frozen=True)
class _FieldArgMetadataBase(LFRicArgMetadata):
    """Common fields and behaviour of all field metadata variants."""

    datatype: str
    access: str
    function_space: str
    # The optional fields are keyword-only so that subclasses can add required
    # positional fields without violating dataclass argument ordering.
    stencil: Optional[str] = field(default=None, kw_only=True)
    nlayers: Optional[str] = field(default="", kw_only=True)
    ndata: Optional[str] = field(default="1", kw_only=True)
    form: ClassVar[str] = "gh_field"

    def __post_init__(self) -> None:
        """Validate and normalise the common field metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "datatype",
            _normalise(
                self.datatype,
                "field datatype descriptor",
                const.VALID_FIELD_DATA_TYPES,
            ),
        )
        object.__setattr__(
            self,
            "access",
            _normalise(
                self.access,
                "field access descriptor",
                const.VALID_FIELD_ACCESS_TYPES,
            ),
        )
        object.__setattr__(
            self,
            "function_space",
            _normalise(
                self.function_space,
                "field function space",
                const.VALID_FUNCTION_SPACE_NAMES,
            ),
        )
        if self.stencil is not None:
            object.__setattr__(
                self,
                "stencil",
                _normalise(
                    self.stencil,
                    "stencil type",
                    const.VALID_STENCIL_TYPES,
                ),
            )
        object.__setattr__(
            self,
            "nlayers",
            _normalise_label(self.nlayers, "nlayers", ""),
        )
        object.__setattr__(
            self,
            "ndata",
            _normalise_label(self.ndata, "ndata", "1"),
        )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        form = self.form
        vector_length = getattr(self, "vector_length", None)
        if vector_length is not None:
            form += f"*{vector_length}"
        result = (
            f"arg_type({form}, {self.datatype}, {self.access}, "
            f"{self.function_space}"
        )
        if self.stencil:
            result += f", stencil({self.stencil})"
        if self.nlayers:
            result += f", nlayers='{self.nlayers}'"
        if self.ndata != "1":
            result += f", ndata='{self.ndata}'"
        mesh_arg = getattr(self, "mesh_arg", None)
        if mesh_arg is not None:
            result += f", mesh_arg={mesh_arg}"
        return result + ")"


@dataclass(frozen=True)
class ScalarArgMetadata(LFRicArgMetadata):
    """Metadata for a scalar kernel argument."""

    datatype: str
    access: str
    form: ClassVar[str] = "gh_scalar"
    check_name: ClassVar[str] = "scalar"

    def __post_init__(self) -> None:
        """Validate and normalise the scalar metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "datatype",
            _normalise(
                self.datatype,
                "scalar datatype descriptor",
                const.VALID_SCALAR_DATA_TYPES,
            ),
        )
        object.__setattr__(
            self,
            "access",
            _normalise(
                self.access,
                "scalar access descriptor",
                const.VALID_SCALAR_ACCESS_TYPES,
            ),
        )
        if self.access == "gh_reduction" and self.datatype != "gh_real":
            raise ValueError(
                "Reduction access is only valid for a real scalar argument."
            )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        return f"arg_type({self.form}, {self.datatype}, {self.access})"


@dataclass(frozen=True)
class ScalarArrayArgMetadata(LFRicArgMetadata):
    """Metadata for a scalar-array kernel argument."""

    datatype: str
    access: str
    array_ndims: int
    form: ClassVar[str] = "gh_scalar_array"
    check_name: ClassVar[str] = "array"

    def __post_init__(self) -> None:
        """Validate and normalise the scalar-array metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "datatype",
            _normalise(
                self.datatype,
                "scalar-array datatype descriptor",
                const.VALID_ARRAY_DATA_TYPES,
            ),
        )
        object.__setattr__(
            self,
            "access",
            _normalise(
                self.access,
                "scalar-array access descriptor",
                const.VALID_ARRAY_ACCESS_TYPES,
            ),
        )
        if not isinstance(self.array_ndims, int) or self.array_ndims < 1:
            raise ValueError(
                "The number of scalar-array dimensions must be an integer "
                "greater than or equal to one."
            )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        return (
            f"arg_type({self.form}, {self.datatype}, {self.access}, "
            f"{self.array_ndims})"
        )


@dataclass(frozen=True)
class FieldArgMetadata(_FieldArgMetadataBase):
    """Metadata for a field kernel argument."""

    check_name: ClassVar[str] = "field"


@dataclass(frozen=True)
class FieldVectorArgMetadata(_FieldArgMetadataBase):
    """Metadata for a field-vector kernel argument."""

    vector_length: str
    check_name: ClassVar[str] = "field-vector"

    def __post_init__(self) -> None:
        """Validate and normalise the field-vector metadata.

        :raises TypeError: if a field value has the wrong type.
        :raises ValueError: if a value or vector length is invalid.
        """
        super().__post_init__()
        try:
            length = int(self.vector_length)
        except (TypeError, ValueError) as err:
            raise ValueError(
                f"Vector length must be an integer string but found "
                f"'{self.vector_length}'."
            ) from err
        if length <= 1:
            raise ValueError("Vector length must be greater than one.")
        object.__setattr__(self, "vector_length", str(length))


@dataclass(frozen=True)
class InterGridArgMetadata(_FieldArgMetadataBase):
    """Metadata for an inter-grid field argument."""

    mesh_arg: str
    check_name: ClassVar[str] = "inter-grid"

    def __post_init__(self) -> None:
        """Validate and normalise the inter-grid field metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        super().__post_init__()
        object.__setattr__(
            self,
            "mesh_arg",
            _normalise(
                self.mesh_arg,
                "mesh_arg",
                LFRicConstants().VALID_MESH_TYPES,
            ),
        )


@dataclass(frozen=True)
class InterGridVectorArgMetadata(_FieldArgMetadataBase):
    """Metadata for an inter-grid field-vector argument."""

    mesh_arg: str
    vector_length: str
    check_name: ClassVar[str] = "inter-grid-vector"

    def __post_init__(self) -> None:
        """Validate and normalise the inter-grid vector metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        super().__post_init__()
        try:
            length = int(self.vector_length)
        except (TypeError, ValueError) as err:
            raise ValueError(
                f"Vector length must be an integer string but found "
                f"'{self.vector_length}'."
            ) from err
        if length <= 1:
            raise ValueError("Vector length must be greater than one.")
        object.__setattr__(self, "vector_length", str(length))
        object.__setattr__(
            self,
            "mesh_arg",
            _normalise(
                self.mesh_arg,
                "mesh_arg",
                LFRicConstants().VALID_MESH_TYPES,
            ),
        )


@dataclass(frozen=True)
class OperatorArgMetadata(LFRicArgMetadata):
    """Metadata for an LMA operator argument."""

    datatype: str
    access: str
    function_space_to: str
    function_space_from: str
    form: ClassVar[str] = "gh_operator"
    check_name: ClassVar[str] = "operator"

    def __post_init__(self) -> None:
        """Validate and normalise the operator metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "datatype",
            _normalise(
                self.datatype,
                "operator datatype descriptor",
                const.VALID_OPERATOR_DATA_TYPES,
            ),
        )
        object.__setattr__(
            self,
            "access",
            _normalise(
                self.access,
                "operator access descriptor",
                const.VALID_OPERATOR_ACCESS_TYPES,
            ),
        )
        for name in ("function_space_to", "function_space_from"):
            object.__setattr__(
                self,
                name,
                _normalise(
                    getattr(self, name),
                    name,
                    const.VALID_FUNCTION_SPACE_NAMES,
                ),
            )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        return (
            f"arg_type({self.form}, {self.datatype}, {self.access}, "
            f"{self.function_space_to}, {self.function_space_from})"
        )


@dataclass(frozen=True)
class ColumnwiseOperatorArgMetadata(OperatorArgMetadata):
    """Metadata for a columnwise operator argument."""

    form: ClassVar[str] = "gh_columnwise_operator"
    check_name: ClassVar[str] = "columnwise-operator"


@dataclass(frozen=True)
class MetaFuncsArgMetadata:
    """Metadata describing basis functions on one function space."""

    function_space: str
    basis_function: bool = False
    diff_basis_function: bool = False

    def __post_init__(self) -> None:
        """Validate and normalise the meta-functions metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value or flag combination is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "function_space",
            _normalise(
                self.function_space,
                "meta_funcs function space",
                const.VALID_FUNCTION_SPACE_NAMES,
            ),
        )
        if not isinstance(self.basis_function, bool) or not isinstance(
            self.diff_basis_function, bool
        ):
            raise TypeError("Basis-function flags must be booleans.")
        if not self.basis_function and not self.diff_basis_function:
            raise ValueError(
                "At least one basis-function flag must be true."
            )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        values = [self.function_space]
        if self.basis_function:
            values.append("gh_basis")
        if self.diff_basis_function:
            values.append("gh_diff_basis")
        return f"func_type({', '.join(values)})"


@dataclass(frozen=True)
class MetaRefElementArgMetadata:
    """One requested reference-element property."""

    reference_element: str

    def __post_init__(self) -> None:
        """Validate and normalise the reference-element metadata.

        :raises TypeError: if the property is not a string.
        :raises ValueError: if the property is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "reference_element",
            _normalise(
                self.reference_element,
                "reference-element property",
                const.VALID_REF_ELEMENT_NAMES,
            ),
        )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        return (
            f"reference_element_data_type({self.reference_element})"
        )


@dataclass(frozen=True)
class MetaMeshArgMetadata:
    """One requested mesh property."""

    mesh: str

    def __post_init__(self) -> None:
        """Validate and normalise the mesh-property metadata.

        :raises TypeError: if the property is not a string.
        :raises ValueError: if the property is invalid.
        """
        const = LFRicConstants()
        object.__setattr__(
            self,
            "mesh",
            _normalise(
                self.mesh, "mesh property", const.VALID_MESH_NAMES
            ),
        )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        return f"mesh_data_type({self.mesh})"


KernelArgumentMetadata = Union[
    ScalarArgMetadata, ScalarArrayArgMetadata, FieldArgMetadata,
    FieldVectorArgMetadata, InterGridArgMetadata, InterGridVectorArgMetadata,
    OperatorArgMetadata, ColumnwiseOperatorArgMetadata,
]


_ARG_TYPES = (
    ScalarArgMetadata,
    ScalarArrayArgMetadata,
    FieldArgMetadata,
    FieldVectorArgMetadata,
    InterGridArgMetadata,
    InterGridVectorArgMetadata,
    OperatorArgMetadata,
    ColumnwiseOperatorArgMetadata,
)


@dataclass(frozen=True)
class LFRicKernelMetadata(KernelMetadata):
    """Typed, immutable language-level LFRic metadata."""

    operates_on: Optional[str] = None
    shapes: tuple[str, ...] = ()
    evaluator_targets: tuple[str, ...] = ()
    meta_args: tuple[KernelArgumentMetadata, ...] = ()
    meta_funcs: tuple[MetaFuncsArgMetadata, ...] = ()
    meta_ref_element: tuple[MetaRefElementArgMetadata, ...] = ()
    meta_mesh: tuple[MetaMeshArgMetadata, ...] = ()
    procedure_name: Optional[str] = None
    name: Optional[str] = None

    @property
    def nargs(self) -> int:
        """:returns: the number of algorithm-layer metadata arguments."""
        return len(self.meta_args)

    @property
    def iterates_over(self) -> Optional[str]:
        """:returns: the iteration space consumed by the PSy layer."""
        return self.operates_on

    @property
    def eval_shapes(self) -> tuple[str, ...]:
        """:returns: evaluator shapes requested by this kernel."""
        return self.shapes

    @property
    def eval_targets(self) -> tuple[str, ...]:
        """:returns: explicit or inferred evaluator targets."""
        targets = list(self.evaluator_targets)
        if not targets and "gh_evaluator" in self.shapes:
            for argument in self.meta_args:
                spaces = self._argument_spaces(argument)
                if (argument.access != "gh_read" and spaces and
                        spaces[0] not in targets):
                    targets.append(spaces[0])
        return tuple(targets)

    @property
    def cma_operation(self) -> Optional[str]:
        """:returns: the CMA operation, if this is a CMA kernel."""
        category = self.kernel_type
        return (category.removeprefix("cma-")
                if category.startswith("cma-") else None)

    @property
    def is_intergrid(self) -> bool:
        """:returns: whether this is an inter-grid kernel."""
        return self.kernel_type == "inter-grid"

    def __post_init__(self) -> None:
        """Validate and normalise the complete kernel metadata.

        :raises TypeError: if a collection or its entries have wrong types.
        :raises ValueError: if a value or Fortran name is invalid.
        """
        const = LFRicConstants()
        for name in (
            "shapes",
            "evaluator_targets",
            "meta_args",
            "meta_funcs",
            "meta_ref_element",
            "meta_mesh",
        ):
            value = getattr(self, name)
            if not isinstance(value, (list, tuple)):
                raise TypeError(f"Expected {name} to be a list or tuple.")
            object.__setattr__(self, name, tuple(value))
        if self.operates_on is not None:
            object.__setattr__(
                self,
                "operates_on",
                _normalise(
                    self.operates_on,
                    "operates_on",
                    const.VALID_ITERATION_SPACES,
                ),
            )
        object.__setattr__(
            self,
            "shapes",
            tuple(
                _normalise(
                    value,
                    "evaluator shape",
                    const.VALID_EVALUATOR_SHAPES,
                )
                for value in self.shapes
            ),
        )
        object.__setattr__(
            self,
            "evaluator_targets",
            tuple(
                _normalise(
                    value,
                    "evaluator target",
                    const.VALID_FUNCTION_SPACE_NAMES,
                )
                for value in self.evaluator_targets
            ),
        )
        if any(not isinstance(arg, _ARG_TYPES) for arg in self.meta_args):
            raise TypeError("All meta_args entries must be argument metadata.")
        if any(
            not isinstance(arg, MetaFuncsArgMetadata)
            for arg in self.meta_funcs
        ):
            raise TypeError(
                "All meta_funcs entries must be MetaFuncsArgMetadata."
            )
        if self.procedure_name is not None:
            FortranReader.validate_name(self.procedure_name)
            object.__setattr__(
                self, "procedure_name", self.procedure_name.lower())
        if self.name is not None:
            FortranReader.validate_name(self.name)
            object.__setattr__(self, "name", self.name.lower())

    @classmethod
    def create_from_psyir(
        cls, psyir: Union[DataTypeSymbol, Node], name: Optional[str] = None
    ) -> "LFRicKernelMetadata":
        """Create typed metadata from a PSyIR tree or type symbol.

        :param psyir: PSyIR containing the metadata declaration.
        :param name: optional metadata type name when ``psyir`` is a tree.

        :returns: the parsed language-level kernel metadata.

        :raises TypeError: if the PSyIR or its datatype is invalid.
        :raises ParseError: if the metadata declaration is invalid.
        """
        # pylint: disable=too-many-branches,too-many-locals
        symbol = cls._validate_and_get_symbol(psyir, "LFRic", name=name)
        datatype = cls._validate_and_get_structure(symbol, "LFRic")

        components = datatype.components
        valid_components = {
            "meta_args",
            "meta_funcs",
            "meta_reference_element",
            "meta_mesh",
            "operates_on",
            "gh_shape",
            "gh_evaluator_targets",
        }
        unexpected = set(components) - valid_components
        if unexpected:
            raise ParseError(
                "Unexpected LFRic metadata component(s): "
                f"{sorted(unexpected)}.")
        if "meta_args" not in components:
            raise ParseError(
                f"No meta_args found in kernel metadata '{symbol.name}'."
            )

        def array_values(component_name: str) -> tuple[Node, ...]:
            """Return and validate values of an array component.

            :param component_name: the metadata array component to read.
            :returns: the component's validated initial values.
            :raises ParseError: if the array declaration or extent is invalid.
            """
            return cls._validate_and_get_array_values(
                components[component_name], component_name)

        meta_args = tuple(
            _parse_arg(node) for node in array_values("meta_args")
        )

        meta_funcs = ()
        if "meta_funcs" in components:
            meta_funcs = tuple(
                _parse_func(node) for node in array_values("meta_funcs")
            )

        def names(component_name: str) -> tuple[str, ...]:
            """Return scalar names from an optional component.

            :param component_name: the optional metadata component to read.
            :returns: the component's lower-case values, or an empty tuple if
                absent.
            :raises ParseError: if the values or array declaration are invalid.
            """
            if component_name not in components:
                return ()
            initial_value = components[component_name].initial_value
            values = (
                array_values(component_name)
                if isinstance(initial_value, ArrayConstructor)
                else (initial_value,)
            )
            return tuple(cls._validate_and_get_value(value)
                         for value in values)

        ref_element = ()
        if "meta_reference_element" in components:
            ref_element_values = []
            for node in array_values("meta_reference_element"):
                if not isinstance(node, Call):
                    raise ParseError(
                        "Expected a metadata constructor but found "
                        f"'{type(node).__name__}'."
                    )
                if (node.routine.symbol.name.lower() !=
                        "reference_element_data_type"):
                    raise ParseError(
                        "meta_reference_element entries must use the "
                        "reference_element_data_type constructor.")
                ref_element_values.append(MetaRefElementArgMetadata(
                    cls._validate_and_get_value(node.arguments[0])))
            ref_element = tuple(ref_element_values)
            if len({item.reference_element for item in ref_element}) != len(
                    ref_element):
                raise ParseError(
                    "meta_reference_element must not contain duplicates.")

        mesh = ()
        if "meta_mesh" in components:
            mesh_values = []
            for node in array_values("meta_mesh"):
                if not isinstance(node, Call):
                    raise ParseError(
                        "Expected a metadata constructor but found "
                        f"'{type(node).__name__}'."
                    )
                if node.routine.symbol.name.lower() != "mesh_data_type":
                    raise ParseError(
                        "meta_mesh entries must use the mesh_data_type "
                        "constructor.")
                mesh_values.append(MetaMeshArgMetadata(
                    cls._validate_and_get_value(node.arguments[0])))
            mesh = tuple(mesh_values)
            if len({item.mesh for item in mesh}) != len(mesh):
                raise ParseError("meta_mesh must not contain duplicates.")

        procedure_name = None
        if datatype.procedure_components:
            procedure = next(iter(datatype.procedure_components.values()))
            procedure_name = (
                cls._validate_and_get_value(procedure.initial_value)
                if procedure.initial_value else procedure.name
            )

        return cls(
            operates_on=(names("operates_on")[0]
                         if "operates_on" in components else None),
            shapes=names("gh_shape"),
            evaluator_targets=names("gh_evaluator_targets"),
            meta_args=meta_args,
            meta_funcs=meta_funcs,
            meta_ref_element=ref_element,
            meta_mesh=mesh,
            procedure_name=procedure_name,
            name=symbol.name,
        )

    @property
    def kernel_type(self) -> str:
        """Validate and return the inferred LFRic kernel category.

        :returns: the kernel category.

        :raises ParseError: if constraints for that category are violated.
        """
        if any(
            isinstance(arg, (InterGridArgMetadata,
                             InterGridVectorArgMetadata))
            for arg in self.meta_args
        ):
            self._validate_intergrid()
            return "inter-grid"
        if any(
            isinstance(arg, ColumnwiseOperatorArgMetadata)
            for arg in self.meta_args
        ):
            return self._validate_cma()
        self._validate_generic()
        if self.operates_on == "domain":
            self._validate_domain()
            return "domain"
        return "general-purpose"

    def _validate_generic(self) -> None:
        """Validate constraints shared by ordinary kernels.

        :raises ParseError: if a generic constraint is violated.
        """
        field_types = (
            FieldArgMetadata,
            FieldVectorArgMetadata,
            InterGridArgMetadata,
            InterGridVectorArgMetadata,
        )
        operator_types = (
            OperatorArgMetadata,
            ColumnwiseOperatorArgMetadata,
        )
        if self.operates_on != "domain" and not any(
            isinstance(arg, field_types + operator_types)
            for arg in self.meta_args
        ):
            raise ParseError(
                "Kernel metadata not operating on the domain must contain "
                "at least one field or operator argument."
            )
        if any(isinstance(arg, operator_types) for arg in self.meta_args):
            for arg in self.meta_args:
                if isinstance(arg, field_types) and arg.datatype != "gh_real":
                    raise ParseError(
                        "A kernel with an operator argument must only contain "
                        "real-valued field arguments."
                    )

    def _validate_domain(self) -> None:
        """Validate domain-kernel constraints.

        :raises ParseError: if a domain-kernel constraint is violated.
        """
        valid_types = (
            ScalarArgMetadata,
            FieldArgMetadata,
            FieldVectorArgMetadata,
        )
        if any(not isinstance(arg, valid_types) for arg in self.meta_args):
            raise ParseError(
                "Domain kernels may only contain scalar or field arguments."
            )
        if self.meta_funcs or self.meta_mesh:
            raise ParseError(
                "Domain kernels may not request basis functions or mesh "
                "properties."
            )
        const = LFRicConstants()
        for argument in self.meta_args:
            if not isinstance(argument, (FieldArgMetadata,
                                         FieldVectorArgMetadata)):
                continue
            if argument.function_space not in const.VALID_DISCONTINUOUS_NAMES:
                raise ParseError(
                    "Domain kernels only accept fields on discontinuous "
                    f"function spaces but found '{argument.function_space}'."
                )

    def _validate_intergrid(self) -> None:
        """Validate inter-grid constraints.

        :raises ParseError: if an inter-grid constraint is violated.

        :raises InternalError: if the API defines other than two mesh types.
        """
        if self.operates_on != "cell_column":
            raise ParseError(
                "An inter-grid kernel must operate on cell_column."
            )
        valid_types = (
            InterGridArgMetadata,
            InterGridVectorArgMetadata,
        )
        if any(not isinstance(arg, valid_types) for arg in self.meta_args):
            raise ParseError(
                "Inter-grid kernels may only contain inter-grid fields."
            )
        const = LFRicConstants()
        if len(const.VALID_MESH_TYPES) != 2:
            raise InternalError(
                "Inter-grid support requires exactly two mesh types but "
                f"found {len(const.VALID_MESH_TYPES)}: "
                f"{const.VALID_MESH_TYPES}.")
        meshes = {arg.mesh_arg for arg in self.meta_args}
        if meshes != set(const.VALID_MESH_TYPES):
            raise ParseError(
                "Inter-grid kernels must have arguments on both mesh types."
            )
        spaces = {
            mesh: {
                arg.function_space
                for arg in self.meta_args
                if arg.mesh_arg == mesh
            }
            for mesh in meshes
        }
        first, second = tuple(meshes)
        if spaces[first] & spaces[second]:
            raise ParseError(
                "Inter-grid fields on different meshes must use different "
                "function spaces."
            )

    def _validate_cma(self) -> str:
        """Validate and identify a CMA operation.

        :returns: the type of CMA operation.

        :raises ParseError: if a CMA constraint is violated.
        """
        # Exact concrete types distinguish LMA operators and scalar fields
        # from their specialised records.
        # pylint: disable=unidiomatic-typecheck
        cma_args = [
            arg
            for arg in self.meta_args
            if isinstance(arg, ColumnwiseOperatorArgMetadata)
        ]
        lma_args = [
            arg
            for arg in self.meta_args
            if type(arg) is OperatorArgMetadata
        ]
        field_args = [
            arg
            for arg in self.meta_args
            if type(arg) is FieldArgMetadata
        ]
        if self.operates_on != "cell_column":
            raise ParseError("CMA kernels must operate on cell_column.")
        for argument in self.meta_args:
            if (int(getattr(argument, "vector_length", 1)) > 1 or
                    getattr(argument, "stencil", None)):
                raise ParseError(
                    "CMA kernels may not use vector or stencil arguments."
                )
            if (getattr(argument, "ndata", "1") != "1" or
                    getattr(argument, "nlayers", "")):
                raise ParseError(
                    "CMA kernels require default NDATA and NLAYERS."
                )
        if any(argument.datatype != "gh_real"
               for argument in field_args):
            raise ParseError(
                "CMA kernels may only contain real-valued fields."
            )
        writers = [arg for arg in cma_args if arg.access != "gh_read"]
        if lma_args:
            if len(cma_args) != 1 or len(writers) != 1:
                raise ParseError(
                    "A CMA assembly kernel must write one CMA operator."
                )
            if any(argument.access != "gh_read" for argument in lma_args):
                raise ParseError(
                    "A CMA assembly kernel may only read LMA operators."
                )
            return "cma-assembly"
        if field_args:
            if (
                len(cma_args) != 1
                or writers
                or len(field_args) != 2
            ):
                raise ParseError(
                    "A CMA apply kernel requires one read-only CMA operator "
                    "and two fields."
                )
            read_fields = [
                arg for arg in field_args if arg.access == "gh_read"
            ]
            write_fields = [
                arg for arg in field_args if arg.access != "gh_read"
            ]
            if len(read_fields) != 1 or len(write_fields) != 1:
                raise ParseError(
                    "A CMA apply kernel requires one read and one written "
                    "field."
                )
            cma = cma_args[0]
            if (
                read_fields[0].function_space != cma.function_space_from
                or write_fields[0].function_space != cma.function_space_to
            ):
                raise ParseError(
                    "CMA apply field spaces must match the operator spaces."
                )
            return "cma-apply"
        readers = [arg for arg in cma_args if arg.access == "gh_read"]
        if len(writers) != 1 or not readers or any(
            not isinstance(
                arg, (ColumnwiseOperatorArgMetadata, ScalarArgMetadata)
            )
            for arg in self.meta_args
        ):
            raise ParseError(
                "A CMA matrix-matrix kernel must write exactly one CMA "
                "operator and contain only CMA operators and scalars."
            )
        return "cma-matrix-matrix"

    def meta_args_get(
        self, types: Union[type, list[type]]
    ) -> list[KernelArgumentMetadata]:
        """Return meta_args entries whose concrete types match.

        :param types: the concrete type or types to match.

        :returns: the matching metadata arguments.
        """
        requested = tuple(types) if isinstance(types, list) else (types,)
        return [
            arg for arg in self.meta_args if type(arg) in requested
        ]

    def field_meta_args_on_fs(
        self, types: Union[type, list[type]], function_space: str
    ) -> list[KernelArgumentMetadata]:
        """Return field arguments of the requested types on a space.

        :param types: the concrete field type or types to match.
        :param function_space: the function space to match.

        :returns: the matching field metadata arguments.
        """
        requested = tuple(types) if isinstance(types, list) else (types,)
        return [
            argument
            for argument in self.meta_args
            if type(argument) in requested
            and argument.function_space == function_space
        ]

    def operator_meta_args_on_fs(
        self, types: Union[type, list[type]], function_space: str
    ) -> list[KernelArgumentMetadata]:
        """Return operators connected to the supplied function space.

        :param types: the concrete operator type or types to match.
        :param function_space: the function space to match.

        :returns: the matching operator metadata arguments.
        """
        requested = tuple(types) if isinstance(types, list) else (types,)
        return [
            argument
            for argument in self.meta_args
            if type(argument) in requested
            and function_space in (
                argument.function_space_to,
                argument.function_space_from,
            )
        ]

    def validate(self) -> None:
        """Validate all kernel-category constraints.

        :raises ParseError: if a kernel-category constraint is violated.
        """
        _ = self.kernel_type
        self._validate_field_accesses()
        self._validate_writes()
        needs_evaluator = self._validate_evaluators()
        self._validate_consumer_cma()
        self._validate_domain_dof(needs_evaluator)

    def _validate_field_accesses(self) -> None:
        """Validate field access modes for the iteration space.

        :raises ParseError: if a field access is invalid for its iteration
            space.
        """
        const = LFRicConstants()
        for argument in self.meta_args:
            if not isinstance(argument, (FieldArgMetadata,
                                         FieldVectorArgMetadata)):
                continue
            if (self.operates_on == "dof" or
                    argument.function_space in
                    const.VALID_DISCONTINUOUS_NAMES):
                valid_accesses = ("gh_read", "gh_write", "gh_readwrite")
            else:
                valid_accesses = ("gh_read", "gh_write", "gh_inc",
                                  "gh_readinc")
            if argument.access not in valid_accesses:
                raise ParseError(
                    f"Field '{argument.function_space}' in a kernel "
                    f"operating on '{self.operates_on}' must have one of "
                    f"{list(valid_accesses)} accesses but found "
                    f"'{argument.access}'.")

    @staticmethod
    def _argument_spaces(argument: KernelArgumentMetadata) -> tuple[str, ...]:
        """Return the function spaces used by one typed argument.

        :param argument: the argument metadata whose function spaces are
            required.
        :returns: the field or operator spaces, or an empty tuple for a scalar.
        """
        if isinstance(argument, (OperatorArgMetadata,
                                 ColumnwiseOperatorArgMetadata)):
            return (argument.function_space_to,
                    argument.function_space_from)
        function_space = getattr(argument, "function_space", None)
        return (function_space,) if function_space else ()

    def _validate_writes(self) -> None:
        """Validate that the kernel has a permitted written argument.

        :raises ParseError: if a read-only space or a user-kernel scalar is
            written,
            or no argument is updated.
        """
        # Avoid a circular import with the built-in metadata definitions.
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP

        const = LFRicConstants()
        writes = 0
        for argument in self.meta_args:
            if argument.access == "gh_read":
                continue
            writes += 1
            if (isinstance(argument, (FieldArgMetadata,
                                      FieldVectorArgMetadata)) and
                    argument.function_space in
                    const.READ_ONLY_FUNCTION_SPACES):
                raise ParseError(
                    f"Found kernel metadata in '{self.name}' that specifies "
                    "writing to the read-only function space "
                    f"'{argument.function_space}'."
                )
            if (self.name not in BUILTIN_MAP and
                    isinstance(argument,
                               (ScalarArgMetadata,
                                ScalarArrayArgMetadata))):
                raise ParseError(
                    "A user-supplied LFRic kernel must not write/update a "
                    f"scalar argument but kernel '{self.name}' does."
                )
        if not writes:
            raise ParseError(
                "An LFRic kernel must have at least one argument that is "
                f"updated (written to) but found none for kernel "
                f"'{self.name}'."
            )

    def _validate_evaluators(self) -> bool:
        """Validate evaluator metadata and return whether it is needed.

        :returns: whether any argument requires basis or differential basis
            data.
        :raises ParseError: if function spaces, shapes or evaluator targets are
            missing, repeated or inconsistent with the arguments.
        """
        spaces = {
            space for argument in self.meta_args
            for space in self._argument_spaces(argument)
        }
        used = set()
        need = False
        for metadata in self.meta_funcs:
            if metadata.function_space not in spaces:
                raise ParseError(
                    f"Function space '{metadata.function_space}' in "
                    "meta_funcs does not exist in meta_args."
                )
            if metadata.function_space in used:
                raise ParseError(
                    f"Function space '{metadata.function_space}' is "
                    "repeated in meta_funcs."
                )
            used.add(metadata.function_space)
            need |= metadata.basis_function or metadata.diff_basis_function
        if need and not self.shapes:
            raise ParseError(
                "A kernel requiring quadrature or an evaluator must also "
                f"supply gh_shape, but this is missing for kernel "
                f"'{self.name}'."
            )
        if not need and self.shapes:
            raise ParseError(
                f"Kernel '{self.name}' specifies gh_shape but does not need "
                "an evaluator."
            )
        if self.evaluator_targets:
            if not need or "gh_evaluator" not in self.shapes:
                raise ParseError(
                    "gh_evaluator_targets requires gh_shape=gh_evaluator."
                )
            missing = set(self.evaluator_targets) - spaces
            if missing:
                raise ParseError(
                    "Evaluator targets are not present in meta_args: "
                    f"{sorted(missing)}."
                )
        return need

    def _validate_consumer_cma(self) -> None:
        """Validate restrictions needed by CMA code generation."""
        # Accessing this property performs all of the language-level CMA
        # validation. This method remains as the consumer-facing hook used by
        # validate().
        _ = self.cma_operation

    def _validate_domain_dof(self, need: bool) -> None:
        """Validate domain and degree-of-freedom kernel restrictions.

        :param need: whether the kernel requires evaluator data.
        :raises ParseError: if the iteration space has unsupported argument
            types,
            evaluator or mesh data, or inconsistent function spaces.
        """
        if self.operates_on not in ("domain", "dof"):
            return
        # Avoid a circular import with the built-in metadata definitions.
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
        if self.operates_on == "dof" and self.name in BUILTIN_MAP:
            return
        valid = (ScalarArgMetadata, ScalarArrayArgMetadata,
                 FieldArgMetadata, FieldVectorArgMetadata)
        if any(not isinstance(argument, valid)
               for argument in self.meta_args):
            raise ParseError(
                f"Kernels operating on '{self.operates_on}' may only "
                "contain scalar and field arguments."
            )
        if need or self.meta_ref_element or self.meta_mesh:
            raise ParseError(
                f"Kernels operating on '{self.operates_on}' may not "
                "request evaluator, reference-element or mesh data."
            )
        if self.operates_on == "dof":
            spaces = {
                space for argument in self.meta_args
                for space in self._argument_spaces(argument)
            }
            if len(spaces) > 1:
                raise ParseError("A dof kernel must use one function space.")

    def fortran_string(self) -> str:
        """Return this metadata as a Fortran derived-type declaration.

        :returns: this metadata as Fortran source.

        :raises ValueError: if required metadata is missing.
        """
        if not self.operates_on or not self.meta_args or not self.name:
            raise ValueError(
                "operates_on, meta_args and name are required."
            )

        def array_declaration(
            type_name: str, name: str, entries: tuple[object, ...]
        ) -> str:
            """Create one Fortran metadata-array declaration.

            :param type_name: the Fortran type of each entry.
            :param name: the metadata component name.
            :param entries: the metadata entries to serialise.

            :returns: the Fortran array declaration.
            """
            values = ", ".join(entry.fortran_string() for entry in entries)
            return (
                f"  TYPE({type_name}), DIMENSION({len(entries)}) :: "
                f"{name} = (/{values}/)\n"
            )

        result = (
            f"TYPE, PUBLIC, EXTENDS(kernel_type) :: {self.name}\n"
            f"{array_declaration('arg_type', 'meta_args', self.meta_args)}"
        )
        if self.meta_funcs:
            result += array_declaration(
                "func_type", "meta_funcs", self.meta_funcs
            )
        if self.meta_ref_element:
            result += array_declaration(
                "reference_element_data_type",
                "meta_reference_element",
                self.meta_ref_element,
            )
        if self.meta_mesh:
            result += array_declaration(
                "mesh_data_type", "meta_mesh", self.meta_mesh
            )
        if self.shapes:
            if len(self.shapes) == 1:
                result += f"  INTEGER :: gh_shape = {self.shapes[0]}\n"
            else:
                values = ", ".join(self.shapes)
                result += (
                    f"  INTEGER, DIMENSION({len(self.shapes)}) :: "
                    f"gh_shape = (/{values}/)\n"
                )
        if self.evaluator_targets:
            values = ", ".join(self.evaluator_targets)
            result += (
                f"  INTEGER, DIMENSION({len(self.evaluator_targets)}) :: "
                f"gh_evaluator_targets = (/{values}/)\n"
            )
        result += f"  INTEGER :: operates_on = {self.operates_on}\n"
        if self.procedure_name:
            result += (
                "  CONTAINS\n"
                f"  PROCEDURE, NOPASS :: code => {self.procedure_name}\n"
            )
        return result + f"END TYPE {self.name}\n"

    @classmethod
    def find_symbol(
            cls,
            psyir: Node,
            name: Optional[str] = None,
    ) -> tuple[Container, DataTypeSymbol]:
        """Find the module and metadata symbol using this API's naming rules.

        :param psyir: the complete kernel PSyIR.
        :param name: optional metadata type name.

        :returns: the containing module and metadata symbol.
        :raises ParseError: if discovery fails.
        """
        containers = [
            node for node in psyir.walk(Container)
            if not isinstance(node, FileContainer)
        ]
        if not containers:
            raise ParseError(
                "The file does not contain a module. Is it a Kernel file?"
            )
        if name is None:
            if len(containers) != 1:
                raise ParseError(
                    "A metadata name is required for multiple modules."
                )
            module_name = containers[0].name
            if len(module_name) < 5:
                raise ParseError(
                    f"Module name '{module_name}' is too short to have "
                    "'_mod' as an extension."
                )
            if not module_name.lower().endswith("_mod"):
                raise ParseError(
                    f"Module name '{module_name}' does not have '_mod' as "
                    "an extension."
                )
            name = module_name[:-4] + "_type"
        matches = cls.find_symbols(psyir, name=name)
        if not matches:
            raise ParseError(f"Kernel type {name} does not exist.")
        if len(matches) > 1:
            raise ParseError(f"Kernel type {name} is not unique.")
        return matches[0]

    def resolve_procedures(
            self,
            container: Container,
    ) -> tuple[str, tuple[Routine, ...]]:
        """Resolve this kernel's procedure implementations from its module.

        :param container: the module containing the kernel.

        :returns: the resolved procedure name and tuple of implementations.
        :raises ParseError: if the implementation cannot be resolved.
        """
        routines = tuple(container.walk(Routine))
        if self.procedure_name:
            implementations = tuple(
                routine
                for routine in routines
                if routine.name.lower() == self.procedure_name.lower()
            )
            if not implementations:
                raise ParseError(
                    f"Kernel subroutine '{self.procedure_name}' not found."
                )
            return self.procedure_name.lower(), implementations
        interfaces = [
            symbol
            for symbol in container.symbol_table.symbols
            if isinstance(symbol, GenericInterfaceSymbol)
        ]
        if len(interfaces) != 1:
            raise ParseError(
                f"Kernel '{self.name}' requires exactly one generic "
                "interface when it has no type-bound procedure."
            )
        interface = interfaces[0]
        names = {
            info.symbol.name.lower()
            for info in interface.routines
            if info.from_container
        }
        implementations = tuple(
            routine for routine in routines if routine.name.lower() in names
        )
        if len(implementations) != len(names):
            raise ParseError(
                f"Not all procedures for interface '{interface.name}' exist."
            )
        return interface.name.lower(), implementations


def _parse_arg(
    node: Node,
) -> KernelArgumentMetadata:
    """Convert one PSyIR arg_type constructor into typed metadata.

    :param node: the ``arg_type`` constructor to convert.

    :returns: the typed metadata for the argument.

    :raises ParseError: if the constructor structure is invalid.
    :raises NotImplementedError: if it specifies a fixed stencil extent.
    :raises ValueError: if a scalar-array dimension is not an integer.
    """
    # This is deliberately a single dispatch point for the compact
    # arg_type constructor grammar.
    # pylint: disable=too-many-locals,too-many-return-statements
    # pylint: disable=too-many-branches,too-many-statements
    if not isinstance(node, Call):
        raise ParseError(
            "Expected a metadata constructor but found "
            f"'{type(node).__name__}'."
        )
    if node.routine.symbol.name.lower() != "arg_type":
        raise ParseError(
            "Each meta_args entry must use the arg_type constructor."
        )
    arguments = tuple(node.arguments)
    value_name = KernelMetadata._validate_and_get_value
    names = tuple(node.argument_names)
    if len(arguments) < 3:
        raise ParseError(
            "Each arg_type constructor must have at least three arguments."
        )
    if len(arguments) > 7:
        raise ParseError(
            "each 'meta_arg' entry must have at most 7 arguments."
        )
    form_node = arguments[0]
    vector_length = None
    if isinstance(form_node, BinaryOperation):
        if form_node.operator != BinaryOperation.Operator.MUL:
            raise ParseError("Field vectors must use multiplication syntax.")
        form = value_name(form_node.children[0])
        vector_length = value_name(form_node.children[1])
    else:
        form = value_name(form_node)
    if vector_length is not None and form != "gh_field":
        raise ParseError(
            "Vector notation is only supported for gh_field metadata but "
            f"found '{form}'.")
    datatype = value_name(arguments[1])
    access = value_name(arguments[2])
    named = {
        name.lower(): argument
        for name, argument in zip(names, arguments)
        if name is not None
    }
    unexpected_names = set(named) - {"mesh_arg", "ndata", "nlayers"}
    if unexpected_names:
        raise ParseError(
            "Unexpected named arg_type metadata argument(s): "
            f"{sorted(unexpected_names)}.")
    positional = [
        argument
        for name, argument in zip(names, arguments)
        if name is None
    ]
    stencil = None
    if form == "gh_field" and len(positional) > 4:
        if len(positional) > 5:
            raise ParseError("Unexpected positional field metadata argument.")
        stencil_node = positional[4]
        if (not isinstance(stencil_node, Call) or
                stencil_node.routine.symbol.name.lower() != "stencil"):
            raise ParseError("Expected stencil(type) metadata.")
        if len(stencil_node.arguments) not in (1, 2):
            raise ParseError("Expected stencil(type[, extent]) metadata.")
        stencil = _normalise(
            value_name(stencil_node.arguments[0]), "stencil type",
            LFRicConstants().VALID_STENCIL_TYPES)
        if len(stencil_node.arguments) == 2:
            extent = stencil_node.arguments[1]
            if (not isinstance(extent, Literal) or
                    extent.datatype.intrinsic !=
                    ScalarType.Intrinsic.INTEGER or
                    not extent.value.isdecimal() or int(extent.value) < 1):
                raise ParseError(
                    "Stencil extent must be a positive integer literal.")
            raise NotImplementedError(
                "Kernels with fixed stencil extents are not currently "
                "supported.")
        if access != "gh_read":
            raise ParseError(
                "In the LFRic API a field with a stencil access must be "
                "read-only ('gh_read'), but found "
                f"'{access}'."
            )
    nlayers = value_name(named["nlayers"]) if "nlayers" in named else ""
    ndata = value_name(named["ndata"]) if "ndata" in named else "1"

    if form == "gh_scalar":
        if len(arguments) != 3:
            raise ParseError("Scalar metadata must have three arguments.")
        return ScalarArgMetadata(datatype, access)
    if form == "gh_scalar_array":
        if len(arguments) != 4:
            raise ParseError(
                "Scalar-array metadata must have four arguments."
            )
        return ScalarArrayArgMetadata(
            datatype, access, int(value_name(arguments[3]))
        )
    if form in ("gh_operator", "gh_columnwise_operator"):
        if len(arguments) != 5:
            raise ParseError("Operator metadata must have five arguments.")
        metadata_type = (
            OperatorArgMetadata
            if form == "gh_operator"
            else ColumnwiseOperatorArgMetadata
        )
        return metadata_type(
            datatype,
            access,
            value_name(arguments[3]),
            value_name(arguments[4]),
        )
    if form != "gh_field":
        const = LFRicConstants()
        raise ParseError(
            "The first argument of arg_type must be one of "
            f"{const.VALID_ARG_TYPE_NAMES}, but found '{form}'."
        )
    if len(positional) < 4:
        raise ParseError("Field metadata must have a function space.")
    function_space = value_name(positional[3])
    mesh = value_name(named["mesh_arg"]) if "mesh_arg" in named else None
    if mesh and vector_length:
        return InterGridVectorArgMetadata(
            datatype,
            access,
            function_space,
            mesh,
            vector_length,
            stencil=stencil,
            nlayers=nlayers,
            ndata=ndata,
        )
    if mesh:
        return InterGridArgMetadata(
            datatype,
            access,
            function_space,
            mesh,
            stencil=stencil,
            nlayers=nlayers,
            ndata=ndata,
        )
    if vector_length:
        return FieldVectorArgMetadata(
            datatype,
            access,
            function_space,
            vector_length,
            stencil=stencil,
            nlayers=nlayers,
            ndata=ndata,
        )
    return FieldArgMetadata(
        datatype,
        access,
        function_space,
        stencil=stencil,
        nlayers=nlayers,
        ndata=ndata,
    )


def _parse_func(node: Node) -> MetaFuncsArgMetadata:
    """Convert one PSyIR func_type constructor.

    :param node: the ``func_type`` constructor to convert.

    :returns: the typed meta-functions metadata.

    :raises ParseError: if the constructor structure is invalid.
    """
    if not isinstance(node, Call):
        raise ParseError(
            "Expected a metadata constructor but found "
            f"'{type(node).__name__}'."
        )
    if node.routine.symbol.name.lower() != "func_type":
        raise ParseError(
            "Each meta_funcs entry must use the func_type constructor."
        )
    values = []
    for argument in node.arguments:
        if isinstance(argument, Reference):
            values.append(argument.symbol.name.lower())
        elif isinstance(argument, Literal):
            values.append(argument.value.lower())
        else:
            raise ParseError(
                "Expected a metadata name or literal but found "
                f"'{type(argument).__name__}'."
            )
    values = tuple(values)
    if len(values) < 2 or len(values) > 3:
        raise ParseError(
            "func_type requires a function space and one or two operators."
        )
    operators = values[1:]
    if len(set(operators)) != len(operators):
        raise ParseError(
            "A basis-function name must not be repeated in func_type."
        )
    const = LFRicConstants()
    invalid = set(operators) - set(const.VALID_METAFUNC_NAMES)
    if invalid:
        raise ParseError(
            f"Invalid meta_funcs operator(s): {sorted(invalid)}."
        )
    return MetaFuncsArgMetadata(
        values[0],
        basis_function="gh_basis" in operators,
        diff_basis_function="gh_diff_basis" in operators,
    )
