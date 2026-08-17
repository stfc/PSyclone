# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""Immutable LFRic kernel metadata built from language-level PSyIR."""

from dataclasses import dataclass, field
import re
from typing import ClassVar, Iterable, Optional, TypeAlias

from psyclone.core import AccessType
from psyclone.domain.lfric.lfric_constants import LFRicConstants
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
    UnsupportedFortranType,
)

# Metadata records naturally contain more state than behavioural classes.
# pylint: disable=too-many-instance-attributes,too-many-lines


def _normalise(
    value: str,
    description: str,
    valid_values: Optional[Iterable[str]] = None,
) -> str:
    """Validate and lower-case one metadata name.

    :param value: the value to normalise.
    :param description: a textual description of the value.
    :param valid_values: an optional collection of permitted values.

    :returns: the validated, lower-case value.

    :raises TypeError: if the value is not a string.
    :raises ValueError: if the value is not one of the valid values.
    """
    if not isinstance(value, str):
        raise TypeError(
            f"Expected {description} to be a string but found "
            f"'{type(value).__name__}'."
        )
    value = value.lower()
    if valid_values and value not in valid_values:
        raise ValueError(
            f"Expected {description} to be one of {valid_values} but found "
            f"'{value}'."
        )
    return value


@dataclass(frozen=True, slots=True)
class ScalarArgMetadata:
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


@dataclass(frozen=True, slots=True)
class ScalarArrayArgMetadata:
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


@dataclass(frozen=True, slots=True)
class FieldArgMetadata:
    """Metadata for a field kernel argument."""

    datatype: str
    access: str
    function_space: str
    stencil: Optional[str] = None
    nlevels: Optional[str] = None
    ndata: Optional[str] = "1"
    form: ClassVar[str] = "gh_field"
    check_name: ClassVar[str] = "field"

    def __post_init__(self) -> None:
        """Validate and normalise the field metadata.

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
        if self.nlevels is not None:
            object.__setattr__(
                self, "nlevels", _normalise(self.nlevels, "nlevels")
            )
        if self.ndata is None:
            object.__setattr__(self, "ndata", "1")
        else:
            object.__setattr__(
                self, "ndata", _normalise(str(self.ndata), "ndata")
            )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        result = (
            f"arg_type({self.form}, {self.datatype}, {self.access}, "
            f"{self.function_space}"
        )
        if self.stencil:
            result += f", stencil({self.stencil})"
        if self.nlevels:
            result += f", nlevels='{self.nlevels}'"
        if self.ndata != "1":
            result += f", ndata='{self.ndata}'"
        return result + ")"


@dataclass(frozen=True, slots=True)
class FieldVectorArgMetadata:
    """Metadata for a field-vector kernel argument."""

    datatype: str
    access: str
    function_space: str
    vector_length: str
    stencil: Optional[str] = None
    nlevels: Optional[str] = None
    ndata: Optional[str] = "1"
    form: ClassVar[str] = "gh_field"
    check_name: ClassVar[str] = "field-vector"

    def __post_init__(self) -> None:
        """Validate and normalise the field-vector metadata.

        :raises TypeError: if a field value has the wrong type.
        :raises ValueError: if a value or vector length is invalid.
        """
        field_metadata = FieldArgMetadata(
            self.datatype,
            self.access,
            self.function_space,
            self.stencil,
            self.nlevels,
            self.ndata,
        )
        for name in (
            "datatype",
            "access",
            "function_space",
            "stencil",
            "nlevels",
            "ndata",
        ):
            object.__setattr__(self, name, getattr(field_metadata, name))
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

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        result = (
            f"arg_type({self.form}*{self.vector_length}, {self.datatype}, "
            f"{self.access}, {self.function_space}"
        )
        if self.stencil:
            result += f", stencil({self.stencil})"
        if self.nlevels:
            result += f", nlevels='{self.nlevels}'"
        if self.ndata != "1":
            result += f", ndata='{self.ndata}'"
        return result + ")"


@dataclass(frozen=True, slots=True)
class InterGridArgMetadata:
    """Metadata for an inter-grid field argument."""

    datatype: str
    access: str
    function_space: str
    mesh_arg: str
    stencil: Optional[str] = None
    nlevels: Optional[str] = None
    ndata: Optional[str] = "1"
    form: ClassVar[str] = "gh_field"
    check_name: ClassVar[str] = "inter-grid"

    def __post_init__(self) -> None:
        """Validate and normalise the inter-grid field metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        field_metadata = FieldArgMetadata(
            self.datatype,
            self.access,
            self.function_space,
            self.stencil,
            self.nlevels,
            self.ndata,
        )
        for name in (
            "datatype",
            "access",
            "function_space",
            "stencil",
            "nlevels",
            "ndata",
        ):
            object.__setattr__(self, name, getattr(field_metadata, name))
        const = LFRicConstants()
        object.__setattr__(
            self,
            "mesh_arg",
            _normalise(
                self.mesh_arg, "mesh_arg", const.VALID_MESH_TYPES
            ),
        )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        result = FieldArgMetadata(
            self.datatype,
            self.access,
            self.function_space,
            self.stencil,
            self.nlevels,
            self.ndata,
        ).fortran_string()
        return f"{result[:-1]}, mesh_arg={self.mesh_arg})"


@dataclass(frozen=True, slots=True)
class InterGridVectorArgMetadata:
    """Metadata for an inter-grid field-vector argument."""

    datatype: str
    access: str
    function_space: str
    mesh_arg: str
    vector_length: str
    stencil: Optional[str] = None
    nlevels: Optional[str] = None
    ndata: Optional[str] = "1"
    form: ClassVar[str] = "gh_field"
    check_name: ClassVar[str] = "inter-grid-vector"

    def __post_init__(self) -> None:
        """Validate and normalise the inter-grid vector metadata.

        :raises TypeError: if a value has the wrong type.
        :raises ValueError: if a value is invalid.
        """
        vector_metadata = FieldVectorArgMetadata(
            self.datatype,
            self.access,
            self.function_space,
            self.vector_length,
            self.stencil,
            self.nlevels,
            self.ndata,
        )
        for name in (
            "datatype",
            "access",
            "function_space",
            "vector_length",
            "stencil",
            "nlevels",
            "ndata",
        ):
            object.__setattr__(self, name, getattr(vector_metadata, name))
        const = LFRicConstants()
        object.__setattr__(
            self,
            "mesh_arg",
            _normalise(
                self.mesh_arg, "mesh_arg", const.VALID_MESH_TYPES
            ),
        )

    def fortran_string(self) -> str:
        """
        :returns: this descriptor as Fortran constructor syntax.
        """
        result = FieldVectorArgMetadata(
            self.datatype,
            self.access,
            self.function_space,
            self.vector_length,
            self.stencil,
            self.nlevels,
            self.ndata,
        ).fortran_string()
        return f"{result[:-1]}, mesh_arg={self.mesh_arg})"


@dataclass(frozen=True, slots=True)
class OperatorArgMetadata:
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


@dataclass(frozen=True, slots=True)
class ColumnwiseOperatorArgMetadata(OperatorArgMetadata):
    """Metadata for a columnwise operator argument."""

    form: ClassVar[str] = "gh_columnwise_operator"
    check_name: ClassVar[str] = "columnwise-operator"


@dataclass(frozen=True, slots=True)
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


@dataclass(frozen=True, slots=True)
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


@dataclass(frozen=True, slots=True)
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


KernelArgumentMetadata: TypeAlias = (
    ScalarArgMetadata
    | ScalarArrayArgMetadata
    | FieldArgMetadata
    | FieldVectorArgMetadata
    | InterGridArgMetadata
    | InterGridVectorArgMetadata
    | OperatorArgMetadata
    | ColumnwiseOperatorArgMetadata
)

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


@dataclass(frozen=True, slots=True)
class LFRicKernelMetadata:
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
        if self.name is not None:
            FortranReader.validate_name(self.name)

    @classmethod
    def create_from_psyir(
        cls, symbol: DataTypeSymbol
    ) -> "LFRicKernelMetadata":
        """Create typed metadata from a PSyIR DataTypeSymbol.

        :param symbol: the symbol containing the metadata declaration.

        :returns: the parsed language-level kernel metadata.

        :raises TypeError: if the symbol or its datatype is invalid.
        :raises ParseError: if the metadata declaration is invalid.
        """
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found "
                f"'{type(symbol).__name__}'."
            )
        if not isinstance(symbol.datatype, UnsupportedFortranType):
            raise TypeError(
                "Expected metadata to use UnsupportedFortranType but found "
                f"'{type(symbol.datatype).__name__}'."
            )
        return _metadata_from_declaration(
            symbol.name, symbol.datatype.declaration
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

    def _validate_intergrid(self) -> None:
        """Validate inter-grid constraints.

        :raises ParseError: if an inter-grid constraint is violated.
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
        writers = [arg for arg in cma_args if arg.access != "gh_read"]
        if lma_args:
            if len(cma_args) != 1 or len(writers) != 1:
                raise ParseError(
                    "A CMA assembly kernel must write one CMA operator."
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
        if len(writers) != 1 or any(
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
        self, types: type | list[type]
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
        self, types: type | list[type], function_space: str
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
        self, types: type | list[type], function_space: str
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

    def lower_to_psyir(self) -> DataTypeSymbol:
        """
        :returns: the language-level PSyIR symbol for this metadata.
        """
        return DataTypeSymbol(
            self.name, UnsupportedFortranType(self.fortran_string())
        )


def _expression(source: str) -> Node:
    """Parse one metadata initializer into PSyIR.

    :param source: the metadata expression to parse.

    :returns: the PSyIR representation of the expression.

    :raises ParseError: if the expression cannot be parsed.
    """
    try:
        return FortranReader().psyir_from_expression(source)
    except Exception as err:
        raise ParseError(
            f"Failed to parse metadata initializer '{source}'."
        ) from err


def _call_name(node: Node) -> str:
    """Return the lower-case routine name of a PSyIR Call.

    :param node: the node expected to contain a call.

    :returns: the lower-case name of the called routine.

    :raises ParseError: if ``node`` is not a Call.
    """
    if not isinstance(node, Call):
        raise ParseError(
            f"Expected a metadata constructor but found "
            f"'{type(node).__name__}'."
        )
    return node.routine.symbol.name.lower()


def _name(node: Node) -> str:
    """Return a metadata scalar represented by a Reference or Literal.

    :param node: the node containing the metadata value.

    :returns: the lower-case metadata value.

    :raises ParseError: if the node is not a Reference or Literal.
    """
    if isinstance(node, Reference):
        return node.symbol.name.lower()
    if isinstance(node, Literal):
        return node.value.lower()
    raise ParseError(
        f"Expected a metadata name or literal but found "
        f"'{type(node).__name__}'."
    )


def _array_values(node: Node) -> tuple[Node, ...]:
    """Return the children of an array constructor or one scalar.

    :param node: an array constructor or scalar node.

    :returns: the array elements or a tuple containing the scalar.
    """
    if isinstance(node, ArrayConstructor):
        return tuple(node.children)
    return (node,)


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
    if _call_name(node) != "arg_type":
        raise ParseError(
            "Each meta_args entry must use the arg_type constructor."
        )
    arguments = tuple(node.arguments)
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
        form = _name(form_node.children[0])
        vector_length = _name(form_node.children[1])
    else:
        form = _name(form_node)
    datatype = _name(arguments[1])
    access = _name(arguments[2])
    named = {
        name.lower(): argument
        for name, argument in zip(names, arguments)
        if name is not None
    }
    positional = [
        argument
        for name, argument in zip(names, arguments)
        if name is None
    ]
    stencil = None
    if len(positional) > 4 and isinstance(positional[4], Call):
        if _call_name(positional[4]) != "stencil":
            raise ParseError("Expected stencil(type) metadata.")
        if len(positional[4].arguments) != 1:
            raise NotImplementedError(
                "Kernels with fixed stencil extents are not currently "
                "supported."
            )
        stencil = _name(positional[4].arguments[0])
        if access != "gh_read":
            raise ParseError(
                "In the LFRic API a field with a stencil access must be "
                "read-only ('gh_read'), but found "
                f"'{access}'."
            )
    nlevels = _name(named["nlevels"]) if "nlevels" in named else None
    ndata = _name(named["ndata"]) if "ndata" in named else "1"

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
            datatype, access, int(_name(arguments[3]))
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
            _name(arguments[3]),
            _name(arguments[4]),
        )
    if form != "gh_field":
        const = LFRicConstants()
        raise ParseError(
            "The first argument of arg_type must be one of "
            f"{const.VALID_ARG_TYPE_NAMES}, but found '{form}'."
        )
    if len(positional) < 4:
        raise ParseError("Field metadata must have a function space.")
    function_space = _name(positional[3])
    mesh = _name(named["mesh_arg"]) if "mesh_arg" in named else None
    if mesh and vector_length:
        return InterGridVectorArgMetadata(
            datatype,
            access,
            function_space,
            mesh,
            vector_length,
            stencil,
            nlevels,
            ndata,
        )
    if mesh:
        return InterGridArgMetadata(
            datatype,
            access,
            function_space,
            mesh,
            stencil,
            nlevels,
            ndata,
        )
    if vector_length:
        return FieldVectorArgMetadata(
            datatype,
            access,
            function_space,
            vector_length,
            stencil,
            nlevels,
            ndata,
        )
    return FieldArgMetadata(
        datatype,
        access,
        function_space,
        stencil,
        nlevels,
        ndata,
    )


def _parse_func(node: Node) -> MetaFuncsArgMetadata:
    """Convert one PSyIR func_type constructor.

    :param node: the ``func_type`` constructor to convert.

    :returns: the typed meta-functions metadata.

    :raises ParseError: if the constructor structure is invalid.
    """
    if _call_name(node) != "func_type":
        raise ParseError(
            "Each meta_funcs entry must use the func_type constructor."
        )
    values = tuple(_name(argument) for argument in node.arguments)
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


def _component_initializers(
    declaration: str,
) -> dict[str, tuple[str, str]]:
    """Return known component initializers from a declaration.

    :param declaration: the Fortran metadata declaration.

    :returns: component names mapped to their source lines and initializers.
    """
    known = (
        "meta_args",
        "meta_funcs",
        "meta_reference_element",
        "meta_mesh",
        "gh_shape",
        "gh_evaluator_targets",
        "operates_on",
    )
    result = {}
    for line in declaration.splitlines():
        if "=" not in line or "::" not in line:
            continue
        lhs, rhs = line.split("=", 1)
        component = lhs.split("::", 1)[1].strip()
        for name in known:
            if re.match(rf"(?i)^{name}\b", component):
                result[name] = (line, rhs.strip())
                break
    return result


def _declared_extent(line: str, component: str) -> Optional[int]:
    """Return a literal rank-one component extent, if present.

    :param line: the declaration line to inspect.
    :param component: the component name.

    :returns: the declared extent, or ``None`` if none is found.
    """
    match = re.search(r"(?i)\bdimension\s*\(\s*(\d+)\s*\)", line)
    if not match:
        match = re.search(
            rf"(?i)\b{component}\s*\(\s*(\d+)\s*\)", line
        )
    return int(match.group(1)) if match else None


def _checked_array(
    line: str, component: str, rhs: str
) -> tuple[Node, ...]:
    """Parse an initializer and check its declared extent.

    :param line: the component declaration line.
    :param component: the component name.
    :param rhs: the component initializer.

    :returns: the parsed initializer values.

    :raises ParseError: if the extent is absent or does not match.
    """
    values = _array_values(_expression(rhs))
    extent = _declared_extent(line, component)
    if extent is None:
        raise ParseError(f"Metadata component '{component}' must be an array.")
    if extent != len(values):
        raise ParseError(
            f"Metadata component '{component}' has extent {extent} but its "
            f"constructor contains {len(values)} values."
        )
    return values


def _metadata_from_declaration(
    name: str, declaration: str
) -> LFRicKernelMetadata:
    """Build typed metadata from an UnsupportedFortranType declaration.

    :param name: the name of the metadata type.
    :param declaration: its Fortran declaration.

    :returns: the parsed language-level LFRic metadata.

    :raises ParseError: if the declaration is incomplete or invalid.
    """
    header = declaration.splitlines()[0]
    if "extends(kernel_type)" not in header.lower().replace(" ", ""):
        raise ParseError(
            "LFRic kernel metadata must extend kernel_type."
        )
    components = _component_initializers(declaration)
    if "meta_args" not in components:
        raise ParseError(f"No meta_args found in kernel metadata '{name}'.")
    line, rhs = components["meta_args"]
    meta_args = tuple(
        _parse_arg(node)
        for node in _checked_array(line, "meta_args", rhs)
    )
    meta_funcs = ()
    if "meta_funcs" in components:
        line, rhs = components["meta_funcs"]
        meta_funcs = tuple(
            _parse_func(node)
            for node in _checked_array(line, "meta_funcs", rhs)
        )

    def names(component: str) -> tuple[str, ...]:
        """Parse all scalar names in one metadata component.

        :param component: the component name.

        :returns: the component's lower-case scalar values.
        """
        if component not in components:
            return ()
        line, rhs = components[component]
        node = _expression(rhs)
        if isinstance(node, ArrayConstructor):
            values = _checked_array(line, component, rhs)
        else:
            values = (node,)
        return tuple(_name(value) for value in values)

    ref_element = ()
    if "meta_reference_element" in components:
        line, rhs = components["meta_reference_element"]
        ref_element = tuple(
            MetaRefElementArgMetadata(_name(node.arguments[0]))
            for node in _checked_array(
                line, "meta_reference_element", rhs
            )
            if _call_name(node) == "reference_element_data_type"
        )
    mesh = ()
    if "meta_mesh" in components:
        line, rhs = components["meta_mesh"]
        mesh = tuple(
            MetaMeshArgMetadata(_name(node.arguments[0]))
            for node in _checked_array(line, "meta_mesh", rhs)
            if _call_name(node) == "mesh_data_type"
        )

    procedure_name = None
    for line in declaration.splitlines():
        match = re.search(
            r"(?i)^\s*procedure\b.*::\s*(?:code\s*=>\s*)?(\w+)",
            line,
        )
        if match:
            procedure_name = match.group(1)
            break
    operates_on = (
        names("operates_on")[0] if "operates_on" in components else None
    )
    return LFRicKernelMetadata(
        operates_on=operates_on,
        shapes=names("gh_shape"),
        evaluator_targets=names("gh_evaluator_targets"),
        meta_args=meta_args,
        meta_funcs=meta_funcs,
        meta_ref_element=ref_element,
        meta_mesh=mesh,
        procedure_name=procedure_name,
        name=name,
    )


@dataclass(frozen=True, slots=True)
class KernelProcedure:
    """The name and PSyIR implementation(s) of a kernel procedure."""

    name: str
    ast: Optional[Routine] = field(default=None, compare=False, repr=False)
    implementations: tuple[Routine, ...] = field(
        default=(), compare=False, repr=False
    )


@dataclass(frozen=True, slots=True)
class LFRicArgDescriptor:
    """An immutable consumer-facing view of one meta_args entry."""

    access: AccessType
    function_space: Optional[str]
    metadata_index: int
    mesh: Optional[str]
    argument_type: str
    data_type: str
    function_space_to: Optional[str] = None
    function_space_from: Optional[str] = None
    vector_size: int = 1
    array_ndims: int = 1
    nlevels: Optional[str] = None
    ndata: str = "1"
    stencil_type: Optional[str] = None
    stencil_extent: Optional[str] = None

    @property
    def stencil(self) -> Optional[dict[str, Optional[str]]]:
        """
        :returns: the legacy stencil view expected by consumers, if any.
        """
        if self.stencil_type is None:
            return None
        return {"type": self.stencil_type, "extent": self.stencil_extent}

    @property
    def function_spaces(self) -> tuple[Optional[str], ...]:
        """
        :returns: all function spaces associated with this argument.
        """
        if self.function_space_to is not None:
            return (self.function_space_to, self.function_space_from)
        if self.function_space is not None:
            return (self.function_space,)
        return ()

    def __str__(self) -> str:
        """
        :returns: a human-readable description of this descriptor.
        """
        return (
            "LFRicArgDescriptor object\n"
            f"  argument_type='{self.argument_type}'\n"
            f"  data_type='{self.data_type}'\n"
            f"  access_descriptor='{self.access.api_specific_name()}'\n"
        )


@dataclass(frozen=True, slots=True)
class LFRicFuncDescriptor:
    """An immutable consumer-facing meta_funcs descriptor."""

    function_space_name: str
    operator_names: tuple[str, ...]

    def __repr__(self) -> str:
        """
        :returns: a concise representation of this descriptor.
        """
        values = ", ".join(
            (self.function_space_name,) + self.operator_names
        )
        return f"LFRicFuncDescriptor(func_type({values}))"

    def __str__(self) -> str:
        """
        :returns: a human-readable description of this descriptor.
        """
        values = ", ".join(self.operator_names)
        return (
            "LFRicFuncDescriptor object\n"
            f"  function_space_name='{self.function_space_name}'\n"
            f"  operator_names=({values})"
        )


@dataclass(frozen=True, slots=True)
class LFRicPropertyMetadata:
    """An immutable collection of requested LFRic properties."""

    properties: tuple[str, ...] = ()


@dataclass(frozen=True, slots=True)
class LFRicKernMetadata:
    """Immutable consumer-facing kernel metadata."""

    name: str
    iterates_over: str
    procedure: KernelProcedure
    arg_descriptors: tuple[LFRicArgDescriptor, ...]
    psyir: object = field(compare=False, repr=False)
    func_descriptors: tuple[LFRicFuncDescriptor, ...] = ()
    eval_shapes: tuple[str, ...] = ()
    eval_targets: tuple[str, ...] = ()
    cma_operation: Optional[str] = None
    is_intergrid: bool = False
    reference_element: LFRicPropertyMetadata = field(
        default_factory=LFRicPropertyMetadata
    )
    mesh: LFRicPropertyMetadata = field(
        default_factory=LFRicPropertyMetadata
    )

    def __post_init__(self) -> None:
        """Validate all consumer-facing kernel metadata.

        :raises ParseError: if any kernel constraint is violated.
        """
        self._validate_writes()
        need_evaluator = self._validate_evaluators()
        self._validate_cma()
        self._validate_domain_dof(need_evaluator)

    @property
    def _ast(self) -> object:
        """
        :returns: the PSyIR tree retained for existing consumers.
        """
        return self.psyir

    @property
    def nargs(self) -> int:
        """
        :returns: the number of metadata arguments.
        """
        return len(self.arg_descriptors)

    def _validate_writes(self) -> None:
        """Validate that the kernel has a permitted written argument.

        :raises ParseError: if the write requirements are violated.
        """
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP

        const = LFRicConstants()
        writes = 0
        for arg in self.arg_descriptors:
            if arg.access == AccessType.READ:
                continue
            writes += 1
            if (
                arg.argument_type in const.VALID_FIELD_NAMES
                and arg.function_space in const.READ_ONLY_FUNCTION_SPACES
            ):
                raise ParseError(
                    f"Found kernel metadata in '{self.name}' that specifies "
                    f"writing to the read-only function space "
                    f"'{arg.function_space}'."
                )
            if (
                self.name not in BUILTIN_MAP
                and arg.argument_type in const.VALID_SCALAR_NAMES
            ):
                raise ParseError(
                    f"A user-supplied LFRic kernel must not write/update a "
                    f"scalar argument but kernel '{self.name}' does."
                )
        if not writes:
            raise ParseError(
                "An LFRic kernel must have at least one argument that is "
                f"updated (written to) but found none for kernel "
                f"'{self.name}'."
            )

    def _validate_evaluators(self) -> bool:
        """Validate evaluator metadata.

        :returns: whether the kernel needs an evaluator.

        :raises ParseError: if the evaluator metadata is inconsistent.
        """
        const = LFRicConstants()
        spaces = {
            space
            for arg in self.arg_descriptors
            for space in arg.function_spaces
            if space
        }
        used = set()
        need = False
        for descriptor in self.func_descriptors:
            space = descriptor.function_space_name
            if space not in spaces:
                raise ParseError(
                    f"Function space '{space}' in meta_funcs does not exist "
                    "in meta_args."
                )
            if space in used:
                raise ParseError(
                    f"Function space '{space}' is repeated in meta_funcs."
                )
            used.add(space)
            need |= bool(
                set(descriptor.operator_names)
                & set(const.VALID_EVALUATOR_NAMES)
            )
        if need and not self.eval_shapes:
            raise ParseError(
                "A kernel requiring quadrature or an evaluator must also "
                "supply gh_shape, but this is missing for kernel "
                f"'{self.name}'."
            )
        if not need and self.eval_shapes:
            raise ParseError(
                f"Kernel '{self.name}' specifies gh_shape but does not need "
                "an evaluator."
            )
        if self.eval_targets:
            if not need or "gh_evaluator" not in self.eval_shapes:
                raise ParseError(
                    "gh_evaluator_targets requires gh_shape=gh_evaluator."
                )
            missing = set(self.eval_targets) - spaces
            if missing:
                raise ParseError(
                    f"Evaluator targets are not present in meta_args: "
                    f"{sorted(missing)}."
                )
        return need

    def _validate_cma(self) -> None:
        """Validate restrictions on consumer-facing CMA metadata.

        :raises ParseError: if a CMA restriction is violated.
        """
        if self.cma_operation is None:
            return
        for arg in self.arg_descriptors:
            if arg.vector_size > 1 or arg.stencil:
                raise ParseError(
                    "CMA kernels may not use vector or stencil arguments."
                )
            if arg.ndata != "1" or arg.nlevels:
                raise ParseError(
                    "CMA kernels require default NDATA and NLEVELS."
                )

    def _validate_domain_dof(self, need: bool) -> None:
        """Validate metadata for domain and degree-of-freedom kernels.

        :param need: whether the kernel needs an evaluator.

        :raises ParseError: if a domain or dof restriction is violated.
        """
        if self.iterates_over not in ("domain", "dof"):
            return
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP

        if self.iterates_over == "dof" and self.name in BUILTIN_MAP:
            return
        const = LFRicConstants()
        valid = const.VALID_SCALAR_NAMES + const.VALID_FIELD_NAMES
        if any(
            arg.argument_type not in valid
            for arg in self.arg_descriptors
        ):
            raise ParseError(
                f"Kernels operating on '{self.iterates_over}' may only "
                "contain scalar and field arguments."
            )
        if need or self.reference_element.properties or self.mesh.properties:
            raise ParseError(
                f"Kernels operating on '{self.iterates_over}' may not "
                "request evaluator, reference-element or mesh data."
            )
        if self.iterates_over == "dof":
            spaces = {
                space
                for arg in self.arg_descriptors
                for space in arg.function_spaces
            }
            if len(spaces) > 1:
                raise ParseError(
                    "A dof kernel must use one function space."
                )

    @classmethod
    def create_from_psyir(
        cls, psyir: Node, name: Optional[str] = None
    ) -> "LFRicKernMetadata":
        """Create immutable consumer metadata from a complete PSyIR tree.

        :param psyir: the complete PSyIR containing the kernel.
        :param name: optional name of the metadata type to extract.

        :returns: the extracted consumer-facing kernel metadata.

        :raises TypeError: if ``psyir`` is not a PSyIR tree.
        :raises ParseError: if the metadata or implementation is invalid.
        """
        # pylint: disable=too-many-locals
        container, symbol, _ = find_metadata_symbol(psyir, name)
        try:
            metadata = LFRicKernelMetadata.create_from_psyir(symbol)
            kernel_type = metadata.kernel_type
        except (TypeError, ValueError) as err:
            raise ParseError(
                f"Invalid LFRic metadata in '{symbol.name}': {err}"
            ) from err
        procedure = _kernel_procedure(container, metadata)
        arguments = tuple(
            _descriptor_from_metadata(entry, index)
            for index, entry in enumerate(metadata.meta_args)
        )
        functions = tuple(
            LFRicFuncDescriptor(
                entry.function_space,
                tuple(
                    value
                    for value, required in (
                        ("gh_basis", entry.basis_function),
                        ("gh_diff_basis", entry.diff_basis_function),
                    )
                    if required
                ),
            )
            for entry in metadata.meta_funcs
        )
        targets = list(metadata.evaluator_targets)
        if not targets and "gh_evaluator" in metadata.shapes:
            for argument in arguments:
                if (
                    argument.access != AccessType.READ
                    and argument.function_spaces
                    and argument.function_spaces[0] not in targets
                ):
                    targets.append(argument.function_spaces[0])
        # These enumerations are still the public vocabulary consumed by the
        # LFRic code-generation classes. Importing them here is safe because
        # extraction happens after module initialisation is complete.
        # pylint: disable=import-outside-toplevel
        from psyclone.lfric import MeshProperty, RefElementMetaData

        reference = LFRicPropertyMetadata(
            tuple(
                RefElementMetaData.Property[
                    entry.reference_element.upper()
                ]
                for entry in metadata.meta_ref_element
            )
        )
        mesh = LFRicPropertyMetadata(
            tuple(
                MeshProperty[entry.mesh.upper()]
                for entry in metadata.meta_mesh
            )
        )
        operation = (
            kernel_type.removeprefix("cma-")
            if kernel_type.startswith("cma-")
            else None
        )
        return cls(
            metadata.name,
            metadata.operates_on,
            procedure,
            arguments,
            psyir,
            functions,
            metadata.shapes,
            tuple(targets),
            operation,
            kernel_type == "inter-grid",
            reference,
            mesh,
        )

    @classmethod
    def create_from_fortran_string(
        cls, source: str, name: Optional[str] = None
    ) -> "LFRicKernMetadata":
        """Create metadata by first translating complete Fortran to PSyIR.

        :param source: complete Fortran source containing the kernel.
        :param name: name of the metadata type to extract.

        :returns: immutable metadata extracted from the generated PSyIR.

        :raises TypeError: if ``source`` is not a string.
        :raises ValueError: if the source cannot be translated to PSyIR.
        :raises ParseError: if the kernel metadata is invalid.
        """
        if not isinstance(source, str):
            raise TypeError(
                "LFRic kernel source must be supplied as a string."
            )
        try:
            psyir = FortranReader().psyir_from_source(source)
        except Exception as err:
            raise ValueError(
                "Failed to translate the supplied LFRic kernel source to "
                "PSyIR."
            ) from err
        return cls.create_from_psyir(psyir, name=name)


def _module_containers(psyir: Node) -> list[Container]:
    """Return module containers from a complete PSyIR tree.

    :param psyir: the PSyIR tree to search.

    :returns: all module containers in the tree.

    :raises TypeError: if ``psyir`` is not a PSyIR tree.
    """
    if not hasattr(psyir, "walk"):
        raise TypeError(
            f"Expected PSyIR but found '{type(psyir).__name__}'."
        )
    return [
        node
        for node in psyir.walk(Container)
        if not isinstance(node, FileContainer)
    ]


def find_metadata_symbol(
    psyir: Node, name: Optional[str] = None
) -> tuple[Container, DataTypeSymbol, str]:
    """Find a unique kernel metadata symbol in PSyIR.

    :param psyir: the PSyIR tree to search.
    :param name: optional metadata type name to match or infer.

    :returns: the containing module, metadata symbol and metadata name.

    :raises TypeError: if ``psyir`` is not a PSyIR tree.
    :raises ParseError: if matching metadata is absent or not unique.
    """
    containers = _module_containers(psyir)
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
                f"Module name '{module_name}' is too short to have '_mod' "
                "as an extension."
            )
        if not module_name.lower().endswith("_mod"):
            raise ParseError(
                f"Module name '{module_name}' does not have '_mod' as an "
                "extension."
            )
        name = module_name[:-4] + "_type"
    matches = []
    for container in containers:
        matches.extend(
            (container, symbol)
            for symbol in container.symbol_table.symbols
            if isinstance(symbol, DataTypeSymbol)
            and isinstance(symbol.datatype, UnsupportedFortranType)
            and symbol.name.lower() == name.lower()
        )
    if not matches:
        raise ParseError(f"Kernel type {name} does not exist.")
    if len(matches) > 1:
        raise ParseError(f"Kernel type {name} is not unique.")
    return matches[0][0], matches[0][1], name


def _kernel_procedure(
    container: Container, metadata: LFRicKernelMetadata
) -> KernelProcedure:
    """Resolve kernel procedure implementations from PSyIR.

    :param container: the module containing the kernel.
    :param metadata: the language-level kernel metadata.

    :returns: the resolved procedure and its implementation or variants.

    :raises ParseError: if the procedure metadata cannot be resolved.
    """
    routines = tuple(container.walk(Routine))
    if metadata.procedure_name:
        implementations = tuple(
            routine
            for routine in routines
            if routine.name.lower() == metadata.procedure_name.lower()
        )
        if not implementations:
            raise ParseError(
                f"Kernel subroutine '{metadata.procedure_name}' not found."
            )
        return KernelProcedure(
            metadata.procedure_name.lower(),
            implementations[0] if len(implementations) == 1 else None,
            implementations,
        )
    interfaces = [
        symbol
        for symbol in container.symbol_table.symbols
        if isinstance(symbol, GenericInterfaceSymbol)
    ]
    if len(interfaces) != 1:
        raise ParseError(
            f"Kernel '{metadata.name}' requires exactly one generic "
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
    return KernelProcedure(
        interface.name.lower(),
        implementations[0] if len(implementations) == 1 else None,
        implementations,
    )


def _descriptor_from_metadata(
    entry: KernelArgumentMetadata, index: int
) -> LFRicArgDescriptor:
    """Create a consumer descriptor from one typed metadata record.

    :param entry: the language-level argument metadata.
    :param index: its position in the metadata argument list.

    :returns: the consumer-facing argument descriptor.
    """
    const = LFRicConstants()
    access = const.ACCESS_MAPPING[entry.access]
    entry_type = type(entry)
    function_space = getattr(entry, "function_space", None)
    function_space_to = getattr(entry, "function_space_to", None)
    function_space_from = getattr(entry, "function_space_from", None)
    if function_space_from is not None:
        function_space = function_space_from
    vector_size = int(getattr(entry, "vector_length", 1))
    array_ndims = getattr(entry, "array_ndims", 1)
    if entry_type in (ScalarArgMetadata, ScalarArrayArgMetadata):
        vector_size = 0
    if entry_type is ScalarArgMetadata:
        array_ndims = 0
    mesh = getattr(entry, "mesh_arg", None)
    return LFRicArgDescriptor(
        access,
        function_space,
        index,
        mesh,
        entry.form,
        entry.datatype,
        function_space_to,
        function_space_from,
        vector_size,
        array_ndims,
        getattr(entry, "nlevels", None),
        str(getattr(entry, "ndata", None) or "1"),
        getattr(entry, "stencil", None),
    )
