# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""
The GOcean kernel metadata is stored with the following immutable nested
dataclasses:

GOceanKernelMetadata:
    iterates_over: str
    index_offset: str
    meta_args: tuple[Union[
        GOceanFieldArgMetadata:
            access: str
            grid_point_type: str
            stencil: GOceanStencilMetadata
                rows: Optional[tuple[str, str, str]]
                pointwise_name: ClassVar[str]
        GOceanScalarArgMetadata:
            access: str
            datatype: str
            form: str = "go_pointwise"
        GOceanGridPropertyArgMetadata:
            access: str
            name: str
    ]]
    procedure_name: str
    name: str

# TODO: For legacy reasons the following are not yet nested to the kernel
# metadata. Currently these replicate sibling or parent metadata information.

GOceanArgDescriptor:
    access: AccessType
    function_space: str
    metadata_index: int
    stencil: GOceanStencilMetadata
    argument_type: str
    grid_prop: str = ""

GOceanKernelProcedure:
    name: str
    ast: Optional[Routine] = field(default=None, compare=False, repr=False)
"""

from dataclasses import dataclass, field
import re
from typing import ClassVar, Iterable, Optional

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.gocean.gocean_constants import GOceanConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ArrayConstructor, Call, Container, FileContainer, Literal, Reference,
    Node, Routine)
from psyclone.psyir.symbols import ArrayType, DataTypeSymbol, StructureType


def _normalise(
    value: str,
    description: str,
    valid_values: Optional[Iterable[str]] = None
) -> str:
    '''
    :param value: the given value.
    :param description: a textual description of the value.
    :param valid_values: a list of valid values.

    :returns: a validated, lower-case version of the given value.

    :raises TypeError: if the value is not a 'str'.
    :raises ValueError: if the value is not one of the valid values.
    '''
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
class GOceanStencilMetadata:
    """GOcean stencil metadata"""

    rows: Optional[tuple[str, str, str]] = None
    pointwise_name: ClassVar[str] = "go_pointwise"

    def __post_init__(self) -> None:
        """Validate the stencil rows.

        :raises TypeError: if any stencil row is not a string.
        :raises ValueError: if the stencil has an invalid shape or value.
        """
        if self.rows is None:
            return
        if not isinstance(self.rows, tuple) or len(self.rows) != 3:
            raise ValueError(
                "A GOcean stencil must contain exactly three rows."
            )
        for row in self.rows:
            if not isinstance(row, str):
                raise TypeError("GOcean stencil rows must be strings.")
            if not re.fullmatch(r"[0-9]{3}", row):
                raise ValueError(
                    "GOcean stencil rows must each contain three decimal "
                    f"depths but found '{row}'."
                )
        if (
            self.rows[0] == "000"
            and self.rows[1] in ("000", "010")
            and self.rows[2] == "000"
        ):
            raise ValueError(
                "A zero-sized stencil must use go_pointwise."
            )

    @property
    def has_stencil(self) -> bool:
        """
        :returns: whether this represents a non-pointwise access.
        """
        return self.rows is not None

    @property
    def name(self) -> Optional[str]:
        """
        :returns: the pointwise name, or ``None`` for an explicit stencil.
        """
        return None if self.has_stencil else self.pointwise_name

    def depth(self, index0: int, index1: int) -> int:
        """
        :param index0: the first index.
        :param index1: the second index.

        :returns: the access depth at the supplied relative indices.

        :raises GenerationError: if either index is out of range.
        """
        if index0 not in (-1, 0, 1) or index1 not in (-1, 0, 1):
            raise GenerationError(
                "The indices arguments to the depth method in the "
                "GOStencil object must be between -1 and 1 but found "
                f"({index0},{index1})"
            )
        if self.rows is None:
            return int(index0 == 0 and index1 == 0)
        return int(self.rows[1 - index1][index0 + 1])


@dataclass(frozen=True, slots=True)
class GOceanFieldArgMetadata:
    """GOcean field argument."""

    access: str
    grid_point_type: str
    stencil: GOceanStencilMetadata = field(
        default_factory=GOceanStencilMetadata
    )

    def __post_init__(self) -> None:
        """Validate and normalise the field-argument metadata.

        :raises TypeError: if any value has the wrong type.
        :raises ValueError: if any value is outside the valid vocabulary.
        """
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            _normalise(self.access,  "field access", const.VALID_ACCESS_TYPES))
        object.__setattr__(
            self, "grid_point_type",
            _normalise(
                self.grid_point_type, "field grid-point type",
                const.VALID_FIELD_GRID_TYPES))
        if not isinstance(self.stencil, GOceanStencilMetadata):
            raise TypeError("Field stencil must be a GOceanStencilMetadata.")

    @property
    def form(self) -> str:
        """
        :returns: the 'form' metadata field.
        """
        return (
            GOceanConstants().VALID_STENCIL_NAME
            if self.stencil.has_stencil
            else "go_pointwise"
        )

    def fortran_string(self) -> str:
        """
        :returns: this argument as a Fortran metadata string.
        """
        if self.stencil.has_stencil:
            form = (
                f"{GOceanConstants().VALID_STENCIL_NAME}"
                f"({', '.join(self.stencil.rows)})"
            )
        else:
            form = "go_pointwise"
        return (
            f"go_arg({self.access}, {self.grid_point_type}, {form})"
        )


@dataclass(frozen=True, slots=True)
class GOceanScalarArgMetadata:
    """Metadata for one GOcean scalar argument."""

    access: str
    datatype: str
    form: str = "go_pointwise"

    def __post_init__(self) -> None:
        """Validate and normalise the scalar-argument metadata.

        :raises TypeError: if any value has the wrong type.
        :raises ValueError: if any value is outside the valid vocabulary.
        """
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            _normalise(self.access, "scalar access", const.VALID_ACCESS_TYPES))
        object.__setattr__(
            self, "datatype",
            _normalise(
                self.datatype, "scalar datatype", const.VALID_SCALAR_TYPES))
        object.__setattr__(
            self, "form",
            _normalise(
                self.form,  "scalar access form", const.VALID_STENCIL_NAMES))

    def fortran_string(self) -> str:
        """
        :returns: this argument as a Fortran metadata string.
        """
        return f"go_arg({self.access}, {self.datatype}, {self.form})"


@dataclass(frozen=True, slots=True)
class GOceanGridPropertyArgMetadata:
    """Metadata for one GOcean grid-property argument."""

    access: str
    name: str

    def __post_init__(self) -> None:
        """Validate and normalise the grid-property metadata.

        :raises TypeError: if any value has the wrong type.
        :raises ValueError: if any value is outside the valid vocabulary.
        """
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            _normalise(
                self.access, "grid-property access",
                const.VALID_ACCESS_TYPES))
        name = _normalise(self.name, "grid-property name")
        properties = Config.get().api_conf("gocean").grid_properties
        if name not in properties:
            raise ValueError(
                f"Expected grid-property name to be one of "
                f"{list(properties)} but found '{name}'."
            )
        object.__setattr__(self, "name", name)

    def fortran_string(self) -> str:
        """
        :returns: this argument as a Fortran metadata string.
        """
        return f"go_arg({self.access}, {self.name})"


_META_ARG_TYPES = (
    GOceanFieldArgMetadata,
    GOceanScalarArgMetadata,
    GOceanGridPropertyArgMetadata,
)


@dataclass(frozen=True, slots=True)
class GOceanKernelProcedure:
    """The name and optional PSyIR implementation of a GOcean kernel."""

    name: str
    ast: Optional[Routine] = field(default=None, compare=False, repr=False)


@dataclass(frozen=True, slots=True)
class GOceanArgDescriptor:
    """GOcean argument metadata"""

    access: AccessType
    function_space: str
    metadata_index: int
    stencil: GOceanStencilMetadata
    argument_type: str
    grid_prop: str = ""

    def __post_init__(self) -> None:
        """Validate the consumer-facing argument descriptor.

        :raises TypeError: if the access is not an AccessType.
        :raises InternalError: if the metadata index is invalid.
        """
        if not isinstance(self.access, AccessType):
            raise TypeError("Descriptor access must be an AccessType.")
        if not isinstance(self.metadata_index, int) or self.metadata_index < 0:
            raise InternalError(
                "The metadata index must be an integer and greater than or "
                f"equal to zero but got: {self.metadata_index}"
            )

    def __repr__(self) -> str:
        """
        :returns: a concise representation of this descriptor.
        """
        return (
            f"Descriptor({self.access}, {self.function_space}, "
            f"{self.metadata_index})"
        )

    __str__ = __repr__


@dataclass(frozen=True, slots=True)
class GOceanKernelMetadata:
    """GOcean kernel metadata."""

    iterates_over: str
    index_offset: str
    meta_args: tuple[
        GOceanFieldArgMetadata
        | GOceanScalarArgMetadata
        | GOceanGridPropertyArgMetadata,
        ...,
    ]
    procedure_name: str
    name: str
    psyir: Optional[Node] = field(default=None, compare=False, repr=False)

    # Preserve the concise nested names used by metadata-to-argument rules.
    FieldArg: ClassVar[type] = GOceanFieldArgMetadata
    ScalarArg: ClassVar[type] = GOceanScalarArgMetadata
    GridArg: ClassVar[type] = GOceanGridPropertyArgMetadata

    def __post_init__(self) -> None:
        """Validate and normalise the complete kernel metadata.

        :raises TypeError: if any value has the wrong type.
        :raises ValueError: if a value or Fortran name is invalid.
        :raises ParseError: if grid properties have no associated field.
        """
        const = GOceanConstants()
        object.__setattr__(
            self, "iterates_over",
            _normalise(
                self.iterates_over, "iterates_over",
                const.VALID_ITERATES_OVER))
        object.__setattr__(
            self, "index_offset",
            _normalise(
                self.index_offset, "index_offset",
                const.VALID_OFFSET_NAMES))
        object.__setattr__(self, "meta_args", tuple(self.meta_args))
        if any(
            not isinstance(argument, _META_ARG_TYPES)
            for argument in self.meta_args
        ):
            raise TypeError(
                "All meta_args entries must be GOcean argument metadata."
            )
        object.__setattr__(
            self, "procedure_name",
            _normalise(self.procedure_name, "procedure name"))
        object.__setattr__(self, "name", _normalise(self.name, "kernel name"))
        FortranReader.validate_name(self.procedure_name)
        FortranReader.validate_name(self.name)
        has_grid_property = any(
            isinstance(argument, GOceanGridPropertyArgMetadata)
            for argument in self.meta_args
        )
        has_field = any(
            isinstance(argument, GOceanFieldArgMetadata)
            for argument in self.meta_args
        )
        if has_grid_property and not has_field:
            raise ParseError(
                f"Kernel {self.name} requires a property of the grid but "
                "does not have any field objects as arguments."
            )

    @classmethod
    def create_from_psyir(
        cls, symbol: DataTypeSymbol
    ) -> "GOceanKernelMetadata":
        """Create metadata from a language-level PSyIR type symbol.

        :param symbol: the symbol containing the metadata declaration.

        :returns: the parsed GOcean kernel metadata.

        :raises TypeError: if ``symbol`` is not a DataTypeSymbol.
        :raises InternalError: if its datatype is not a StructureType.
        :raises ParseError: if the metadata declaration is invalid.
        """
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found "
                f"'{type(symbol).__name__}'."
            )
        if not isinstance(symbol.datatype, StructureType):
            raise InternalError(
                "Expected kernel metadata to be stored in the PSyIR as an "
                "instance of StructureType, but found "
                f"'{type(symbol.datatype).__name__}'."
            )
        datatype = symbol.datatype
        if (not datatype.extends or
                datatype.extends.name.lower() != "kernel_type"):
            raise ParseError(
                "GOcean kernel metadata must extend kernel_type."
            )

        components = datatype.components
        missing = {
            value
            for value in ("meta_args", "iterates_over", "index_offset")
            if value not in components
        }
        if missing:
            raise ParseError(
                f"Missing GOcean metadata component(s): {sorted(missing)}."
            )

        try:
            meta_args = components["meta_args"]
            if not isinstance(meta_args.initial_value, ArrayConstructor):
                raise ParseError("meta_args must be an array constructor.")
            meta_args_type = meta_args.datatype
            if (not isinstance(meta_args_type, ArrayType) or
                    len(meta_args_type.shape) != 1 or
                    not isinstance(meta_args_type.shape[0],
                                   ArrayType.ArrayBounds) or
                    not isinstance(meta_args_type.shape[0].lower, Literal) or
                    not isinstance(meta_args_type.shape[0].upper, Literal)):
                raise ParseError("meta_args must declare a literal extent.")
            bounds = meta_args_type.shape[0]
            try:
                extent = int(bounds.upper.value) - int(bounds.lower.value) + 1
            except ValueError as err:
                raise ParseError(
                    "meta_args must declare a literal extent."
                ) from err
            if extent != len(meta_args.initial_value.children):
                raise ParseError(
                    f"meta_args has extent {extent} but its constructor "
                    f"contains {len(meta_args.initial_value.children)} "
                    "entries."
                )
            arguments = tuple(
                _parse_meta_arg(node)
                for node in meta_args.initial_value.children
            )

            if not datatype.procedure_components:
                raise ParseError(
                    "GOcean metadata must bind a kernel procedure."
                )
            procedure = next(iter(datatype.procedure_components.values()))
            if procedure.initial_value:
                value = procedure.initial_value
                if isinstance(value, Reference):
                    procedure_name = value.symbol.name.lower()
                elif isinstance(value, Literal):
                    procedure_name = value.value.lower()
                else:
                    raise ParseError(
                        "Expected a metadata name or literal but found "
                        f"'{type(value).__name__}'."
                    )
            else:
                procedure_name = procedure.name

            scalar_values = []
            for component_name in ("iterates_over", "index_offset"):
                value = components[component_name].initial_value
                if isinstance(value, Reference):
                    scalar_values.append(value.symbol.name.lower())
                elif isinstance(value, Literal):
                    scalar_values.append(value.value.lower())
                else:
                    raise ParseError(
                        "Expected a metadata name or literal but found "
                        f"'{type(value).__name__}'."
                    )

            return cls(
                scalar_values[0],
                scalar_values[1],
                arguments,
                procedure_name,
                symbol.name,
            )
        except (TypeError, ValueError) as err:
            raise ParseError(
                f"Invalid GOcean metadata '{symbol.name}': {err}"
            ) from err

    @classmethod
    def create_from_kernel_psyir(
        cls, psyir: Node, name: Optional[str] = None
    ) -> "GOceanKernelMetadata":
        """Extract the unique named GOcean metadata from complete PSyIR.

        :param psyir: the complete PSyIR containing the kernel.
        :param name: optional name of the metadata type to extract.

        :returns: the extracted GOcean kernel metadata.

        :raises TypeError: if ``psyir`` is not a PSyIR tree.
        :raises ParseError: if the metadata or implementation is not found.
        """
        if not isinstance(psyir, Node):
            raise TypeError(
                f"Expected PSyIR Node but found '{type(psyir).__name__}'."
            )
        candidates = []
        for container in psyir.walk(Container):
            if isinstance(container, FileContainer):
                continue
            for symbol in container.symbol_table.symbols:
                if (
                    isinstance(symbol, DataTypeSymbol)
                    and isinstance(symbol.datatype, StructureType)
                    and symbol.datatype.extends
                    and symbol.datatype.extends.name.lower() == "kernel_type"
                    and (name is None or symbol.name.lower() == name.lower())
                ):
                    candidates.append((container, symbol))
        if not candidates:
            description = f" '{name}'" if name else ""
            raise ParseError(
                f"GOcean kernel metadata{description} does not exist in "
                "PSyIR."
            )
        if len(candidates) != 1:
            raise ParseError("GOcean kernel metadata is not unique in PSyIR.")
        container, symbol = candidates[0]
        metadata = cls.create_from_psyir(symbol)
        routines = [
            routine
            for routine in container.walk(Routine)
            if routine.name.lower() == metadata.procedure_name
        ]
        if not routines:
            raise ParseError(
                f"Kernel subroutine '{metadata.procedure_name}' not found."
            )
        return cls(
            metadata.iterates_over,
            metadata.index_offset,
            metadata.meta_args,
            metadata.procedure_name,
            metadata.name,
            psyir,
        )

    @classmethod
    def create_from_fortran_string(
        cls, source: str
    ) -> "GOceanKernelMetadata":
        """Create metadata by first translating its declaration to PSyIR.

        :param source: the Fortran metadata declaration.

        :returns: the parsed GOcean kernel metadata.

        :raises TypeError: if ``source`` is not a string.
        :raises ValueError: if the source cannot be translated to PSyIR.
        :raises ParseError: if it does not contain exactly one declaration.
        """
        if not isinstance(source, str):
            raise TypeError("GOcean metadata source must be a string.")
        wrapped = f"module metadata_mod\n{source}\nend module metadata_mod\n"
        try:
            psyir = FortranReader().psyir_from_source(wrapped)
        except Exception as err:
            raise ValueError(
                "Expected kernel metadata to be a Fortran derived type, but "
                f"found '{source}'."
            ) from err
        symbols = [
            symbol
            for container in psyir.walk(Container)
            if not isinstance(container, FileContainer)
            for symbol in container.symbol_table.symbols
            if isinstance(symbol, DataTypeSymbol)
            and isinstance(symbol.datatype, StructureType)
            and symbol.datatype.extends
            and symbol.datatype.extends.name.lower() == "kernel_type"
        ]
        if len(symbols) != 1:
            raise ParseError(
                "Expected exactly one GOcean kernel metadata declaration."
            )
        return cls.create_from_psyir(symbols[0])

    @property
    def _ast(self) -> Optional[Node]:
        """
        :returns: the language-level PSyIR retained by the parser, if any.
        """
        return self.psyir

    @property
    def procedure(self) -> GOceanKernelProcedure:
        """
        :returns: the kernel procedure information.
        """
        implementation = None
        if self.psyir is not None:
            matches = [
                routine
                for routine in self.psyir.walk(Routine)
                if routine.name.lower() == self.procedure_name
            ]
            implementation = matches[0] if matches else None
        return GOceanKernelProcedure(self.procedure_name, implementation)

    @property
    def arg_descriptors(self) -> tuple[GOceanArgDescriptor, ...]:
        """
        :returns: a descriptor for each metadata argument.
        """
        const = GOceanConstants()
        descriptors = []
        for index, argument in enumerate(self.meta_args):
            if isinstance(argument, GOceanFieldArgMetadata):
                descriptors.append(
                    GOceanArgDescriptor(
                        const.ACCESS_MAPPING[argument.access],
                        argument.grid_point_type,
                        index,
                        argument.stencil,
                        "field",
                    )
                )
            elif isinstance(argument, GOceanScalarArgMetadata):
                descriptors.append(
                    GOceanArgDescriptor(
                        const.ACCESS_MAPPING[argument.access],
                        argument.datatype,
                        index,
                        GOceanStencilMetadata(),
                        "scalar",
                    )
                )
            else:
                descriptors.append(
                    GOceanArgDescriptor(
                        const.ACCESS_MAPPING[argument.access],
                        "",
                        index,
                        GOceanStencilMetadata(),
                        "grid_property",
                        argument.name,
                    )
                )
        return tuple(descriptors)

    @property
    def nargs(self) -> int:
        """
        :returns: the number of arguments supplied by the algorithm layer.
        """
        return sum(
            not isinstance(argument, GOceanGridPropertyArgMetadata)
            for argument in self.meta_args
        )

    def fortran_string(self) -> str:
        """
        :returns: this metadata as a Fortran derived-type declaration.
        """
        arguments = ", &\n".join(
            f"    {argument.fortran_string()}"
            for argument in self.meta_args
        )
        return (
            f"TYPE, EXTENDS(kernel_type) :: {self.name}\n"
            f"  TYPE(go_arg), DIMENSION({len(self.meta_args)}) :: "
            f"meta_args = (/ &\n{arguments}/)\n"
            f"  INTEGER :: ITERATES_OVER = {self.iterates_over}\n"
            f"  INTEGER :: INDEX_OFFSET = {self.index_offset}\n"
            "  CONTAINS\n"
            f"    PROCEDURE, NOPASS :: code => {self.procedure_name}\n"
            f"END TYPE {self.name}\n"
        )

    def lower_to_psyir(self) -> DataTypeSymbol:
        """
        :returns: the language-level PSyIR symbol for this metadata.
        """
        source = (f"module metadata_mod\n{self.fortran_string()}"
                  "end module metadata_mod\n")
        container = next(
            node for node in FortranReader().psyir_from_source(source).walk(
                Container) if not isinstance(node, FileContainer))
        return container.symbol_table.lookup(self.name)

    def __str__(self) -> str:
        """
        :returns: a concise description of this kernel metadata.
        """
        return (
            f"GOcean kernel {self.name}, index-offset = "
            f"{self.index_offset}, iterates-over = {self.iterates_over}"
        )


def _parse_meta_arg(
    node: Node,
) -> (
    GOceanFieldArgMetadata
    | GOceanScalarArgMetadata
    | GOceanGridPropertyArgMetadata
):
    """Convert one go_arg PSyIR constructor to typed metadata.

    :param node: the ``go_arg`` constructor to convert.

    :returns: the typed metadata for the argument.

    :raises ParseError: if the constructor structure is invalid.
    :raises ValueError: if a field access form is invalid.
    """
    if not isinstance(node, Call):
        raise ParseError(
            "Expected a metadata constructor but found "
            f"'{type(node).__name__}'."
        )
    if node.routine.symbol.name.lower() != "go_arg":
        raise ParseError(
            "Each meta_args entry must use the go_arg constructor."
        )
    arguments = tuple(node.arguments)
    def value_name(value: Node) -> str:
        """Return and validate a scalar metadata value."""
        if isinstance(value, Reference):
            return value.symbol.name.lower()
        if isinstance(value, Literal):
            return value.value.lower()
        raise ParseError(
            "Expected a metadata name or literal but found "
            f"'{type(value).__name__}'."
        )

    if len(arguments) not in (2, 3):
        raise ParseError(
            "Each go_arg constructor must contain two or three arguments "
            f"but found {len(arguments)}."
        )
    access = value_name(arguments[0])
    second = value_name(arguments[1])
    if len(arguments) == 2:
        return GOceanGridPropertyArgMetadata(access, second)
    const = GOceanConstants()
    if second in const.VALID_FIELD_GRID_TYPES:
        form = arguments[2]
        if isinstance(form, Call):
            if form.routine.symbol.name.lower() != const.VALID_STENCIL_NAME:
                raise ParseError(
                    "A field metadata call must use go_stencil."
                )
            rows = tuple(value_name(value) for value in form.arguments)
            stencil = GOceanStencilMetadata(rows)
        else:
            value = value_name(form)
            if value not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"Expected field access form to be one of "
                    f"{const.VALID_STENCIL_NAMES} or "
                    f"{const.VALID_STENCIL_NAME} but found '{value}'."
                )
            stencil = GOceanStencilMetadata()
        return GOceanFieldArgMetadata(access, second, stencil)
    if second in const.VALID_SCALAR_TYPES:
        return GOceanScalarArgMetadata(
            access, second, value_name(arguments[2]))
    raise ParseError(
        "Expected the second go_arg entry to identify a field or scalar, "
        f"but found '{second}'."
    )
