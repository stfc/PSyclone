# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""
The GOcean kernel metadata is stored with the following immutable nested
dataclasses:

.. code-block:: text

    GOceanKernelMetadata:
        iterates_over: str
        index_offset: str
        meta_args: tuple[Union[
            GOceanFieldArgMetadata:
                access: str
                grid_point_type: str
                stencil: Union[
                    GOceanPointwiseMetadata,
                    GOceanStencilMetadata:
                        rows: tuple[str, str, str]
                ]
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

"""

from __future__ import annotations

from dataclasses import dataclass, field
import re
from typing import ClassVar, Optional, TYPE_CHECKING, Union

from psyclone.configuration import Config
from psyclone.domain.common.kernel.metadata import (
    KernelMetadata, normalise)
from psyclone.domain.gocean.gocean_constants import GOceanConstants
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ArrayConstructor, Call, Node, Routine)
from psyclone.psyir.symbols import DataTypeSymbol

if TYPE_CHECKING:
    from psyclone.core import AccessType
    from psyclone.psyir.nodes import Container


@dataclass(frozen=True)
class GOceanPointwiseMetadata:
    """Metadata for a pointwise GOcean field access."""

    form: ClassVar[str] = "go_pointwise"

    def fortran_string(self) -> str:
        """:returns: this access form as Fortran metadata."""
        return self.form


@dataclass(frozen=True)
class GOceanStencilMetadata:
    """Metadata for an explicit GOcean stencil access.

    ``rows`` describes the stencil relative to the field element at
    ``(i, j)``. The three strings are ordered from north to south and
    therefore represent offsets in the second array dimension of ``j+1``,
    ``j`` and ``j-1``, respectively. Within each string, the three digits
    are ordered from west to east and represent offsets in the first array
    dimension of ``i-1``, ``i`` and ``i+1``, respectively. For example::

        ("010",
         "212",
         "010")

    specifies stencil accesses of depth one to the north and south and
    depth two to the west and east. For an off-centre position, a digit
    gives the maximum access depth in that direction: ``0`` means no
    access, ``1`` means an access at depth one, and so on up to depth nine.
    A depth of two therefore represents accesses at both depths one and
    two in that direction.

    The central digit represents the local field element at ``(i, j)``.
    It may be either ``0`` or ``1`` but is currently ignored by PSyclone.
    A field with no stencil access is represented by
    :class:`GOceanPointwiseMetadata` instead of by zero-valued stencil
    rows.

    :param rows: the north, centre and south rows of the stencil.
    """

    rows: tuple[str, str, str]
    form: ClassVar[str] = "go_stencil"

    def __post_init__(self) -> None:
        """Validate the stencil rows.

        :raises TypeError: if any stencil row is not a string.
        :raises ValueError: if the stencil has an invalid shape or value.
        """
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

    def fortran_string(self) -> str:
        """:returns: this stencil as Fortran metadata."""
        return f"{self.form}({', '.join(self.rows)})"


@dataclass(frozen=True)
class GOceanFieldArgMetadata:
    """GOcean field argument."""

    access: str
    grid_point_type: str
    stencil: GOceanPointwiseMetadata | GOceanStencilMetadata = field(
        default_factory=GOceanPointwiseMetadata
    )

    def __post_init__(self) -> None:
        """Validate and normalise the field-argument metadata.

        :raises TypeError: if any value has the wrong type.
        :raises ValueError: if any value is outside the valid vocabulary.
        """
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            normalise(self.access,  "field access", const.VALID_ACCESS_TYPES))
        object.__setattr__(
            self, "grid_point_type",
            normalise(
                self.grid_point_type, "field grid-point type",
                const.VALID_FIELD_GRID_TYPES))
        if not isinstance(
                self.stencil,
                (GOceanPointwiseMetadata, GOceanStencilMetadata)):
            raise TypeError(
                "Field stencil must be a GOceanPointwiseMetadata or "
                "GOceanStencilMetadata object.")

    @property
    def form(self) -> str:
        """
        :returns: the 'form' metadata field.
        """
        return self.stencil.form

    @property
    def access_type(self) -> AccessType:
        """:returns: the generic access type for this argument."""
        return GOceanConstants().ACCESS_MAPPING[self.access]

    def fortran_string(self) -> str:
        """
        :returns: this argument as a Fortran metadata string.
        """
        return (
            f"go_arg({self.access}, {self.grid_point_type}, "
            f"{self.stencil.fortran_string()})"
        )


@dataclass(frozen=True)
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
            normalise(self.access, "scalar access", const.VALID_ACCESS_TYPES))
        object.__setattr__(
            self, "datatype",
            normalise(
                self.datatype, "scalar datatype", const.VALID_SCALAR_TYPES))
        object.__setattr__(
            self, "form",
            normalise(
                self.form,  "scalar access form", const.VALID_STENCIL_NAMES))

    def fortran_string(self) -> str:
        """
        :returns: this argument as a Fortran metadata string.
        """
        return f"go_arg({self.access}, {self.datatype}, {self.form})"

    @property
    def access_type(self) -> AccessType:
        """:returns: the generic access type for this argument."""
        return GOceanConstants().ACCESS_MAPPING[self.access]


@dataclass(frozen=True)
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
            normalise(
                self.access, "grid-property access",
                const.VALID_ACCESS_TYPES))
        name = normalise(self.name, "grid-property name")
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

    @property
    def access_type(self) -> AccessType:
        """:returns: the generic access type for this argument."""
        return GOceanConstants().ACCESS_MAPPING[self.access]


_META_ARG_TYPES = (
    GOceanFieldArgMetadata,
    GOceanScalarArgMetadata,
    GOceanGridPropertyArgMetadata,
)


GOceanArgumentMetadata = Union[
    GOceanFieldArgMetadata, GOceanScalarArgMetadata,
    GOceanGridPropertyArgMetadata]


@dataclass(frozen=True)
class GOceanKernelMetadata(KernelMetadata):
    """GOcean kernel metadata."""

    iterates_over: str
    index_offset: str
    meta_args: tuple[GOceanArgumentMetadata, ...]
    procedure_name: str
    name: str

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
            normalise(
                self.iterates_over, "iterates_over",
                const.VALID_ITERATES_OVER))
        object.__setattr__(
            self, "index_offset",
            normalise(
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
            normalise(self.procedure_name, "procedure name"))
        object.__setattr__(self, "name", normalise(self.name, "kernel name"))
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
        cls, psyir: DataTypeSymbol
    ) -> "GOceanKernelMetadata":
        """Create metadata from a PSyIR type symbol.

        :param psyir: PSyIR symbol containing the metadata declaration.

        :returns: the parsed GOcean kernel metadata.

        :raises TypeError: if ``psyir`` has an unsupported type.
        :raises TypeError: if its datatype is not a StructureType.
        :raises ParseError: if the metadata declaration is invalid.
        """
        datatype = cls._validate_and_get_structure(psyir, "GOcean")

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
            arguments = tuple(
                _parse_meta_arg(node)
                for node in cls._validate_and_get_array_values(
                    meta_args, "meta_args")
            )

            if not datatype.procedure_components:
                raise ParseError(
                    f"GOcean metadata must bind a kernel procedure but "
                    f"{psyir.name} does not have any procedure declarations."
                )
            procedure = next(iter(datatype.procedure_components.values()))
            procedure_name = (
                cls._validate_and_get_value(procedure.initial_value)
                if procedure.initial_value else procedure.name
            )
            scalar_values = [
                cls._validate_and_get_value(components[name].initial_value)
                for name in ("iterates_over", "index_offset")
            ]

            return cls(
                scalar_values[0],
                scalar_values[1],
                arguments,
                procedure_name,
                psyir.name,
            )
        except (TypeError, ValueError) as err:
            raise ParseError(
                f"Invalid GOcean metadata '{psyir.name}': {err}"
            ) from err

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

    def __str__(self) -> str:
        """
        :returns: a concise description of this kernel metadata.
        """
        return (
            f"GOcean kernel {self.name}, index-offset = "
            f"{self.index_offset}, iterates-over = {self.iterates_over}"
        )

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
        candidates = cls.find_symbols(psyir, name=name)
        if not candidates:
            description = f" '{name}'" if name else ""
            raise ParseError(
                f"GOcean kernel metadata{description} does not exist in "
                "PSyIR."
            )
        if len(candidates) != 1:
            raise ParseError("GOcean kernel metadata is not unique in PSyIR.")
        return candidates[0]

    def resolve_procedures(
            self,
            container: Container,
    ) -> tuple[str, tuple[Routine, ...]]:
        """Resolve this kernel's procedure implementations from its module.

        :param container: the module containing the kernel.

        :returns: the resolved procedure name and tuple of implementations.
        :raises ParseError: if the implementation cannot be resolved.
        """
        routines = [
            routine
            for routine in container.walk(Routine)
            if routine.name.lower() == self.procedure_name
        ]
        if not routines:
            raise ParseError(
                f"Kernel subroutine '{self.procedure_name}' not found."
            )
        return self.procedure_name, tuple(routines)


def _parse_meta_arg(
    node: Node,
) -> GOceanArgumentMetadata:
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

    if len(arguments) not in (2, 3):
        raise ParseError(
            "Each go_arg constructor must contain two or three arguments "
            f"but found {len(arguments)}."
        )
    access = KernelMetadata._validate_and_get_value(arguments[0])
    second = KernelMetadata._validate_and_get_value(arguments[1])
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
            rows = tuple(KernelMetadata._validate_and_get_value(value)
                         for value in form.arguments)
            stencil = GOceanStencilMetadata(rows)
        else:
            value = KernelMetadata._validate_and_get_value(form)
            if value not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"Expected field access form to be one of "
                    f"{const.VALID_STENCIL_NAMES} or "
                    f"{const.VALID_STENCIL_NAME} but found '{value}'."
                )
            stencil = GOceanPointwiseMetadata()
        return GOceanFieldArgMetadata(access, second, stencil)
    if second in const.VALID_SCALAR_TYPES:
        return GOceanScalarArgMetadata(
            access, second,
            KernelMetadata._validate_and_get_value(arguments[2]))
    raise ParseError(
        "Expected the second go_arg entry to identify a field or scalar, "
        f"but found '{second}'."
    )
