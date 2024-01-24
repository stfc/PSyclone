# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic, O. Brunt and L. Turner, Met Office

'''This module provides functionality for the PSyclone kernel extraction
functionality for LFRic. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.domain.lfric import LFRicConstants
from psyclone.errors import InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse import ModuleManager
from psyclone.psyGen import InvokeSchedule, Kern
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Assignment, Call, FileContainer,
                                  IntrinsicCall, Literal, Reference,
                                  Routine, StructureReference)
from psyclone.psyir.symbols import (ArrayType, CHARACTER_TYPE,
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, UnresolvedType,
                                    ImportInterface, INTEGER_TYPE,
                                    RoutineSymbol, UnsupportedFortranType)
from psyclone.psyir.transformations import ExtractTrans


class LFRicExtractDriverCreator:
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    The driver is created as follows:

    1. The corresponding :py:class:`psyclone.psyGen.Invoke` statement that
       contains the kernel(s) is copied. This way we avoid affecting the tree
       of the caller. We need the invoke since it contains the symbol table.
    2. We remove all halo exchange nodes. For now, the extract transformation
       will not work when distributed memory is enabled, but since this
       restriction is expected to be lifted, the code to handle this is
       already added.
    3. We lower each kernel (child of the invoke) that was requested to
       be extracted, all others are removed. This is required since the kernel
       extraction will not contain the required data for the other kernels to
       be called. The lowering is important to fix the variable names for the
       loop boundaries of the :py:class:`psyclone.domain.lfric.LFRicLoop`: the
       loop start/stop expressions (`loop0_start` etc.) depend on the position
       of the loop in the tree. For example, if there are two kernels, they
       will be using `loop0_start` and `loop1_start`. If only the second is
       extracted, the former second (and now only) loop would be using
       `loop0_start` without lowering, but the kernel extraction would have
       written the values for `loop1_start`.
    4. We create a program for the driver with a new symbol table and start
       adding symbols for the program unit, precision symbols, PSyData read
       module etc to it.
    5. We add all required symbols to the new symbol table. The copied tree
       will still rely on the symbol table in the original PSyIR, so the
       symbols must be declared in the symbol table of the driver program.
       This is done by replacing all references in the extracted region with
       new references, which use new symbols which are declared in the driver
       symbol table.

       a. We first handle all non user-defined type. We can be certain that
          these symbols are already unique (since it's the original kernel
          code).
       b. Then we handle user-defined types. Since we only use basic Fortran
          types, accesses to these types need to be 'flattened': an access
          like ``a%b%c`` will be flattened to ``a_b_c`` to create a valid
          symbol name without needing the user-defined type. We use the
          original access string (``a%b%c``) as tag, since we know this tag
          is unique, and create a new, unique symbol based on ``a_b_c``. This
          takes care if the user should be using this newly generated name
          (e.g. if the user uses ``a%b%c`` and ``a_b_c``, ``a_b_c`` as non
          user defined symbol will be added to the symbol table first. When
          then ``a%b%c`` is flattened, the symbol table will detect that the
          symbol ``a_b_c`` already exists and create ``a_b_c_1`` for the tag
          ``a%b%c``). For known LFRic types, the actual name used in a
          reference will be changed to the name the user expects. For example,
          if field ``f`` is used, the access will be ``f_proxy%data``. The
          kernel extraction does the same and stores the values under the name
          ``f``, so the driver similarly simplifies the name back to the
          original ``f``.
          The :py:class:`psyclone.domain.lfric.KernCallArgList` class will
          have enforced the appropriate basic Fortran type declaration for
          each reference to a user defined variable. For example, if a field
          ``f`` is used, the reference to ``f_proxy%data`` will have a data
          type attribute of a 1D real array (with the correct precision).

    6. We create the code for reading in all of the variables in the input-
       and output-lists. Mostly, no special handling of argument type is
       required (since the generic interface will make sure to call the
       appropriate function). But in case of user-defined types, we need to
       use the original names with '%' when calling the functions for reading
       in data, since this is the name that was used when creating the data
       file. For example, the name of a parameter like
       ``f_proxy%local_stencil`` will be stored in the data file with the
       '%' notation (which is also the tag used for the symbol). So when
       reading the values in the driver, we need to use the original name
       (or tag) with '%', but the values will be stored in a flattened
       variable. For example, the code created might be:
       `call extract_psy_data%ReadVariable('f_proxy%local_stencil',
       fproxy_local_stencil)`

       a. Input variables are read in using functions from the PSyData
          ``ReadKernelData`` module. These function will allocate all array
          variables to the right size based on the data from the input file.
       b. For parameters that are read and written, two variables will be
          declared: the input will be stored in the unmodified variable name,
          and the output values in a variable with ``_post`` appended. For
          example, a field ``f`` as input will be read into ``f`` using the
          name ``f``, and output values will be read into ``f_post`` using
          the name ``f_post``. The symbol table will make sure that the
          ``_post`` name is unique.
       c. Similar to b., output only parameters will be read into a variable
          named with '_post' attached, e.g. output field ``f`` will be stored
          in a variable ``f_post``. Then the array ``f`` is allocated based on
          the shape of ``f_post`` and initialised to 0 (since it's an
          output-only parameter the value doesn't really matter).

    7. The extracted kernels are added to the program. Since in step 5 all
       references have been replaced, the created code will use the correct
       new variable names (which just have been read in). The output variables
       with ``_post`` attached will not be used at all so far.
    8. After the kernel calls are executed, each output variable is compared
       with the value stored in the corresponding ``_post`` variable. For
       example, a variable ``f`` which was modified in the kernel call(s),
       will then be compared with ``f_post``.

    :param precision: a mapping of the various precisions used in LFRic to \
        the actual Fortran data type to be used in a stand-alone driver.
    :type precision: Optional[Dict[str, str]]

    :raises InternalError: if the precision argument is specified but \
        is not a dictionary.

    '''
    def __init__(self):
        # TODO #2069: check if this list can be taken from LFRicConstants
        self._all_field_types = ["integer_field_type", "field_type",
                                 "r_bl_field", "r_phys_field",
                                 "r_solver_field_type", "r_tran_field_type"]

    # -------------------------------------------------------------------------
    @staticmethod
    def _make_valid_unit_name(name):
        '''Valid program or routine names are restricted to 63 characters,
        and no special characters like ':'.

        :param str name: a proposed unit name.

        :returns: a valid program or routine  name with special characters \
            removed and restricted to a length of 63 characters.
        :rtype: str

        '''
        return name.replace(":", "")[:63]

    # -------------------------------------------------------------------------
    def _get_proxy_name_mapping(self, schedule):
        '''This function creates a mapping of each proxy name of an argument
        to the field map. This mapping is used to convert proxy names used
        in a lowered kernel call back to the original name, which is the name
        used in extraction. For example, a field 'f' will be provided as
        ``f_proxy%data`` to the kernel, but the extraction will just write
        the name 'f', which is easier to understand for the user. The mapping
        created here is used as a first step, to convert ``f_proxy`` back
        to ``f``.

        :param schedule: the schedule with all kernels.
        :type schedule: :py:class:`psyclone.psyir.nodes.Schedule`

        :returns: a mapping of proxy names to field names.
        :rtype: Dict[str,str]

        '''
        proxy_name_mapping = {}
        for kern in schedule.walk(Kern):
            for arg in kern.args:
                if arg.data_type in self._all_field_types:
                    proxy_name_mapping[arg.proxy_name] = arg.name
        return proxy_name_mapping

    # -------------------------------------------------------------------------
    @staticmethod
    def _flatten_signature(signature):
        '''Creates a 'flattened' string for a signature by using ``_`` to
        separate the parts of a signature. For example, in Fortran
        a reference to ``a%b`` would be flattened to be ``a_b``.

        :param signature: the signature to be flattened.
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: a flattened string (all '%' replaced with '_'.)
        :rtype: str

        '''
        return str(signature).replace("%", "_")

    # -------------------------------------------------------------------------
    def _flatten_reference(self, old_reference, symbol_table,
                           proxy_name_mapping):
        '''Replaces ``old_reference``, which is a structure type, with a new
        simple Reference and a flattened name (replacing all % with _). It will
        also remove a '_proxy' in the name, so that the program uses the names
        the user is familiar with, and which are also used in the extraction
        driver.

        :param old_reference: a reference to a structure member.
        :type old_reference: \
            :py:class:`psyclone.psyir.nodes.StructureReference`
        :param symbol_table: the symbol table to which to add the newly \
            defined flattened symbol.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param proxy_name_mapping: a mapping of proxy names to the original \
            names.
        :type proxy_name_mapping: Dict[str,str]

        :raises InternalError: if the old_reference is not a \
            :py:class:`psyclone.psyir.nodes.StructureReference`
        :raises GenerationError: if an array of structures is used

        '''

        if not isinstance(old_reference, StructureReference):
            raise InternalError(f"Unexpected type "
                                f"'{type(old_reference).__name__}'"
                                f" in _flatten_reference, it must be a "
                                f"'StructureReference'.")
        # A field access (`fld%data`) will get the `%data` removed, since then
        # this avoids a potential name clash (`fld` is guaranteed to
        # be unique, since it's a variable already, but `fld_data` could clash
        # with a user variable if the user uses `fld` and `fld_data`).
        # Furthermore, the NetCDF file declares the variable without `%data`,
        # so removing `%data` here also simplifies code creation later on.

        signature, _ = old_reference.get_signature_and_indices()
        # Now remove '_proxy' that might have been added to a variable name,
        # to preserve the expected names from a user's point of view.
        symbol_name = proxy_name_mapping.get(signature[0], signature[0])

        # Other types need to get the member added to the name,
        # to make unique symbols (e.g. 'op_a_proxy%ncell_3d').
        signature = Signature(symbol_name, signature[1:])

        # We use this string as a unique tag - it must be unique since no
        # other tag uses a '%' in the name. So even if the flattened name
        # (e.g. f1_data) is not unique, the tag `f1%data` is unique, and
        # the symbol table will then create a unique name for this symbol.
        signature_str = str(signature)
        try:
            symbol = symbol_table.lookup_with_tag(signature_str)
        except KeyError:
            flattened_name = self._flatten_signature(signature)
            symbol = DataSymbol(flattened_name, old_reference.datatype)
            symbol_table.add(symbol, tag=signature_str)

        new_ref = Reference(symbol)
        old_reference.replace_with(new_ref)

    # -------------------------------------------------------------------------
    def _add_all_kernel_symbols(self, sched, symbol_table, proxy_name_mapping):
        '''This function adds all symbols used in ``sched`` to the symbol
        table. It uses LFRic-specific knowledge to declare fields and flatten
        their name.

        :param sched: the schedule that will be called by this driver program.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param symbol_table: the symbol table to which to add all found \
            symbols.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param proxy_name_mapping: a mapping of proxy names to the original \
            names.
        :type proxy_name_mapping: Dict[str,str]


        '''
        all_references = sched.walk(Reference)

        # First we add all non-structure names to the symbol table. This way
        # the flattened name can be ensured not to clash with a variable name
        # used in the program.
        for reference in all_references:
            # For now ignore structure names, which require flattening (which
            # could introduce duplicated symbols, so they need to be processed
            # after all existing symbols have been added.
            if isinstance(reference, StructureReference):
                continue
            old_symbol = reference.symbol
            if old_symbol.name in symbol_table:
                # The symbol has already been declared. We then still
                # replace the old symbol with the new symbol to have all
                # symbols consistent (otherwise if we would for whatever
                # reason modify a symbol in the driver's symbol table, only
                # some references would use the new values, the others
                # would be the symbol from the original kernel for which
                # the driver is being created).
                reference.symbol = symbol_table.lookup(old_symbol.name)
                continue

            # Now we have a reference with a symbol that is in the old symbol
            # table (i.e. not in the one of the driver). Create a new symbol
            # (with the same name) in the driver's symbol table), and use
            # it in the reference.
            datatype = old_symbol.datatype
            if isinstance(datatype, UnsupportedFortranType):
                # Currently fields are of UnsupportedFortranType because they
                # are pointers in the PSy layer. Here we just want the base
                # type (i.e. not a pointer).
                datatype = old_symbol.datatype.partial_datatype

            new_symbol = symbol_table.new_symbol(root_name=reference.name,
                                                 tag=reference.name,
                                                 symbol_type=DataSymbol,
                                                 datatype=datatype)
            reference.symbol = new_symbol

        # Now handle all derived type. The name of a derived type is
        # 'flattened', i.e. all '%' are replaced with '_', and this is then
        # declared as a non-structured type. We also need to make sure that a
        # flattened name does not clash with a variable declared by the user.
        # We use the structured name (with '%') as tag to handle this.
        for reference in all_references:
            if not isinstance(reference, StructureReference):
                continue
            self._flatten_reference(reference, symbol_table,
                                    proxy_name_mapping)

    # -------------------------------------------------------------------------
    @staticmethod
    def _add_call(program, name, args):
        '''This function creates a call to the subroutine of the given name,
        providing the arguments. The call will be added to the program and
        to the symbol table.

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param str name: name of the subroutine to call.
        :param args: all arguments for the call.
        :type args: List[:py:class:`psyclone.psyir.nodes.Node`]

        :raises TypeError: if there is a symbol with the \
            specified name defined that is not a RoutineSymbol.
        '''
        if name in program.symbol_table:
            routine_symbol = program.symbol_table.lookup(name)
            if not isinstance(routine_symbol, RoutineSymbol):
                raise TypeError(
                    f"Error when adding call: Routine '{name}' is "
                    f"a symbol of type '{type(routine_symbol).__name__}', "
                    f"not a 'RoutineSymbol'.")
        else:
            routine_symbol = RoutineSymbol(name)
            program.symbol_table.add(routine_symbol)
        call = Call.create(routine_symbol, args)
        program.addchild(call)

    # -------------------------------------------------------------------------
    @staticmethod
    def _create_output_var_code(name, program, is_input, read_var,
                                postfix, index=None):
        # pylint: disable=too-many-arguments
        '''
        This function creates all code required for an output variable.
        It creates the '_post' variable which stores the correct result
        from the file, which is read in. If the variable is not also an
        input variable, the variable itself will be declared (based on
        the size of the _post variable) and initialised to 0.
        This function also handles array of fields, which need to get
        an index number added.

        :param str name: the name of original variable (i.e.
            without _post), which will be looked up as a tag in the symbol
            table. If index is provided, it is incorporated in the tag using
            f"{name}_{index}_data".
        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param bool is_input: True if this variable is also an input
            parameter.
        :param str read_var: the readvar method to be used including the
            name of the PSyData object (e.g. 'psy_data%ReadVar')
        :param str postfix: the postfix to use for the expected output
            values, which are read from the file.
        :param index: if present, the index to the component of a field vector.
        :type index: Optional[int]

        :returns: a 2-tuple containing the output Symbol after the kernel,
             and the expected output read from the file.
        :rtype: Tuple[:py:class:`psyclone.psyir.symbols.Symbol`,
                      :py:class:`psyclone.psyir.symbols.Symbol`]

        '''
        symbol_table = program.symbol_table
        if index is not None:
            sym = symbol_table.lookup_with_tag(f"{name}_{index}_data")
        else:
            # If it is not indexed then `name` will already end in "_data"
            sym = symbol_table.lookup_with_tag(name)

        # Declare a 'post' variable of the same type and read in its value.
        post_name = sym.name + postfix
        post_sym = symbol_table.new_symbol(post_name,
                                           symbol_type=DataSymbol,
                                           datatype=sym.datatype)
        if index is not None:
            post_tag = f"{name}{postfix}%{index}"
        else:
            # If it is not indexed then `name` will already end in "_data"
            post_tag = f"{name}{postfix}"
        name_lit = Literal(post_tag, CHARACTER_TYPE)
        LFRicExtractDriverCreator._add_call(program, read_var,
                                            [name_lit,
                                             Reference(post_sym)])

        # Now if a variable is written to, but not read, the variable
        # is not allocated. So we need to allocate it and set it to 0.
        if not is_input:
            if (isinstance(post_sym.datatype, ArrayType) or
                    (isinstance(post_sym.datatype, UnsupportedFortranType) and
                     isinstance(post_sym.datatype.partial_datatype,
                                ArrayType))):
                alloc = IntrinsicCall.create(
                    IntrinsicCall.Intrinsic.ALLOCATE,
                    [Reference(sym), ("mold", Reference(post_sym))])
                program.addchild(alloc)
            set_zero = Assignment.create(Reference(sym),
                                         Literal("0", INTEGER_TYPE))
            program.addchild(set_zero)
        return (sym, post_sym)

    # -------------------------------------------------------------------------
    def _create_read_in_code(self, program, psy_data, original_symbol_table,
                             read_write_info, postfix):
        # pylint: disable=too-many-arguments, too-many-locals
        '''This function creates the code that reads in the NetCDF file
        produced during extraction. For each:

        - variable that is read-only, it will declare the symbol and add code
          that reads in the variable using the PSyData library.
        - variable that is read and written, it will create code to read in the
          variable that is read, and create a new variable with the same name
          and "_post" added which is read in to store the values from the
          NetCDF file after the instrumented region was executed. In the end,
          the variable that was read and written should have the same value
          as the corresponding "_post" variable.
        - variable that is written only, it will create a variable with "_post"
          as postfix that reads in the output data from the NetCDF file. It
          then also declares a variable without postfix (which will be the
          parameter to the function), allocates it based on the shape of
          the corresponding "_post" variable, and initialises it with 0.

        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data: the PSyData symbol to be used.
        :type psy_data: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str postfix: a postfix that is added to a variable name to
            create the corresponding variable that stores the output
            value from the kernel data file.

        :returns: all output parameters, i.e. variables that need to be
            verified after executing the kernel. Each entry is a 2-tuple
            containing the symbol of the computed variable, and the symbol
            of the variable that contains the value read from the file.
        :rtype: List[Tuple[:py:class:`psyclone.psyir.symbols.Symbol`,
                           :py:class:`psyclone.psyir.symbols.Symbol`]]

        '''
        def _sym_is_field(sym):
            '''Utility that determines whether the supplied Symbol represents
            an LFRic field.

            :param sym: the Symbol to check.
            :type sym: :py:class:`psyclone.psyir.symbols.TypedSymbol`

            :returns: True if the Symbol represents a field, False otherwise.
            :rtype: bool

            '''
            if isinstance(orig_sym.datatype, UnsupportedFortranType):
                intrinsic_name = sym.datatype.partial_datatype.intrinsic.name
            else:
                intrinsic_name = sym.datatype.intrinsic.name
            return intrinsic_name in self._all_field_types

        symbol_table = program.scope.symbol_table
        read_var = f"{psy_data.name}%ReadVariable"

        # First handle variables that are read:
        # -------------------------------------
        for signature in read_write_info.signatures_read:
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables have References, and will already have been declared
            # in the symbol table (in _add_all_kernel_symbols).
            sig_str = self._flatten_signature(signature)
            orig_sym = original_symbol_table.lookup(signature[0])
            if orig_sym.is_array and _sym_is_field(orig_sym):
                # This is a field vector, so add all individual fields
                upper = int(orig_sym.datatype.shape[0].upper.value)
                for i in range(1, upper+1):
                    sym = symbol_table.lookup_with_tag(f"{sig_str}_{i}_data")
                    name_lit = Literal(f"{sig_str}%{i}", CHARACTER_TYPE)
                    self._add_call(program, read_var, [name_lit,
                                                       Reference(sym)])
                continue

            sym = symbol_table.lookup_with_tag(str(signature))
            name_lit = Literal(str(signature), CHARACTER_TYPE)
            self._add_call(program, read_var, [name_lit, Reference(sym)])

        # Then handle all variables that are written (note that some
        # variables might be read and written)
        # ----------------------------------------------------------
        # Collect all output symbols to later create the tests for
        # correctness. This list stores 2-tuples: first one the
        # variable that stores the output from the kernel, the second
        # one the variable that stores the output values read from the
        # file. The content of these two variables should be identical
        # at the end.
        output_symbols = []
        for signature in read_write_info.signatures_written:
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables have References, and will already have been declared
            # in the symbol table (in _add_all_kernel_symbols).
            orig_sym = original_symbol_table.lookup(signature[0])
            is_input = read_write_info.is_read(signature)
            if orig_sym.is_array and _sym_is_field(orig_sym):
                # This is a field vector, so handle each individual field
                # adding a number
                flattened = self. _flatten_signature(signature)
                upper = int(orig_sym.datatype.shape[0].upper.value)
                for i in range(1, upper+1):
                    sym_tuple = \
                        self._create_output_var_code(flattened, program,
                                                     is_input, read_var,
                                                     postfix, index=i)
                    output_symbols.append(sym_tuple)
            else:
                sig_str = str(signature)
                sym_tuple = self._create_output_var_code(str(signature),
                                                         program, is_input,
                                                         read_var, postfix)
                output_symbols.append(sym_tuple)

        return output_symbols

    # -------------------------------------------------------------------------
    @staticmethod
    def _import_modules(symbol_table, sched):
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the schedule and
        checks for calls with an ImportInterface. Any such call will
        add a ContainerSymbol for the module and a RoutineSymbol (pointing
        to the container) to the symbol table.

        :param symbol_table: the symbol table to which the symbols are added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param sched: the schedule to analyse for module imports.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        for call in sched.walk(Call):
            routine = call.routine
            if not isinstance(routine.interface, ImportInterface):
                # No import required, can be ignored.
                continue
            if routine.name in symbol_table:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (the PSyIR backend will then create a suitable import
            # statement).
            module = ContainerSymbol(routine.interface.container_symbol.name)
            symbol_table.add(module)
            new_routine_sym = RoutineSymbol(routine.name, UnresolvedType(),
                                            interface=ImportInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    @staticmethod
    def _add_precision_symbols(symbol_table):
        '''This function adds an import of the various precision
        symbols used by LFRic from the constants_mod module.

        :param symbol_table: the symbol table to which the precision symbols \
            must be added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        const = LFRicConstants()
        mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
        constant_mod = ContainerSymbol(mod_name)
        symbol_table.add(constant_mod)
        # r_quad is defined in constants_mod, but not exported. So
        # we have to remove it from the lists of precisions to import.
        # TODO #2018
        api_config = Config.get().api_conf("dynamo0.3")
        all_precisions = [name for name in api_config.precision_map
                          if name != "r_quad"]
        for prec_name in all_precisions:
            symbol_table.new_symbol(prec_name,
                                    symbol_type=DataSymbol,
                                    datatype=INTEGER_TYPE,
                                    interface=ImportInterface(constant_mod))

    # -------------------------------------------------------------------------
    @staticmethod
    def _add_result_tests(program, output_symbols):
        '''Adds tests to check that all output variables have the expected
        value.

        :param program: the program to which the tests should be added.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param output_symbols: a list containing all output variables of \
            the executed code. Each entry in the list is a 2-tuple, \
            containing first the symbol that was computed when executing \
            the kernels, and then the symbol containing the expected \
            values that have been read in from a file.
        :type output_symbols: \
            List[Tuple[:py:class:`psyclone.psyir.symbols.Symbol`
                       :py:class:`psyclone.psyir.symbols.Symbol`]]

        '''
        # TODO #2083: check if this can be combined with psyad result
        # comparison.
        for (sym_computed, sym_read) in output_symbols:
            if (isinstance(sym_computed.datatype, ArrayType) or
                    (isinstance(sym_computed.datatype, UnsupportedFortranType)
                     and isinstance(sym_computed.datatype.partial_datatype,
                                    ArrayType))):
                cond = f"all({sym_computed.name} - {sym_read.name} == 0.0)"
            else:
                cond = f"{sym_computed.name} == {sym_read.name}"
            # The PSyIR has no support for output functions, so we parse
            # Fortran code to create a code block which stores the output
            # statements.
            code = f'''
                subroutine tmp()
                  integer :: {sym_computed.name}, {sym_read.name}
                  if ({cond}) then
                     print *,"{sym_computed.name} correct"
                  else
                     print *,"{sym_computed.name} incorrect. Values are:"
                     print *,{sym_computed.name}
                     print *,"{sym_computed.name} values should be:"
                     print *,{sym_read.name}
                  endif
                end subroutine tmp'''

            fortran_reader = FortranReader()
            container = fortran_reader.psyir_from_source(code)
            if_block = container.children[0].children[0]
            program.addchild(if_block.detach())

    # -------------------------------------------------------------------------
    def create(self, nodes, read_write_info, prefix, postfix, region_name):
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. The `nodes` are
        consecutive nodes from the PSyIR tree.
        It returns the file container which contains the driver.

        :param nodes: a list of nodes.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output \
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols ``extract_psydata``.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.

        :returns: the program PSyIR for a stand-alone driver.
        :rtype: :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        '''
        # pylint: disable=too-many-locals

        # Since this is a 'public' method of an entirely separate class,
        # we check that the list of nodes is what it expects. This is done
        # by invoking the validate function of the basic extract function.
        extract_trans = ExtractTrans()
        # We need to provide the prefix to the validation function:
        extract_trans.validate(nodes, options={"prefix": prefix})

        module_name, local_name = region_name
        unit_name = self._make_valid_unit_name(f"{module_name}_{local_name}")

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        if prefix:
            prefix = prefix + "_"

        psy_data_mod = ContainerSymbol("read_kernel_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol("ReadKernelDataType", UnresolvedType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        # The validation of the extract transform guarantees that all nodes
        # in the node list have the same parent.
        invoke_sched = nodes[0].ancestor(InvokeSchedule)

        # The invoke-schedule might have children that are not in the node
        # list. So get the indices of the nodes for which a driver is to
        # be created, and then remove all other nodes from the copy.This
        # needs to be done before potential halo exchange nodes are removed,
        # to make sure we use the same indices (for e.g. loop boundary
        # names, which are dependent on the index of the nodes in the tree).
        # TODO #1731: this might not be required anymore if the loop
        # boundaries are fixed earlier.
        all_indices = [node.position for node in nodes]

        schedule_copy = invoke_sched.copy()

        # TODO #1992: if required, the following code will
        # remove halo exchange nodes from the driver.
        # halo_nodes = schedule_copy.walk(HaloExchange)
        # for halo_node in halo_nodes:
        #     halo_node.parent.children.remove(halo_node)

        original_symbol_table = invoke_sched.symbol_table
        proxy_name_mapping = self._get_proxy_name_mapping(schedule_copy)

        # Now clean up the try: remove nodes in the copy that are not
        # supposed to be extracted. Any node that should be extract
        # needs to be lowered, which will fix the loop boundaries
        # (TODO: #1731 - that might not be required anymore with 1731).
        # Otherwise, if e.g. the second loop is only extracted, this
        # loop would switch from using loop1_start/stop to loop0_start/stop
        # since it is then the first loop (hence we need to do this
        # backwards to maintain the loop indices). Note that the
        # input/output list will already contain the loop boundaries,
        # so we can't simply change them (also, the original indices
        # will be used when writing the file).
        children = schedule_copy.children[:]
        children.reverse()
        for child in children:
            if child.position not in all_indices:
                child.detach()
            else:
                child.lower_to_language_level()

        # Find all imported routines and add them to the symbol table
        # of the driver, so the driver will have the correct import
        # statements.
        self._import_modules(program.scope.symbol_table, schedule_copy)
        self._add_precision_symbols(program.scope.symbol_table)
        self._add_all_kernel_symbols(schedule_copy, program_symbol_table,
                                     proxy_name_mapping)

        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        # Provide the module and region name to the OpenRead method, which
        # will reconstruct the name of the data file to read.
        module_str = Literal(module_name, CHARACTER_TYPE)
        region_str = Literal(local_name, CHARACTER_TYPE)
        self._add_call(program, f"{psy_data.name}%OpenRead",
                       [module_str, region_str])

        output_symbols = self._create_read_in_code(program, psy_data,
                                                   original_symbol_table,
                                                   read_write_info, postfix)
        # Move the nodes making up the extracted region into the Schedule
        # of the driver program
        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        self._add_result_tests(program, output_symbols)

        return file_container

    # -------------------------------------------------------------------------
    @staticmethod
    def collect_all_required_modules(file_container):
        '''Collects recursively all modules used in the file container.
        It returns a dictionary, with the keys being all the (directly or
        indirectly) used modules.

        :param file_container: the FileContainer for which to collect all \
            used modules.
        :type file_container: \
            :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        :returns: a dictionary, with the required module names as key, and \
            as value a set of all modules required by the key module.
        :rtype: Dict[str, Set[str]]

        '''
        all_mods = set()
        for container in file_container.children:
            sym_tab = container.symbol_table
            # Add all imported modules (i.e. all container symbols)
            all_mods.update(symbol.name for symbol in sym_tab.symbols
                            if isinstance(symbol, ContainerSymbol))

        mod_manager = ModuleManager.get()
        return mod_manager.get_all_dependencies_recursively(all_mods)

    # -------------------------------------------------------------------------
    def get_driver_as_string(self, nodes, read_write_info, prefix, postfix,
                             region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the `create()` function to get the PSyIR of a
        stand-alone driver, and then uses the provided language writer
        to create a string representation in the selected language
        (defaults to Fortran).
        All required modules will be inlined in the correct order, i.e. each
        module will only depend on modules inlined earlier, which will allow
        compilation of the driver. No other dependencies (except system
        dependencies like NetCDF) are required for compilation.

        :param nodes: a list of nodes.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type language_writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :returns: the driver in the selected language.
        :rtype: str

        :raises NotImplementedError: if the driver creation fails.

        '''
        try:
            file_container = self.create(nodes, read_write_info, prefix,
                                         postfix, region_name)
        # TODO #2120 (Handle failures in Kernel Extraction): Now that all
        # built-ins are lowered, an alternative way of triggering a
        # NotImplementedError is needed.
        except NotImplementedError:
            # print(f"Cannot create driver for '{region_name[0]}-"
            #      f"{region_name[1]}' because:")
            # print(str(err))
            return ""

        module_dependencies = self.collect_all_required_modules(file_container)
        # Sort the modules by dependencies, i.e. start with modules
        # that have no dependency. This is required for compilation, the
        # compiler must have found any dependent modules before it can
        # compile a module.
        sorted_modules = ModuleManager.sort_modules(module_dependencies)

        # Inline all required modules into the driver source file so that
        # it is stand-alone:
        out = []
        mod_manager = ModuleManager.get()
        for module in sorted_modules:
            # Note that all modules in `sorted_modules` are known to be in
            # the module manager, so we can always get the module info here.
            mod_info = mod_manager.get_module_info(module)
            out.append(mod_info.get_source_code())

        out.append(writer(file_container))

        return "\n".join(out)

    # -------------------------------------------------------------------------
    def write_driver(self, nodes, read_write_info, prefix, postfix,
                     region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the ``get_driver_as_string()`` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".F90"

        :param nodes: a list of nodes containing the body of the driver
            routine.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output \
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.
        :param writer: a backend visitor to convert PSyIR \
            representation to the selected language. It defaults to \
            the FortranWriter.
        :type writer: \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        '''
        code = self.get_driver_as_string(nodes, read_write_info, prefix,
                                         postfix, region_name, writer=writer)
        fll = FortLineLength()
        code = fll.process(code)
        if not code:
            # This indicates an error that was already printed,
            # so ignore it here.
            # TODO #2120 (Handle failures in Kernel Extraction): revisit
            # how this is handled in 'get_driver_as_string'.
            return
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.F90", "w",
                  encoding='utf-8') as out:
            out.write(code)
