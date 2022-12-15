# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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

'''This module provides functionality for the PSyclone kernel extraction
functionality for LFRic. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

from psyclone.core import Signature
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.psyGen import HaloExchange, InvokeSchedule, Kern
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (ArrayMember, ArrayReference, Assignment,
                                  Call, FileContainer, Literal, Node,
                                  Reference, Routine, StructureReference)
from psyclone.psyir.symbols import (ArrayType, BOOLEAN_TYPE, CHARACTER_TYPE,
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, DeferredType,
                                    ImportInterface, INTEGER_TYPE,
                                    INTEGER8_TYPE, REAL8_TYPE, RoutineSymbol,
                                    ScalarType)
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.transformations import ExtractTrans


class LFRicExtractDriverCreator:
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    The driver is created as follows:
    1. The corresponding Invoke statement that contains the kernel(s) is
       copied. This way we avoid affecting the tree of the caller. We need
       the invoke since it contains the symbol table.
    2. We remove all halo exchange nodes. While atm the extract transformation
       will not work when distributed memory is enabled, but since this
       restriction is expected to be lifted, the code to handle this is
       already added.
    3. We either lower each kernel (child of the invoke) that was requested to
       be extracted, all others are removed. This is required since the kernel
       extraction will not contain the required data for the other kernels to
       be called. The lowering is important to fix the loop boundaries for
       the DynLoop: the loop start/stop expressions (`loop0_start` etc.)
       depend on the position of the loop in the tree. For example, if there
       are two kernels, they will be using `loop0_start` and `loop1_start`. If
       only the second is extracted, the former second (now only) loop would
       be using `loop0_start` without lowering, but the kernel extraction
       would have written the values for `loop1_start`.
    4. We create a program for the driver with a new symbol table and start
       adding symbols for the program unit, precision symbols, PSyData read
       module etc to it.
    5. We add all required symbols to the new symbol table. This is done by
       looping over all references in the extracted region and declaring
       the symbols in the new symbol table. The existing references in the
       tree will be replaced with newly created references to the new symbols.
       a. We first handle all non user-defined type. We can be certain that
          these symbols are already unique (since it's the original kernel
          code).
       b. Then we handle user-defined types. Since we only use basic Fortran
          types, accesses to these types need to be 'flattened': an access
          like `a%b%c` will be flattened to `a_b_c` to create a valid symbol
          name without needing the user-defined type. We use the original
          access string (`a%b%c`) as tag, since we know this tag is unique,
          and create a new, unique symbol based on `a_b_c`. This takes care
          if the user should be using this newly generated name (e.g. if the
          user uses `a%b%c` and `a_b_c`, `a_b_c` as non user defined symbol
          will be added to the symbol table first. When then `a%b%c` is
          flattened, the symbol table will detect that the symbol `a_b_c`
          already exists and create `a_b_c_1` for the tag `a%b%c`).
          For known LFRic types, the actual name used in a reference will
          be changed to the name the user expects. For example, if field
          `f` is used, the access will be `f_proxy%data`. The kernel
          extraction does the same and stores the values under the name `f`,
          so the driver similarly simplifies the name back to the original
          `f`.
          The KernCallArgList class will have enforced the appropriate
          basic Fortran type declaration for each reference to a user defined
          variable. For example, if a field `f` is used, the reference to
          `f_proxy%data` will have a data type attribute of a 1D real array
          (with the correct precision).
    6. We create the read-in code for all variables in the input- and output-
       lists. Mostly, no special handling of argument type is required (since
       the generic interface will make sure to call the appropriate function).
       But in case of user-defined types, we need to make sure to use the
       name with '%' (unless it is a standard LFRic type like field which
       as described above has already been simplified). Example are names like
       `f_proxy%local_stencil` and `f_proxy%ncell_3d`. They will be using the
       name with `%`, but a flattened variable name (`%` replaced with `_`).
       a. Input parameter are read in from the PSyData ReadKernelData module.
          These function will allocate all array variables to the right size
          based on the data from the input file.
       b. For parameters that are read and written, two variables will be
          declared: the input will be stored in the unmodified variable name,
          and the output values in a variable with `_post` appended. For
          example, a field `f` as input will be read into `f` using the name
          `f`, and output values will be read into `f_post` using the name
          `f_post`. The symbol table will make sure that the `_post` name
          is unique.
       c. Similar to b., output only parameters will be read into a variable
          named with '_post' attached, e.g. output field `f` will be stored
          in a variable `f_post`. Then the array `f` is allocated based on
          the shape of `f_post` and initialised to 0 (since it's an
          output-only parameter the value doesn't really matter).
    7. The extracted kernels are added to the program. Since in step 5 all
       references have been replaced, there created code will use the correct
       new variable names (which just have been read in). The output variables
       with `_post` attached will not be used at all so far.
    8. After the kernel calls are executed, each output variable is compared
       with the value stored in the corresponding `_post` variable. For
       example, a variable `f` which was modified in the kernel calls, will
       then be compared with `f_post`.

    :type integer_type: :py:class:`psyclone.psyir.symbols.ScalarType`
    :param real_type: default scalar real type to be used for real \
        variables. Defaults to REAL8_TYPE.
    :type real_type: :py:class:`psyclone.psyir.symbols.ScalarType`

    '''
    def __init__(self, integer_type=INTEGER_TYPE, real_type=REAL8_TYPE):
        # Set the integer and real types to use.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {ScalarType.Intrinsic.INTEGER: integer_type,
                               "integer": integer_type,
                               ScalarType.Intrinsic.REAL: real_type,
                               ScalarType.Intrinsic.BOOLEAN: BOOLEAN_TYPE,
                               "real": real_type}
        self._all_field_types = ["field_type", "integer_field_type",
                                 "r_solver_field_type", "r_tran_field_type"]
        self._default_precision = {"r_def": "real64",
                                   "r_solver": "real32",
                                   "i_def": "integer64"}

        const = LFRicConstants()
        self._map_fields_to_precision = {}
        for field_info in const.DATA_TYPE_MAP.values():
            if field_info["proxy_type"] is not None:
                self._map_fields_to_precision[field_info["proxy_type"]] \
                    = field_info["kind"]

    # -------------------------------------------------------------------------
    @staticmethod
    def make_valid_unit_name(name):
        '''Unit names are restricted to 63 characters, and no special
        characters like ':'.
        '''
        return name.replace(":", "")[:63]

    # -------------------------------------------------------------------------
    def get_proxy_name_mapping(self, schedule):
        '''This function creates a mapping of each proxy name of an argument
        to the field map. This mapping is used to convert proxy names used
        in a lowered kernel call back to the original name, which is the name
        used in extraction.

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
    def flatten_string(fortran_string):
        '''Replaces all `%` with `_` in the string, creating a 'flattened'
        name.

        :param str fortran_string: the Fortran string containing '%'.

        :returns: a flattened string (all '%' replaced with '_'.)
        :rtype: str

        '''
        return fortran_string.replace("%", "_")

    # -------------------------------------------------------------------------
    def flatten_reference(self, old_reference, symbol_table,
                          proxy_name_mapping):
        '''Replaces `old_reference` which is a structure type with a new
        simple Reference and a flattened name (replacing all % with _).

        :param old_reference: a reference to a structure member.
        :type old_reference: \
            :py:class:`psyclone.psyir.nodes.StructureReference`
        :param symbol_table: the symbol table to which to add the newly \
            defined flattened symbol.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a Fortran writer used when flattening a \
            `StructureReference`.
        :type writer: :py:class:`psyclone.psyir.backend.fortran.FortranWriter`

        '''
        # A field access (`fld%data`) will get the `%data` removed, since then
        # this avoids a potential name clash (`fld` is guaranteed to
        # be unique, since it's a variable already, but `fld_data` could clash
        # with a user variable if the user uses `fld` and `fld_data`).
        # Furthermore, the netcdf file declares the variable without `%data`,
        # so removing `%data` here also simplifies code creation later on.

        signature, _ = old_reference.get_signature_and_indices()
        # Now remove '_proxy' that might have been added to a variable name,
        # to preserve the expected names from a user's point of view.
        symbol_name = proxy_name_mapping.get(signature[0], signature[0])
        # Create the new signature, e.g. f1_proyx%data --> f1
        signature = Signature(symbol_name, signature[1:])

        # We use this string as a unique tag - it must be unique since no
        # other tag uses a '%' in the name. So even if the flattened name
        # (e.g. f1_data) is not unique, the tatg `f1%data` is unique, and
        # the symbol table will then create a unique name for this symbol.

        if isinstance(old_reference.symbol.datatype, ArrayType):
            # Vector field. Get the index that is being accessed:
            indx = int(old_reference.children[-1].value)
            signature = Signature(f"{symbol_name}_{indx}", signature[1:])
        else:
            field_type = old_reference.symbol.datatype.name
            if field_type in ["field_proxy_type", "r_solver_field_proxy_type",
                              "r_tran_field_proxy_type"]:
                # Field proxy are accessed using '%data'. Remove this to
                # have more familiar names for the user, and also because
                # the plain name is used in the file written.
                signature = Signature(signature[0])
            else:
                # Other types need to get the member added to the name,
                # to make unique symbols (e.g. 'op_a_proxy%ncell_3d'
                # and 'op_a_proxy%local_stencil'
                signature = Signature(symbol_name, signature[1:])

        signature_str = str(signature)
        try:
            symbol = symbol_table.lookup_with_tag(signature_str)
        except KeyError:
            flattened_name = self.flatten_string(signature_str)
            symbol = DataSymbol(flattened_name, old_reference.datatype)
            symbol_table.add(symbol, tag=f"{signature_str}")

        array_member = None
        current = old_reference
        while current:
            if isinstance(current, ArrayMember):
                array_member = current
                break
            if not current.children:
                break
            current = current.children[0]

        if array_member:
            # If there is an array access, we need to replace the structure
            # reference with an ArrayReference
            ind = array_member.pop_all_children()
            new_ref = ArrayReference.create(symbol, ind)
        else:
            new_ref = Reference(symbol)

        old_reference.replace_with(new_ref)

    # -------------------------------------------------------------------------
    def add_all_kernel_symbols(self, sched, symbol_table, proxy_name_mapping):
        '''This function adds all symbols used in `sched` to the symbol table.
        It uses GOcean-specific knowledge to declare fields and flatten their
        name.

        :param sched: the schedule that will be called by this driver program.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param symbol_table: the symbol table to which to add all found \
            symbols.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a Fortran writer used when flattening a \
            `StructureReference`.
        :type writer: :py:class:`psyclone.psyir.backend.fortan.FortranWriter`

        :raises InternalError: if a non-derived type has an unknown \
            intrinsic type.
        :raises InternalError: if an unknown derived type is \
            encountered. At this stage only the dl_esm_inf `field` type \
            is supported.

        '''
        # pylint: disable=too-many-locals
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
                # symbols consistent:
                reference.symbol = symbol_table.lookup(old_symbol.name)
                continue

            new_symbol = symbol_table.new_symbol(root_name=reference.name,
                                                 tag=reference.name,
                                                 symbol_type=DataSymbol,
                                                 datatype=old_symbol.datatype)
            reference.symbol = new_symbol

        # Now handle all derived type. The name of a derived type is
        # 'flattened', i.e. all '%' are replaced with '_', and this is then
        # declared as a non-structured type. We also need to make sure that a
        # flattened name does not clash with a variable declared by the user.
        # We use the structured name (with '%') as tag to handle this.
        for reference in all_references:
            if not isinstance(reference, StructureReference):
                continue
            self.flatten_reference(reference, symbol_table,
                                   proxy_name_mapping)

    # -------------------------------------------------------------------------
    @staticmethod
    def add_call(program, name, args):
        '''This function creates a call to the subroutine of the given name,
        providing the arguments. The call will be added to the program and
        to the symbol table.

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param str name: name of the subroutine to call.
        :param args: list of all arguments for the call.
        :type args: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises TypeError: if there is a symbol with the \
            specified name defined that is not a RoutineSymbol.
        '''
        print("Adding call", name, args)
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
    def create_read_in_code(self, program, psy_data, original_symbol_table,
                            input_list, output_list, postfix):
        # pylint: disable=too-many-arguments
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

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data: the PSyData symbol to be used.
        :type psy_data: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param input_list: list of all signatures that are input variables \
            to the instrumented region.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of all signatures that are output \
            variables of the instrumented region.
        :type output_list: list of :py:class:`psyclone.core.Signature`
        :param str postfix: a postfix that is added to a variable to \
            create the corresponding variable that stores the output \
            value from the kernel data file.

        :returns: a list with all output parameters, i.e. variables that \
            need to be verified after executing the kernel. Each entry is \
            a 2-tuple containing the symbol of the computed variable, and \
            the symbol of the variable that contains the value read from \
            the file.
        :rtype: list of 2-tuples of \
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        # pylint: disable=too-many-locals
        all_sigs = list(set(input_list).union(set(output_list)))
        all_sigs.sort()
        symbol_table = program.scope.symbol_table
        read_var = f"{psy_data.name}%ReadVariable"

        # Collect all output symbols to later create the tests for
        # correctness. This list stores 2-tuples: first one the
        # variable that stores the output from the kernel, the second
        # one the variable that stores the output values read from the
        # file. The content of these two variables should be identical
        # at the end.
        output_symbols = []

        # First handle variables that are read:
        # -------------------------------------
        for signature in input_list:
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables have References, and will already have been declared
            # in the symbol table (in add_all_kernel_symbols).
            sig_str = LFRicExtractDriverCreator.flatten_string(str(signature))
            orig_sym = original_symbol_table.lookup(signature[0])
            if orig_sym.is_array and orig_sym.datatype.intrinsic.name in \
                    self._all_field_types:
                upper = int(orig_sym.datatype.shape[0].upper.value)
                for i in range(1, upper+1):
                    sym = symbol_table.lookup_with_tag(f"{sig_str}_{i}%data")
                    name_lit = Literal(f"{sig_str}%{i}", CHARACTER_TYPE)
                    LFRicExtractDriverCreator.add_call(program, read_var,
                                                       [name_lit,
                                                        Reference(sym)])
                continue

            try:
                sym = symbol_table.lookup_with_tag(str(signature))
            except KeyError:
                sig_str += "%data"
                sym = symbol_table.lookup_with_tag(sig_str)
            name_lit = Literal(str(signature), CHARACTER_TYPE)
            LFRicExtractDriverCreator.add_call(program, read_var,
                                               [name_lit, Reference(sym)])

        # Then handle all variables that are written (note that some
        # variables might be read and written)
        for signature in output_list:
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables have References, and will already have been declared
            # in the symbol table (in add_all_kernel_symbols).
            sig_str = LFRicExtractDriverCreator.flatten_string(str(signature))
            orig_sym = original_symbol_table.lookup(signature[0])
            if orig_sym.is_array and orig_sym.datatype.intrinsic.name in \
                    self._all_field_types:
                upper = int(orig_sym.datatype.shape[0].upper.value)
                for i in range(1, upper+1):
                    sym = symbol_table.lookup_with_tag(f"{sig_str}_{i}%data")
                    name_lit = Literal(f"{sig_str}_{i}_data", CHARACTER_TYPE)
                    LFRicExtractDriverCreator.add_call(program, read_var,
                                                       [name_lit,
                                                        Reference(sym)])
                continue

            try:
                sig_str = str(signature)
                sym = symbol_table.lookup_with_tag(str(signature))
            except KeyError:
                sig_str += "%data"
                sym = symbol_table.lookup_with_tag(sig_str)
            is_input = signature in input_list

            # The variable is written (and maybe read as well)
            # ------------------------------------------------
            # Declare a 'post' variable of the same type and
            # read in its value.
            post_name = sym.name+postfix
            post_sym = symbol_table.new_symbol(post_name,
                                               symbol_type=DataSymbol,
                                               datatype=sym.datatype)
            name_lit = Literal(sig_str+postfix, CHARACTER_TYPE)
            LFRicExtractDriverCreator.add_call(program, read_var,
                                               [name_lit,
                                                Reference(post_sym)])

            # Now if a variable is written to, but not read, the variable
            # is not allocated. So we need to allocate it and set it to 0.
            if not is_input:
                if isinstance(post_sym.datatype, ArrayType):
                    # TODO #1366 Once allocate is supported in PSyIR
                    # this parsing of a file can be replaced. Also,
                    # if the mold parameter is supported, we can
                    # use the second allocate statement to create code
                    # that's independent of the number of dimensions.
                    shape = post_sym.datatype.shape
                    dims = ",".join([":"]*len(shape))
                    code = (f'''
                        subroutine tmp()
                          integer, allocatable, dimension({dims}) :: b
                          allocate({sym.name}, mold={post_name})
                        end subroutine tmp''')
                    fortran_reader = FortranReader()
                    container = fortran_reader.psyir_from_source(code)\
                        .children[0]
                    alloc = container.children[0].detach()
                    program.addchild(alloc)
                set_zero = Assignment.create(Reference(sym),
                                             Literal("0", INTEGER_TYPE))
                program.addchild(set_zero)
            output_symbols.append((sym, post_sym))
        return output_symbols

    # -------------------------------------------------------------------------
    @staticmethod
    def import_modules(program, sched):
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the PSyIR tree and
        checks for calls with a ImportInterface. Any such call will
        get a ContainerSymbol added for the module, and a RoutineSymbol
        with an import interface pointing to this module.

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param sched: the schedule that will be called by the driver \
            program created.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        symbol_table = program.scope.symbol_table
        for call in sched.walk(Call):
            routine = call.routine
            if not isinstance(routine.interface, ImportInterface):
                continue
            if routine.name in symbol_table:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (the PSyIR backend will then create a suitable import
            # statement).
            module = ContainerSymbol(routine.interface.container_symbol.name)
            symbol_table.add(module)
            new_routine_sym = RoutineSymbol(routine.name, DeferredType(),
                                            interface=ImportInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    def add_precision_symbols(self, program):
        '''This function defines the LFRic precision symbols r_def and
        i_def, after importing the required types from the Fortran intrinci
        module. The actual type is picked depending on the setting of
        self._default_types["real"/"integer"]

        :param program: the PSyIR Routine to which any code must \
            be added. It contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`

        '''
        symbol_table = program.scope.symbol_table
        # Using the intrinsic module requires a ",". Just add this
        # as part of the name, the writerthen produces the expected
        # Fortran code (`use ,intrinsic::iso_fortran_env, only:`)
        intrinsic_mod = ContainerSymbol(",intrinsic::iso_fortran_env")
        symbol_table.add(intrinsic_mod)

        # Add r_def:
        if self._default_types["real"] == REAL8_TYPE:
            name = "real64"
        else:
            name = "real32"
        real32_type = DataTypeSymbol("real32", INTEGER_TYPE,
                                     interface=ImportInterface(intrinsic_mod))
        real64_type = DataTypeSymbol("real64", INTEGER_TYPE,
                                     interface=ImportInterface(intrinsic_mod))
        symbol_table.add(real32_type)
        symbol_table.add(real64_type)
        symbol_table.new_symbol("r_def",
                                symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE,
                                constant_value=Reference(real64_type))
        symbol_table.new_symbol("r_second",
                                symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE,
                                constant_value=Reference(real64_type))
        symbol_table.new_symbol("r_solver",
                                symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE,
                                constant_value=Reference(real32_type))

        # Add i_def:
        if self._default_types["integer"] == INTEGER8_TYPE:
            name = "int64"
        else:
            name = "int32"
        int_type = DataTypeSymbol(name, INTEGER_TYPE,
                                  interface=ImportInterface(intrinsic_mod))
        symbol_table.add(int_type)

        symbol_table.new_symbol("i_def",
                                symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE,
                                constant_value=Reference(int_type))

        # Add l_def:
        symbol_table.new_symbol("l_def",
                                symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE,
                                constant_value=Reference(int_type))

    # -------------------------------------------------------------------------
    @staticmethod
    def add_result_tests(program, output_symbols):
        '''Adds tests to check that all output variables have the expected
        value.

        :param program: the program to which the tests should be added.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param output_symbols: a list containing all output variables of \
            the executed code. Each entry in the list is a 2-tuple, \
            containing first the symbol that was computed when executing \
            the kernels, and then the symbol containing the expected \
            values that have been read in from a file.
        :type output_symbols: list of 2-tuples of \
            :py:class:`psyclone.psyir.symbols.Symbol`
        '''

        for (sym_computed, sym_read) in output_symbols:
            if isinstance(sym_computed.datatype, ArrayType):
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
    def create(self, nodes, input_list, output_list,
               prefix, postfix, region_name):
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. It returns the
        file container which contains the driver.

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param (str,str) region_name: an optional name to \
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

        for node in nodes:
            for builtin in node.walk(LFRicBuiltIn):
                # If the lower_to_language function is not overwritten from
                # the implementation in Node, the builtin is not yet supported:
                if type(builtin).lower_to_language_level == \
                        Node.lower_to_language_level:
                    raise NotImplementedError(
                        f"LFRic builtin '{builtin.name}' is not supported")

        dep = DependencyTools()
        input_list, output_list = dep.get_in_out_parameters(nodes)

        module_name, local_name = region_name
        unit_name = LFRicExtractDriverCreator.\
            make_valid_unit_name(f"{module_name}_{local_name}")

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
        psy_data_type = DataTypeSymbol("ReadKernelDataType", DeferredType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        # The validation of the extract transform guarantees that all nodes
        # in the node list have the same parent.
        invoke_sched = nodes[0].ancestor(InvokeSchedule)

        # The invoke-schedule might have children that are not in the node
        # list. So get the indices of the nodes for which a driver is to
        # be created, and then remove all other nodes from the copy
        all_indices = [node.position for node in nodes]

        schedule_copy = invoke_sched.copy()

        halo_nodes = schedule_copy.walk(HaloExchange)
        for halo_node in halo_nodes:
            halo_node.parent.children.remove(halo_node)
        original_symbol_table = invoke_sched.symbol_table
        proxy_name_mapping = self.get_proxy_name_mapping(schedule_copy)

        # Now clean up the try: remove nodes in the copy that are not
        # supposed to be extracted. Any node that should be extract
        # needs to be lowered, which will fix the loop boundaries.
        # Otherwise, if e.g. the second loop is only extracted, this
        # loop would switch from using loop1_start/stop to loop0_start/stop
        # since is is then the first loop (hence we need to do this
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

        self.import_modules(program, schedule_copy)
        self.add_precision_symbols(program)
        self.add_all_kernel_symbols(schedule_copy, program_symbol_table,
                                    proxy_name_mapping)

        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        module_str = Literal(module_name, CHARACTER_TYPE)
        region_str = Literal(local_name, CHARACTER_TYPE)
        self.add_call(program, f"{psy_data.name}%OpenRead",
                      [module_str, region_str])

        output_symbols = self.create_read_in_code(program, psy_data,
                                                  original_symbol_table,
                                                  input_list, output_list,
                                                  postfix)
        # Copy over all of the executable part of the extracted region
        # program.addchild(schedule_copy)
        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        self.add_result_tests(program, output_symbols)

        return file_container

    # -------------------------------------------------------------------------
    def get_driver_as_string(self, nodes, input_list, output_list,
                             prefix, postfix, region_name,
                             writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses `create()` function to get a PSyIR of a
        stand-alone driver, and then uses the provided language writer
        to create a string representation in the selected language
        (defaults to Fortran).

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param (str,str) region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR \
            representation to the selected language. It defaults to \
            the FortranWriter.
        :type language_writer: \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :returns: the driver in the selected language.
        :rtype: str

        '''
        try:
            file_container = self.create(nodes, input_list, output_list,
                                         prefix, postfix, region_name)
        except NotImplementedError as err:
            print(f"Cannot create driver for '{region_name[0]}-"
                  f"{region_name[1]}' because:")
            print(str(err))
            return ""

        return writer(file_container)

    # -------------------------------------------------------------------------
    def write_driver(self, nodes, input_list, output_list,
                     prefix, postfix, region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the `get_driver_as_string()` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".f90"

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param (str,str) region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR \
            representation to the selected language. It defaults to \
            the FortranWriter.
        :type language_writer: \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        '''
        code = self.get_driver_as_string(nodes, input_list, output_list,
                                         prefix, postfix, region_name,
                                         writer=writer)
        if not code:
            # This indicates an error, that was already printed,
            # so ignore it here.
            return
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.f90", "w",
                  encoding='utf-8') as out:
            out.write(code)
