# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Modified: S. Siso, STFC Daresbury Lab

'''This module provides functionality for the PSyclone kernel extraction
functionality for LFRic. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

# TODO #1382: refactoring common functionality between the various driver
# creation implementation should make this file much smaller.
# pylint: disable=too-many-lines

from typing import Optional, Tuple

from psyclone.configuration import Config
from psyclone.domain.common import BaseDriverCreator
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Call, FileContainer,
                                  Routine, StructureReference)
from psyclone.psyir.symbols import (
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, UnresolvedType,
                                    ImportInterface, INTEGER_TYPE,
                                    UnsupportedFortranType)


class LFRicExtractDriverCreator(BaseDriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    The driver is created as follows:

    1. The corresponding :py:class:`psyclone.psyGen.Invoke` statement that
       contains the kernel(s) is copied. This way we avoid affecting the tree
       of the caller. We need the invoke since it contains the symbol table.
    2. We remove all halo exchange nodes.
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

    :param region_name: the suggested region_name.
    '''
    def __init__(self, region_name: Optional[Tuple[str, str]] = None):
        super().__init__(region_name)
        # TODO #2069: check if this list can be taken from LFRicConstants
        # TODO #2018: once r_field is defined in the LFRic infrastructure,
        #             it should be added to this list.
        self._all_field_types = ["integer_field_type", "field_type",
                                 "r_bl_field", "r_solver_field_type",
                                 "r_tran_field_type"]

    # -------------------------------------------------------------------------
    @staticmethod
    def _add_precision_symbols(symbol_table):
        '''This function adds an import of the various precision
        symbols used by LFRic from the constants_mod module.

        :param symbol_table: the symbol table to which the precision symbols
            must be added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        const = LFRicConstants()
        mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
        constant_mod = ContainerSymbol(mod_name)
        symbol_table.add(constant_mod)

        # r_quad is defined in constants_mod, but not exported. And r_phys
        # does not exist at all in LFRic, but is still in LFRic's psyclone.cfg
        # file. TODO #2018 and
        # https://code.metoffice.gov.uk/trac/lfric/ticket/4674
        api_config = Config.get().api_conf("lfric")
        all_precisions = [name for name in api_config.precision_map
                          if name not in ["r_quad", "r_phys"]]
        for prec_name in all_precisions:
            symbol_table.new_symbol(prec_name,
                                    tag=f"{prec_name}@{mod_name}",
                                    symbol_type=DataSymbol,
                                    datatype=INTEGER_TYPE,
                                    interface=ImportInterface(constant_mod))

    # -------------------------------------------------------------------------
    def _add_command_line_handler(self, program, psy_data_var, module_name,
                                  region_name):
        '''
        This function adds code to handle the command line. For now an
        alternative filename (to the default one that is hard-coded by
        the created driver) can be specified, which allows the driver to
        be used with different files, e.g. several dumps from one run, and/or
        a separate file from each process. It will also add the code to
        open the input file using the read_kernel_data routine from the
        extraction library.

        :param program: The driver PSyIR.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data_var: the symbol of the PSyDataExtraction type.
        :type psy_data_var: :py:class:`psyclone.psyir.symbols.Symbol`
        :param str module_name: the name of the module, used to create the
            implicit default kernel dump file name.
        :param str region_name: the name of the region, used to create the
            implicit default kernel dump file name.

        '''
        # pylint: disable=too-many-locals
        program_symbol_table = program.symbol_table

        # PSyIR does not support allocatable strings, so create the two
        # variables we need in a loop.
        # TODO #2137: The UnsupportedFortranType could be reused for all
        #             variables once this is fixed.
        for str_name in ["psydata_filename", "psydata_arg"]:
            str_unique_name = \
                program_symbol_table.next_available_name(str_name)
            str_type = UnsupportedFortranType(
                f"character(:), allocatable :: {str_unique_name}")
            sym = DataTypeSymbol(str_unique_name, str_type)
            program_symbol_table.add(sym)
            if str_name == "psydata_filename":
                psydata_filename = str_unique_name
            else:
                psydata_arg = str_unique_name

        psydata_len = \
            program_symbol_table.find_or_create("psydata_len",
                                                symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE).name
        psydata_i = \
            program_symbol_table.find_or_create("psydata_i",
                                                symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE).name
        # We can only parse one statement at a time, so start with the
        # command line handling:
        code = f"""
        do {psydata_i}=1,command_argument_count()
           call get_command_argument({psydata_i}, length={psydata_len})
           allocate(character({psydata_len})::{psydata_arg})
           call get_command_argument({psydata_i}, {psydata_arg}, &
                                     length={psydata_len})
           if ({psydata_arg} == "--update") then
              ! For later to allow marking fields as being updated
           else
              allocate(character({psydata_len})::{psydata_filename})
              {psydata_filename} = {psydata_arg}
           endif
           deallocate({psydata_arg})
        enddo
        """
        command_line = \
            FortranReader().psyir_from_statement(code, program_symbol_table)
        program.children.insert(0, command_line)

        # Now add the handling of the filename parameter
        code = f"""
        if (allocated({psydata_filename})) then
           call {psy_data_var.name}%OpenReadFileName({psydata_filename})
        else
           call {psy_data_var.name}%OpenReadModuleRegion('{module_name}', &
                                                         '{region_name}')
        endif
        """
        filename_test = \
            FortranReader().psyir_from_statement(code, program_symbol_table)
        program.children.insert(1, filename_test)

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
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols ``extract_psydata``.
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

        :returns: the program PSyIR for a stand-alone driver.
        :rtype: :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        '''
        # pylint: disable=too-many-locals
        module_name, local_name = region_name
        unit_name = self._make_valid_unit_name(f"{module_name}_{local_name}")

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine.create(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        original_symbol_table = nodes[0].ancestor(InvokeSchedule).symbol_table
        file_container.addchild(program)

        # Add the extraction library symbols
        psy_data_mod = ContainerSymbol("read_kernel_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol("ReadKernelDataType", UnresolvedType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)
        if prefix:
            prefix = prefix + "_"
        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        extract_region = nodes[0].copy()
        # StructureReference must have been flattened before creating the
        # driver, or are method calls. In both cases they are not allowed.
        for sref in extract_region.walk(StructureReference):
            dm_methods = ("set_dirty", "set_clean")
            if (isinstance(sref.parent, Call) and
                    sref.member.name in dm_methods):
                # Some methods regarding distributed-memory can be deleted as
                # we know the driver is executed with a single rank.
                sref.parent.detach()
            else:
                raise ValueError(f"The provided PSyIR should not have "
                                 f"StructureReferences, but found: "
                                 f"{sref.debug_string()}")

        # Add cmd line hander, read in, and result comparison for the code
        self._add_command_line_handler(program, psy_data, module_name,
                                       local_name)
        output_symbols = self._create_read_in_code(program, psy_data,
                                                   original_symbol_table,
                                                   read_write_info, postfix)

        # Copy the nodes that are part of the extraction
        program.children.extend(extract_region.pop_all_children())

        # Find all imported modules and add them to the symbol table
        self.import_modules(program)
        self._add_precision_symbols(program.scope.symbol_table)

        BaseDriverCreator.add_result_tests(program, output_symbols)

        # Replace pointers with allocatables
        for symbol in program_symbol_table.datasymbols:
            if isinstance(symbol.datatype, UnsupportedFortranType):
                symbol.datatype = symbol.datatype.copy()
                newt = symbol.datatype._declaration
                newt = newt.replace('pointer', 'allocatable')
                newt = newt.replace('=> null()', '')
                symbol.datatype._declaration = newt

        return file_container
