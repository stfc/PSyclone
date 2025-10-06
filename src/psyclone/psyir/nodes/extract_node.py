# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office
# Modified A. R. Porter, S. Siso, R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''
This module provides support for extraction of code within a specified
invoke. The extracted code may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke (extraction
applied to all Nodes).

There is currently only one class in this module: ExtractNode (see below).

Another class which contains helper functions for code extraction, such as
wrapping up settings for generating driver for the extracted code, will
be added in Issue #298.
'''

from typing import List

from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.psyir.nodes.assignment import Assignment
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, REAL8_TYPE, ArrayType, ContainerSymbol,
    ImportInterface, DataType, SymbolTable)
from psyclone.errors import InternalError


class ExtractNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    code extraction using the ExtractRegionTrans transformation. By
    applying the transformation the Nodes marked for extraction become
    children of (the Schedule of) an ExtractNode.

    :param ast: reference into the fparser2 parse tree corresponding to
        this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options provided via transformations.
    :type options: Optional[Dict[str, Any]]
    :param str options["prefix"]: a prefix to use for the PSyData module name
        (``prefix_psy_data_mod``) and the PSyDataType
        (``prefix_PSyDataType``) - a "_" will be added automatically.
        It defaults to "extract", which means the module name used will be
        ``extract_psy_data_mode``, and the data type ``extract_PSyDataType``.
    :param str options["post_var_postfix"]: a postfix to be used when
        creating names to store values of output variable. A variable 'a'
        would store its value as 'a', and its output values as 'a_post' with
        the default post_var_postfix of '_post'.
    :param options["read_write_info"]: information about variables that are
        read and/or written in the instrumented code.
    :type options["read_write_info"]:
        py:class:`psyclone.psyir.tools.ReadWriteInfo`

    '''
    # Textual description of the node.
    _text_name = "Extract"
    _colour = "green"
    # The default prefix to add to the PSyData module name and PSyDataType
    _default_prefix = "extract"

    # This dictionary keeps track of region+module names that are already
    # used. For each key (which is module_name+"|"+region_name) it contains
    # how many regions with that name have been created. This number will
    # then be added as an index to create unique region identifiers.
    _used_kernel_names = {}

    def __init__(self, ast=None, children=None, parent=None, options=None):
        super().__init__(ast=ast, children=children,
                         parent=parent, options=options)

        # Define a postfix that will be added to variable that are
        # modified to make sure the names can be distinguished between pre-
        # and post-variables (i.e. here input and output). A variable
        # "myvar" will be stored as "myvar" with its input value, and
        # "myvar_post" with its output value. It is the responsibility
        # of the transformation that inserts this node to make sure this
        # name is consistent with the name used when creating the driver
        # (otherwise the driver will not be able to read in the dumped
        # valued), and also to handle any potential name clashes (e.g. a
        # variable 'a' exists, which creates 'a_out' for the output variable,
        # which would clash with a variable 'a_out' used in the program unit).

        if options is None:
            options = {}

        self._post_name = options.get("post_var_postfix", "_post")

        # Keep a copy of the argument list:
        self._read_write_info = options.get("read_write_info")
        self._driver_creator = None

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two ExtractNodes are equal if
        their extract_body members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.post_name == other.post_name
        return is_eq

    @property
    def extract_body(self):
        '''
        :returns: the Schedule associated with this ExtractNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super().psy_data_body

    @property
    def post_name(self):
        '''
        :returns: the _post_name member of this ExtractNode.
        :rtype: str
        '''
        return self._post_name

    def lower_to_language_level(self):
        # pylint: disable=arguments-differ
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.call_tree_utils import CallTreeUtils
        self._populate_region_name()

        # get_non_local_read_write_info doesn't work with the lowered tree,
        # so we save a copy of the higher dsl tree
        copy_dsl_tree = self.copy()

        for child in self.children:
            child.lower_to_language_level()

        self.flatten_references()

        # Determine the variables to write:
        ctu = CallTreeUtils()
        read_write_info = ctu.get_in_out_parameters(
            self, include_non_data_accesses=False)
        # Use the copy of the dsl_tree to get the external symbols
        ctu.get_non_local_read_write_info(copy_dsl_tree.children,
                                          read_write_info)

        # TODO #3024: We could be more data efficient by better selecting
        # which don't need to be copied in (because the extraction region
        # will only write to them)
        options = {'pre_var_list': read_write_info.all_used_vars_list,
                   'post_var_list': read_write_info.write_list,
                   'post_var_postfix': self._post_name}

        if self._driver_creator:
            nodes = self.children
            region_name_tuple = self.get_unique_region_name(nodes)

            self.bring_external_symbols(read_write_info,
                                        self.ancestor(Routine).symbol_table)

            # Determine a unique postfix to be used for output variables
            # that avoid any name clashes
            postfix = self.determine_postfix(read_write_info,
                                             postfix="_post")
            # Remove the spurious "_" at the end of the prefix or use default
            prefix = self._prefix[:-1] if self._prefix else "extract"
            # Create and write the driver code
            self._driver_creator.write_driver(self.children,
                                              read_write_info,
                                              postfix=postfix,
                                              prefix=prefix,
                                              region_name=region_name_tuple)

        return super().lower_to_language_level(options)

    # -------------------------------------------------------------------------
    @staticmethod
    def determine_postfix(read_write_info, postfix: str = "_post") -> str:
        '''
        This function prevents any name clashes that can occur when adding
        the postfix to output variable names. For example, if there is an
        output variable 'a', the driver (and the output file) will contain
        two variables: 'a' and 'a_post'. But if there is also another variable
        called 'a_post', a name clash would occur (two identical keys in the
        output file, and two identical local variables in the driver). In
        order to avoid this, the suffix 'post' is changed (to 'post0',
        'post1', ...) until any name clashes are avoided. This works for
        structured and non-structured types.

        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str postfix: the postfix to append to each output variable.

        :returns: a postfix that can be added to each output variable without
            generating a name clash.

        '''
        suffix = ""
        # Create the a set of all input and output variables (to avoid
        # checking input+output variables more than once)
        all_vars = read_write_info.all_used_vars_list
        # The signatures in the input/output list need to be converted
        # back to strings to easily append the suffix.
        all_vars_string = [str(input_var) for _, input_var in all_vars]
        while any(str(out_sig)+postfix+str(suffix) in all_vars_string
                  for out_sig in read_write_info.signatures_written):
            if suffix == "":
                suffix = 0
            else:
                suffix += 1
        return postfix+str(suffix)

    def get_unique_region_name(self, nodes: List[Node]):
        '''This function returns the region and module name. If they are
        specified in the user options, these names will just be returned (it
        is then up to the user to guarantee uniqueness). Otherwise a name
        based on the module and invoke will be created using indices to
        make sure the name is unique.

        :param nodes: a list of nodes.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import InvokeSchedule
        invoke = nodes[0].ancestor(InvokeSchedule)
        if invoke:
            module_name = invoke.invoke.invokes.psy.name
        else:
            module_name = nodes[0].root.name
        return (module_name, self._region_name)

    # -------------------------------------------------------------------------
    @staticmethod
    def _flatten_signature(signature: Signature) -> str:
        '''Creates a 'flattened' string for a signature by using ``_`` to
        separate the parts of a signature. For example, in Fortran
        a reference to ``a%b`` would be flattened to be ``a_b``.

        :param signature: the signature to be flattened.

        :returns: a flattened string (all '%' replaced with '_'.)

        '''
        return str(signature).replace("%", "_")

    # -------------------------------------------------------------------------
    def flatten_references(self):
        '''Replace StructureReferencces with a simple Reference and a flattened
        name (replacing all % with _).

        '''
        already_flattened = {}  # dict of name: symbol

        for structure_ref in self.walk(StructureReference)[:]:
            if isinstance(structure_ref.parent, Call):
                if structure_ref.position == 0:
                    return  # Method calls are fine

            signature, _ = structure_ref.get_signature_and_indices()
            flattened_name = self._flatten_signature(signature)
            try:
                symbol = already_flattened[flattened_name]
            except KeyError:
                symtab = structure_ref.ancestor(Routine).symbol_table
                symbol = symtab.new_symbol(
                            flattened_name,
                            symbol_type=DataSymbol,
                            datatype=self._flatten_datatype(structure_ref))
                already_flattened[flattened_name] = symbol
                # We also need two assignments to copy the initial and final
                # values to/from the flattened temporary
                self.parent.addchild(Assignment.create(Reference(symbol),
                                                       structure_ref.copy()),
                                     index=self.position)
                self.parent.addchild(Assignment.create(structure_ref.copy(),
                                                       Reference(symbol)),
                                     index=self.position+1)

            # Replace the structure access with the flattened reference
            structure_ref.replace_with(Reference(symbol))

    @staticmethod
    def _flatten_datatype(structure_reference: StructureReference) -> DataType:
        ''' Ideally this should be replaced by structure_reference.datatype
        but until it works, this utility method provides hardcoded type
        information depending on the PSyKAL DSL and names involved.

        :returns: the datatype of the symbol with the flattened expression.
        '''
        signature, _ = structure_reference.get_signature_and_indices()
        if Config.get().api == "gocean":
            api_config = Config.get().api_conf("gocean")
            grid_properties = api_config.grid_properties
            for prop_name in grid_properties:
                gocean_property = grid_properties[prop_name]
                property_name = gocean_property.fortran.split('%')[-1]
                # Search for a property with the same name as the signature
                # inner accessor
                if signature[-1] == property_name:
                    break
            else:
                raise InternalError(
                    f"Could not find type for reference "
                    f"'{structure_reference.debug_string()}' "
                    f"in the config file '{Config.get().filename}'.")
            if gocean_property.intrinsic_type == 'real':
                scalar_type = REAL8_TYPE
            else:
                scalar_type = INTEGER_TYPE
            if gocean_property.type == "scalar":
                return scalar_type
            # Everything else is a 2D field
            return ArrayType(scalar_type, [ArrayType.Extent.DEFERRED,
                                           ArrayType.Extent.DEFERRED])

        # Everything else defaults to integer
        return INTEGER_TYPE

    @staticmethod
    def bring_external_symbols(read_write_info, symbol_table: SymbolTable):
        '''
        Use the ModuleManager to explore external dependencies and bring
        symbols used in other modules into scope (with ImportInterface). The
        symbols will be tagged with a 'signature@module_name' tag.

        :param read_write_info: information about the symbols usage in the
            scope.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param symbol_table: the associated symbol table.

        '''
        # Cyclic import
        # pylint: disable=import-outside-toplevel
        from psyclone.parse import ModuleManager
        mod_man = ModuleManager.get()
        for module_name, signature in read_write_info.all_used_vars_list:
            if not module_name:
                # Ignore local symbols, which will have been added above
                continue
            container = symbol_table.find_or_create(
                module_name, symbol_type=ContainerSymbol)

            # Now look up the original symbol. While the variable could
            # be declared Unresolved here (i.e. just imported), we need the
            # type information for the output variables (VAR_post), which
            # are created later and which will query the original symbol for
            # its type. And since they are not imported, they need to be
            # explicitly declared.
            mod_info = mod_man.get_module_info(module_name)
            container_symbol = mod_info.get_symbol(signature[0])
            if not container_symbol:
                # TODO #2120: This typically indicates a problem with parsing
                # a module: the psyir does not have the full tree structure.
                continue

            # It is possible that external symbol name (signature[0]) already
            # exist in the symbol table (the same name is used in the local
            # subroutine and in a module). In this case, the imported symbol
            # must be renamed:
            if signature[0] in symbol_table:
                interface = ImportInterface(container, orig_name=signature[0])
            else:
                interface = ImportInterface(container)

            symbol_table.find_or_create_tag(
                tag=f"{signature[0]}@{module_name}", root_name=signature[0],
                symbol_type=DataSymbol, interface=interface,
                datatype=container_symbol.datatype)


# For AutoAPI documentation generation
__all__ = ['ExtractNode']
