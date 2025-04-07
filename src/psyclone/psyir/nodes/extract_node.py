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

from psyclone.configuration import Config
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.assignment import Assignment
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, REAL8_TYPE, ArrayType, ContainerSymbol,
    ImportInterface)


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
        from psyclone.psyGen import Kern
        from psyclone.psyir.nodes import CodeBlock, Routine
        module_name = self._module_name
        if not self._region_name:
            kerns = self.walk(Kern)
            if len(kerns) == 1:
                # This PSyData region only has one kernel within it,
                # so append the kernel name.
                region_name = f"{kerns[0].name}-"
            else:
                region_name = ""
            # Create a name for this region by finding where this PSyDataNode
            # is in the list of PSyDataNodes in this Invoke. We allow for any
            # previously lowered PSyDataNodes by checking for CodeBlocks with
            # the "psy-data-start" annotation.
            routine_schedule = self.ancestor(Routine)
            pnodes = routine_schedule.walk((PSyDataNode, CodeBlock))
            region_idx = 0
            for node in pnodes[0:pnodes.index(self)]:
                if (isinstance(node, PSyDataNode) or
                        "psy-data-start" in node.annotations):
                    region_idx += 1
            region_name = f"{region_name}r{region_idx}"
            # If the routine name is not used as 'module name' (in case of a
            # subroutine outside of any modules), add the routine name
            # to the region. Otherwise just use the number
            if module_name != routine_schedule.name:
                region_name = f"{routine_schedule.name}-{region_name}"
            self._region_name = region_name

        # get_non_local_read_write_info doesn't work with the lowered tree,
        # so we save a copy of the higher dsl tree
        copy_dsl_tree = self.copy()

        for child in self.children:
            child.lower_to_language_level()

        self.flatten_references()

        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.call_tree_utils import CallTreeUtils
        # Determine the variables to write:
        ctu = CallTreeUtils()
        read_write_info = ctu.get_in_out_parameters(
            self, include_non_data_accesses=False)
        # Use the copy of the dsl_tree to get the external symbols
        ctu.get_non_local_read_write_info(copy_dsl_tree.children,
                                          read_write_info)

        options = {'pre_var_list': read_write_info.read_list,
                   'post_var_list': read_write_info.write_list,
                   'post_var_postfix': self._post_name}

        if self._driver_creator:
            nodes = self.children
            region_name_tuple = self.get_unique_region_name(nodes, {})
            region_name_tuple = (region_name_tuple[0], self._region_name)

            self._bring_external_symbols(read_write_info,
                                         self.ancestor(Routine).symbol_table)

            # Even variables that are output-only need to be written with their
            # values at the time the kernel is called: many kernels will only
            # write to part of a field (e.g. in case of MPI the halo region
            # will not be written). Since the comparison in the driver uses
            # the whole field (including values not updated), we need to write
            # the current value of an output-only field as well. This is
            # achieved by adding any written-only field to the list of fields
            # read. This will trigger to write the values in the extraction,
            # and the driver code created will read in their values.
            for sig in read_write_info.write_list:
                if sig not in read_write_info.read_list:
                    read_write_info.read_list.append(sig)

            # Determine a unique postfix to be used for output variables
            # that avoid any name clashes
            postfix = self.determine_postfix(read_write_info,
                                             postfix="_post")
            # Remove the spurious "_" at the end of the prefix or use default
            prefix = self._prefix[:-1] if self._prefix else "extract"
            # We need to create the driver before inserting the ExtractNode
            # (since some of the visitors used in driver creation do not
            # handle an ExtractNode in the tree)
            self._driver_creator.write_driver(self.children,
                                              read_write_info,
                                              postfix=postfix,
                                              prefix=prefix,
                                              region_name=region_name_tuple)

        return super().lower_to_language_level(options)

    # -------------------------------------------------------------------------
    @staticmethod
    def determine_postfix(read_write_info, postfix="_post"):
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

        :param read_write_info: information about all input and output \
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str postfix: the postfix to append to each output variable.

        :returns: a postfix that can be added to each output variable without
            generating a name clash.
        :rtype: str

        '''
        suffix = ""
        # Create the a set of all input and output variables (to avoid
        # checking input+output variables more than once)
        all_vars = read_write_info.set_of_all_used_vars
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

    def get_unique_region_name(self, nodes, options):
        '''This function returns the region and module name. If they are
        specified in the user options, these names will just be returned (it
        is then up to the user to guarantee uniqueness). Otherwise a name
        based on the module and invoke will be created using indices to
        make sure the name is unique.

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Dict[str, Any]
        :param (str,str) options["region_name"]: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region unless aggregate information \
            is required (and is supported by the runtime library).

        '''
        # We don't use a static method here since it might be useful to
        # overwrite this functions in derived classes
        name = options.get("region_name", None)
        if name:
            # pylint: disable=too-many-boolean-expressions
            if not isinstance(name, tuple) or not len(name) == 2 or \
               not name[0] or not isinstance(name[0], str) or \
               not name[1] or not isinstance(name[1], str):
                raise ValueError(
                    "Error in PSyDataTrans. The name must be a "
                    "tuple containing two non-empty strings.")
            # pylint: enable=too-many-boolean-expressions
            # Valid PSyData names have been provided by the user.
            return name

        from psyclone.psyGen import InvokeSchedule, Kern
        invoke = nodes[0].ancestor(InvokeSchedule).invoke
        module_name = invoke.invokes.psy.name

        # Use the invoke name as a starting point.
        region_name = invoke.name
        kerns = []
        for node in nodes:
            kerns.extend(node.walk(Kern))

        if len(kerns) == 1:
            # This PSyData region only has one kernel within it,
            # so append the kernel name.
            region_name += f"-{kerns[0].name}"

        # Add a region index to ensure uniqueness when there are
        # multiple regions in an invoke.
        key = module_name + "|" + region_name
        idx = self._used_kernel_names.get(key, 0)
        self._used_kernel_names[key] = idx + 1
        region_name += f"-r{idx}"
        return (module_name, region_name)

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
    def _flatten_datatype(structure_reference):
        ''' Ideally this should be replaces by structure_reference.datatype
        but until it works, this utility method provides hardcoded type
        information depending on the PSyKAL DSL and names involved
        '''
        signature, _ = structure_reference.get_signature_and_indices()
        if Config.get().api == "gocean":
            if signature[-1] in ("data", "gphiu"):
                return ArrayType(REAL8_TYPE, [ArrayType.Extent.DEFERRED,
                                              ArrayType.Extent.DEFERRED])
            if signature[-1] == "tmask":
                return ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED,
                                                ArrayType.Extent.DEFERRED])
            if signature[-1] == "dx":
                return REAL8_TYPE

        # Everything else defaults to integer
        return INTEGER_TYPE

    @staticmethod
    def _bring_external_symbols(read_write_info, symbol_table):
        from psyclone.parse import ModuleManager
        mod_man = ModuleManager.get()
        for module_name, signature in read_write_info.set_of_all_used_vars:
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
