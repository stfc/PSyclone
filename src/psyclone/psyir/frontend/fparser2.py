# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' This module provides the fparser2 to PSyIR front-end, it follows a
    Visitor Pattern to traverse relevant fparser2 nodes and contains the logic
    to transform each node into the equivalent PSyIR representation.'''

from collections import OrderedDict
from fparser.two import Fortran2003
from fparser.two.utils import walk_ast
from psyclone.psyGen import UnaryOperation, BinaryOperation, NaryOperation, \
    Schedule, Directive, CodeBlock, IfBlock, Reference, Literal, Loop, \
    KernelSchedule, Container, Assignment, Return, Array, InternalError, \
    GenerationError
from psyclone.psyir.symbols import DataSymbol, ContainerSymbol, SymbolError, \
    GlobalInterface, ArgumentInterface, UnresolvedInterface, LocalInterface

# The list of Fortran instrinsic functions that we know about (and can
# therefore distinguish from array accesses). These are taken from
# fparser.
FORTRAN_INTRINSICS = Fortran2003.Intrinsic_Name.function_names


class Fparser2Reader(object):
    '''
    Class to encapsulate the functionality for processing the fparser2 AST and
    convert the nodes to PSyIR.
    '''

    unary_operators = OrderedDict([
        ('+', UnaryOperation.Operator.PLUS),
        ('-', UnaryOperation.Operator.MINUS),
        ('.not.', UnaryOperation.Operator.NOT),
        ('abs', UnaryOperation.Operator.ABS),
        ('ceiling', UnaryOperation.Operator.CEIL),
        ('exp', UnaryOperation.Operator.EXP),
        ('log', UnaryOperation.Operator.LOG),
        ('log10', UnaryOperation.Operator.LOG10),
        ('sin', UnaryOperation.Operator.SIN),
        ('asin', UnaryOperation.Operator.ASIN),
        ('cos', UnaryOperation.Operator.COS),
        ('acos', UnaryOperation.Operator.ACOS),
        ('tan', UnaryOperation.Operator.TAN),
        ('atan', UnaryOperation.Operator.ATAN),
        ('sqrt', UnaryOperation.Operator.SQRT),
        ('real', UnaryOperation.Operator.REAL),
        ('int', UnaryOperation.Operator.INT)])

    binary_operators = OrderedDict([
        ('+', BinaryOperation.Operator.ADD),
        ('-', BinaryOperation.Operator.SUB),
        ('*', BinaryOperation.Operator.MUL),
        ('/', BinaryOperation.Operator.DIV),
        ('**', BinaryOperation.Operator.POW),
        ('==', BinaryOperation.Operator.EQ),
        ('.eq.', BinaryOperation.Operator.EQ),
        ('/=', BinaryOperation.Operator.NE),
        ('.ne.', BinaryOperation.Operator.NE),
        ('<=', BinaryOperation.Operator.LE),
        ('.le.', BinaryOperation.Operator.LE),
        ('<', BinaryOperation.Operator.LT),
        ('.lt.', BinaryOperation.Operator.LT),
        ('>=', BinaryOperation.Operator.GE),
        ('.ge.', BinaryOperation.Operator.GE),
        ('>', BinaryOperation.Operator.GT),
        ('.gt.', BinaryOperation.Operator.GT),
        ('.and.', BinaryOperation.Operator.AND),
        ('.or.', BinaryOperation.Operator.OR),
        ('sign', BinaryOperation.Operator.SIGN),
        ('size', BinaryOperation.Operator.SIZE),
        ('sum', BinaryOperation.Operator.SUM),
        ('max', BinaryOperation.Operator.MAX),
        ('min', BinaryOperation.Operator.MIN),
        ('mod', BinaryOperation.Operator.REM)])

    nary_operators = OrderedDict([
        ('max', NaryOperation.Operator.MAX),
        ('min', NaryOperation.Operator.MIN),
        ('sum', NaryOperation.Operator.SUM)])

    def __init__(self):
        from fparser.two import utils
        # Map of fparser2 node types to handlers (which are class methods)
        self.handlers = {
            Fortran2003.Assignment_Stmt: self._assignment_handler,
            Fortran2003.Name: self._name_handler,
            Fortran2003.Parenthesis: self._parenthesis_handler,
            Fortran2003.Part_Ref: self._part_ref_handler,
            Fortran2003.If_Stmt: self._if_stmt_handler,
            utils.NumberBase: self._number_handler,
            utils.BinaryOpBase: self._binary_op_handler,
            Fortran2003.End_Do_Stmt: self._ignore_handler,
            Fortran2003.End_Subroutine_Stmt: self._ignore_handler,
            Fortran2003.If_Construct: self._if_construct_handler,
            Fortran2003.Case_Construct: self._case_construct_handler,
            Fortran2003.Return_Stmt: self._return_handler,
            Fortran2003.UnaryOpBase: self._unary_op_handler,
            Fortran2003.Block_Nonlabel_Do_Construct:
                self._do_construct_handler,
            Fortran2003.Intrinsic_Function_Reference: self._intrinsic_handler,
            Fortran2003.Where_Construct: self._where_construct_handler,
            Fortran2003.Where_Stmt: self._where_construct_handler,
        }

    @staticmethod
    def nodes_to_code_block(parent, fp2_nodes):
        '''Create a CodeBlock for the supplied list of fparser2 nodes and then
        wipe the list. A CodeBlock is a node in the PSyIR (Schedule)
        that represents a sequence of one or more Fortran statements
        and/or expressions which PSyclone does not attempt to handle.

        :param parent: Node in the PSyclone AST to which to add this code \
                       block.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param fp2_nodes: list of fparser2 AST nodes constituting the \
                          code block.
        :type fp2_nodes: list of :py:class:`fparser.two.utils.Base`

        :returns: a CodeBlock instance.
        :rtype: :py:class:`psyclone.CodeBlock`

        '''
        if not fp2_nodes:
            return None

        # Determine whether this code block is a statement or an
        # expression. Statements always have a `Schedule` as parent
        # and expressions do not. The only unknown at this point are
        # directives whose structure are in discussion. Therefore, for
        # the moment, an exception is raised if a directive is found
        # as a parent.
        if isinstance(parent, Schedule):
            structure = CodeBlock.Structure.STATEMENT
        elif isinstance(parent, Directive):
            raise InternalError(
                "Fparser2Reader:nodes_to_code_block: A CodeBlock with "
                "a Directive as parent is not yet supported.")
        else:
            structure = CodeBlock.Structure.EXPRESSION

        code_block = CodeBlock(fp2_nodes, structure, parent=parent)
        parent.addchild(code_block)
        del fp2_nodes[:]
        return code_block

    @staticmethod
    def get_inputs_outputs(nodes):
        '''
        Identify variables that are inputs and outputs to the section of
        Fortran code represented by the supplied list of nodes in the
        fparser2 parse tree. Loop variables are ignored.

        :param nodes: list of Nodes in the fparser2 AST to analyse.
        :type nodes: list of :py:class:`fparser.two.utils.Base`

        :return: 3-tuple of list of inputs, list of outputs, list of in-outs
        :rtype: (list of str, list of str, list of str)
        '''
        from fparser.two.Fortran2003 import Assignment_Stmt, Part_Ref, \
            Data_Ref, If_Then_Stmt, Array_Section
        readers = set()
        writers = set()
        readwrites = set()
        # A dictionary of all array accesses that we encounter - used to
        # sanity check the readers and writers we identify.
        all_array_refs = {}

        # Loop over a flat list of all the nodes in the supplied region
        for node in walk_ast(nodes):

            if isinstance(node, Assignment_Stmt):
                # Found lhs = rhs
                structure_name_str = None

                lhs = node.items[0]
                rhs = node.items[2]
                # Do RHS first as we cull readers after writers but want to
                # keep a = a + ... as the RHS is computed before assigning
                # to the LHS
                for node2 in walk_ast([rhs]):
                    if isinstance(node2, Part_Ref):
                        name = node2.items[0].string
                        if name.upper() not in FORTRAN_INTRINSICS:
                            if name not in writers:
                                readers.add(name)
                    if isinstance(node2, Data_Ref):
                        # TODO we need a robust implementation - issue #309.
                        raise NotImplementedError(
                            "get_inputs_outputs: derived-type references on "
                            "the RHS of assignments are not yet supported.")
                # Now do LHS
                if isinstance(lhs, Data_Ref):
                    # This is a structure which contains an array access.
                    structure_name_str = lhs.items[0].string
                    writers.add(structure_name_str)
                    lhs = lhs.items[1]
                if isinstance(lhs, (Part_Ref, Array_Section)):
                    # This is an array reference
                    name_str = lhs.items[0].string
                    if structure_name_str:
                        # Array ref is part of a derived type
                        name_str = "{0}%{1}".format(structure_name_str,
                                                    name_str)
                        structure_name_str = None
                    writers.add(name_str)
            elif isinstance(node, If_Then_Stmt):
                # Check for array accesses in IF statements
                array_refs = walk_ast([node], [Part_Ref])
                for ref in array_refs:
                    name = ref.items[0].string
                    if name.upper() not in FORTRAN_INTRINSICS:
                        if name not in writers:
                            readers.add(name)
            elif isinstance(node, Part_Ref):
                # Keep a record of all array references to check that we
                # haven't missed anything. Once #309 is done we should be
                # able to get rid of this check.
                name = node.items[0].string
                if name.upper() not in FORTRAN_INTRINSICS and \
                   name not in all_array_refs:
                    all_array_refs[name] = node
            elif node:
                # TODO #309 handle array accesses in other contexts, e.g. as
                # loop bounds in DO statements.
                pass

        # Sanity check that we haven't missed anything. To be replaced when
        # #309 is done.
        accesses = list(readers) + list(writers)
        for name, node in all_array_refs.items():
            if name not in accesses:
                # A matching bare array access hasn't been found but it
                # might have been part of a derived-type access so check
                # for that.
                found = False
                for access in accesses:
                    if "%"+name in access:
                        found = True
                        break
                if not found:
                    raise InternalError(
                        "Array '{0}' present in source code ('{1}') but not "
                        "identified as being read or written.".
                        format(name, str(node)))
        # Now we check for any arrays that are both read and written
        readwrites = readers & writers
        # Remove them from the readers and writers sets
        readers = readers - readwrites
        writers = writers - readwrites
        # Convert sets to lists and sort so that we get consistent results
        # between Python versions (for testing)
        rlist = list(readers)
        rlist.sort()
        wlist = list(writers)
        wlist.sort()
        rwlist = list(readwrites)
        rwlist.sort()

        return (rlist, wlist, rwlist)

    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty KernelSchedule.

        :param str name: Name of the subroutine represented by the kernel.
        :returns: New KernelSchedule empty object.
        :rtype: py:class:`psyclone.psyGen.KernelSchedule`
        '''
        return KernelSchedule(name)

    def generate_container(self, module_ast):
        '''
        Create a Container from the supplied fparser2 module AST.

        :param module_ast: fparser2 AST of the full module.
        :type module_ast: :py:class:`fparser.two.Fortran2003.Program`

        :returns: PSyIR container representing the given module_ast.
        :rtype: :py:class:`psyclone.psyGen.Container`

        :raises GenerationError: unable to generate a Container from the \
                                 provided fpaser2 parse tree.
        '''
        # Assume just 1 Fortran module definition in the file
        if len(module_ast.content) > 1:
            raise GenerationError(
                "Could not process {0}. Just one module definition per file "
                "supported.".format(str(module_ast)))

        module = module_ast.content[0]
        mod_content = module.content
        mod_name = str(mod_content[0].items[1])

        # Create a container to capture the module information
        new_container = Container(mod_name)

        # Parse the declarations if it has any
        if isinstance(mod_content[1], Fortran2003.Specification_Part):
            decl_list = mod_content[1].content
            self.process_declarations(new_container, decl_list, [])

        return new_container

    def generate_schedule(self, name, module_ast):
        '''
        Create a KernelSchedule from the supplied fparser2 AST.

        :param str name: name of the subroutine represented by the kernel.
        :param module_ast: fparser2 AST of the full module where the kernel \
                           code is located.
        :type module_ast: :py:class:`fparser.two.Fortran2003.Program`

        :returns: PSyIR schedule representing the kernel.
        :rtype: :py:class:`psyclone.psyGen.KernelSchedule`

        :raises GenerationError: unable to generate a kernel schedule from \
                                 the provided fpaser2 parse tree.
        '''
        def first_type_match(nodelist, typekind):
            '''
            Returns the first instance of the specified type in the given
            node list.

            :param list nodelist: List of fparser2 nodes.
            :param type typekind: The fparse2 Type we are searching for.
            '''
            for node in nodelist:
                if isinstance(node, typekind):
                    return node
            raise ValueError  # Type not found

        def search_subroutine(nodelist, searchname):
            '''
            Returns the first instance of the specified subroutine in the given
            node list.

            :param list nodelist: List of fparser2 nodes.
            :param str searchname: Name of the subroutine we are searching for.
            '''
            for node in nodelist:
                if (isinstance(node, Fortran2003.Subroutine_Subprogram) and
                        str(node.content[0].get_name()) == searchname):
                    return node
            raise ValueError  # Subroutine not found

        new_schedule = self._create_schedule(name)

        # Generate the Container of the module enclosing the Kernel
        new_container = self.generate_container(module_ast)
        mod_content = module_ast.content[0].content

        new_schedule.parent = new_container
        new_container.children = [new_schedule]

        try:
            subroutines = first_type_match(mod_content,
                                           Fortran2003.Module_Subprogram_Part)
            # TODO remove once fparser/#102 is done
            # pylint: disable=protected-access
            module_ast.content[0]._parent = module_ast
            for item in mod_content:
                item._parent = module_ast.content[0]
            subroutines._parent = mod_content
            subroutine = search_subroutine(subroutines.content, name)
        except (ValueError, IndexError):
            raise GenerationError("Unexpected kernel AST. Could not find "
                                  "subroutine: {0}".format(name))

        try:
            sub_spec = first_type_match(subroutine.content,
                                        Fortran2003.Specification_Part)
            # TODO remove once fparser/#102 is done
            # pylint: disable=protected-access
            sub_spec._parent = subroutine
            decl_list = sub_spec.content
            # TODO this if test can be removed once fparser/#211 is fixed
            # such that routine arguments are always contained in a
            # Dummy_Arg_List, even if there's only one of them.
            from fparser.two.Fortran2003 import Dummy_Arg_List
            if isinstance(subroutine.content[0].items[2], Dummy_Arg_List):
                arg_list = subroutine.content[0].items[2].items
            else:
                # Routine has no arguments
                arg_list = []
        except ValueError:
            # Subroutine without declarations, continue with empty lists.
            decl_list = []
            arg_list = []
        finally:
            self.process_declarations(new_schedule, decl_list, arg_list)

        try:
            sub_exec = first_type_match(subroutine.content,
                                        Fortran2003.Execution_Part)
            # TODO remove once fparser/#102 is done
            # pylint: disable=protected-access
            sub_exec._parent = subroutine
        except ValueError:
            pass
        else:
            self.process_nodes(new_schedule, sub_exec.content, sub_exec)

        return new_schedule

    @staticmethod
    def _parse_dimensions(dimensions, symbol_table):
        '''
        Parse the fparser dimension attribute into a shape list with
        the extent of each dimension.

        :param dimensions: fparser dimension attribute
        :type dimensions: \
            :py:class:`fparser.two.Fortran2003.Dimension_Attr_Spec`
        :param symbol_table: Symbol table of the declaration context.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :returns: Shape of the attribute in column-major order (leftmost \
                  index is contiguous in memory). Each entry represents \
                  an array dimension. If it is 'None' the extent of that \
                  dimension is unknown, otherwise it holds an integer \
                  with the extent. If it is an empty list then the symbol \
                  represents a scalar.
        :rtype: list
        '''
        shape = []

        # Traverse shape specs in Depth-first-search order
        for dim in walk_ast([dimensions], [Fortran2003.Assumed_Shape_Spec,
                                           Fortran2003.Explicit_Shape_Spec,
                                           Fortran2003.Assumed_Size_Spec]):

            if isinstance(dim, Fortran2003.Assumed_Shape_Spec):
                shape.append(None)

            elif isinstance(dim, Fortran2003.Explicit_Shape_Spec):
                def _unsupported_type_error(dimensions):
                    raise NotImplementedError(
                        "Could not process {0}. Only scalar integer literals"
                        " or symbols are supported for explicit shape array "
                        "declarations.".format(dimensions))
                if isinstance(dim.items[1],
                              Fortran2003.Int_Literal_Constant):
                    shape.append(int(dim.items[1].items[0]))
                elif isinstance(dim.items[1], Fortran2003.Name):
                    # Fortran does not regulate the order in which variables
                    # may be declared so it's possible for the shape
                    # specification of an array to reference variables that
                    # come later in the list of declarations. The reference
                    # may also be to a symbol present in a parent symbol table
                    # (e.g. if the variable is declared in an outer, module
                    # scope).
                    dim_name = dim.items[1].string.lower()
                    try:
                        sym = symbol_table.lookup(dim_name)
                        if sym.datatype != 'integer' or sym.shape:
                            _unsupported_type_error(dimensions)
                    except KeyError:
                        # We haven't seen this symbol before so create a new
                        # one with a deferred interface (since we don't
                        # currently know where it is declared).
                        sym = DataSymbol(dim_name, "integer",
                                         interface=UnresolvedInterface())
                        symbol_table.add(sym)
                    shape.append(sym)
                else:
                    _unsupported_type_error(dimensions)

            elif isinstance(dim, Fortran2003.Assumed_Size_Spec):
                raise NotImplementedError(
                    "Could not process {0}. Assumed-size arrays"
                    " are not supported.".format(dimensions))

            else:
                raise InternalError(
                    "Reached end of loop body and array-shape specification "
                    "{0} has not been handled.".format(type(dim)))

        return shape

    def process_declarations(self, parent, nodes, arg_list):
        '''
        Transform the variable declarations in the fparser2 parse tree into
        symbols in the symbol table of the PSyIR parent node.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyGen.KernelSchedule`
        :param nodes: fparser2 AST nodes to search for declaration statements.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param arg_list: fparser2 AST node containing the argument list.
        :type arg_list: :py:class:`fparser.Fortran2003.Dummy_Arg_List`

        :raises NotImplementedError: the provided declarations contain \
                                     attributes which are not supported yet.
        :raises GenerationError: if the parse tree for a USE statement does \
                                 not have the expected structure.
        :raises SymbolError: if a declaration is found for a Symbol that is \
                    already in the symbol table with a defined interface.
        '''
        # Look at any USE statments
        for decl in walk_ast(nodes, [Fortran2003.Use_Stmt]):

            # Check that the parse tree is what we expect
            if len(decl.items) != 5:
                # We can't just do str(decl) as that also checks that items
                # is of length 5
                text = ""
                for item in decl.items:
                    if item:
                        text += str(item)
                raise GenerationError(
                    "Expected the parse tree for a USE statement to contain "
                    "5 items but found {0} for '{1}'".format(len(decl.items),
                                                             text))

            mod_name = str(decl.items[2])

            # Add the module symbol in the symbol table
            container = ContainerSymbol(mod_name)
            parent.symbol_table.add(container)

            # Create a 'deferred' symbol for each element in the ONLY clause.
            if isinstance(decl.items[4], Fortran2003.Only_List):
                for name in decl.items[4].items:
                    parent.symbol_table.add(
                        DataSymbol(str(name).lower(), datatype='deferred',
                                   interface=GlobalInterface(container)))

        for decl in walk_ast(nodes, [Fortran2003.Type_Declaration_Stmt]):
            (type_spec, attr_specs, entities) = decl.items

            # Parse type_spec, currently just 'real', 'integer', 'logical' and
            # 'character' intrinsic types are supported.
            datatype = None
            if isinstance(type_spec, Fortran2003.Intrinsic_Type_Spec):
                if str(type_spec.items[0]).lower() == 'real':
                    datatype = 'real'
                elif str(type_spec.items[0]).lower() == 'integer':
                    datatype = 'integer'
                elif str(type_spec.items[0]).lower() == 'character':
                    datatype = 'character'
                elif str(type_spec.items[0]).lower() == 'logical':
                    datatype = 'boolean'
                # Check for a KIND specification
                precision = self._process_kind_selector(parent.symbol_table,
                                                        type_spec)
            if datatype is None:
                raise NotImplementedError(
                    "Could not process {0}. Only 'real', 'integer', "
                    "'logical' and 'character' intrinsic types are "
                    "supported.".format(str(decl.items)))

            # Parse declaration attributes:
            # 1) If no dimension attribute is provided, it defaults to scalar.
            attribute_shape = []
            # 2) If no intent attribute is provided, it is provisionally
            # marked as a local variable (when the argument list is parsed,
            # arguments with no explicit intent are updated appropriately).
            interface = LocalInterface()
            # 3) Record initialized constant values
            has_constant_value = False
            if attr_specs:
                for attr in attr_specs.items:
                    if isinstance(attr, Fortran2003.Attr_Spec):
                        normalized_string = str(attr).lower().replace(' ', '')
                        if "intent(in)" in normalized_string:
                            interface = ArgumentInterface(
                                ArgumentInterface.Access.READ)
                        elif "intent(out)" in normalized_string:
                            interface = ArgumentInterface(
                                ArgumentInterface.Access.WRITE)
                        elif "intent(inout)" in normalized_string:
                            interface = ArgumentInterface(
                                ArgumentInterface.Access.READWRITE)
                        elif normalized_string == "parameter":
                            # Flag the existence of a constant value in the RHS
                            has_constant_value = True
                        else:
                            raise NotImplementedError(
                                "Could not process {0}. Unrecognized "
                                "attribute '{1}'.".format(decl.items,
                                                          str(attr)))
                    elif isinstance(attr, Fortran2003.Dimension_Attr_Spec):
                        attribute_shape = \
                            self._parse_dimensions(attr, parent.symbol_table)
                    else:
                        raise NotImplementedError(
                            "Could not process {0}. Unrecognized attribute "
                            "type {1}.".format(decl.items, str(type(attr))))

            # Parse declarations RHS and declare new symbol into the
            # parent symbol table for each entity found.
            for entity in entities.items:
                (name, array_spec, char_len, initialisation) = entity.items
                ct_value = None

                # If the entity has an array-spec shape, it has priority.
                # Otherwise use the declaration attribute shape.
                if array_spec is not None:
                    entity_shape = \
                        self._parse_dimensions(array_spec, parent.symbol_table)
                else:
                    entity_shape = attribute_shape

                if initialisation:
                    if has_constant_value:
                        # If it is a parameter, get the initialization value
                        expr = initialisation.items[1]
                        if isinstance(expr, Fortran2003.NumberBase):
                            value_str = expr.items[0]
                            # Convert string literal to the Symbol datatype
                            ct_value = DataSymbol.mapping[datatype](value_str)
                        else:
                            raise NotImplementedError(
                                "Could not process {0}. Initialisations with "
                                "static expressions are not supported."
                                "".format(decl.items))
                    else:
                        raise NotImplementedError(
                            "Could not process {0}. Initialisations on the"
                            " declaration statements are only supported for "
                            "parameter declarations.".format(decl.items))

                if char_len is not None:
                    raise NotImplementedError(
                        "Could not process {0}. Character length "
                        "specifications are not supported."
                        "".format(decl.items))

                sym_name = str(name).lower()

                if sym_name not in parent.symbol_table:
                    parent.symbol_table.add(DataSymbol(sym_name, datatype,
                                                       shape=entity_shape,
                                                       constant_value=ct_value,
                                                       interface=interface,
                                                       precision=precision))
                else:
                    # The symbol table already contains an entry with this name
                    # so update its interface information.
                    sym = parent.symbol_table.lookup(sym_name)
                    if not sym.unresolved_interface:
                        raise SymbolError(
                            "Symbol '{0}' already present in SymbolTable with "
                            "a defined interface ({1}).".format(
                                sym_name, str(sym.interface)))
                    sym.interface = interface

        try:
            arg_symbols = []
            # Ensure each associated symbol has the correct interface info.
            for arg_name in [x.string.lower() for x in arg_list]:
                symbol = parent.symbol_table.lookup(arg_name)
                if symbol.is_local:
                    # We didn't previously know that this Symbol was an
                    # argument (as it had no 'intent' qualifier). Mark
                    # that it is an argument by specifying its interface.
                    # A Fortran argument has intent(inout) by default
                    symbol.interface = ArgumentInterface(
                        ArgumentInterface.Access.READWRITE)
                arg_symbols.append(symbol)
            # Now that we've updated the Symbols themselves, set the
            # argument list
            parent.symbol_table.specify_argument_list(arg_symbols)
        except KeyError:
            raise InternalError("The kernel argument "
                                "list '{0}' does not match the variable "
                                "declarations for fparser nodes {1}."
                                "".format(str(arg_list), nodes))

        # fparser2 does not always handle Statement Functions correctly, this
        # loop checks for Stmt_Functions that should be an array statement
        # and recovers them, otherwise it raises an error as currently
        # Statement Functions are not supported in PSyIR.
        for stmtfn in walk_ast(nodes, [Fortran2003.Stmt_Function_Stmt]):
            (fn_name, arg_list, scalar_expr) = stmtfn.items
            try:
                symbol = parent.symbol_table.lookup(fn_name.string.lower())
                if symbol.is_array:
                    # This is an array assignment wrongly categorized as a
                    # statement_function by fparser2.
                    array_name = fn_name
                    array_subscript = arg_list.items

                    assignment_rhs = scalar_expr

                    # Create assingment node
                    assignment = Assignment(parent=parent)
                    parent.addchild(assignment)

                    # Build lhs
                    lhs = Array(array_name.string.lower(), parent=assignment)
                    self.process_nodes(parent=lhs, nodes=array_subscript,
                                       nodes_parent=arg_list)
                    assignment.addchild(lhs)

                    # Build rhs
                    self.process_nodes(parent=assignment,
                                       nodes=[assignment_rhs],
                                       nodes_parent=scalar_expr)
                else:
                    raise InternalError(
                        "Could not process '{0}'. Symbol '{1}' is in the"
                        " SymbolTable but it is not an array as expected, so"
                        " it can not be recovered as an array assignment."
                        "".format(str(stmtfn), symbol.name))
            except KeyError:
                raise NotImplementedError(
                    "Could not process '{0}'. Statement Function declarations "
                    "are not supported.".format(str(stmtfn)))

    @staticmethod
    def _process_kind_selector(symbol_table, type_spec):
        '''
        Processes the fparser2 parse tree of the type specification of a
        variable declaration in order to extract KIND information. This
        information is used to determine the precision of the variable (as
        supplied to the DataSymbol constructor).

        :param symbol_table: the SymbolTable associated with the code \
            being processed.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param type_spec: the fparser2 parse tree of the type specification.
        :type type_spec: :py:class:`fparser.two.Fortran2003.Intrinsic_Type_Spec

        :returns: the precision associated with the type specification.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol.Precision` or \
            :py:class:`psyclone.psyir.symbols.DataSymbol` or int or NoneType

        :raises NotImplementedError: if a KIND intrinsic is found with an \
            argument other than a real or integer literal.
        :raises NotImplementedError: if we have `kind=xxx` but cannot find \
            a valid variable name.
        '''
        if not isinstance(type_spec.items[1], Fortran2003.Kind_Selector):
            return None
        # The KIND() intrinsic itself is Fortran specific and has no direct
        # representation in the PSyIR. We therefore can't use
        # self.process_nodes() here.
        kind_selector = type_spec.items[1]
        intrinsics = walk_ast(kind_selector.items,
                              [Fortran2003.Intrinsic_Function_Reference])
        if intrinsics and isinstance(intrinsics[0].items[0],
                                     Fortran2003.Intrinsic_Name) and \
           str(intrinsics[0].items[0]).lower() == "kind":
            # We have kind=KIND(X) where X may be of any intrinsic type. It
            # may be a scalar or an array. items[1] is an
            # Actual_Arg_Spec_List with the first entry being the argument.
            kind_arg = intrinsics[0].items[1].items[0]

            # We currently only support literals as arguments to KIND
            if isinstance(kind_arg, (Fortran2003.Int_Literal_Constant,
                                     Fortran2003.Real_Literal_Constant)):

                if len(kind_arg.items) > 1 and kind_arg.items[1]:
                    # The literal has an explicit kind specifier (e.g. 1.0_wp)
                    # so return a Symbol representing it (e.g.'wp').
                    return Fparser2Reader._kind_symbol_from_name(
                        str(kind_arg.items[1]), symbol_table)

                if isinstance(kind_arg, Fortran2003.Real_Literal_Constant):
                    import re
                    if re.search("d[0-9]", str(kind_arg).lower()):
                        return DataSymbol.Precision.DOUBLE
                    return DataSymbol.Precision.SINGLE

                if isinstance(kind_arg, Fortran2003.Int_Literal_Constant):
                    # An integer with no explict kind specifier must be of
                    # default precision
                    return DataSymbol.Precision.SINGLE

            raise NotImplementedError(
                "Only real and integer literals are supported "
                "as arguments to the KIND intrinsic but found '{0}' in: "
                "{1}".format(type(kind_arg).__name__, str(kind_selector)))

        # We have kind=kind-param
        kind_names = walk_ast(kind_selector.items, [Fortran2003.Name])
        if not kind_names:
            raise NotImplementedError(
                "Failed to find valid Name in Fortran Kind "
                "Selector: '{0}'".format(str(kind_selector)))
        return Fparser2Reader._kind_symbol_from_name(str(kind_names[0]),
                                                     symbol_table)

    @staticmethod
    def _kind_symbol_from_name(name, symbol_table):
        '''
        Utility method that returns a Symbol representing the named KIND
        parameter. If the supplied Symbol Table does not contain an appropriate
        entry then one is created. If it does contain a matching entry then
        its datatype must be 'integer' or 'deferred'. If the latter then the
        fact that we now know that this Symbol represents a KIND parameter
        means that we can change the datatype to be 'integer'.

        :param str name: the name of the variable holding the KIND value.
        :param symbol_table: the Symbol Table associated with the code being\
                             processed.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the Symbol representing the KIND parameter.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the Symbol Table already contains an entry for \
                       `name` and its datatype is not 'integer' or 'deferred'.
        '''
        lower_name = name.lower()
        try:
            kind_symbol = symbol_table.lookup(lower_name)
            if kind_symbol.datatype not in ["integer", "deferred"]:
                raise TypeError(
                    "SymbolTable already contains an entry for "
                    "variable '{0}' used as a kind parameter but its "
                    "type ('{1}') is not 'deferred' or 'integer'.".
                    format(lower_name, kind_symbol.datatype))
            # A KIND parameter must be of type integer so set it here (in case
            # it was previously 'deferred')
            kind_symbol.datatype = "integer"
        except KeyError:
            # The SymbolTable does not contain an entry for this kind parameter
            # so create one. We specify an UnresolvedInterface as we don't
            # currently know how this symbol is brought into scope.
            kind_symbol = DataSymbol(lower_name, "integer",
                                     interface=UnresolvedInterface())
            symbol_table.add(kind_symbol)
        return kind_symbol

    # TODO remove nodes_parent argument once fparser2 AST contains
    # parent information (fparser/#102).
    def process_nodes(self, parent, nodes, nodes_parent):
        '''
        Create the PSyIR of the supplied list of nodes in the
        fparser2 AST. Currently also inserts parent information back
        into the fparser2 AST. This is a workaround until fparser2
        itself generates and stores this information.

        :param parent: Parent node in the PSyIR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param nodes: List of sibling nodes in fparser2 AST.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param nodes_parent: the parent of the supplied list of nodes in \
                             the fparser2 AST.
        :type nodes_parent: :py:class:`fparser.two.utils.Base`
        '''
        code_block_nodes = []
        for child in nodes:
            # TODO remove this line once fparser2 contains parent
            # information (fparser/#102)
            child._parent = nodes_parent  # Retro-fit parent info

            try:
                psy_child = self._create_child(child, parent)
            except NotImplementedError:
                # If child type implementation not found, add them on the
                # ongoing code_block node list.
                code_block_nodes.append(child)
            else:
                if psy_child:
                    self.nodes_to_code_block(parent, code_block_nodes)
                    parent.addchild(psy_child)
                # If psy_child is not initialised but it didn't produce a
                # NotImplementedError, it means it is safe to ignore it.

        # Complete any unfinished code-block
        self.nodes_to_code_block(parent, code_block_nodes)

    def _create_child(self, child, parent=None):
        '''
        Create a PSyIR node representing the supplied fparser 2 node.

        :param child: node in fparser2 AST.
        :type child: :py:class:`fparser.two.utils.Base`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :raises NotImplementedError: There isn't a handler for the provided \
                child type.
        :returns: Returns the PSyIR representation of child, which can be a \
                  single node, a tree of nodes or None if the child can be \
                  ignored.
        :rtype: :py:class:`psyclone.psyGen.Node` or NoneType
        '''
        handler = self.handlers.get(type(child))
        if handler is None:
            # If the handler is not found then check with the first
            # level parent class. This is done to simplify the
            # handlers map when multiple fparser2 types can be
            # processed with the same handler. (e.g. Subclasses of
            # BinaryOpBase: Mult_Operand, Add_Operand, Level_2_Expr,
            # ... can use the same handler.)
            generic_type = type(child).__bases__[0]
            handler = self.handlers.get(generic_type)
            if not handler:
                raise NotImplementedError()
        return handler(child, parent)

    def _ignore_handler(self, *_):
        '''
        This handler returns None indicating that the associated
        fparser2 node can be ignored.

        Note that this method contains ignored arguments to comform with
        the handler(node, parent) method interface.

        :returns: None
        :rtype: NoneType
        '''
        return None

    def _create_loop(self, parent, variable_name):
        '''
        Create a Loop instance. This is done outside _do_construct_handler
        because some APIs may want to instantiate a specialised Loop.

        :param parent: the parent of the node.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param str variable_name: name of the iteration variable.

        :return: a new Loop instance.
        :rtype: :py:class:`psyclone.psyGen.Loop`

        '''
        return Loop(parent=parent, variable_name=variable_name)

    def _process_loopbody(self, loop_body, node):
        ''' Process the loop body. This is done outside _do_construct_handler
        because some APIs may want to perform specialised actions. By default
        continue processing the tree nodes inside the loop body.

        :param loop_body: Schedule representing the body of the loop.
        :type loop_body: :py:class:`psyclone.psyGen.Schedule`
        :param node: fparser loop node being processed.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
        '''
        # Process loop body (ignore 'do' and 'end do' statements with [1:-1])
        self.process_nodes(parent=loop_body, nodes=node.content[1:-1],
                           nodes_parent=node)

    def _do_construct_handler(self, node, parent):
        '''
        Transforms a fparser2 Do Construct into its PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Loop`

        :raises InternalError: if the fparser2 tree has an unexpected \
            structure.
        '''
        ctrl = walk_ast(node.content, [Fortran2003.Loop_Control])
        if not ctrl:
            raise InternalError(
                "Unrecognised form of DO loop - failed to find Loop_Control "
                "element in the node '{0}'.".format(str(node)))
        if ctrl[0].items[0]:
            # If this is a DO WHILE then the first element of items will not
            # be None. (See `fparser.two.Fortran2003.Loop_Control`.)
            # TODO #359 DO WHILE's are currently just put into CodeBlocks
            # rather than being properly described in the PSyIR.
            raise NotImplementedError()

        # Second element of items member of Loop Control is itself a tuple
        # containing:
        #   Loop variable, [start value expression, end value expression, step
        #   expression]
        # Loop variable will be an instance of Fortran2003.Name
        loop_var = str(ctrl[0].items[1][0])
        variable_name = str(loop_var)
        loop = self._create_loop(parent, variable_name)
        loop._ast = node

        # Get the loop limits. These are given in a list which is the second
        # element of a tuple which is itself the second element of the items
        # tuple:
        # (None, (Name('jk'), [Int_Literal_Constant('1', None), Name('jpk'),
        #                      Int_Literal_Constant('1', None)]), None)
        limits_list = ctrl[0].items[1][1]

        # Start expression child
        self.process_nodes(parent=loop, nodes=[limits_list[0]],
                           nodes_parent=ctrl)

        # Stop expression child
        self.process_nodes(parent=loop, nodes=[limits_list[1]],
                           nodes_parent=ctrl)

        # Step expression child
        if len(limits_list) == 3:
            self.process_nodes(parent=loop, nodes=[limits_list[2]],
                               nodes_parent=ctrl)
        else:
            # Default loop increment is 1
            default_step = Literal("1", parent=loop)
            loop.addchild(default_step)

        # Create Loop body Schedule
        loop_body = Schedule(parent=loop)
        loop_body._ast = node
        loop.addchild(loop_body)
        self._process_loopbody(loop_body, node)

        return loop

    def _if_construct_handler(self, node, parent):
        '''
        Transforms an fparser2 If_Construct to the PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: :py:class:`fparser.two.Fortran2003.If_Construct`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.IfBlock`
        :raises InternalError: If the fparser2 tree has an unexpected \
            structure.
        '''

        # Check that the fparser2 parsetree has the expected structure
        if not isinstance(node.content[0], Fortran2003.If_Then_Stmt):
            raise InternalError(
                "Failed to find opening if then statement in: "
                "{0}".format(str(node)))
        if not isinstance(node.content[-1], Fortran2003.End_If_Stmt):
            raise InternalError(
                "Failed to find closing end if statement in: "
                "{0}".format(str(node)))

        # Search for all the conditional clauses in the If_Construct
        clause_indices = []
        for idx, child in enumerate(node.content):
            child._parent = node  # Retrofit parent info
            if isinstance(child, (Fortran2003.If_Then_Stmt,
                                  Fortran2003.Else_Stmt,
                                  Fortran2003.Else_If_Stmt,
                                  Fortran2003.End_If_Stmt)):
                clause_indices.append(idx)

        # Deal with each clause: "if", "else if" or "else".
        ifblock = None
        currentparent = parent
        num_clauses = len(clause_indices) - 1
        for idx in range(num_clauses):
            start_idx = clause_indices[idx]
            end_idx = clause_indices[idx+1]
            clause = node.content[start_idx]

            if isinstance(clause, (Fortran2003.If_Then_Stmt,
                                   Fortran2003.Else_If_Stmt)):
                # If it's an 'IF' clause just create an IfBlock, otherwise
                # it is an 'ELSE' clause and it needs an IfBlock annotated
                # with 'was_elseif' inside a Schedule.
                newifblock = None
                if isinstance(clause, Fortran2003.If_Then_Stmt):
                    ifblock = IfBlock(parent=currentparent)
                    ifblock.ast = node  # Keep pointer to fpaser2 AST
                    newifblock = ifblock
                else:
                    elsebody = Schedule(parent=currentparent)
                    currentparent.addchild(elsebody)
                    newifblock = IfBlock(parent=elsebody,
                                         annotations=['was_elseif'])
                    elsebody.addchild(newifblock)

                    # Keep pointer to fpaser2 AST
                    elsebody.ast = node.content[start_idx]
                    newifblock.ast = node.content[start_idx]

                # Create condition as first child
                self.process_nodes(parent=newifblock,
                                   nodes=[clause.items[0]],
                                   nodes_parent=node)

                # Create if-body as second child
                ifbody = Schedule(parent=ifblock)
                ifbody.ast = node.content[start_idx + 1]
                ifbody.ast_end = node.content[end_idx - 1]
                newifblock.addchild(ifbody)
                self.process_nodes(parent=ifbody,
                                   nodes=node.content[start_idx + 1:end_idx],
                                   nodes_parent=node)

                currentparent = newifblock

            elif isinstance(clause, Fortran2003.Else_Stmt):
                if not idx == num_clauses - 1:
                    raise InternalError(
                        "Else clause should only be found next to last "
                        "clause, but found {0}".format(node.content))
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                elsebody.ast = node.content[start_idx]
                elsebody.ast_end = node.content[end_idx]
                self.process_nodes(parent=elsebody,
                                   nodes=node.content[start_idx + 1:end_idx],
                                   nodes_parent=node)
            else:
                raise InternalError(
                    "Only fparser2 If_Then_Stmt, Else_If_Stmt and Else_Stmt "
                    "are expected, but found {0}.".format(clause))

        return ifblock

    def _if_stmt_handler(self, node, parent):
        '''
        Transforms an fparser2 If_Stmt to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.If_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.IfBlock`
        '''
        ifblock = IfBlock(parent=parent, annotations=['was_single_stmt'])
        ifblock.ast = node
        self.process_nodes(parent=ifblock, nodes=[node.items[0]],
                           nodes_parent=node)
        ifbody = Schedule(parent=ifblock)
        ifblock.addchild(ifbody)
        self.process_nodes(parent=ifbody, nodes=[node.items[1]],
                           nodes_parent=node)
        return ifblock

    def _case_construct_handler(self, node, parent):
        '''
        Transforms an fparser2 Case_Construct to the PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: :py:class:`fparser.two.Fortran2003.Case_Construct`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.IfBlock`

        :raises InternalError: If the fparser2 tree has an unexpected \
            structure.
        :raises NotImplementedError: If the fparser2 tree contains an \
            unsupported structure and should be placed in a CodeBlock.

        '''
        # Check that the fparser2 parsetree has the expected structure
        if not isinstance(node.content[0], Fortran2003.Select_Case_Stmt):
            raise InternalError(
                "Failed to find opening case statement in: "
                "{0}".format(str(node)))
        if not isinstance(node.content[-1], Fortran2003.End_Select_Stmt):
            raise InternalError(
                "Failed to find closing case statement in: "
                "{0}".format(str(node)))

        # Search for all the CASE clauses in the Case_Construct. We do this
        # because the fp2 parse tree has a flat structure at this point with
        # the clauses being siblings of the contents of the clauses. The
        # final index in this list will hold the position of the end-select
        # statement.
        clause_indices = []
        selector = None
        # The position of the 'case default' clause, if any
        default_clause_idx = None
        for idx, child in enumerate(node.content):
            child._parent = node  # Retrofit parent info
            if isinstance(child, Fortran2003.Select_Case_Stmt):
                selector = child.items[0]
            if isinstance(child, Fortran2003.Case_Stmt):
                if not isinstance(child.items[0], Fortran2003.Case_Selector):
                    raise InternalError(
                        "Unexpected parse tree structure. Expected child of "
                        "Case_Stmt to be a Case_Selector but got: '{0}'".
                        format(type(child.items[0]).__name__))
                case_expression = child.items[0].items[0]
                if case_expression is None:
                    # This is a 'case default' clause - store its position.
                    # We do this separately as this clause is special and
                    # will be added as a final 'else'.
                    default_clause_idx = idx
                clause_indices.append(idx)
            if isinstance(child, Fortran2003.End_Select_Stmt):
                clause_indices.append(idx)

        # Deal with each Case_Stmt
        rootif = None
        currentparent = parent
        num_clauses = len(clause_indices) - 1
        for idx in range(num_clauses):
            # Skip the 'default' clause for now because we handle it last
            if clause_indices[idx] == default_clause_idx:
                continue
            start_idx = clause_indices[idx]
            end_idx = clause_indices[idx+1]
            clause = node.content[start_idx]
            case = clause.items[0]

            ifblock = IfBlock(parent=currentparent,
                              annotations=['was_case'])
            if idx == 0:
                # If this is the first IfBlock then have it point to
                # the original SELECT CASE in the parse tree
                ifblock.ast = node
            else:
                # Otherwise, this IfBlock represents a CASE clause in the
                # Fortran and so we point to the parse tree of the content
                # of the clause.
                ifblock.ast = node.content[start_idx + 1]
                ifblock.ast_end = node.content[end_idx - 1]

            # Process the logical expression
            self._process_case_value_list(selector,
                                          case.items[0].items,
                                          case.items[0], ifblock)

            # Add If_body
            ifbody = Schedule(parent=ifblock)
            self.process_nodes(parent=ifbody,
                               nodes=node.content[start_idx + 1:
                                                  end_idx],
                               nodes_parent=node)
            ifblock.addchild(ifbody)
            ifbody.ast = node.content[start_idx + 1]
            ifbody.ast_end = node.content[end_idx - 1]

            if rootif:
                # If rootif is already initialised we chain the new
                # case in the last else branch.
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                elsebody.addchild(ifblock)
                ifblock.parent = elsebody
                elsebody.ast = node.content[start_idx + 1]
                elsebody.ast_end = node.content[end_idx - 1]
            else:
                rootif = ifblock

            currentparent = ifblock

        if default_clause_idx:
            # Finally, add the content of the 'default' clause as a last
            # 'else' clause.
            elsebody = Schedule(parent=currentparent)
            start_idx = default_clause_idx
            # Find the next 'case' clause that occurs after 'case default'
            # (if any)
            end_idx = -1
            for idx in clause_indices:
                if idx > default_clause_idx:
                    end_idx = idx
                    break
            self.process_nodes(parent=elsebody,
                               nodes=node.content[start_idx + 1:
                                                  end_idx],
                               nodes_parent=node)
            currentparent.addchild(elsebody)
            elsebody.ast = node.content[start_idx + 1]
            elsebody.ast_end = node.content[end_idx - 1]
        return rootif

    def _process_case_value_list(self, selector, nodes, nodes_parent, parent):
        '''
        Processes the supplied list of fparser2 nodes representing case-value
        expressions and constructs the equivalent PSyIR representation.
        e.g. for:

               SELECT CASE(my_flag)
               CASE(var1, var2:var3, :var5)
                 my_switch = .true.
               END SELECT

        the equivalent logical expression is:

        my_flag == var1 OR (myflag>=var2 AND myflag <= var3) OR my_flag <= var5

        and the corresponding structure of the PSyIR that we create is:

                    OR
                   /  \
                 EQ    OR
                      /  \
                   AND    LE
                  /  \
                GE    LE

        :param selector: the fparser2 parse tree representing the \
                      selector_expression in SELECT CASE(selector_expression).
        :type selector: sub-class of :py:class:`fparser.two.utils.Base`
        :param nodes: the nodes representing the label-list of the current \
                      CASE() clause.
        :type nodes: list of :py:class:`fparser.two.Fortran2003.Name` or \
                     :py:class:`fparser.two.Fortran2003.Case_Value_Range`
        :param nodes_parent: the parent in the fparser2 parse tree of the \
                             nodes making up the label-list.
        :type nodes_parent: sub-class of :py:class:`fparser.two.utils.Base`
        :param parent: parent node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`

        '''
        if len(nodes) == 1:
            # Only one item in list so process it
            self._process_case_value(selector, nodes[0], nodes_parent, parent)
            return
        # More than one item in list. Create an OR node with the first item
        # on the list as one arg then recurse down to handle the remainder
        # of the list.
        orop = BinaryOperation(BinaryOperation.Operator.OR,
                               parent=parent)
        self._process_case_value(selector, nodes[0], nodes_parent, orop)
        self._process_case_value_list(selector, nodes[1:], nodes_parent, orop)
        parent.addchild(orop)

    def _process_case_value(self, selector, node, node_parent, parent):
        '''
        Handles an individual condition inside a CASE statement. This can
        be a single scalar expression (e.g. CASE(1)) or a range specification
        (e.g. CASE(lim1:lim2)).

        :param selector: the node in the fparser2 parse tree representing the
                         'some_expr' of the SELECT CASE(some_expr).
        :type selector: sub-class of :py:class:`fparser.two.utils.Base`
        :param node: the node representing the case-value expression in the \
                     fparser2 parse tree.
        :type node: sub-class of :py:class:`fparser.two.utils.Base`
        :param node_parent: the parent in the fparser2 parse tree of the \
                            node representing this case-value.
        :type node_parent: sub-class of :py:class:`fparser.two.utils.Base`
        :param parent: parent node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`

        '''
        node._parent = node_parent  # Retrofit parent information

        if isinstance(node, Fortran2003.Case_Value_Range):
            # The case value is a range (e.g. lim1:lim2)
            if node.items[0] and node.items[1]:
                # Have lower and upper limits so need a parent AND
                aop = BinaryOperation(BinaryOperation.Operator.AND,
                                      parent=parent)
                parent.addchild(aop)
                new_parent = aop
            else:
                # No need to create new parent node
                new_parent = parent

            if node.items[0]:
                # A lower limit is specified
                geop = BinaryOperation(BinaryOperation.Operator.GE,
                                       parent=new_parent)
                self.process_nodes(parent=geop,
                                   nodes=[selector],
                                   nodes_parent=node)
                self.process_nodes(parent=geop,
                                   nodes=[node.items[0]],
                                   nodes_parent=node)
                new_parent.addchild(geop)
            if node.items[1]:
                # An upper limit is specified
                leop = BinaryOperation(BinaryOperation.Operator.LE,
                                       parent=new_parent)
                self.process_nodes(parent=leop,
                                   nodes=[selector],
                                   nodes_parent=node)
                self.process_nodes(parent=leop,
                                   nodes=[node.items[1]],
                                   nodes_parent=node)
                new_parent.addchild(leop)
        else:
            # The case value is some scalar initialisation expression
            bop = BinaryOperation(BinaryOperation.Operator.EQ,
                                  parent=parent)
            parent.addchild(bop)
            self.process_nodes(parent=bop,
                               nodes=[selector],
                               nodes_parent=node)
            self.process_nodes(parent=bop,
                               nodes=[node],
                               nodes_parent=node_parent)

    @staticmethod
    def _array_notation_rank(array):
        '''
        Check that the supplied candidate array reference uses supported
        array notation syntax and return the rank of the sub-section of the
        array that is specified. e.g. for a reference "a(:, 2, :)" the rank
        of the sub-section is 2.

        :param array: the array reference to check.
        :type array: :py:class:`psyclone.psyGen.Array`

        :returns: rank of the sub-section of the array.
        :rtype: int

        :raises NotImplementedError: if the array node does not have any \
                                     children.
        :raises NotImplementedError: if the Array does not have at least one \
                                     CodeBlock as a child.
        :raises NotImplementedError: if each CodeBlock child does not \
                                     consist entirely of Subscript_Triplets.
        :raises NotImplementedError: if any of the array slices have bounds \
                                     (as this is not yet supported).
        '''
        if not array.children:
            raise NotImplementedError("An Array reference in the PSyIR must "
                                      "have at least one child but '{0}' has "
                                      "none".format(array.name))
        # TODO #412. This code will need re-writing once array notation is
        # supported in the PSyIR (becaues then we won't have any CodeBlocks).
        cblocks = array.walk(CodeBlock)
        if not cblocks:
            raise NotImplementedError(
                "A PSyIR Array node representing an array access that uses "
                "Fortran array notation is assumed to have at least one "
                "CodeBlock as its child but '{0}' has none.".
                format(array.name))
        # We only currently support array refs using the colon syntax,
        # e.g. array(:, :). Each colon is represented in the fparser2
        # parse tree by a Subscript_Triplet.
        num_colons = 0
        for cblock in cblocks:
            for stmt in cblock.get_ast_nodes:
                if not isinstance(stmt, Fortran2003.Subscript_Triplet):
                    raise NotImplementedError(
                        "Only array notation of the form my_array(:, :, ...) "
                        "is supported but got: {0}".format(str(array)))
                if any(stmt.items):
                    raise NotImplementedError(
                        "Bounds on array slices are not supported but found: "
                        "'{0}'".format(str(array)))
                num_colons += 1
        return num_colons

    def _array_syntax_to_indexed(self, parent, loop_vars):
        '''
        Utility function that modifies each Array object in the supplied PSyIR
        fragment so that they are indexed using the supplied loop variables
        rather than having colon array notation.

        :param parent: root of PSyIR sub-tree to search for Array \
                       references to modify.
        :type parent:  :py:class:`psyclone.psyGen.Node`
        :param loop_vars: the variable names for the array indices.
        :type loop_vars: list of str

        :raises NotImplementedError: if array sections of differing ranks are \
                                     found.
        '''
        assigns = parent.walk(Assignment)
        # Check that the LHS of any assignment uses recognised array
        # notation.
        for assign in assigns:
            _ = self._array_notation_rank(assign.lhs)
        # TODO #500 if the supplied code accidentally omits array
        # notation for an array reference on the RHS then we will
        # identify it as a scalar and the code produced from the
        # PSyIR (using e.g. the Fortran backend) will not
        # compile. In practise most scalars are likely to be local
        # and so we can check for their declarations. However,
        # those imported from a module will still be missed.
        arrays = parent.walk(Array)
        first_rank = None
        for array in arrays:
            # Check that this is a supported array reference and that
            # all arrays are of the same rank
            rank = self._array_notation_rank(array)
            if first_rank:
                if rank != first_rank:
                    raise NotImplementedError(
                        "Found array sections of differing ranks within a "
                        "WHERE construct: array section of {0} has rank {1}".
                        format(array.name, rank))
            else:
                first_rank = rank
            # Replace the CodeBlocks containing the Subscript_Triplets with
            # the index expressions
            cblocks = array.walk(CodeBlock)
            for idx, cblock in enumerate(cblocks):
                posn = array.children.index(cblock)
                array.children[posn] = Reference(loop_vars[idx], parent=array)

    def _where_construct_handler(self, node, parent):
        '''
        Construct the canonical PSyIR representation of a WHERE construct or
        statement. A construct has the form:

            WHERE(logical-mask)
              statements
            [ELSE WHERE(logical-mask)
              statements]
            [ELSE
              statements]
            END WHERE

        while a statement is just:

            WHERE(logical-mask) statement

        :param node: node in the fparser2 parse tree representing the WHERE.
        :type node: :py:class:`fparser.two.Fortran2003.Where_Construct` or \
                    :py:class:`fparser.two.Fortran2003.Where_Stmt`
        :param parent: parent node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: the top-level Loop object in the created loop nest.
        :rtype: :py:class:`psyclone.psyGen.Loop`

        :raises InternalError: if the parse tree does not have the expected \
                               structure.

        '''
        if isinstance(node, Fortran2003.Where_Stmt):
            # We have a Where statement. Check that the parse tree has the
            # expected structure.
            if not len(node.items) == 2:
                raise InternalError(
                    "Expected a Fortran2003.Where_Stmt to have exactly two "
                    "entries in 'items' but found {0}: {1}".format(
                        len(node.items), str(node.items)))
            if not isinstance(node.items[1], Fortran2003.Assignment_Stmt):
                raise InternalError(
                    "Expected the second entry of a Fortran2003.Where_Stmt "
                    "items tuple to be an Assignment_Stmt but found: {0}".
                    format(type(node.items[1]).__name__))
            was_single_stmt = True
            annotations = ["was_where", "was_single_stmt"]
            logical_expr = [node.items[0]]
        else:
            # We have a Where construct. Check that the first and last
            # children are what we expect.
            if not isinstance(node.content[0],
                              Fortran2003.Where_Construct_Stmt):
                raise InternalError("Failed to find opening where construct "
                                    "statement in: {0}".format(str(node)))
            if not isinstance(node.content[-1], Fortran2003.End_Where_Stmt):
                raise InternalError("Failed to find closing end where "
                                    "statement in: {0}".format(str(node)))
            was_single_stmt = False
            annotations = ["was_where"]
            logical_expr = node.content[0].items

        # Examine the logical-array expression (the mask) in order to
        # determine the number of nested loops required. The Fortran
        # standard allows bare array notation here (e.g. `a < 0.0` where
        # `a` is an array) and thus we would need to examine our SymbolTable
        # to find out the rank of `a`. For the moment we limit support to
        # the NEMO style where the fact that `a` is an array is made
        # explicit using the colon notation, e.g. `a(:, :) < 0.0`.

        # For this initial processing of the logical-array expression we
        # use a temporary parent as we haven't yet constructed the PSyIR
        # for the loop nest and innermost IfBlock. Once we have a valid
        # parent for this logical expression we will repeat the processing.
        fake_parent = Schedule()
        self.process_nodes(fake_parent, logical_expr, None)
        arrays = fake_parent[0].walk(Array)
        if not arrays:
            # If the PSyIR doesn't contain any Arrays then that must be
            # because the code doesn't use explicit array syntax. At least one
            # variable in the logical-array expression must be an array for
            # this to be a valid WHERE().
            # TODO #500 remove this check once the SymbolTable is working for
            # the NEMO API.
            raise NotImplementedError("Only WHERE constructs using explicit "
                                      "array notation (e.g. my_array(:, :)) "
                                      "are supported.")
        # All array sections in a Fortran WHERE must have the same rank so
        # just look at the first array.
        rank = self._array_notation_rank(arrays[0])
        # Create a list to hold the names of the loop variables as we'll
        # need them to index into the arrays.
        loop_vars = rank*[""]

        # Create a set of all of the symbol names in the fparser2 parse
        # tree so that we can find any clashes. We go as far back up the tree
        # as we can before searching for all instances of Fortran2003.Name.
        # TODO #500 - replace this by using the SymbolTable instead.
        # pylint: disable=protected-access
        fp2_parent = node
        while hasattr(fp2_parent, "_parent") and fp2_parent._parent:
            fp2_parent = fp2_parent._parent
        name_list = walk_ast([fp2_parent], [Fortran2003.Name])
        all_names = {str(name) for name in name_list}
        # pylint: enable=protected-access

        # Now create a loop nest of depth `rank`
        new_parent = parent
        for idx in range(rank, 0, -1):
            # TODO #500 we should be using the SymbolTable for the new loop
            # variable but that doesn't currently work for NEMO. As part of
            # #500 we should handle clashes gracefully rather than
            # simply aborting.
            loop_vars[idx-1] = "widx{0}".format(idx)
            if loop_vars[idx-1] in all_names:
                raise InternalError(
                    "Cannot create Loop with variable '{0}' because code "
                    "already contains a symbol with that name.".format(
                        loop_vars[idx-1]))
            loop = Loop(parent=new_parent, variable_name=loop_vars[idx-1],
                        annotations=annotations)
            # Point to the original WHERE statement in the parse tree.
            loop.ast = node
            # Add loop lower bound
            loop.addchild(Literal("1", parent=loop))
            # Add loop upper bound - we use the SIZE operator to query the
            # extent of the current array dimension
            size_node = BinaryOperation(BinaryOperation.Operator.SIZE,
                                        parent=loop)
            loop.addchild(size_node)
            size_node.addchild(Reference(arrays[0].name, parent=size_node))
            size_node.addchild(Literal(str(idx), parent=size_node))
            # Add loop increment
            loop.addchild(Literal("1", parent=loop))
            # Fourth child of a Loop must be a Schedule
            sched = Schedule(parent=loop)
            loop.addchild(sched)
            # Finally, add the Loop we've constructed to its parent (but
            # not into the existing PSyIR tree - that's done in
            # process_nodes()).
            if new_parent is not parent:
                new_parent.addchild(loop)
            else:
                # Keep a reference to the first loop as that's what this
                # handler returns
                root_loop = loop
            new_parent = sched
        # Now we have the loop nest, add an IF block to the innermost
        # schedule
        ifblock = IfBlock(parent=new_parent, annotations=annotations)
        new_parent.addchild(ifblock)
        ifblock.ast = node  # Point back to the original WHERE construct

        # We construct the conditional expression from the original
        # logical-array-expression of the WHERE. We process_nodes() a
        # second time here now that we have the correct parent node in the
        # PSyIR (and thus a SymbolTable) to refer to.
        self.process_nodes(ifblock, logical_expr, None)

        # Each array reference must now be indexed by the loop variables
        # of the loops we've just created.
        self._array_syntax_to_indexed(ifblock.children[0], loop_vars)

        # Now construct the body of the IF using the body of the WHERE
        sched = Schedule(parent=ifblock)
        ifblock.addchild(sched)

        if not was_single_stmt:
            # Do we have an ELSE WHERE?
            for idx, child in enumerate(node.content):
                if isinstance(child, Fortran2003.Elsewhere_Stmt):
                    self.process_nodes(sched, node.content[1:idx], node)
                    self._array_syntax_to_indexed(sched, loop_vars)
                    # Add an else clause to the IF block for the ELSEWHERE
                    # clause
                    sched = Schedule(parent=ifblock)
                    ifblock.addchild(sched)
                    self.process_nodes(sched, node.content[idx+1:-1], node)
                    break
            else:
                # No elsewhere clause was found
                self.process_nodes(sched, node.content[1:-1], node)
        else:
            # We only had a single-statement WHERE
            self.process_nodes(sched, node.items[1:], node)
        # Convert all uses of array syntax to indexed accesses
        self._array_syntax_to_indexed(sched, loop_vars)
        # Return the top-level loop generated by this handler
        return root_loop

    def _return_handler(self, node, parent):
        '''
        Transforms an fparser2 Return_Stmt to the PSyIR representation.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Return_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :return: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyGen.Return`

        '''
        rtn = Return(parent=parent)
        rtn.ast = node
        return rtn

    def _assignment_handler(self, node, parent):
        '''
        Transforms an fparser2 Assignment_Stmt to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyGen.Assignment`
        '''
        assignment = Assignment(node, parent=parent)
        self.process_nodes(parent=assignment, nodes=[node.items[0]],
                           nodes_parent=node)
        self.process_nodes(parent=assignment, nodes=[node.items[2]],
                           nodes_parent=node)

        return assignment

    def _unary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 UnaryOpBase or Intrinsic_Function_Reference
        to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.UnaryOpBase` or \
               :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :return: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.UnaryOperation`

        :raises NotImplementedError: if the supplied operator is not \
                                     supported by this handler.
        :raises InternalError: if the fparser parse tree does not have the \
                               expected structure.

        '''
        operator_str = str(node.items[0]).lower()
        try:
            operator = Fparser2Reader.unary_operators[operator_str]
        except KeyError:
            # Operator not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        if isinstance(node.items[1], Fortran2003.Actual_Arg_Spec_List):
            if len(node.items[1].items) > 1:
                # We have more than one argument and therefore this is not a
                # unary operation!
                raise InternalError(
                    "Operation '{0}' has more than one argument and is "
                    "therefore not unary!".format(str(node)))
            node_list = node.items[1].items
        else:
            node_list = [node.items[1]]
        unary_op = UnaryOperation(operator, parent=parent)
        self.process_nodes(parent=unary_op, nodes=node_list,
                           nodes_parent=node)

        return unary_op

    def _binary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 BinaryOp or Intrinsic_Function_Reference to
        the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.BinaryOpBase` or \
               :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.BinaryOperation`

        :raises NotImplementedError: if the supplied operator/intrinsic is \
                                     not supported by this handler.
        :raises InternalError: if the fparser parse tree does not have the \
                               expected structure.

        '''
        if isinstance(node, Fortran2003.Intrinsic_Function_Reference):
            operator_str = node.items[0].string.lower()
            # Arguments are held in an Actual_Arg_Spec_List
            if not isinstance(node.items[1], Fortran2003.Actual_Arg_Spec_List):
                raise InternalError(
                    "Unexpected fparser parse tree for binary intrinsic "
                    "operation '{0}'. Expected second child to be "
                    "Actual_Arg_Spec_List but got '{1}'.".format(
                        str(node), type(node.items[1])))
            arg_nodes = node.items[1].items
            if len(arg_nodes) != 2:
                raise InternalError(
                    "Binary operator should have exactly two arguments but "
                    "found {0} for '{1}'.".format(len(arg_nodes), str(node)))
        else:
            operator_str = node.items[1].lower()
            arg_nodes = [node.items[0], node.items[2]]

        try:
            operator = Fparser2Reader.binary_operators[operator_str]
        except KeyError:
            # Operator not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        binary_op = BinaryOperation(operator, parent=parent)
        self.process_nodes(parent=binary_op, nodes=[arg_nodes[0]],
                           nodes_parent=node)
        self.process_nodes(parent=binary_op, nodes=[arg_nodes[1]],
                           nodes_parent=node)

        return binary_op

    def _nary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 Intrinsic_Function_Reference with three or
        more arguments to the PSyIR representation.
        :param node: node in fparser2 Parse Tree.
        :type node: \
             :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyGen.NaryOperation`

        :raises NotImplementedError: if the supplied Intrinsic is not \
                                     supported by this handler.
        :raises InternalError: if the fparser parse tree does not have the \
                               expected structure.

        '''
        operator_str = str(node.items[0]).lower()
        try:
            operator = Fparser2Reader.nary_operators[operator_str]
        except KeyError:
            # Intrinsic not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        nary_op = NaryOperation(operator, parent=parent)

        if not isinstance(node.items[1], Fortran2003.Actual_Arg_Spec_List):
            raise InternalError(
                "Expected second 'item' of N-ary intrinsic '{0}' in fparser "
                "parse tree to be an Actual_Arg_Spec_List but found '{1}'.".
                format(str(node), type(node.items[1])))
        if len(node.items[1].items) < 3:
            raise InternalError(
                "An N-ary operation must have more than two arguments but "
                "found {0} for '{1}'.".format(len(node.items[1].items),
                                              str(node)))

        # node.items[1] is a Fortran2003.Actual_Arg_Spec_List so we have
        # to process the `items` of that...
        self.process_nodes(parent=nary_op, nodes=list(node.items[1].items),
                           nodes_parent=node.items[1])
        return nary_op

    def _intrinsic_handler(self, node, parent):
        '''
        Transforms an fparser2 Intrinsic_Function_Reference to the PSyIR
        representation. Since Fortran Intrinsics can be unary, binary or
        nary this handler identifies the appropriate 'sub handler' by
        examining the number of arguments present.

        :param node: node in fparser2 Parse Tree.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.UnaryOperation` or \
                :py:class:`psyclone.psyGen.BinaryOperation` or \
                :py:class:`psyclone.psyGen.NaryOperation`

        '''
        # First item is the name of the intrinsic
        name = node.items[0].string.upper()
        # Now work out how many arguments it has
        num_args = 0
        if len(node.items) > 1:
            num_args = len(node.items[1].items)

        # We don't handle any intrinsics that don't have arguments
        if num_args == 1:
            return self._unary_op_handler(node, parent)
        if num_args == 2:
            return self._binary_op_handler(node, parent)
        if num_args > 2:
            return self._nary_op_handler(node, parent)

        # Intrinsic is not handled - this will result in a CodeBlock
        raise NotImplementedError(name)

    def _name_handler(self, node, parent):
        '''
        Transforms an fparser2 Name to the PSyIR representation. If the node
        is connected to a SymbolTable, it checks the reference has been
        previously declared.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Name`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Reference`
        '''
        reference = Reference(node.string, parent)
        reference.check_declared()
        return reference

    def _parenthesis_handler(self, node, parent):
        '''
        Transforms an fparser2 Parenthesis to the PSyIR representation.
        This means ignoring the parentheis and process the fparser2 children
        inside.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Parenthesis`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        # Use the items[1] content of the node as it contains the required
        # information (items[0] and items[2] just contain the left and right
        # brackets as strings so can be disregarded.
        return self._create_child(node.items[1], parent)

    def _part_ref_handler(self, node, parent):
        '''
        Transforms an fparser2 Part_Ref to the PSyIR representation. If the
        node is connected to a SymbolTable, it checks the reference has been
        previously declared.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :raises NotImplementedError: If the fparser node represents \
            unsupported PSyIR features and should be placed in a CodeBlock.

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Array`

        '''
        reference_name = node.items[0].string.lower()

        array = Array(reference_name, parent)
        array.check_declared()
        self.process_nodes(parent=array, nodes=node.items[1].items,
                           nodes_parent=node.items[1])
        return array

    def _number_handler(self, node, parent):
        '''
        Transforms an fparser2 NumberBase to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.NumberBase`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Literal`
        '''
        return Literal(str(node.items[0]), parent=parent)
