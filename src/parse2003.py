''' Module containing classes related to parsing Fortran code using
    the f2003 parser '''

from fparser import Fortran2003

LOOP_TYPES = [Fortran2003.Block_Nonlabel_Do_Construct]

def walk_ast(children, my_types, indent=0, debug=False):
    '''' Walk down the tree produced by the f2003 parser where children
    are listed under 'content'.  Returns a list of all nodes with the
    specified type(s). '''
    local_list = []
    for child in children:
        if debug:
            print indent*"  " + "child type = ", type(child)
        if type(child) in my_types:
            local_list.append(child)

        # Depending on their level in the tree produced by fparser2003,
        # some nodes have children listed in .content and some have them
        # listed under .items. If a node has neither then it has no
        # children.
        if hasattr(child, "content"):
            local_list += walk_ast(child.content, my_types, indent+1, debug)
        elif hasattr(child, "items"):
            local_list += walk_ast(child.items, my_types, indent+1, debug)

    return local_list


def get_child(root_node, node_type):
    ''' Searches for the immediate child of root_node that is of the
    specified type '''
    for node in root_node.content:
        if isinstance(node, node_type):
            return node
    raise ParseError("Node {0} has no child of type {1}".
                     format(str(root_node), str(node_type)))


class ParseError(Exception):
    ''' Class for parse-error exceptions '''
    def __init__(self, value):
        self.value = "Parse Error: " + value

    def __str__(self):
        return repr(self.value)


class Loop(object):
    ''' Representation of a Do loop '''

    def __init__(self):
        self._var_name = ""

    def load(self, parsed_loop):
        ''' Takes the supplied loop object produced by the f2003 parser
        and extracts relevant information from it to populate this object '''
        from fparser.Fortran2003 import Nonlabel_Do_Stmt, Name
        for node in parsed_loop.content:
            if isinstance(node, Nonlabel_Do_Stmt):
                var_name = walk_ast(node.items, [Name])
                if var_name:
                    self._var_name = str(var_name[0])
                else:
                    # Loop is just "DO" with no loop control
                    self._var_name = None

    @property
    def var_name(self):
        ''' Return a string containing the name of the loop variable or
        None if there isn't one '''
        return self._var_name


class Variable(object):
    ''' Representation of a Fortran variable. Can be a scalar or an
    array reference '''

    def __init__(self):
        # Name of this quantity in the DAG (may not be the same as
        # _orig_name because of assignment)
        self._name = None
        # Base name of the variable as used in the raw Fortran code
        # (i.e. excluding any array indexing if present)
        self._orig_name = None
        # Full name of the variable (including indices if it is an
        # array reference) as it appears in the raw Fortran
        self._full_orig_name = None
        self._is_array_ref = False
        # List of the variables used to index into the array
        self._index_vars = []
        # List of the index expressions: one for each index of the
        # array
        self._index_exprns = []

    @property
    def _index_str(self):
        ''' Returns a string containing the full array-indexing
        expression part of this variable reference. If it is not an
        array reference then returns an empty string. '''
        index_str = ""
        if self._is_array_ref:
            index_str += "(" + self.index_expr + ")"
        return index_str

    def __str__(self):
        return self._name + self._index_str

    @property
    def full_orig_name(self):
        ''' Returns a string containing the full original name of this
        variable - i.e.  including any array indexing '''
        return self._full_orig_name

    # TODO decide on a name for this routine!
    @property
    def indexed_name(self):
        ''' Returns a string containing the original base name but with
        the index expression allowing for any updates to the index
        variables, e.g. my_array(ji,jj') '''
        return self._orig_name + self._index_str

    @property
    def full_name(self):
        ''' Returns a string containing the full name of this
        variable - i.e.  including any array indexing and any "'" chars
        due to repeated assignment to this variable resulting in new
        nodes in the DAG. '''
        return self._name + self._index_str

    @property
    def index_expr(self):
        ''' Return the full index expression of this variable if it is an
        array reference. '''
        if not self._is_array_ref:
            return ""
        return ",".join(self.indices)

    @property
    def indices(self):
        ''' Returns a list of strings containing the expression for each
        array index in this array reference '''
        for idx, tok in enumerate(self._index_exprns):
            # This is a very simplistic piece of code intended to
            # process array index expressions of the form
            # ji+1-1+1. It ignores anything other than '+1' and '-1'.
            num_plus = tok.count("+1")
            num_minus = tok.count("-1")
            if num_plus > 0 or num_minus > 0:
                # We only manipulate this index expression if it contains
                # one or more '+1's or '-1's
                basic_expr = tok.replace("+1", "")
                basic_expr = basic_expr.replace("-1", "")

                net_incr = num_plus - num_minus
                if net_incr < 0:
                    basic_expr += str(net_incr)
                elif net_incr > 0:
                    basic_expr += "+" + str(net_incr)
                else:
                    # The +1's and -1's have cancelled each other out
                    pass
                # Store the simplified expression for this array index
                self._index_exprns[idx] = basic_expr
        return self._index_exprns

    def load(self, node, mapping=None, lhs=False):
        ''' Populate the state of this Variable object using the supplied
        output of the f2003 parser. If lhs is True then this variable
        appears on the LHS of an assignment and thus represents a new
        entity in a DAG. '''
        from fparser import Fortran2003

        _is_constant = False

        if isinstance(node, Fortran2003.Name):
            # This node is simply the name of a variable
            name = str(node)
            self._orig_name = name[:]
            self._full_orig_name = self._orig_name
            if mapping and name in mapping:
                self._name = mapping[name]
            else:
                self._name = name
            self._is_array_ref = False

        elif isinstance(node, Fortran2003.Data_Ref):
            # Reference to a component of a derived type
            name = str(node).replace(" ", "")
            self._orig_name = name[:]
            self._full_orig_name = self._orig_name
            if mapping and name in mapping:
                self._name = mapping[name]
            else:
                self._name = name
            self._is_array_ref = False

        elif (isinstance(node, Fortran2003.Part_Ref) or
              isinstance(node, Fortran2003.Array_Section)):
            # This node might be an array access or a function call
            self._name = str(node.items[0])
            self._orig_name = self._name
            self._full_orig_name = str(node).replace(" ", "")
            self._is_array_ref = True
            array_index_vars = []

            if isinstance(node, Fortran2003.Array_Section):
                # This node is an array section which means that it has
                # one index specified using ':'
                self._index_exprns = str(node.items[1])
            elif isinstance(node.items[1], Fortran2003.Array_Section):
                # An Array reference cannot itself contain an array
                # section so this must be a function call
                self._is_array_ref = False
            elif isinstance(node.items[1], Fortran2003.Section_Subscript_List):
                # Obtain the expression for each index of the array ref
                for item in node.items[1].items:
                    self._index_exprns.append(str(item).replace(" ", ""))

                # This recurses down and finds the names of all of
                # the *variables* in the array-index expression
                # (i.e. ignoring whether they are "+1" etc.)
                array_index_vars = walk_ast(node.items[1].items,
                                            [Fortran2003.Name])
            elif (isinstance(node.items[1], Fortran2003.Name) or
                  isinstance(node.items[1],
                             Fortran2003.Int_Literal_Constant) or
                  isinstance(node.items[1], Fortran2003.Data_Ref)):
                # There's only a single array index/argument
                self._index_exprns.append(str(node.items[1]).replace(" ", ""))
                array_index_vars = [node.items[1]]
            elif isinstance(node.items[1], Fortran2003.Level_2_Expr):
                # Array index expression itself contains an array access - i.e.
                # this is an indirect access.
                self._index_exprns.append(
                    ''.join([str(item).replace(" ", "")
                             for item in node.items[1].items]))
                # TODO currently if we get an array access of the form
                # a(map(i)) then we will store 'map' and 'i' as the
                # array-index variables. This needs to be extended to
                # properly support indirect array accesses.
                array_index_vars = walk_ast(node.items[1].items,
                                            [Fortran2003.Name])
            elif isinstance(node.items[1], Fortran2003.Part_Ref):
                # Array index expression is itself an array access
                # TODO don't flatten the array expression into a string
                # so that we can handle loop-unrolling for such cases
                self._index_exprns.append(str(node.items[1]).replace(" ", ""))
                array_index_vars = self._index_exprns[-1]
            elif isinstance(node.items[1], Fortran2003.Add_Operand):
                # Array index expression is something like "2*i"
                array_index_vars = walk_ast(node.items[1].items,
                                            [Fortran2003.Name])
            elif isinstance(node.items[1], Fortran2003.Parenthesis):
                array_index_vars = walk_ast(node.items[1].items,
                                            [Fortran2003.Name])
            else:
                raise ParseError(
                    "Unrecognised array-index expression (type={0}): {1}".
                    format(type(node.items[1]), str(node)))

            # Now that we've captured the array-index expressions we can
            # apply the naming map to them
            for var in array_index_vars:
                if isinstance(var, str):
                    name = var
                else:
                    name = var.string
                if mapping and name in mapping:
                    self._index_vars.append(mapping[name])
                    # Replace any references to this variable in the index
                    # expressions with the new name
                    for idx, exprn in enumerate(self._index_exprns):
                        self._index_exprns[idx] = exprn.replace(name,
                                                                mapping[name])
                else:
                    # This variable name is not in our name map so we
                    # use it as it is
                    self._index_vars.append(name)
            # Finally, having ensured that we're naming the index variables
            # correctly, we can apply the naming map to the full array
            # reference
            if mapping and self.indexed_name in mapping:
                self._name = mapping[self.indexed_name]

        elif (isinstance(node, Fortran2003.Real_Literal_Constant) or
              isinstance(node, Fortran2003.Int_Literal_Constant) or
              isinstance(node, Fortran2003.Char_Literal_Constant) or
              isinstance(node, Fortran2003.Logical_Literal_Constant)):
            self._name = str(node)
            self._orig_name = self._name
            self._is_array_ref = False
            _is_constant = True

        else:
            raise ParseError("Unrecognised type for variable '{0}': {1}".
                             format(str(node), type(node)))

        # Add this variable name to the map if it's not already present and
        # this instance is not on the LHS of an assignment. (Because if it is
        # then we handle its naming at a higher level)
        if not lhs and not _is_constant and self.indexed_name not in mapping:
            mapping[self.indexed_name] = self.orig_name

    @property
    def orig_name(self):
        ''' Return the name of this variable as it appeared in the
        the parsed Fortran code '''
        return self._orig_name

    @property
    def name(self):
        ''' Return the name of this variable as a string. Includes any "'"
        chars appended due to repeated assignment to this variable. '''
        return self._name

    @property
    def is_array_ref(self):
        ''' Returns True if this Variable is an array access '''
        return self._is_array_ref
