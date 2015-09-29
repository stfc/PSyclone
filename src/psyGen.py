#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module provides generic support for PSyclone's PSy code optimisation
    and generation. The classes in this method need to be specialised for a
    particular API and implementation. '''

import abc

class GenerationError(Exception):
    ''' Provides a PSyclone specific error class for errors found during PSy
        code generation. '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Generation Error: "+value

    def __str__(self):
        return repr(self.value)


class FieldNotFoundError(Exception):
    ''' Provides a PSyclone-specific error class when a field with the
    requested property/ies is not found '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Field not found error: "+value

    def __str__(self):
        return repr(self.value)


class PSyFactory(object):
    ''' Creates a specific version of the PSy. If a particular api is not
        provided then the default api, as specified in the configs.py file,
        is chosen. '''
    def __init__(self, api = ""):
        if api == "":
            from config import DEFAULTAPI
            self._type = DEFAULTAPI
        else:
            from config import SUPPORTEDAPIS as supported_types
            self._type = api
            if self._type not in supported_types:
                raise GenerationError("PSyFactory: Unsupported API '{0}' "
                                      "specified. Supported types are "
                                      "{1}.".format(self._type,
                                                    supported_types))

    def create(self, invoke_info):
        ''' Return the specified version of PSy. '''
        if self._type == "gunghoproto":
            from ghproto import GHProtoPSy
            return GHProtoPSy(invoke_info)
        elif self._type == "dynamo0.1":
            from dynamo0p1 import DynamoPSy
            return DynamoPSy(invoke_info)
        elif self._type == "dynamo0.3":
            from dynamo0p3 import DynamoPSy
            return DynamoPSy(invoke_info)
        elif self._type == "gocean0.1":
            from gocean0p1 import GOPSy
            return GOPSy(invoke_info)
        elif self._type == "gocean1.0":
            from gocean1p0 import GOPSy
            return GOPSy(invoke_info)
        else:
            raise GenerationError("PSyFactory: Internal Error: Unsupported "
                                  "api type '{0}' found. Should not be "
                                  "possible.".format(self._type))

class PSy(object):
    '''
        Base class to help manage and generate PSy code for a single
        algorithm file. Takes the invocation information output from the
        function :func:`parse.parse` as its input and stores this in a
        way suitable for optimisation and code generation.

        :param FileInfo invoke_info: An object containing the required invocation
                                     information for code optimisation and
                                     generation. Produced by the function
                                     :func:`parse.parse`.

        For example:

        >>> from parse import parse
        >>> ast, info = parse("argspec.F90")
        >>> from psyGen import PSyFactory
        >>> api = "..."
        >>> psy = PSyFactory(api).create(info)
        >>> print(psy.gen)

    '''
    def __init__(self, invoke_info):

        self._name = invoke_info.name
        self._invokes = None

    def __str__(self):
        return "PSy"
    @property
    def invokes(self):
        return self._invokes
    @property
    def name(self):
        return "psy_"+self._name
    @property
    def gen(self):
        raise NotImplementedError("Error: PSy.gen() must be implemented "
                                  "by subclass")

class Invokes(object):
    ''' Manage the invoke calls '''
    def __init__(self, alg_calls, Invoke):
        self.invoke_map = {}
        self.invoke_list = []
        for idx, alg_invocation in enumerate(alg_calls.values()):
            my_invoke = Invoke(alg_invocation, idx)
            self.invoke_map[my_invoke.name] = my_invoke
            self.invoke_list.append(my_invoke)
    def __str__(self):
        return "Invokes object containing "+str(self.names)
    @property
    def names(self):
        return self.invoke_map.keys()
    def get(self, invoke_name):
        # add a try here for keyerror
        try:
            return self.invoke_map[invoke_name]
        except KeyError:
            raise RuntimeError("Cannot find an invoke named '{0}' in {1}".
                               format(invoke_name,
                                      str(self.names)))

    def gen_code(self, parent):
        for invoke in self.invoke_list:
            invoke.gen_code(parent)

class Dependencies(object):
    def __init__(self, this_arg):
        self._arg = this_arg
        self._precedes = []
        self._follows = []
    def set(self):
        if self._arg.is_literal:
            pass
        else:
            for following_call in self._arg._call.following_calls:
                for argument in following_call.arguments._args:
                    if argument.name == self._arg._name:
                        self.add_follows(argument)
            for preceding_call in self._arg._call.preceding_calls:
                for argument in preceding_call.arguments._args:
                    if argument.name == self._arg._name:
                        self.add_precedes(argument)

    def add_precedes(self, obj):
        self._precedes.append(obj)
    def add_follows(self, obj):
        self._follows.append(obj)
    def get_precedes(self):
        return self._precedes
    def get_follows(self):
        return self._follows

class NameSpaceFactory(object):
        # storage for the instance reference
    _instance = None
    def __init__(self, reset = False):
        """ Create singleton instance """
        # Check whether we already have an instance
        if NameSpaceFactory._instance is None or reset:
            # Create and remember instance
            NameSpaceFactory._instance = NameSpace()
    def create(self):
        return NameSpaceFactory._instance

class NameSpace(object):
    ''' keeps a record of reserved names and used names for clashes and provides a
        new name if there is a clash. '''

    def __init__(self, case_sensitive=False):
        self._reserved_names = []
        self._added_names = []
        self._context = {}
        self._case_sensitive = case_sensitive

    def create_name(self, root_name=None, context=None, label=None):
        '''Returns a unique name. If root_name is supplied, the name returned
            is based on this name, otherwise one is made up.  If
            context and label are supplied and a previous create_name
            has been called with the same context and label then the
            name provided by the previous create_name is returned.
        '''
        # make up a base name if one has not been supplied
        if root_name is None:
            root_name = "anon"
        # if not case sensitive then make the name lower case
        if not self._case_sensitive:
            lname = root_name.lower()
        else:
            lname = root_name
        # check context and label validity
        if context is None and label is not None or context is not None and label is None:
            raise RuntimeError("NameSpace:create_name() requires both context and label to be set")

        # if the same context and label have already been supplied then return the previous name
        if context is not None and label is not None:
            # labels may have spurious white space
            label = label.strip()
            if not self._case_sensitive:
                label = label.lower()
                context = context.lower()
            if context in self._context:
                if label in self._context[context]:
                    # context and label have already been supplied
                    return self._context[context][label]
            else:
                # initialise the context so we can add the label value later
                self._context[context]={}

        # create our name
        if not lname in self._reserved_names and not lname in self._added_names:
            proposed_name = lname
        else:
            count = 1
            proposed_name = lname+"_"+str(count)
            while proposed_name in self._reserved_names or \
                  proposed_name in self._added_names:
                count+=1
                proposed_name = lname+"_"+str(count)

        # store our name
        self._added_names.append(proposed_name)
        if context is not None and label is not None:
            self._context[context][label] = proposed_name

        return proposed_name
        
    def add_reserved_name(self, name):
        ''' adds a reserved name. create_name() will not return this name '''
        if not self._case_sensitive:
            lname = name.lower()
        else:
            lname = name
        # silently ignore if this is already a reserved name
        if not lname in self._reserved_names:
            if lname in self._added_names:
                raise RuntimeError("attempted to add a reserved name to a namespace that has already used that name")
            self._reserved_names.append(lname)

    def add_reserved_names(self, names):
        ''' adds a list of reserved names '''
        for name in names:
            self.add_reserved_name(name)

class Invoke(object):
    ''' Manage an individual invoke call '''

    def __str__(self):
        return self._name+"("+str(self.unique_args)+")"
    def __init__(self, alg_invocation, idx, Schedule, reserved_names = []):

        if alg_invocation == None and idx == None: return

        # create our namespace manager - must be done before creating the
        # schedule
        self._name_space_manager = NameSpaceFactory(reset = True).create()
        self._name_space_manager.add_reserved_names(reserved_names)

        # create the schedule
        self._schedule = Schedule(alg_invocation.kcalls)

        # let the schedule have access to me
        self._schedule.invoke = self

        # Set up the ordering constraints between the calls in the schedule
        # Obviously the schedule must be created first
        for call in self._schedule.calls():
            call.set_constraints()

        # create a name for the call if one does not already exist
        if alg_invocation.name is not None:
            self._name = alg_invocation.name
        elif len(alg_invocation.kcalls) == 1 and \
                 alg_invocation.kcalls[0].type == "kernelCall":
            # use the name of the kernel call with the position appended.
            # Appended position is needed in case we have two separate invokes
            # in the same algorithm code containing the same (single) kernel
            self._name = "invoke_"+str(idx)+"_"+alg_invocation.kcalls[0].ktype.name
        else:
            # use the position of the invoke
            self._name = "invoke_"+str(idx)

        # extract the argument list for the algorithm call and psy
        # layer subroutine.
        self._alg_unique_args = []
        self._psy_unique_vars = []
        tmp_arg_names = []
        for call in self.schedule.calls():
            for arg in call.arguments.args:
                if arg.text is not None:
                    if not arg.text in self._alg_unique_args:
                        self._alg_unique_args.append(arg.text)
                    if not arg.name in tmp_arg_names:
                        tmp_arg_names.append(arg.name)
                        self._psy_unique_vars.append(arg)
                else:
                    # literals have no name
                    pass

        # work out the unique dofs required in this subroutine
        self._dofs = {}
        for kern_call in self._schedule.kern_calls():
            dofs = kern_call.arguments.dofs
            for dof in dofs:
                if not self._dofs.has_key(dof):
                    # Only keep the first occurence for the moment. We will
                    # need to change this logic at some point as we need to
                    # cope with writes determining the dofs that are used.
                    self._dofs[dof] = [kern_call, dofs[dof][0]]
    @property
    def name(self):
        return self._name
    @property
    def alg_unique_args(self):
        return self._alg_unique_args
    @property
    def psy_unique_vars(self):
        return self._psy_unique_vars
    @property
    def psy_unique_var_names(self):
        names=[]
        for var in self._psy_unique_vars:
            names.append(var.name)
        return names
    @property
    def schedule(self):
        return self._schedule
    @schedule.setter
    def schedule(self, obj):
        self._schedule = obj
    def gen(self):
        from f2pygen import ModuleGen
        module = ModuleGen("container")
        self.gen_code(module)
        return module.root

    def gen_code(self, parent):
        from f2pygen import SubroutineGen, TypeDeclGen, DeclGen, \
                            SelectionGen, AssignGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name = self.name,
                                   args = self.psy_unique_vars)
        # add the subroutine argument declarations
        my_typedecl = TypeDeclGen(invoke_sub, datatype = "field_type",
                                  entity_decls = self.psy_unique_vars,
                                  intent = "inout")
        invoke_sub.add(my_typedecl)
        # declare field-type, column topology and function-space types
        column_topology_name = "topology"
        my_typedecl = TypeDeclGen(invoke_sub, datatype = "ColumnTopology",
                                  entity_decls = [column_topology_name],
                                  pointer = True)
        invoke_sub.add(my_typedecl)
        # declare any basic types required
        my_decl = DeclGen(invoke_sub, datatype = "integer",
                          entity_decls = ["nlayers"])
        invoke_sub.add(my_decl)

        for (idx, dof) in enumerate(self._dofs):
            call = self._dofs[dof][0]
            arg = self._dofs[dof][1]
            # declare a type select clause which is used to map from a base
            # class to FunctionSpace_type
            type_select = SelectionGen(invoke_sub,
                expr = arg.name+"_space=>"+arg.name+"%function_space",
                typeselect = True)
            invoke_sub.add(type_select)

            my_typedecl = TypeDeclGen(invoke_sub,
                                      datatype = "FunctionSpace_type",
                                      entity_decls = [arg.name+"_space"],
                                      pointer = True)
            invoke_sub.add(my_typedecl)

            content = []
            if idx == 0:
                # use the first model to provide nlayers
                # *** assumption that all fields operate over the same number
                # of layers
                assign_1 = AssignGen(type_select, lhs = "topology",
                                     rhs = arg.name+"_space%topology",
                                     pointer = True)
                assign_2 = AssignGen(type_select, lhs = "nlayers",
                                  rhs = "topology%layer_count()")
                content.append(assign_1)
                content.append(assign_2)
            iterates_over = call.iterates_over
            stencil = arg.stencil
            assign_3 = AssignGen(type_select, lhs = dof+"dofmap",
                                 rhs = arg.name+ \
                                 "_space%dof_map("+iterates_over+", "+ \
                                                 stencil+")",
                pointer = True)
            content.append(assign_3)
            type_select.addcase(["FunctionSpace_type"], content = content)
            # declare our dofmap
            my_decl = DeclGen(invoke_sub, datatype = "integer",
                            entity_decls = [dof+"dofmap(:,:)"], pointer = True)
            invoke_sub.add(my_decl)

        # create the subroutine kernel call content
        self.schedule.gen_code(invoke_sub)
        parent.add(invoke_sub)

class Node(object):
    ''' baseclass for a node in a schedule '''

    def view(self):
        raise NotImplementedError("BaseClass of a Node must implement the "
                                  "view method")

    def indent(self, count, indent = "    "):
        result = ""
        for i in range(count):
            result += indent
        return result

    def list(self, indent = 0):
        result=""
        for entity in self._children:
            result += str(entity)+"\n"
        return result

    def list_to_string(self, my_list):
        result = ""
        for idx,value in enumerate(my_list):
            result += str(value)
            if idx < (len(my_list) - 1):
                result += ","
        return result

    def __init__(self, children = [], parent = None):
        self._children = children
        self._parent = parent

    def __str__(self):
        raise NotImplementedError("Please implement me")

    def addchild(self, child, index = None):
        if index is not None:
            self._children.insert(index,child)
        else:
            self._children.append(child)

    @property
    def children(self):
        return self._children

    @children.setter
    def children(self,my_children):
        self._children=my_children

    @property
    def parent(self):
        return self._parent

    @parent.setter
    def parent(self,my_parent):
        self._parent=my_parent

    @property
    def position(self):
        if self.parent is None: return 0
        return self.parent.children.index(self)

        current = self.root
        position = 0

    @property
    def abs_position(self):
        ''' Find my position in the schedule. Needs to be computed
            dynamically as my position may change. '''

        if self.root == self: return 0
        found, position = self._find_position(self.root.children, 0)
        if not found: raise Exception("Error in search for my position in "
                                      "the tree")
        return position

    def _find_position(self, children, position):
        ''' Recurse through the tree depth first returning position if
            found.'''
        for child in children:
            position += 1
            if child == self:
                return True, position
            if isinstance(child, Loop):
                found, position = self._find_position(child.children, position)
                if found: return True, position
        return False, position

    @property
    def root(self):
        node = self
        while node.parent is not None:
            node = node.parent
        return node

    def sameRoot(self, node_2):
        if self.root == node_2.root: return True
        return False

    def sameParent(self, node_2):
        if self.parent is None or node_2.parent is None: return False
        if self.parent == node_2.parent: return True
        return False

    def walk(self, children, my_type):
        ''' recurse through tree and return objects of mytype '''
        local_list = []
        for child in children:
            if isinstance(child, my_type):
                local_list.append(child)
            local_list += self.walk(child.children, my_type)
        return local_list

    def ancestor(self, my_type):
        ''' Search back up tree and check whether we have an
        ancestor of the supplied type. If we do then we return
        it otherwise we return None '''
        myparent = self.parent
        while myparent is not None:
            if isinstance(myparent, my_type):
                return myparent
            myparent = myparent.parent
        return None

    def calls(self):
        ''' return all calls in this schedule '''
        return self.walk(self.root.children, Call)

    @property
    def following_calls(self):
        ''' return all calls after me in the schedule '''
        all_calls = self.calls()
        position = all_calls.index(self)
        return all_calls[position+1:]

    @property
    def preceding_calls(self):
        ''' return all calls before me in the schedule '''
        all_calls = self.calls()
        position = all_calls.index(self)
        return all_calls[:position-1]

    def kern_calls(self):
        ''' return all kernel calls in this schedule '''
        #print "looking for kernCalls"
        return self.walk(self._children, Kern)

    def inf_calls(self):
        ''' return all infrastructure calls in this schedule '''
        return self.walk(self._children, Inf)

    def loops(self):
        ''' return all loops currently in this schedule '''
        return self.walk(self._children, Loop)

    def is_openmp_parallel(self):
        '''Returns true if this Node is within an OpenMP parallel region

        '''
        omp_dir = self.ancestor(OMPParallelDirective)
        if omp_dir:
            return True
        return False

    def gen_code(self):
        raise NotImplementedError("Please implement me")

class Schedule(Node):

    ''' Stores schedule information for an invocation call. Schedules can be
        optimised using transformations.
        
        >>> from parse import parse
        >>> ast, info = parse("algorithm.f90")
        >>> from psyGen import PSyFactory
        >>> api = "..."
        >>> psy = PSyFactory(api).create(info)
        >>> invokes = psy.invokes
        >>> invokes.names
        >>> invoke = invokes.get("name")
        >>> schedule = invoke.schedule
        >>> print schedule.view()

    '''

    def tkinter_delete(self):
        for entity in self._children:
            entity.tkinter_delete()

    def tkinter_display(self, canvas, x, y):
        y_offset = 0
        for entity in self._children:
            entity.tkinter_display(canvas, x, y+y_offset)
            y_offset = y_offset+entity.height

    @property
    def invoke(self):
        return self._invoke
    @invoke.setter
    def invoke(self,my_invoke):
        self._invoke = my_invoke

    def __init__(self, Loop, Inf, alg_calls = []):
            
        # we need to separate calls into loops (an iteration space really)
        # and calls so that we can perform optimisations separately on the
        # two entities.
        sequence = []
        from parse import InfCall
        for call in alg_calls:
            if isinstance(call, InfCall):
                sequence.append(Inf.create(call, parent = self))
            else:
                sequence.append(Loop(call, parent = self))
        #for call in alg_calls:
        #    sequence.append(Loop(call, parent = self))
        Node.__init__(self, children = sequence)
        self._invoke = None

    def view(self, indent = 0):
        print self.indent(indent)+"Schedule[invoke='"+self.invoke.name+"']"
        for entity in self._children:
            entity.view(indent = indent + 1)

    def __str__(self):
        result = "Schedule:\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result

    def gen_code(self, parent):
        for entity in self._children:
            entity.gen_code(parent)

class Directive(Node):

    def view(self,indent = 0):
        print self.indent(indent)+"Directive"
        for entity in self._children:
            entity.view(indent = indent + 1)

class OMPDirective(Directive):

    def view(self,indent = 0):
        print self.indent(indent)+"Directive[OMP]"
        for entity in self._children:
            entity.view(indent = indent + 1)

class OMPParallelDirective(OMPDirective):

    def view(self,indent = 0):
        print self.indent(indent)+"Directive[OMP Parallel]"
        for entity in self._children:
            entity.view(indent = indent + 1)

    def gen_code(self, parent):
        from f2pygen import DirectiveGen

        private_str = self.list_to_string(self._get_private_list())

        # We're not doing nested parallelism so make sure that this
        # omp parallel region is not already within some parallel region
        self._not_within_omp_parallel_region()

        # Check that this OpenMP PARALLEL directive encloses other
        # OpenMP directives. Although it is valid OpenMP if it doesn't,
        # this almost certainly indicates a user error.
        self._encloses_omp_directive()

        parent.add(DirectiveGen(parent, "omp", "begin", "parallel",
                                "default(shared), private({0})".\
                                format(private_str)))

        first_type = type(self.children[0])
        for child in self.children:
            if first_type != type(child):
                raise NotImplementedError("Cannot correctly generate code"
                                          " for an OpenMP parallel region"
                                          " containing children of "
                                          "different types")
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "parallel", ""))
        
    def _get_private_list(self):
        '''Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Call
        within the directive.

        '''
        result=[]
        # get variable names from all loops that are a child of this node
        for loop in self.loops():
            if loop._variable_name == "":
                raise GenerationError("Internal error: name of loop "
                                      "variable not set.")
            if loop._variable_name.lower() not in result:
                result.append(loop._variable_name.lower())
        # get variable names from all calls that are a child of this node
        for call in self.calls():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise GenerationError("Internal error: call has a "
                                          "local variable but its name "
                                          "is not set.")
                if variable_name.lower() not in result:
                    result.append(variable_name.lower())
        return result

    def _not_within_omp_parallel_region(self):
        ''' Check that this Directive is not within any other
            parallel region '''
        if self.ancestor(OMPParallelDirective) is not None:
            raise GenerationError("Cannot nest OpenMP parallel regions.")

    def _encloses_omp_directive(self):
        ''' Check that this Parallel region contains other OpenMP
            directives. While it doesn't have to (in order to be valid
            OpenMP), it is likely that an absence of directives
            is an error on the part of the user. '''
        # We need to recurse down through all our children and check
        # whether any of them are an OMPDirective.
        node_list = self.walk(self.children, OMPDirective)
        if len(node_list) == 0:
            # TODO raise a warning here so that the user can decide
            # whether or not this is OK.
            pass
            #raise GenerationError("OpenMP parallel region does not enclose "
            #                      "any OpenMP directives. This is probably "
            #                      "not what you want.")

class OMPDoDirective(OMPDirective):

    def __init__(self, children=[], parent=None, omp_schedule="static"):
        self._omp_schedule = omp_schedule
        # Call the init method of the base class once we've stored
        # the OpenMP schedule
        OMPDirective.__init__(self,
                              children=children,
                              parent=parent)

    def view(self,indent = 0):
        print self.indent(indent)+"Directive[OMP do]"
        for entity in self._children:
            entity.view(indent = indent + 1)

    def gen_code(self,parent):
        from f2pygen import DirectiveGen

        # It is only at the point of code generation that
        # we can check for correctness (given that we don't
        # mandate the order that a user can apply transformations
        # to the code). As an orphaned loop directive, we must
        # have an OMPRegionDirective as a parent somewhere
        # back up the tree.
        self._within_omp_region()

        # As we're an orphaned loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        parent.add(DirectiveGen(parent, 
                                "omp", "begin", "do",
                                "schedule({0})".\
                                format(self._omp_schedule)))

        for child in self.children:
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "do", ""))

    def _within_omp_region(self):
        ''' Check that this orphaned OMP Loop Directive is actually
            within an OpenMP Parallel Region '''
        myparent = self.parent
        while myparent is not None:
            if isinstance(myparent, OMPParallelDirective) and\
               not isinstance(myparent, OMPParallelDoDirective):
                return
            myparent = myparent.parent
        raise GenerationError("OMPOrphanLoopDirective must have an "
                              "OMPRegionDirective as ancestor")

class OMPParallelDoDirective(OMPParallelDirective, OMPDoDirective):
    ''' Class for the !$OMP PARALLEL DO directive. This inherits from
        both OMPParallelDirective (because it creates a new OpenMP
        thread-parallel region) and OMPDoDirective (because it
        causes a loop to be parallelised). '''

    def __init__(self, children=[], parent=None, omp_schedule="static"):
        OMPDoDirective.__init__(self,
                                children=children,
                                parent=parent,
                                omp_schedule=omp_schedule)

    def view(self,indent = 0):
        print self.indent(indent)+"Directive[OMP parallel do]"
        for entity in self._children:
            entity.view(indent = indent + 1)

    def gen_code(self,parent):
        from f2pygen import DirectiveGen

        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        self._not_within_omp_parallel_region()

        private_str = self.list_to_string(self._get_private_list())
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel do",
                                "default(shared), private({0}), "
                                "schedule({1})".\
                                format(private_str, self._omp_schedule)))
        for child in self.children:
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "parallel do", ""))

class Loop(Node):

    @property
    def loop_type(self):
        return self._loop_type

    @loop_type.setter
    def loop_type(self,value):
        assert value in self._valid_loop_types, "Error, loop_type value is invalid"
        self._loop_type=value

    def __init__(self, Inf, Kern, call = None, parent = None,
                 variable_name = "", topology_name = "topology", 
                 valid_loop_types=[]):

        children = []
        # we need to determine whether this is an infrastructure or kernel
        # call so our schedule can do the right thing.

        self._valid_loop_types = valid_loop_types
        self._loop_type = None       # inner, outer, colour, colours, ...
        self._field = None
        self._field_name = None      # name of the field
        self._field_space = None     # v0, v1, ...,     cu, cv, ...
        self._iteration_space = None # cells, ...,      cu, cv, ...

        # TODO replace iterates_over with iteration_space
        self._iterates_over = "unknown"
        if call is not None:
            from parse import InfCall, KernelCall
            if isinstance(call, InfCall):
                my_call = Inf.create(call, parent = self)
                self._iteration_space = "unknown"
                self._iterates_over = "unknown" # needs to inherit this?
                self._field_space = "any"
            elif isinstance(call, KernelCall):
                my_call = Kern(call, parent = self)
                self._iterates_over = my_call.iterates_over
                self._iteration_space = my_call.iterates_over
                self._field_space = my_call.arguments.iteration_space_arg().function_space
                self._field = my_call.arguments.iteration_space_arg()
                self._field_name = self._field.name
            else:
                raise Exception
            children.append(my_call)
        Node.__init__(self, children = children, parent = parent)

        self._variable_name = variable_name

        self._start = ""
        self._stop = ""
        self._step = ""
        self._id = ""

        # visual properties
        self._width = 30
        self._height = 30
        self._shape = None
        self._text = None
        self._canvas = None

    def view(self, indent = 0):
        print self.indent(indent)+"Loop[type='{0}',field_space='{1}',it_space='{2}']".format(self._loop_type,self._field_space,self.iteration_space)
        for entity in self._children:
            entity.view(indent = indent + 1)

    @property
    def height(self):
        calls_height = 0
        for child in self.children:
            calls_height += child.height
        return self._height+calls_height

    def tkinter_delete(self):
        if self._shape is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._shape)
        if self._text is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._text)
        for child in self.children:
            child.tkinter_delete()

    def tkinter_display(self, canvas, x, y):
        self.tkinter_delete()
        self._canvas = canvas
        from Tkinter import ROUND
        name = "Loop"
        min_call_width = 100
        max_calls_width = min_call_width
        calls_height = 0
        for child in self.children:
            calls_height += child.height
            max_calls_width = max(max_calls_width, child.width)

        self._shape = canvas.create_polygon(x, y,
                          x+self._width+max_calls_width, y,
                          x+self._width+max_calls_width, y+self._height,
                          x+self._width, y+self._height,
                          x+self._width, y+self._height+calls_height,
                          x, y+self._height+calls_height,
                          outline = "red", fill = "green", width = 2,
                          activeoutline = "blue", joinstyle = ROUND)
        self._text = canvas.create_text(x+(self._width+max_calls_width)/2,
                                        y+self._height/2,  text = name)

        call_height = 0
        for child in self.children:
            child.tkinter_display(canvas, x+self._width,
                                  y+self._height+call_height)
            call_height += child.height

    @property
    def field_space(self):
        return self._field_space

    @field_space.setter
    def field_space(self,my_field_space):
        self._field_space = my_field_space

    @property
    def field_name(self):
        return self._field_name

    @property
    def field(self):
        return self._field

    @field_name.setter
    def field_name(self,my_field_name):
        self._field_name = my_field_name

    @property
    def iteration_space(self):
        return self._iteration_space

    @iteration_space.setter
    def iteration_space(self,it_space):
        self._iteration_space = it_space

    def __str__(self):
        result = "Loop["+self._id+"]: "+self._variable_name+"="+self._id+ \
               " lower="+self._start+","+self._stop+","+self._step+"\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "EndLoop"
        return result

    def has_inc_arg(self, mapping={}):
        ''' Returns True if any of the Kernels called within this
        loop have an argument with INC access. Returns False otherwise '''
        assert mapping != {}, "psyGen:Loop:has_inc_arg: Error - a mapping "\
                          "must be provided"
        for kern_call in self.kern_calls():
            for arg in kern_call.arguments.args:
                if arg.access.lower() == mapping["inc"]:
                    return True
        return False

    def gen_code(self, parent):
        if self._start == "1" and self._stop == "1": # no need for a loop
            for child in self.children:
                child.gen_code(parent)
        else:
            from f2pygen import DoGen, DeclGen
            do = DoGen(parent, self._variable_name, self._start, self._stop)
            # need to add do loop before children as children may want to add
            # info outside of do loop
            parent.add(do)
            for child in self.children:
                child.gen_code(do)
            my_decl = DeclGen(parent, datatype = "integer",
                            entity_decls = [self._variable_name])
            parent.add(my_decl)


class Call(Node):

    # added this functionality to the f2pycodegen
    #def before_parent_loop(self,node):
    #    ''' A call may want to add some content immediately before its parent
    #        loop(s). However the number of loops and whether the top loop has
    #        a directive before it is not known. This routine recurses up to
    #        the top loop and returns a position before any directives.
    #    '''
    #    return node
    #def after_parent_loop(self,node):
    #    ''' A call may want to add some content immediately after its parent
    #        loop(s). However the number of loops and whether the top loop has
    #        a directive after it is not known by the call. This routine
    #        recurses up to the top loop and returns a position after any
    #        directives that follow the top loop.
    #    '''
    #    return node

    def view(self, indent = 0):
        print self.indent(indent)+"Call", \
              self.name+"("+str(self.arguments.raw_arg_list)+")"
        for entity in self._children:
            entity.view(indent = indent + 1)

    @property
    def width(self):
        return self._width

    @property
    def height(self):
        return self._height

    def tkinter_delete(self):
        if self._shape is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._shape)
        if self._text is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._text)

    def tkinter_display(self, canvas, x, y):
        self.tkinter_delete()
        self._canvas = canvas
        self._x = x
        self._y = y
        self._shape = self._canvas.create_rectangle(self._x, self._y,
                          self._x+self._width, self._y+self._height,
                          outline = "red", fill = "yellow",
                          activeoutline = "blue", width = 2)
        self._text = self._canvas.create_text(self._x+self._width/2,
                                              self._y+self._height/2,
                                              text = self._name)

    def __init__(self, parent, call, name, arguments):
        Node.__init__(self, children = [], parent = parent)
        self._module_name = call.module_name
        self._arguments = arguments
        self._name = name

        # visual properties
        self._width = 250
        self._height = 30
        self._shape = None
        self._text = None
        self._canvas = None

    def set_constraints(self):
        # first set up the dependencies of my arguments
        self.arguments.set_dependencies
        # TODO: set up constraints between calls
        
    @property
    def arguments(self):
        return self._arguments
    @property
    def name(self):
        return self._name
    def __str__(self):
        raise NotImplementedError("Call.__str__ should be implemented")
    def iterates_over(self):
        raise NotImplementedError("Call.iterates_over should be implemented")
    def local_vars(self):
        raise NotImplementedError("Call.local_vars should be implemented")
    def gen_code(self):
        raise NotImplementedError("Call.gen_code should be implemented")

class Inf(object):
    ''' infrastructure call factory, Used to create a call specific class '''
    @staticmethod
    def create(call, parent = None):
        supported_calls = ["set"]
        if call.func_name not in supported_calls:
            raise GenerationError("Unknown infrastructure call. Supported "
                                  "calls are {0} but found {1}".format(
                                  str(supported_calls), call.func_name))
        if call.func_name == "set": return SetInfCall(call, parent)

class SetInfCall(Call):
    ''' the set infrastructure call '''
    def __init__(self, call, parent = None):
        assert call.func_name == "set", "Error"
        access = ["write", None]
        Call.__init__(self, parent, call, call.func_name,
                      InfArguments(call, self, access))
        #self._arguments = InfArguments(call, self, access)
    def __str__(self):
        return "set inf call"
    def gen_code(self, parent):
        from f2pygen import AssignGen
        field_name = self._arguments.arglist[0]
        var_name = field_name+"%data"
        value = self._arguments.arglist[1]
        assign_2 = AssignGen(parent, lhs = var_name, rhs = value)
        parent.add(assign_2)
        return

class Kern(Call):
    def __init__(self, KernelArguments, call, parent = None, check = True):
        Call.__init__(self, parent, call, call.ktype.procedure.name,
                      KernelArguments(call, self))
        self._iterates_over = call.ktype.iterates_over
        if check and len(call.ktype.arg_descriptors) != len(call.args):
            raise GenerationError(
                "error: In kernel '{0}' the number of arguments specified "
                "in the kernel metadata '{1}', must equal the number of "
                "arguments in the algorithm layer. However, I found '{2}'".\
                format(call.ktype.procedure.name,
                       len(call.ktype.arg_descriptors),
                       len(call.args)))
        self._arg_descriptors = call.ktype.arg_descriptors

    def __str__(self):
        return "kern call: "+self._name
    @property
    def iterates_over(self):
        return self._iterates_over

    @property
    def arg_descriptors(self):
        return self._arg_descriptors

    def gen_code(self, parent):
        from f2pygen import CallGen, UseGen
        parent.add(CallGen(parent, self._name, self._arguments.arglist))
        parent.add(UseGen(parent, name = self._module_name, only = True,
                          funcnames = [self._name]))

    def incremented_field(self, mapping={}):
        ''' Returns the argument corresponding to a field that has
        INC access. Raises a GenerationError if none is found. '''
        assert mapping != {}, "psyGen:Kern:incremented_field: Error - a "\
                          "mapping must be provided"
        for arg in self.arguments.args:
            if arg.access.lower() == mapping["inc"]:
                return arg
        raise FieldNotFoundError("Kernel {0} does not have an argument with "
                                 "{1} access".\
                                 format(self.name, mapping["inc"]))

    def written_field(self, mapping={}):
        ''' Returns the argument corresponding to a field that has
        WRITE access '''
        assert mapping != {}, "psyGen:Kern:written_field: Error - a "\
                          "mapping must be provided"
        for arg in self.arguments.args:
            if arg.access.lower() == mapping["write"]:
                return arg
        raise FieldNotFoundError("Kernel {0} does not have an argument with "
                                 "{1} access".\
                                 format(self.name, mapping["write"]))

    def is_coloured(self):
        ''' Returns true if this kernel is being called from within a
        coloured loop '''
        return self.parent.loop_type == "colour"


class Arguments(object):
    ''' arguments abstract base class '''
    def __init__(self, parent_call):
        self._parent_call = parent_call
        self._args = []

    @property
    def raw_arg_list(self):
        ''' returns a comma separated list of the field arguments to the
            kernel call '''
        result = ""
        for idx, arg in enumerate(self.args):
            result += arg.name
            if idx < (len(self.args) - 1):
                result += ","
        return result

    @property
    def args(self):
        return self._args

    def iteration_space_arg(self, mapping={}):
        assert mapping != {}, "psyGen:Arguments:iteration_space_arg: Error a mapping needs to be provided"
        for arg in self._args:
            if arg.access.lower() == mapping["write"] or \
               arg.access.lower() == mapping["readwrite"] or \
               arg.access.lower() == mapping["inc"] :
                return arg
        raise GenerationError("psyGen:Arguments:iteration_space_arg Error, "
                              "we assume there is at least one writer, "
                              "reader/writer, or increment as an argument")

    def set_dependencies(self):
        for argument in self._args:
            argument.set_dependencies()
        # TODO create a summary of dependencies

class InfArguments(Arguments):
    ''' arguments associated with an infrastructure call '''
    def __init__(self, call_info, parent_call, access):
        Arguments.__init__(self, parent_call)
        if False:
            # only here for pyreverse!
            self._0_to_n = InfArgument(None, None, None)
        for idx, arg in enumerate(call_info.args):
            self._args.append(InfArgument(arg, parent_call, access[idx]))
    @property
    def args(self):
        return self._args
    @property
    def arglist(self):
        my_arg_list = []
        for arg in self._args:
            my_arg_list.append(arg.name)
        return my_arg_list

class Argument(object):
    ''' argument base class '''
    def __init__(self, call, arg_info, access):
        self._dependencies = Dependencies(self)
        self._call = call
        self._text = arg_info.text
        self._orig_name = arg_info.varName
        self._form = arg_info.form
        self._is_literal = arg_info.is_literal()
        self._access = access
        if self._orig_name is None:
            # this is an infrastructure call literal argument. Therefore
            # we do not want an argument (_text=None) but we do want to
            # keep the value (_name)
            self._name = arg_info.text
            self._text = None
        else:
            self._name_space_manager = NameSpaceFactory().create()
            # use our namespace manager to create a unique name unless
            # the context and label match and in this case return the
            # previous name
            self._name = self._name_space_manager.create_name(root_name=self._orig_name, context="AlgArgs",label=self._text)

    def __str__(self):
        return self._name
    @property
    def name(self):
        return self._name
    @property
    def text(self):
        return self._text
    @property
    def form(self):
        return self._form
    @property
    def is_literal(self):
        return self._is_literal
    @property
    def access(self):
        return self._access
    def set_dependencies(self):
        writers = ["WRITE", "INC", "SUM"]
        readers = ["READ", "INC"]
        self._true_dependence=[]
        self._anti_dependence=[]
        for following_call in self._call.following_calls:
            for argument in following_call.arguments.args:
                if argument.name == self._name:
                    if self.access in writers and argument.access in readers:
                        self._true_dependence.append(argument)
                    if self.access in readers and argument.access in writers:
                        self._anti_dependence.append(argument)
    def has_true_dependence(self):
        if len(self._true_dependence) > 0:
            return True
        return False
    def has_anti_dependence(self):
        if len(self._anti_dependence) > 0:
            return True
        return False
    def has_dependence(self):
        if self.has_anti_dependence() or self.has_true_dependence():
            return True
        return False
    def true_dependencies(self):
        return self._true_dependence
    def anti_dependencies(self):
        return self._anti_dependence
    def dependencies(self):
        return self.true_dependencies()+self.anti_dependencies()

class KernelArgument(Argument):
    def __init__(self, arg, arg_info, call):
        self._arg = arg
        Argument.__init__(self, call, arg_info, arg.access)
    @property
    def space(self):
        return self._arg.function_space
    @property
    def stencil(self):
        return self._arg.stencil

class InfArgument(Argument):
    ''' infrastructure call argument '''
    def __init__(self, arg_info, call, access):
        Argument.__init__(self, call, arg_info, access)

class TransInfo(object):
    '''
    This class provides information about, and access, to the available
    transformations in this implementation of PSyclone. New transformations
    will be picked up automatically as long as they subclass the abstract
    Transformation class.

    For example:

    >>> from psyGen import TransInfo
    >>> t = TransInfo()
    >>> print t.list
    There is 1 transformation available:
      1: SwapTrans, A test transformation
    >>> # accessing a transformation by index
    >>> trans = t.get_trans_num(1)
    >>> # accessing a transformation by name
    >>> trans = t.get_trans_name("SwapTrans")

    '''

    def __init__(self, module = None, base_class = None):
        ''' if module and/or baseclass are provided then use these else use
            the default module "Transformations" and the default base_class
            "Transformation"'''

        if False:
            self._0_to_n = DummyTransformation() # only here for pyreverse!

        if module is None:
            # default to the transformation module
            import transformations
            module = transformations
        if base_class is None:
            import psyGen
            base_class = psyGen.Transformation
        # find our transformations
        self._classes = self._find_subclasses(module, base_class)

        # create our transformations
        self._objects = []
        self._obj_map = {}
        for my_class in self._classes:
            my_object = my_class()
            self._objects.append(my_object)
            self._obj_map[my_object.name] = my_object

    @property
    def list(self):
        ''' return a string with a human readable list of the available
            transformations '''
        import os
        if len(self._objects) == 1:
            result = "There is 1 transformation available:"
        else:
            result = "There are {0} transformations available:".format(
                     len(self._objects))
        result += os.linesep
        for idx, my_object in enumerate(self._objects):
            result += "  "+str(idx+1)+": "+my_object.name+": "+ \
                      str(my_object)+os.linesep
        return result

    @property
    def num_trans(self):
        ''' return the number of transformations available '''
        return len(self._objects)

    def get_trans_num(self, number):
        ''' return the transformation with this number (use list() first to
            see available transformations) '''
        if number < 1 or number > len(self._objects):
            raise GenerationError("Invalid transformation number supplied")
        return self._objects[number-1]

    def get_trans_name(self, name):
        ''' return the transformation with this name (use list() first to see
            available transformations) '''
        try:
            return self._obj_map[name]
        except KeyError:
            raise GenerationError("Invalid transformation name supplied")

    def _find_subclasses(self, module, base_class):
        ''' return a list of classes defined within the specified module that
            are a subclass of the specified baseclass. '''
        import inspect
        return [
            cls
                for name, cls in inspect.getmembers(module)
                    if inspect.isclass(cls) and \
                       issubclass(cls, base_class) and not cls is base_class
            ]

class Transformation(object):
    ''' abstract baseclass for a transformation. Uses the abc module so it
        can not be instantiated. '''
    __metaclass__ = abc.ABCMeta
    @abc.abstractproperty
    def name(self):
        return
    @abc.abstractmethod
    def apply(self):
        schedule = None
        momento = None
        return schedule, momento

class DummyTransformation(Transformation):
    def name(self):
        return
    def apply(self):
        return None, None
