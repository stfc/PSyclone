# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

''' Fortran code-generation library. This wraps the f2py fortran parser to
    provide routines which can be used to generate fortran code. This library
    includes pytest tests. '''

import fparser
import fparser.one
from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.one.statements import Comment, Case
from fparser.one.block_statements import SelectCase, SelectType, EndSelect
# This alis is useful to refer to parts of fparser.one later but
# cannot be used for imports (as that involves looking for the
# specified name in sys.modules).
from fparser import one as fparser1

# Module-wide utility methods


def bubble_up_type(obj):
    ''' Returns True if the supplied object is of a type which must be
    bubbled-up (from within e.g. DO loops) '''
    return (isinstance(obj, UseGen) or
            isinstance(obj, DeclGen) or
            isinstance(obj, TypeDeclGen))


def index_of_object(alist, obj):
    '''Effectively implements list.index(obj) but returns the index of
    the first item in the list that *is* the supplied object (rather than
    comparing values) '''
    for idx, body in enumerate(alist):
        if body is obj:
            return idx
    raise Exception("Object {0} not found in list".format(str(obj)))


# This section subclasses the f2py comment class so that we can
# reason about directives


class OMPDirective(Comment):
    ''' Subclass f2py comment for OpenMP directives so we can
        reason about them when walking the tree '''
    def __init__(self, root, line, position, dir_type):
        self._types = ["parallel do", "parallel", "do", "master"]
        self._positions = ["begin", "end"]
        if dir_type not in self._types:
            raise RuntimeError("Error, unrecognised directive type '{0}'. "
                               "Should be one of {1}".
                               format(dir_type, self._types))
        if position not in self._positions:
            raise RuntimeError("Error, unrecognised position '{0}'. "
                               "Should be one of {1}".
                               format(position, self._positions))
        self._my_type = dir_type
        self._position = position
        Comment.__init__(self, root, line)

    @property
    def type(self):
        ''' Returns the type of this OMP Directive (one of 'parallel do',
        'parallel' or 'do') '''
        return self._my_type

    @property
    def position(self):
        ''' Returns the position of this OMP Directive ('begin' or 'end') '''
        return self._position

# This section provides new classes which provide a relatively high
# level interface to creating code and adding code to an existing ast


class BaseGen(object):
    ''' The base class for all classes that are responsible for generating
    distinct code elements (modules, subroutines, do loops etc.) '''
    def __init__(self, parent, root):
        self._parent = parent
        self._root = root
        self._children = []

    @property
    def parent(self):
        ''' Returns the parent of this object '''
        return self._parent

    @property
    def children(self):
        ''' Returns the list of children of this object '''
        return self._children

    @property
    def root(self):
        ''' Returns the root of the tree containing this object '''
        return self._root

    def add(self, new_object, position=None):
        '''Adds a new object to the tree. The actual position is determined by
        the position argument. Note, there are two trees, the first is
        the f2pygen object tree, the other is the f2py generated code
        tree. These are similar but different. At the moment we
        specify where to add things in terms of the f2pygen tree
        (which is a higher level api) but we also insert into the f2py
        tree at exactly the same location which needs to be sorted out
        at some point.

        '''

        # By default the position is 'append'. We set it up this way for
        # safety because in python, default arguments are instantiated
        # as objects at the time of definition. If this object is
        # subsequently modified then the value of the default argument
        # is modified for subsequent calls of this routine.
        if position is None:
            position = ["append"]

        if position[0] == "auto":
            raise Exception("Error: BaseGen:add: auto option must be "
                            "implemented by the sub class!")
        options = ["append", "first", "after", "before", "insert",
                   "before_index", "after_index"]
        if position[0] not in options:
            raise Exception("Error: BaseGen:add: supported positions are "
                            "{0} but found {1}".
                            format(str(options), position[0]))
        if position[0] == "append":
            self.root.content.append(new_object.root)
        elif position[0] == "first":
            self.root.content.insert(0, new_object.root)
        elif position[0] == "insert":
            index = position[1]
            self.root.content.insert(index, new_object.root)
        elif position[0] == "after":
            idx = index_of_object(self.root.content, position[1])
            self.root.content.insert(idx+1, new_object.root)
        elif position[0] == "after_index":
            self.root.content.insert(position[1]+1, new_object.root)
        elif position[0] == "before_index":
            self.root.content.insert(position[1], new_object.root)
        elif position[0] == "before":
            try:
                idx = index_of_object(self.root.content, position[1])
            except Exception as err:
                print str(err)
                raise RuntimeError(
                    "Failed to find supplied object in existing content - "
                    "is it a child of the parent?")
            self.root.content.insert(idx, new_object.root)
        else:
            raise Exception("Error: BaseGen:add: internal error, should "
                            "not get to here")
        self.children.append(new_object)

    def previous_loop(self):
        ''' Returns the *last* occurence of a loop in the list of
        siblings of this node '''
        from fparser.one.block_statements import Do
        for sibling in reversed(self.root.content):
            if isinstance(sibling, Do):
                return sibling
        raise RuntimeError("Error, no loop found - there is no previous loop")

    def last_declaration(self):
        '''Returns the *last* occurrence of a Declaration in the list of
            siblings of this node

        '''
        from fparser.one.typedecl_statements import TypeDeclarationStatement
        for sibling in reversed(self.root.content):
            if isinstance(sibling, TypeDeclarationStatement):
                return sibling

        raise RuntimeError("Error, no variable declarations found")

    def start_parent_loop(self, debug=False):
        ''' Searches for the outer-most loop containing this object. Returns
        the index of that line in the content of the parent. '''
        from fparser.one.block_statements import Do
        if debug:
            print "Entered before_parent_loop"
            print "The type of the current node is {0}".format(
                str(type(self.root)))
            print ("If the current node is a Do loop then move up to the "
                   "top of the do loop nest")

        # First off, check that we do actually have an enclosing Do loop
        current = self.root
        while not isinstance(current, Do) and getattr(current, 'parent', None):
            current = current.parent
        if not isinstance(current, Do):
            raise RuntimeError("This node has no enclosing Do loop")

        current = self.root
        local_current = self
        while isinstance(current.parent, Do):
            if debug:
                print "Parent is a do loop so moving to the parent"
            current = current.parent
            local_current = local_current.parent
        if debug:
            print "The type of the current node is now " + str(type(current))
            print "The type of parent is " + str(type(current.parent))
            print "Finding the loops position in its parent ..."
        index = current.parent.content.index(current)
        if debug:
            print "The loop's index is ", index
        parent = current.parent
        local_current = local_current.parent
        if debug:
            print "The type of the object at the index is " + \
                str(type(parent.content[index]))
            print "If preceding node is a directive then move back one"
        if index == 0:
            if debug:
                print "current index is 0 so finish"
        elif isinstance(parent.content[index-1], OMPDirective):
            if debug:
                print (
                    "preceding node is a directive so find out what type ...\n"
                    "type is {0}\ndirective is {1}".
                    format(parent.content[index-1].position,
                           str(parent.content[index-1])))
            if parent.content[index-1].position == "begin":
                if debug:
                    print "type of directive is begin so move back one"
                index -= 1
            else:
                if debug:
                    print "directive type is not begin so finish"
        else:
            if debug:
                print "preceding node is not a directive so finish"
        if debug:
            print "type of final location ", type(parent.content[index])
            print "code for final location ", str(parent.content[index])
        return local_current, parent.content[index]


class ProgUnitGen(BaseGen):
    ''' Functionality relevant to program units (currently modules,
    subroutines)'''
    def __init__(self, parent, sub):
        BaseGen.__init__(self, parent, sub)

    def add(self, content, position=None, bubble_up=False):
        '''Specialise the add method to provide module and subroutine
           specific intelligent adding of use statements, implicit
           none statements and declarations if the position argument
           is set to auto (which is the default)'''

        # By default the position is 'auto'. We set it up this way for
        # safety because in python, default arguments are instantiated
        # as objects at the time of definition. If this object is
        # subsequently modified then the value of the default argument
        # is modified for subsequent calls of this routine.
        if position is None:
            position = ["auto"]

        # For an object to be added to another we require that they
        # share a common ancestor. This means that the added object must
        # have the current object or one of its ancestors as an ancestor.
        # Loop over the ancestors of this object (starting with itself)
        self_ancestor = self.root
        while self_ancestor:
            # Loop over the ancestors of the object being added
            obj_parent = content.root.parent
            while (obj_parent != self_ancestor and
                   getattr(obj_parent, 'parent', None)):
                obj_parent = obj_parent.parent
            if obj_parent == self_ancestor:
                break
            # Object being added is not an ancestor of the current
            # self_ancestor so move one level back up the tree and
            # try again
            if getattr(self_ancestor, 'parent', None):
                self_ancestor = self_ancestor.parent
            else:
                break

        if obj_parent != self_ancestor:
            raise RuntimeError(
                "Cannot add '{0}' to '{1}' because it is not a descendant "
                "of it or of any of its ancestors.".
                format(str(content), str(self)))

        if bubble_up:
            # If content has been passed on (is being bubbled up) then change
            # its parent to be this object
            content.root.parent = self.root

        if position[0] != "auto":
            # position[0] is not 'auto' so the baseclass can deal with it
            BaseGen.add(self, content, position)
        else:
            # position[0] == "auto" so insert in a context sensitive way
            if isinstance(content, DeclGen) or \
               isinstance(content, TypeDeclGen):

                if isinstance(content, DeclGen):
                    # have I already been declared?
                    for child in self._children:
                        if isinstance(child, DeclGen):
                            # is this declaration the same type as me?
                            if child.root.name == content.root.name:
                                # we are modifying the list so we need
                                # to iterate over a copy
                                for var_name in content.root.entity_decls[:]:
                                    for child_name in child.root.entity_decls:
                                        if var_name.lower() == \
                                           child_name.lower():
                                            content.root.entity_decls.\
                                                remove(var_name)
                                            if not content.root.entity_decls:
                                                # return as all variables in
                                                # this declaration already
                                                # exist
                                                return
                if isinstance(content, TypeDeclGen):
                    # have I already been declared?
                    for child in self._children:
                        if isinstance(child, TypeDeclGen):
                            # is this declaration the same type as me?
                            if child.root.selector[1] == \
                               content.root.selector[1]:
                                # we are modifying the list so we need
                                # to iterate over a copy
                                for var_name in content.root.entity_decls[:]:
                                    for child_name in child.root.entity_decls:
                                        if var_name.lower() == \
                                           child_name.lower():
                                            content.root.entity_decls.\
                                                remove(var_name)
                                            if not content.root.entity_decls:
                                                # return as all variables in
                                                # this declaration already
                                                # exist
                                                return

                index = 0
                # skip over any use statements
                index = self._skip_use_and_comments(index)
                # skip over implicit none if it exists
                index = self._skip_imp_none_and_comments(index)
                # skip over any declarations which have an intent
                try:
                    intent = True
                    while intent:
                        intent = False
                        for attr in self.root.content[index].attrspec:
                            if attr.find("intent") == 0:
                                intent = True
                                index += 1
                                break
                except AttributeError:
                    pass
            elif isinstance(content.root, fparser1.statements.Use):
                # have I already been declared?
                for child in self._children:
                    if isinstance(child, UseGen):
                        if child.root.name == content.root.name:
                            # found an existing use with the same name
                            if not child.root.isonly and not \
                               content.root.isonly:
                                # both are generic use statements so
                                # skip this declaration
                                return
                            if child.root.isonly and not content.root.isonly:
                                # new use is generic and existing use
                                # is specific so we can safely add
                                pass
                            if not child.root.isonly and content.root.isonly:
                                # existing use is generic and new use
                                # is specific so we can skip this
                                # declaration
                                return
                            if child.root.isonly and content.root.isonly:
                                # we are modifying the list so we need
                                # to iterate over a copy
                                for new_name in content.root.items[:]:
                                    for existing_name in child.root.items:
                                        if existing_name.lower() == \
                                           new_name.lower():
                                            content.root.items.remove(new_name)
                                            if not content.root.items:
                                                return
                index = 0
            elif isinstance(content, ImplicitNoneGen):
                # does implicit none already exist?
                for child in self._children:
                    if isinstance(child, ImplicitNoneGen):
                        return
                # skip over any use statements
                index = 0
                index = self._skip_use_and_comments(index)
            else:
                index = len(self.root.content) - 1
            self.root.content.insert(index, content.root)
            self._children.append(content)

    def _skip_use_and_comments(self, index):
        ''' skip over any use statements and comments in the ast '''
        while isinstance(self.root.content[index],
                         fparser1.statements.Use) or\
            isinstance(self.root.content[index],
                       fparser1.statements.Comment):
            index += 1
        # now roll back to previous Use
        while isinstance(self.root.content[index-1],
                         fparser1.statements.Comment):
            index -= 1
        return index

    def _skip_imp_none_and_comments(self, index):
        ''' skip over an implicit none statement if it exists and any
        comments before it '''
        end_index = index
        while isinstance(self.root.content[index],
                         fparser1.typedecl_statements.Implicit) or\
            isinstance(self.root.content[index],
                       fparser1.statements.Comment):
            if isinstance(self.root.content[index],
                          fparser1.typedecl_statements.Implicit):
                end_index = index + 1
                break
            else:
                index = index + 1
        return end_index


class ModuleGen(ProgUnitGen):
    ''' create a fortran module '''
    def __init__(self, name="", contains=True, implicitnone=True):
        from fparser import api

        code = '''\
module vanilla
'''
        if contains:
            code += '''\
contains
'''
        code += '''\
end module vanilla
'''
        tree = api.parse(code, ignore_comments=False)
        module = tree.content[0]
        module.name = name
        endmod = module.content[len(module.content)-1]
        endmod.name = name
        ProgUnitGen.__init__(self, None, module)
        if implicitnone:
            self.add(ImplicitNoneGen(self))

    def add_raw_subroutine(self, content):
        ''' adds a subroutine to the module that is a raw f2py parse object.
            This is used for inlining kernel subroutines into a module.
        '''
        from psyclone.parse import KernelProcedure
        if not isinstance(content, KernelProcedure):
            raise Exception(
                "Expecting a KernelProcedure type but received " +
                str(type(content)))
        content.ast.parent = self.root
        # add content after any existing subroutines
        index = len(self.root.content) - 1
        self.root.content.insert(index, content.ast)


class CommentGen(BaseGen):
    ''' Create a Fortran Comment '''
    def __init__(self, parent, content):
        '''
        :param parent: node in AST to which to add the Comment as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str content: the content of the comment
        '''
        reader = FortranStringReader("! content\n")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        subline = reader.next()

        my_comment = Comment(parent.root, subline)
        my_comment.content = content

        BaseGen.__init__(self, parent, my_comment)


class DirectiveGen(BaseGen):
    ''' Base class for creating a Fortran directive. This is then sub-classed
    to support different types of directive, e.g. OpenMP or OpenACC. '''
    def __init__(self, parent, language, position, directive_type, content):
        '''
        :param parent: node in AST to which to add directive as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str language: the type of directive (e.g. OMP or ACC)
        :param str position: "end" if this is the end of a directive block
        :param str directive_type: the directive itself (e.g. "PARALLEL DO")
        :param str content: any additional arguments to add to the directive
                            (e.g. "PRIVATE(ji)")

        :raises RuntimeError: if an unrecognised directive language is
                              specified
        '''
        self._supported_languages = ["omp"]
        self._language = language
        self._directive_type = directive_type

        reader = FortranStringReader("! content\n")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        subline = reader.next()

        if language == "omp":
            my_comment = OMPDirective(parent.root, subline, position,
                                      directive_type)
            my_comment.content = "$omp"
            if position == "end":
                my_comment.content += " end"
            my_comment.content += " " + directive_type
            if content != "":
                my_comment.content += " " + content
        else:
            raise RuntimeError(
                "Error, unsupported directive language. Expecting one of "
                "{0} but found '{1}'".format(str(self._supported_languages),
                                             language))

        BaseGen.__init__(self, parent, my_comment)


class ImplicitNoneGen(BaseGen):
    ''' Generate a Fortran 'implicit none' statement '''
    def __init__(self, parent):
        '''
        :param parent: node in AST to which to add 'implicit none' as a child
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen` or
                      :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises Exception: if `parent` is not a ModuleGen or SubroutineGen
        '''
        if not isinstance(parent, ModuleGen) and not isinstance(parent,
                                                                SubroutineGen):
            raise Exception(
                "The parent of ImplicitNoneGen must be a module or a "
                "subroutine, but found {0}".format(type(parent)))
        reader = FortranStringReader("IMPLICIT NONE\n")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        subline = reader.next()

        from fparser.one.typedecl_statements import Implicit
        my_imp_none = Implicit(parent.root, subline)

        BaseGen.__init__(self, parent, my_imp_none)


class SubroutineGen(ProgUnitGen):
    ''' Generate a Fortran subroutine '''
    def __init__(self, parent, name="", args=None, implicitnone=False):
        '''
        :param parent: node in AST to which to add Subroutine as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str name: name of the Fortran subroutine
        :param list args: list of arguments accepted by the subroutine
        :param bool implicitnone: whether or not we should specify
                                  "implicit none" for the body of this
                                  subroutine
        '''
        reader = FortranStringReader(
            "subroutine vanilla(vanilla_arg)\nend subroutine")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        subline = reader.next()
        endsubline = reader.next()

        from fparser.one.block_statements import Subroutine, EndSubroutine
        self._sub = Subroutine(parent.root, subline)
        self._sub.name = name
        if args is None:
            args = []
        self._sub.args = args
        endsub = EndSubroutine(self._sub, endsubline)
        self._sub.content.append(endsub)
        ProgUnitGen.__init__(self, parent, self._sub)
        if implicitnone:
            self.add(ImplicitNoneGen(self))

    @property
    def args(self):
        ''' Returns the list of arguments of this subroutine '''
        return self._sub.args

    @args.setter
    def args(self, namelist):
        ''' sets the subroutine arguments to the values in the list provide.'''
        self._sub.args = namelist


class CallGen(BaseGen):
    ''' Generates a Fortran call of a subroutine '''
    def __init__(self, parent, name="", args=None):
        '''
        :param parent: node in AST to which to add CallGen as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str name: the name of the routine to call
        :param list args: list of arguments to pass to the call
        '''
        reader = FortranStringReader("call vanilla(vanilla_arg)")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        myline = reader.next()

        from fparser.one.block_statements import Call
        self._call = Call(parent.root, myline)
        self._call.designator = name
        if args is None:
            args = []
        self._call.items = args

        BaseGen.__init__(self, parent, self._call)


class UseGen(BaseGen):
    ''' Generate a Fortran use statement '''
    def __init__(self, parent, name="", only=False, funcnames=None):
        '''
        :param parent: node in AST to which to add UseGen as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str name: name of the module to USE
        :param bool only: whether this USE has an ONLY clause
        :param list funcnames: list of names to follow ONLY clause
        '''
        reader = FortranStringReader("use kern,only : func1_kern=>func1")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        myline = reader.next()
        root = parent.root
        from fparser.one.block_statements import Use
        use = Use(root, myline)
        use.name = name
        use.isonly = only
        if funcnames is None:
            funcnames = []
            use.isonly = False
        local_funcnames = funcnames[:]
        use.items = local_funcnames
        BaseGen.__init__(self, parent, use)


def adduse(name, parent, only=False, funcnames=None):
    '''
    Adds a use statement with the specified name to the supplied object.
    This routine is required when modifying an existing AST (e.g. when
    modifying a kernel). The classes are used when creating an AST from
    scratch (for the PSy layer).

    :param str name: name of module to USE
    :param parent: node in fparser1 AST to which to add this USE as a child
    :type parent: :py:class:`fparser.one.block_statements.*`
    :param bool only: whether this USE has an "ONLY" clause
    :param list funcnames: list of quantities to follow the "ONLY" clause

    :returns: an fparser1 Use object
    :rtype: :py:class:`fparser.one.block_statements.Use`
    '''
    reader = FortranStringReader("use kern,only : func1_kern=>func1")
    reader.set_format(FortranFormat(True, True))  # free form, strict
    myline = reader.next()

    # find an appropriate place to add in our use statement
    while not (isinstance(parent, fparser1.block_statements.Program) or
               isinstance(parent, fparser1.block_statements.Module) or
               isinstance(parent, fparser1.block_statements.Subroutine)):
        parent = parent.parent
    use = fparser1.block_statements.Use(parent, myline)
    use.name = name
    use.isonly = only
    if funcnames is None:
        funcnames = []
        use.isonly = False
    use.items = funcnames

    parent.content.insert(0, use)
    return use


class AllocateGen(BaseGen):
    ''' Generates a Fortran allocate statement '''
    def __init__(self, parent, content):
        '''
        :param parent: node to which to add this ALLOCATE as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param content: string or list of variables to allocate
        :type content: list of strings or a single string

        :raises RuntimeError: if `content` is not of correct type
        '''
        reader = FortranStringReader("allocate(dummy)")
        reader.set_format(FortranFormat(True, False))  # free form, strict
        myline = reader.next()
        self._decl = fparser1.statements.Allocate(parent.root, myline)
        if isinstance(content, str):
            self._decl.items = [content]
        elif isinstance(content, list):
            self._decl.items = content
        else:
            raise RuntimeError(
                "AllocateGen expected the content argument to be a str or"
                " a list, but found {0}".format(type(content)))
        BaseGen.__init__(self, parent, self._decl)


class DeallocateGen(BaseGen):
    ''' Generates a Fortran deallocate statement '''
    def __init__(self, parent, content):
        '''
        :param parent: node to which to add this DEALLOCATE as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param content: string or list of variables to deallocate
        :type content: list of strings or a single string

        :raises RuntimeError: if `content` is not of correct type
        '''
        reader = FortranStringReader("deallocate(dummy)")
        reader.set_format(FortranFormat(True, False))  # free form, strict
        myline = reader.next()
        self._decl = fparser1.statements.Deallocate(parent.root, myline)
        if isinstance(content, str):
            self._decl.items = [content]
        elif isinstance(content, list):
            self._decl.items = content
        else:
            raise RuntimeError(
                "DeallocateGen expected the content argument to be a str"
                " or a list, but found {0}".format(type(content)))
        BaseGen.__init__(self, parent, self._decl)


class DeclGen(BaseGen):
    ''' Generates a Fortran declaration for variables of intrinsic type '''
    def __init__(self, parent, datatype="", entity_decls=None, intent="",
                 pointer=False, kind="", dimension="", allocatable=False):
        '''
        :param parent: node to which to add this declaration as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str datatype: the (intrinsic) type for this declaration
        :param list entity_decls: list of variable names to declare
        :param str intent: the INTENT attribute of this declaration
        :param bool pointer: whether or not this is a pointer declaration
        :param str kind: the KIND attribute to use for this declaration
        :param str dimension: the DIMENSION specifier (i.e. the xx in
                              DIMENSION(xx))
        :param bool allocatable: whether this declaration is for an
                                 ALLOCATABLE quantity

        :raises RuntimeError: if no variable names are specified
        :raises RuntimeError: if datatype is not one of "integer" or "real"
        '''
        if entity_decls is None:
            raise RuntimeError(
                "Cannot create a variable declaration without specifying the "
                "name(s) of the variable(s)")
        fort_fmt = FortranFormat(True, False)  # free form, strict
        if datatype.lower() == "integer":
            reader = FortranStringReader("integer :: vanilla")
            reader.set_format(fort_fmt)
            myline = reader.next()
            self._decl = fparser1.typedecl_statements.Integer(parent.root,
                                                              myline)
        elif datatype.lower() == "real":
            reader = FortranStringReader("real :: vanilla")
            reader.set_format(fort_fmt)
            myline = reader.next()
            self._decl = fparser1.typedecl_statements.Real(parent.root, myline)
        else:
            raise RuntimeError(
                "f2pygen:DeclGen:init: Only integer and real are currently"
                " supported and you specified '{0}'".format(datatype))
        # make a copy of entity_decls as we may modify it
        local_entity_decls = entity_decls[:]
        self._decl.entity_decls = local_entity_decls
        my_attrspec = []
        if intent != "":
            my_attrspec.append("intent({0})".format(intent))
        if pointer is not False:
            my_attrspec.append("pointer")
        if allocatable is not False:
            my_attrspec.append("allocatable")
        self._decl.attrspec = my_attrspec
        if dimension != "":
            my_attrspec.append("dimension({0})".format(dimension))
        if kind is not "":
            self._decl.selector = ('', kind)
        BaseGen.__init__(self, parent, self._decl)


class TypeDeclGen(BaseGen):
    ''' Generates a Fortran declaration for variables of a derived type '''
    def __init__(self, parent, datatype="", entity_decls=None, intent="",
                 pointer=False, attrspec=None):
        '''
        :param parent: the node to which to add this type delcn as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str datatype: the derived type
        :param list entity_decls: List of variable names to declare
        :param str intent: the intent attribute for the declaration
        :param bool pointer: whether or not this is a pointer declaration
        :param attrspec: list of other attributes to add to declaration

        :raises RuntimeError: if no variable names are specified
        '''
        if entity_decls is None:
            raise RuntimeError(
                "Cannot create a declaration of a derived-type variable "
                "without specifying the name(s) of the variable(s)")
        # make a copy of entity_decls as we may modify it
        local_entity_decls = entity_decls[:]
        if attrspec is None:
            attrspec = []
        my_attrspec = [spec for spec in attrspec]
        if intent != "":
            my_attrspec.append("intent({0})".format(intent))
        if pointer is not False:
            my_attrspec.append("pointer")
        self._names = local_entity_decls

        reader = FortranStringReader("type(vanillatype) :: vanilla")
        reader.set_format(FortranFormat(True, False))  # free form, strict
        myline = reader.next()

        self._typedecl = fparser1.typedecl_statements.Type(parent.root, myline)
        self._typedecl.selector = ('', datatype)
        self._typedecl.attrspec = my_attrspec
        self._typedecl.entity_decls = local_entity_decls
        BaseGen.__init__(self, parent, self._typedecl)

    @property
    def names(self):
        ''' Returns the names of the variables being declared '''
        return self._names

    @property
    def root(self):
        ''' Returns the associated Type object '''
        return self._typedecl


class TypeCase(Case):
    ''' Generate a Fortran SELECT CASE statement '''
    # TODO can this whole class be deleted?
    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        type_str = 'TYPE IS'
        if self.items:
            item_list = []
            for item in self.items:
                item_list.append((' : '.join(item)).strip())
            type_str += ' ( %s )' % (', '.join(item_list))
        else:
            type_str = 'CLASS DEFAULT'
        if self.name:
            type_str += ' ' + self.name
        return tab + type_str


class SelectionGen(BaseGen):
    ''' Generate a Fortran SELECT block '''
    # TODO can this whole class be deleted?

    def __init__(self, parent, expr="UNSET", typeselect=False):
        '''
        Construct a SelectionGen for creating a SELECT block

        :param parent: node to which to add this select block as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str expr: the CASE expression
        :param bool typeselect: whether or not this is a SELECT TYPE rather
                                than a SELECT CASE
        '''
        self._typeselect = typeselect
        reader = FortranStringReader(
            "SELECT CASE (x)\nCASE (1)\nCASE DEFAULT\nEND SELECT")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        select_line = reader.next()
        self._case_line = reader.next()
        self._case_default_line = reader.next()
        end_select_line = reader.next()
        if self._typeselect:
            select = SelectType(parent.root, select_line)
        else:
            select = SelectCase(parent.root, select_line)
        endselect = EndSelect(select, end_select_line)
        select.expr = expr
        select.content.append(endselect)
        BaseGen.__init__(self, parent, select)

    def addcase(self, casenames, content=None):
        ''' Add a case to this select block '''
        if content is None:
            content = []
        if self._typeselect:
            case = TypeCase(self.root, self._case_line)
        else:
            case = Case(self.root, self._case_line)
        case.items = [casenames]
        self.root.content.insert(0, case)
        idx = 0
        for stmt in content:
            idx += 1
            self.root.content.insert(idx, stmt.root)

    def adddefault(self):
        ''' Add the default case to this select block '''
        if self._typeselect:
            case_default = TypeCase(self.root, self._case_default_line)
        else:
            case_default = Case(self.root, self._case_default_line)
        self.root.content.insert(len(self.root.content)-1, case_default)


class DoGen(BaseGen):
    ''' Create a Fortran Do loop '''
    def __init__(self, parent, variable_name, start, end, step=None):
        '''
        :param parent: the node to which to add this do loop as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str variable_name: the name of the loop variable
        :param str start: start value for Do loop
        :param str end: upper-limit of Do loop
        :param str step: increment to use in Do loop
        '''
        reader = FortranStringReader("do i=1,n\nend do")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        doline = reader.next()
        enddoline = reader.next()
        dogen = fparser1.block_statements.Do(parent.root, doline)
        dogen.loopcontrol = variable_name + "=" + start + "," + end
        if step is not None:
            dogen.loopcontrol = dogen.loopcontrol + "," + step
        enddo = fparser1.block_statements.EndDo(dogen, enddoline)
        dogen.content.append(enddo)

        BaseGen.__init__(self, parent, dogen)

    def add(self, content, position=None, bubble_up=False):
        if position is None:
            position = ["auto"]

        if position[0] == "auto" and bubble_up:
            # There's currently no case where a bubbled-up statement
            # will live within a do loop so bubble it up again.
            self.parent.add(content, bubble_up=True)
            return

        if position[0] == "auto" or position[0] == "append":
            if position[0] == "auto" and bubble_up_type(content):
                # use and declaration statements cannot appear in a do loop
                # so pass on to parent
                self.parent.add(content, bubble_up=True)
                return
            else:
                # append at the end of the loop. This is not a simple
                # append as the last element in the loop is the "end
                # do" so we insert at the penultimate location
                BaseGen.add(self, content,
                            position=["insert", len(self.root.content)-1])
        else:
            BaseGen.add(self, content, position=position)


class IfThenGen(BaseGen):
    ''' Generate a fortran if, then, end if statement. '''

    def __init__(self, parent, clause):
        '''
        :param parent: Node to which to add this IfThen as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str clause: the condition, xx, to evaluate in the if(xx)then
        '''
        reader = FortranStringReader("if (dummy) then\nend if")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        ifthenline = reader.next()
        endifline = reader.next()

        my_if = fparser1.block_statements.IfThen(parent.root, ifthenline)
        my_if.expr = clause
        my_endif = fparser1.block_statements.EndIfThen(my_if, endifline)
        my_if.content.append(my_endif)

        BaseGen.__init__(self, parent, my_if)

    def add(self, content, position=None):
        if position is None:
            position = ["auto"]
        if position[0] == "auto" or position[0] == "append":
            if position[0] == "auto" and bubble_up_type(content):
                # use and declaration statements cannot appear in an if
                # block so pass on (bubble-up) to parent
                self.parent.add(content, bubble_up=True)
            else:
                # append at the end of the loop. This is not a simple
                # append as the last element in the if is the "end if"
                # so we insert at the penultimate location
                BaseGen.add(self, content,
                            position=["insert", len(self.root.content)-1])
        else:
            BaseGen.add(self, content, position=position)


class AssignGen(BaseGen):
    ''' Generates a Fortran statement where a value is assigned to a
        variable quantity '''

    def __init__(self, parent, lhs="", rhs="", pointer=False):
        '''
        :param parent: the node to which to add this assignment as a child
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param str lhs: the LHS of the assignment expression
        :param str rhs: the RHS of the assignment expression
        :param bool pointer: whether or not this is a pointer assignment
        '''
        if pointer:
            reader = FortranStringReader("lhs=>rhs")
        else:
            reader = FortranStringReader("lhs=rhs")
        reader.set_format(FortranFormat(True, True))  # free form, strict
        myline = reader.next()
        if pointer:
            self._assign = fparser1.statements.PointerAssignment(parent.root,
                                                                 myline)
        else:
            self._assign = fparser1.statements.Assignment(parent.root, myline)
        self._assign.expr = rhs
        self._assign.variable = lhs
        BaseGen.__init__(self, parent, self._assign)
