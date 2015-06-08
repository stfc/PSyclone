#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from psyGen import Transformation

class TransformationError(Exception):
    ''' Provides a PSyclone-specific error class for errors found during
        code transformation operations. '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Transformation Error: "+value
    def __str__(self):
        return repr(self.value)

class SwapTrans(Transformation):
    ''' A test transformation. This swaps two entries in a schedule. These
        entries must be siblings and next to each other in the schedule.

        For example:

        >>> schedule=[please see schedule class for information]
        >>> print schedule
        >>> loop1=schedule.children[0]
        >>> loop2=schedule.children[1]
        >>> trans=SwapTrans()
        >>> newSchedule,memento=SwapTrans.apply(loop1,loop2)
        >>> print newSchedule

    '''
    def __init__(self):
        pass
    def __str__(self):
        return "A test transformation that swaps two adjacent elements in a schedule"
    @property
    def name(self):
        return "SwapTrans"

    def apply(self,node1,node2):

        # First perform any validity checks

        # TBD check node1 and node2 are in the same schedule
        if not node1.sameRoot(node2):
            raise Exception("Error in transformation. nodes are not in the same schedule")
        # TBD check node1 and node2 have the same parent
        if not node1.sameParent(node2):
            raise Exception("Error in transformation. nodes do not have the same parent")
        # TBD check node1 and node2 are next to each other
        if abs(node1.position-node2.position)!=1:
            raise Exception("Error in transformation. nodes are not siblings who are "
                            "next to eachother")

        schedule=node1.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep=Memento(schedule,self,[node1,node2])

        # find the position of nodes in the schedule
        index1=node1.parent.children.index(node1)
        index2=node2.parent.children.index(node2)

        # swap nodes
        node1.parent.children[index1]=node2
        node2.parent.children[index2]=node1

        return schedule,keep

class LoopFuseTrans(Transformation):

    def __str__(self):
        return "Fuse two adjacent loops together"

    @property
    def name(self):
        return "LoopFuse"

    def apply(self,node1,node2):

        # check nodes are loops
        from psyGen import Loop
        if not isinstance(node1,Loop) or not isinstance(node2,Loop):
            raise TransformationError("Error in LoopFuse transformation. "
                                      "At least one of the nodes is not "
                                      "a loop")
        # check loop1 and loop2 have the same parent
        if not node1.sameParent(node2):
            raise TransformationError("Error in LoopFuse transformation. "
                                      "loops do not have the same parent")
        # check node1 and node2 are next to each other
        if abs(node1.position-node2.position)!=1:
            raise TransformationError("Error in LoopFuse transformation. "
                                      "nodes are not siblings who are "
                                      "next to eachother")
        # Check iteration space is the same
        if not(node1.iteration_space == node2.iteration_space):
            raise TransformationError("Error in LoopFuse transformation. "
                                      "Loops do not have the same "
                                      "iteration space")

        schedule=node1.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep=Memento(schedule,self,[node1,node2])

        # add loop contents of node2 to node1
        node1.children.extend(node2.children)

        # change the parent of the loop contents of node2 to node1
        for child in node2.children:
            child.parent = node1

        # remove node2
        node2.parent.children.remove(node2)

        return schedule,keep

class GOceanLoopFuseTrans(LoopFuseTrans):

    def __str__(self):
        return ("Fuse two adjacent loops together with GOcean-specific "
               "validity checks")

    @property
    def name(self):
        return "GOceanLoopFuse"

    def apply(self,node1,node2):

        try:
            if node1.field_space != node2.field_space:
                raise TransformationError("Error in GOceanLoopFuse "
                                          "transformation. "
                                          "Cannot fuse loops that are over "
                                          "different grid-point types: "
                                          "{0} {1}".\
                                          format(node1.field_space,
                                                 node2.field_space))
        except TransformationError as e:
            raise e
        except Exception as e:
            raise TransformationError("Unexpected exception: {}".format(e))

        return LoopFuseTrans.apply(self,node1,node2)

class OMPParallelLoopTrans(Transformation):

    ''' Adds an OpenMP PARALLEL directive to a loop. 

        For example:

        >>> from parse import parse
        >>> from psyGen import PSyFactory
        >>> ast,invokeInfo=parse("dynamo.F90")
        >>> psy=PSyFactory("dynamo0.1").create(invokeInfo)
        >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> schedule.view()
        >>>
        >>> from transformations import OMPParallelLoopTrans
        >>> trans=OMPParallelLoopTrans()
        >>> new_schedule,memento=trans.apply(schedule.children[0])
        >>> new_schedule.view()

    '''

    @property
    def name(self):
        return "OpenMPLoop"

    def __str__(self):
        return "Add an OpenMP directive with no validity checks"

    def apply(self,node):

        schedule=node.root
        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep=Memento(schedule,self,[node])

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our OpenMP loop directive setting its parent to the node's
        # parent and its children to the node
        from psyGen import OMPParallelDoDirective
        directive = OMPParallelDoDirective(parent=node_parent, 
                                           children=[node] )

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index = node_position)

        # change the node's parent to be the loop directive
        node.parent = directive

        # remove the original loop
        node_parent.children.remove(node)

        return schedule,keep

class OMPLoopTrans(Transformation):

    ''' Adds an orphaned OpenMP directive to a loop. i.e. the directive
        must be inside the scope of some OMP Parallel REGION. '''

    @property
    def name(self):
        return "OMPLoopTrans"

    def __str__(self):
        return "Adds an orphan OpenMP Do directive with no validity checks"

    def apply(self,node):
        from psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Cannot apply an OpenMP Loop "
                                      "directive to something that is "
                                      "not a loop")

        schedule = node.root

        # create a memento of the schedule and the proposed
        # transformation
        from undoredo import Memento
        keep = Memento(schedule, self, [node])

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our orphan OpenMP loop directive setting its parent to
        # the node's parent and its children to the node
        from psyGen import OMPDoDirective
        directive = OMPDoDirective(parent=node_parent, 
                                   children=[node] )

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)

        # change the node's parent to be the loop directive
        node.parent = directive

        # remove the original loop
        node_parent.children.remove(node)

        return schedule, keep

class DynamoOMPParallelLoopTrans(OMPParallelLoopTrans):

    ''' Dynamo specific OpenMP loop transformation. Adds Dynamo specific
        validity checks. Actual transformation is done by parent class. '''

    @property
    def name(self):
        return "DynamoOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a Dynamo loop"

    def apply(self,node):

        ''' Perform Dynamo specific loop validity checks then call the parent
            class. '''
        # check node is a loop
        from psyGen import Loop
        if not isinstance(node,Loop):
            raise Exception("Error in "+self.name+" transformation. The "
                            "node is not a loop.")
        # Check iteration space is supported - only cells at the moment
        if not node.iteration_space == "cells":
            raise Exception("Error in "+self.name+" transformation. The "
                            "iteration space is not 'cells'.")
        # Check we do not need colouring
        if node.field_space != "v3" and node.loop_type is None:
            raise Exception("Error in "+self.name+" transformation. The "
                            "field space written to by the kernel is "
                            "not 'v3'. Colouring is required.")
        # Check we are not a sequential loop
        if node.loop_type == 'colours':
            raise Exception("Error in "+self.name+" transformation. "
                            "The requested loop is over colours and must "
                            "be computed serially.")
        return OMPParallelLoopTrans.apply(self,node)

class GOceanOMPParallelLoopTrans(OMPParallelLoopTrans):

    ''' GOcean specific OpenMP Do loop transformation. Adds GOcean specific
        validity checks. Actual transformation is done by parent class. '''

    @property
    def name(self):
        return "GOceanOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a GOcean loop"

    def apply(self,node):

        ''' Perform GOcean specific loop validity checks then call the parent
            class. '''
        # check node is a loop
        from psyGen import Loop
        if not isinstance(node,Loop):
            raise Exception("Error in "+self.name+" transformation. The node is not a loop.")
        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner","outer"]:
            raise Exception("Error in "+self.name+" transformation. The requested loop is not of type inner or outer.")

        return OMPParallelLoopTrans.apply(self,node)


class GOceanOMPLoopTrans(OMPLoopTrans):

    ''' GOcean specific orphan OpenMP loop transformation. Adds GOcean specific
        validity checks. Actual transformation is done by parent class. '''

    @property
    def name(self):
        return "GOceanOMPLoopTrans"

    def __str__(self):
        return "Add an orphaned OpenMP directive to a GOcean loop"

    def apply(self,node):

        ''' Perform GOcean specific loop validity checks then call the parent
            class. '''
        # check node is a loop
        from psyGen import Loop
        if not isinstance(node,Loop):
            raise Exception("Error in "+self.name+" transformation. The "
                            "node is not a loop.")
        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner","outer"]:
            raise Exception("Error in "+self.name+" transformation. The "
                            "requested loop is not of type inner or outer.")

        return OMPLoopTrans.apply(self,node)

class ColourTrans(Transformation):

    def __str__(self):
        return "Split a loop into colours"

    @property
    def name(self):
        return "LoopColour"

    def apply(self,node):

        # check node is a loop
        from psyGen import Loop
        if not isinstance(node,Loop):
            raise Exception("Error in LoopColour transformation. The "
                            "node is not a loop")
        # Check iteration space is supported - only cells at the moment
        if not node.iteration_space == "cells":
            raise Exception("Error in "+self.name+" transformation. The "
                            "iteration space is not 'cells'.")
        # Check we need colouring
        if node.field_space == "v3":
            raise Exception("Error in "+self.name+" transformation. The "
                            "field space written to by the kernel is 'v3'. "
                            "Colouring is not required.")
        # Check this is a kernel loop
        if node.loop_type is not None:
            raise Exception("Error in "+self.name+" transformation. The "
                            "loop is not the correct type for colouring.")

        schedule=node.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep=Memento(schedule,self,[node])

        # TODO CAN WE CREATE A GENERIC LOOP OR DO WE NEED SPECIFIC GH or GO LOOPS?
        node_parent = node.parent
        node_position = node.position

        from dynamo0p1 import DynLoop

        # create a colours loop. This loops over colours and must be run
        # sequentially
        colours_loop = DynLoop(parent = node_parent)
        colours_loop.loop_type="colours"
        colours_loop.field_space=node.field_space
        colours_loop.iteration_space=node.iteration_space
        node_parent.addchild(colours_loop,
                             index = node_position)

        # create a colour loop. This loops over a particular colour and
        # can be run in parallel
        colour_loop = DynLoop(parent = colours_loop)
        colour_loop.loop_type="colour"
        colour_loop.field_space=node.field_space
        colour_loop.iteration_space=node.iteration_space
        colours_loop.addchild(colour_loop)

        # add contents of node to colour loop
        colour_loop.children.extend(node.children)

        # change the parent of the node's contents to the colour loop
        for child in node.children:
            child.parent = colour_loop

        # remove original loop
        node_parent.children.remove(node)

        return schedule,keep

class OMPParallelTrans(Transformation):

    def __str__(self):
        return "Insert an OpenMP Parallel region"

    @property
    def name(self):
        return "OMPParallelTrans"

    def apply(self, nodes):
        ''' Apply this transformation to a subset of the nodes 
            within a schedule - i.e. enclose the specified
            Loops in the schedule within a single OpenMP region '''
        from psyGen import OMPParallelDirective, Schedule, Loop

        # Check whether we've been passed a list of nodes or just a 
        # single node. If the latter then we create ourselves a 
        # list containing just that node.
        from psyGen import Node
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Node):
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in OMPParallel transformation. "
                                      "Argument must be a single Node in a "
                                      "schedule or a list of Nodes in a "
                                      "schedule but have been passed an "
                                      "object of type: {0}".\
                                      format(arg_type))

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a parallel region. Also keep the index of 
        # the first child to be enclosed as that will become the
        # position of the new !$omp parallel directive.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        if not isinstance(node_parent, Schedule):
            raise TransformationError("Error in OMPParallel transformation. "
                                      "Supplied node is not a child of a "
                                      "Schedule.")

        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError("Error in OMPParallel transformation: "
                                          "supplied nodes are not children of "
                                          "the same Schedule/parent.")

        # create a memento of the schedule and the proposed
        # transformation
        schedule = node_list[0].root

        from undoredo import Memento
        keep=Memento(schedule,self)

        # Create the OpenMP parallel directive as a child of the
        # parent of the nodes being enclosed and with those nodes 
        # as its children.
        # We slice the nodes list in order to get a new list object
        # (although the actual items in the list are still those in the
        # original). If we don't do this then we get an infinite
        # recursion in the new schedule.
        directive = OMPParallelDirective(parent=node_parent,
                                         children=node_list[:])

        # Change all of the affected children so that they have
        # the OpenMP directive as their parent. Use a slice 
        # of the list of nodes so that we're looping over a local
        # copy of the list. Otherwise things get confused when
        # we remove children from the list.
        for child in node_list[:]:
            # Remove child from the parent's list of children
            node_parent.children.remove(child)
            child.parent = directive

        # Add the OpenMP region directive as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(directive,
                             index=node_position)
       
        return schedule,keep

