from psyGen import transformation

#class OpenMPParallel(Transform):
#    def __init__(self):
#        pass

class SwapTrans(transformation):
    ''' A test transformation. This swaps two entries in a schedule. These entries must be siblings and next to eachother in the schedule.

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
        return "A test transformation that swaps to adjacent elements in a schedule"
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
            raise Exception("Error in transformation. nodes are not siblings who are next to eachother")

        schedule=node1.getRoot

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep=Memento(schedule,self,[node1,node2])

        # find the nodes in the schedule
        index1=node1.parent.children.index(node1)
        index2=node2.parent.children.index(node2)

        # swap nodes
        node1.parent.children[index1]=node2
        node2.parent.children[index2]=node1

        return schedule,keep

class LoopFuseTrans(transformation):

    def __str__(self):
        return "Fuse two adjacent loops together"

    @property
    def name(self):
        return "LoopFuse"

    def apply(self,node1,node2):

        # check nodes are loops
        from psyGen import Loop
        if not isinstance(node1,Loop) or not isinstance(node2,Loop):
            raise Exception("Error in LoopFuse transformation. at least one of the nodes is not a loop")
        # check node1 and node2 have the same parent
        if not node1.sameParent(node2):
            raise Exception("Error in LoopFuse transformation. nodes do not have the same parent")
        # check node1 and node2 are next to each other
        if abs(node1.position-node2.position)!=1:
            raise Exception("Error in LoopFuse transformation. nodes are not siblings who are next to eachother")
        # TBD Check iteration space is the same

        schedule=node1.getRoot

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep=Memento(schedule,self,[node1,node2])

        #add loop contents of node2 to node1
        node1.children.extend(node2.children)

        # TBD remove node2
        node2.parent.children.remove(node2)

        return schedule,keep
