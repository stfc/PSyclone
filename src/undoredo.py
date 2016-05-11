# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab


class Memento:
    '''Stores a particular schedule and the transformation that was used
    to create this schedule (from the previous one). Takes a copy of the
    schedule and the transformation so that we are guaranteed to return
    what was provided. Without the copy another object could modify the
    schedule and transformation objects.'''
    def __init__(self, schedule, transformation, mylist=[]):
        from copy import deepcopy
        # take copies of the schedule and transformations so that
        # they can not be modified externally and thus we can
        # guarantee to return them without modification.
        # self._schedule=deepcopy(schedule)
        # self._transformation=deepcopy(transformation)
        # self._mylist=deepcopy(mylist)

        # We get recursion errors with the nesting going to deep.
        # I need to create a specific deepcopy that copies what
        # I need, not everything.
        self._schedule = None
        self._transformation = None
        self._mylist = None

    @property
    def schedule(self):
        return self._schedule

    @property
    def transformation(self):
        return self._transformation, self._mylist


class UR:
    '''provides undo/redo facility. There is support for unlimited undo's
    but no support for branching (multiple paths) so all values are lost
    beyond the position where the new object is added. For example, if you
    perform a set of transformations t1, t2 and t3 which correspond to
    schedule s1, s2 and s3 and we were to unddo the last transformation
    and apply a new transformation t4, we would end up storing t1,t2,t4
    and a schedule s1,s2,s4 i.e. t3 and s3 would be deleted.

        For example:

        >>> from parse import parse
        >>> ast,info=parse("algorithm.f90")
        >>> from psyGen import PSy
        >>> psy=PSy(info)
        >>> invokes=psy.invokes
        >>> invokes.names
        >>> invoke=invokes.get("name")
        >>> schedule=invoke.schedule
        >>> print schedule
        >>> loop1=schedule.sequence[0]
        >>> loop2=schedule.sequence[1]
        >>> trans=SwapTrans()
        >>> newSchedule,memento=SwapTrans.apply(loop1,loop2)
        >>> ur=UR()
        >>> ur.add(memento)
        >>> invoke.schedule=newSchedule
        >>> print invoke.schedule
        >>> invoke.schedule=ur.undo.schedule
        >>> print invoke.schedule
        >>> invoke.schedule=ur.redo.schedule
        >>>
        TBC ...

    '''

    def __init__(self, storageclass):
        self._storageclass = storageclass
        self._mylist = []
        self._position = 0

    def add(memento):
        '''add a new object to the stack. Raises an error if the type of the
        object is different from the one specified in the constructor.'''
        if not isinstance(memento, self._storageclass):
            raise GenerationError("UR.add object is not the expected type.")
        # remove anything beyond our current position (no support for
        # branching)
        if position != len(self._mylist):
            del self._mylist[position:]
        self._mylist.append(memento)

    @property
    def position(self):
        ''' return the current position in the undo/redo stack. '''
        return self._position

    @property
    def size(self):
        ''' return the size of the undo/redo stack. '''
        return len(self._mylist)

    @property
    def undoAvailable(self):
        ''' return true if undo is possible '''
        if self._position == 0:
            return False
        return True

    @property
    def undo(self):
        '''return the previous object in the stack if there is one, otherwise
        return an error.'''
        if not self.undoAvailable:
            raise GenerationError("UR.undo. Error, there is nothing to undo.")
        self._position -= 1
        return self._mylist[position]

    @property
    def redoAvailable(self):
        ''' return true if redo is possible '''
        if self._position == len(self._mylist):
            return False
        return True

    @property
    def redo(self):
        '''return the next object in the stack if there is one, otherwise
        return an error.'''
        if not self.redoAvailable:
            raise GenerationError("UR.redo. Error, there is nothing to redo.")
        self._position += 1
        return self._mylist[position]
