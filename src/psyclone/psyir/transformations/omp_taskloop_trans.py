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
# Authors A. B. G. Chalk, A. R. Porter, STFC Daresbury Lab

'''
This module provides the implementation of OMPTaskloopTrans

'''

from psyclone.psyir.transformations.parallel_loop_trans import (
    ParallelLoopTrans)

from psyclone.psyir.transformations.transformation_error import (
    TransformationError)

from psyclone.psyir.nodes import (
    OMPTaskloopDirective)


class OMPTaskloopTrans(ParallelLoopTrans):
    '''
    Adds an OpenMP taskloop directive to a loop. Only one of grainsize or
    num_tasks must be specified.

    TODO: #1364 Taskloops do not yet support reduction clauses.

    :param grainsize: the grainsize to use in for this transformation.
    :type grainsize: int or None
    :param num_tasks: the num_tasks to use for this transformation.
    :type num_tasks: int or None
    :param bool nogroup: whether or not to use a nogroup clause for this
                         transformation. Default is False.

    For example:

    >>> from pysclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import OMPParallelTrans, OMPSingleTrans
    >>> from psyclone.transformations import OMPTaskloopTrans
    >>> from psyclone.psyir.transformations import OMPTaskwaitTrans
    >>> singletrans = OMPSingleTrans()
    >>> paralleltrans = OMPParallelTrans()
    >>> tasklooptrans = OMPTaskloopTrans()
    >>> taskwaittrans = OMPTaskwaitTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Apply the OpenMP Taskloop transformation to *every* loop
    >>> # in the schedule.
    >>> # This ignores loop dependencies. These can be handled
    >>> # by the OMPTaskwaitTrans
    >>> for child in schedule.children:
    >>>     tasklooptrans.apply(child)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # SINGLE region
    >>> singletrans.apply(schedule.children)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> paralleltrans.apply(schedule.children)
    >>> # Ensure loop dependencies are satisfied
    >>> taskwaittrans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    def __init__(self, grainsize=None, num_tasks=None, nogroup=False):
        self._grainsize = None
        self._num_tasks = None
        self.omp_grainsize = grainsize
        self.omp_num_tasks = num_tasks
        self.omp_nogroup = nogroup
        super().__init__()

    def __str__(self):
        return "Adds an 'OpenMP TASKLOOP' directive to a loop"

    @property
    def omp_nogroup(self):
        '''
        Returns whether the nogroup clause should be specified for
        this transformation. By default the nogroup clause is applied.

        :returns: whether the nogroup clause should be specified by
                  this transformation.
        :rtype: bool
        '''
        return self._nogroup

    @omp_nogroup.setter
    def omp_nogroup(self, nogroup):
        '''
        Sets whether the nogroup clause should be specified for this
        transformation.

        :param bool nogroup: value to set whether the nogroup clause should be
                             used for this transformation.

        raises TypeError: if the nogroup parameter is not a bool.
        '''
        if not isinstance(nogroup, bool):
            raise TypeError(f"Expected nogroup to be a bool "
                            f"but got a {type(nogroup).__name__}")
        self._nogroup = nogroup

    @property
    def omp_grainsize(self):
        '''
        Returns the grainsize that will be specified by
        this transformation. By default the grainsize
        clause is not applied, so grainsize is None.

        :returns: The grainsize specified by this transformation.
        :rtype: int or None
        '''
        return self._grainsize

    @omp_grainsize.setter
    def omp_grainsize(self, value):
        '''
        Sets the grainsize that will be specified by
        this transformation. Checks the grainsize is
        a positive integer value or None.

        :param value: integer value to use in the grainsize clause.
        :type value: int or None

        :raises TransformationError: if value is not an int and is not None.
        :raises TransformationError: if value is negative.
        :raises TransformationError: if grainsize and num_tasks are \
                                     both specified.
        '''
        if (not isinstance(value, int)) and (value is not None):
            raise TransformationError(f"grainsize must be an integer or None, "
                                      f"got {type(value).__name__}")

        if (value is not None) and (value <= 0):
            raise TransformationError(f"grainsize must be a positive "
                                      f"integer, got {value}")

        if value is not None and self.omp_num_tasks is not None:
            raise TransformationError(
                "The grainsize and num_tasks clauses would both "
                "be specified for this Taskloop transformation")
        self._grainsize = value

    @property
    def omp_num_tasks(self):
        '''
        Returns the num_tasks that will be specified
        by this transformation. By default the num_tasks
        clause is not applied so num_tasks is None.

        :returns: The grainsize specified by this transformation.
        :rtype: int or None
        '''
        return self._num_tasks

    @omp_num_tasks.setter
    def omp_num_tasks(self, value):
        '''
        Sets the num_tasks that will be specified by
        this transformation. Checks that num_tasks is
        a positive integer value or None.

        :param value: integer value to use in the num_tasks clause.
        :type value: int or None

        :raises TransformationError: if value is not an int and is not None.
        :raises TransformationError: if value is negative.
        :raises TransformationError: if grainsize and num_tasks are \
                                     both specified.

        '''
        if (not isinstance(value, int)) and (value is not None):
            raise TransformationError(f"num_tasks must be an integer or None,"
                                      f" got {type(value).__name__}")

        if (value is not None) and (value <= 0):
            raise TransformationError(f"num_tasks must be a positive "
                                      f"integer, got {value}")

        if value is not None and self.omp_grainsize is not None:
            raise TransformationError(
                "The grainsize and num_tasks clauses would both "
                "be specified for this Taskloop transformation")
        self._num_tasks = value

    def _directive(self, children, collapse=None):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: currently un-used but required to keep
                             interface the same as in base class.
        :returns: the new node representing the directive in the AST.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPTaskloopDirective`

        :raises NotImplementedError: if a collapse argument is supplied
        '''
        # TODO 2672: OpenMP loop functions don't support collapse
        if collapse:
            raise NotImplementedError(
                "The COLLAPSE clause is not yet supported for "
                "'!$omp taskloop' directives (#2672).")
        _directive = OMPTaskloopDirective(children=children,
                                          grainsize=self.omp_grainsize,
                                          num_tasks=self.omp_num_tasks,
                                          nogroup=self.omp_nogroup)
        return _directive

    def apply(self, node, options=None, **kwargs):
        '''Apply the OMPTaskloopTrans transformation to the specified node in
        a Schedule. This node must be a Loop since this transformation
        corresponds to wrapping the generated code with directives like so:

        .. code-block:: fortran

          !$OMP TASKLOOP
          do ...
             ...
          end do
          !$OMP END TASKLOOP

        At code-generation time (when lowering is called), this node must be
        within (i.e. a child of) an OpenMP SERIAL region.

        If the keyword "nogroup" is specified in the options, it will cause a
        nogroup clause be generated if it is set to True. This will override
        the value supplied to the constructor, but will only apply to the
        apply call to which the value is supplied.

        :param node: the supplied node to which we will apply the \
                     OMPTaskloopTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param bool options["nogroup"]:
                indicating whether a nogroup clause should be applied to
                this taskloop.

        '''
        if not options:
            options = {}
        current_nogroup = self.omp_nogroup
        # If nogroup is specified it overrides that supplied to the
        # constructor of the Transformation, but will be reset at the
        # end of this function
        self.omp_nogroup = options.get("nogroup", current_nogroup)

        try:
            super().apply(node, options, **kwargs)
        finally:
            # Reset the nogroup value to the original value
            self.omp_nogroup = current_nogroup
