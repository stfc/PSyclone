.. _coding-style:

Coding and Documentation Style
******************************

Documentation Style
###################
When writing documentation, each reference to a PSyclone class or function
should be set in italics (i.e. enclosed by single backticks) except in headings.
The first time a class or function is mentioned, use the full Python path, e.g.:
`psyclone.core.access_info.VariableAccessInfo`. After that just use the
class name (again in italics).
File names and shell commands should be set in double back-ticks (\`\`).


Coding Style
############
Any new PSyclone code must confirm to Python's pep8 specification, and
must be pylint error and warning free. Installation of the tools is
described in :ref:`system_specific_dev_setup`. 
In some cases pylint errors and warnings can be suppressed using the
pylint markup code::

    # pylint: disable=too-many-branches
    def _upper_bound(self):
        ....

It is up to the developer to decide if code should be refactored to
avoid a warning. For example, a warning about too many local
variables in a function might be better suppressed instead of 
removing one variable, or refactoring the code to create two functions.

Additional rules that apply:

  #) All setter and getter functions must be declared as property and
     setter to allow using them without parenthesis::

        @property
        def ast_end(self):
            '''
            :returns: a reference to the last node in the fparser2 parse tree \
                      that represents a child of this PSyIR node or None.
            :rtype: sub-class of :py:class:`fparser.two.utils.Base`
            '''
            return self._ast_end

        @ast.setter
        def ast(self, ast):
            '''
            Set a reference to the fparser2 node associated with this Node.
                   :param ast: fparser2 node associated with this Node.
            :type ast: :py:class:`fparser.two.utils.Base`
            '''
            self._ast = ast

  #) Any function must contain an interface description, see
     :ref:`interface_description` for full details..
  #) Any new line of code must be covered by at least one test case,
     see :ref:`test_suite` and especially :ref:`test_coverage`.


.. _interface_description:

Interface Description
#####################


The interface to any new or modified routine in PSyclone must be fully documented using Sphinx mark-up. An example of how to do this is shown below:

    .. literalinclude:: interface_example.py

Example layout of the interface description above: 

.. autofunction:: interface_example.some_function

Some important details:

  #) There are up to four major sections in each interface description: function
     description, parameter description and type, return value and type, and
     exceptions. The function description is required, all other sections only
     need be provided if they are applicable for the code being documented.
     The formatting for each section is slightly different:

         .. tabularcolumns:: |l|L|
         
         ===================== ======================================================
         Section               Formatting
         ===================== ======================================================
         function description  The description of the function must start with
                               a capital letter, and end with a full stop.
         parameter description Start the parameter description with a lowercase 
                               letter and end with a full stop. The parameter type
                               declaration must start with a lowercase letter, and
                               no punctuation at then end. References to other
                               classes within PSyclone should be written as
                               ``:py:class:`psyclone.filename.Class```.
         return value          The description of the return value should start with
                               a lowercase letter, and end with a full stop. The type
                               can either be a lower case one word entry
                               (``:py:class:`psyclone.filename.Class```, 'int'
                               etc), or can also be a full sentence, then starting
                               with a capital letter and full punctuation.
         exceptions            These must start with a lower case letter, and end
                               with a full stop.
         ===================== ======================================================


  #) If a parameter description, type, return value or exception is continued
     to the next line, there must be a '\\\\' continuation symbol at the end of
     each line. Align each continued line with the column at which the
     description begins on the previous line. If this would create lines that
     are too short then the first continued line may be indented less, e.g.::

         '''
         :param some_very_long_variable_name: this is some argument that has \
		a very long name and therefore it does not make sense to indent \
		continued lines to align with the start of the description.
	 '''
  #) If an argument type is a Python built-in (e.g. str, int or bool) then the
     type can be specified in-line with the argument description. However, if it
     is of a derived type then, for clarity, it should be specified in a
     separate :type my_arg: line.
  #) The closing \\'\\'\\' of the interface description can be at the end of a
     text line if the overall description is short. Otherwise it should be on a
     separate line. An optional empty line between interface description and
     code should be included in the comment section.

  #) Standard Python functions like `__str__` etc. need only be documented with a
     simple informal comment.
