.. _coding-style:

Coding and Documentation Style
******************************

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


The interface to any new or modified routine in PSyclone must be fully documented using Sphinx mark-up. An example of how to do this is shown below::
    
    def parse(alg_filename, api="", invoke_name="invoke", inf_name="inf",
              kernel_path="", line_length=False):
        '''
        Takes a GungHo algorithm specification as input and outputs an AST of
        this specification and an object containing information about the
        invocation calls in the algorithm specification and any associated kernel
        implementations.
    
        :param str alg_filename: the file containing the algorithm specification.
        :param str invoke_name: the expected name of the invocation calls in the\
                                algorithm specification
        :param str inf_name: the expected module name of any required\
                             infrastructure routines.
        :param str kernel_path: the path to search for kernel source files (if\
                                different from the location of the algorithm\
                                source).
        :param line_length: a logical flag specifying whether we\
                            care about line lengths being longer ...
        :type line_length: bool
    
        :return: A 2-tuple containing the top-level node in the AST and an object\
                 describing all of the invoke()'s found in the Algorithm file
        :rtype: :py:class:`psyclone.psyGen.SubroutineGen`, \
                :py:class:`psyclone.parser.Invokes`
    
        :raises IOError: If the filename or search path does not exist.
        :raises ParseError: If there is an error in the parsing.
        :raises RuntimeError: If there is an error in the parsing.
    psyclone.
        '''

.. autofunction:: psyclone.generator.generate

Some important details:

  #) There are up to four major sections in each interface description: function
     description, parameter description and type, return value and type, and
     exceptions. The formatting for each section is slightly different:

         .. tabularcolumns:: |l|L|
         
         ===================== ======================================================
         Section               Formatting
         ===================== ======================================================
         function description  Sentences describing the function and the return value
                               should start with a capital letter, and end with a
                               full stop.
         parameter description This should start with a lowercase letter and end with
                               a full stop.
         parameter type        Start with a lowercase latter, but no punctuation 
                               at the end. References to other classes within
                               PSyclone should be written as 
                               ``:py:class:`psyclone.filename.py.Class```.
         return value          This should start with a capital letter, and end with
                               a full stop.
         return type           This must start with a lowercase letter, and not have
                               any punctuation at the end. Similar to type above this
                               should use sphinx formatting to reference other
                               PSyclone classes.
         exceptions            This must start with a lower case letter, and end with
                               a full stop.  
         ===================== ======================================================



Some important details:

  #) There are up to four major sections in each interface description:

     - function description

       Sentences describing the function and the return value should start with
       a capital letter, and end with a full stop.
     - parameter and parameter type description

       Both parameter and its type should start with a lowercase letter. The
       parameter must end with a full stop, but no punctuation character at
       the end of the type.
     - return value and return type description

     - exceptions

       These descriptions should start with a lowercase latter, and end with
       a full stop. 

     Each of these sections must be
     separated by an empty line. Between the entries in one section there must
     not be an empty line.
  #) Sentences describing the function and the return value should start with
     a capital letter, and end with a full stop.
  #) Sentences describing a parameter, or exception
     should start with a lowercase letter, and end with a full stop.
  #) The type of a parameter should be set in lower case
     letters, with no punctuation characters at the end. In HTML the type is
     set in parenthesis after the name of the parameter, which would include
     punctuation characters: 
     ``parent (psyclone.f2pygen.SubroutineGen.) – the parent...``
  #) 
  #) If a parameter description, type, return value or exception is continued
     to the next line, there must be a '\'' continuation symbol at the end of
     each line. Align each continued line to start at the same column that the
     previous line starts with the text.
  #) If an argument type is a Python built-in (e.g. str, int or bool) then the
     type can be specified in-line with the argument description. However, if it
     is of a derived type then, for clarity, it should be specified in a
     separate :type my_arg: line.
  #) The closing ''' of the interface description can be at the end of a text
     line if the overall description is short. Otherwise it should be on a
     separate line, and there should be an empty line before the '''.

