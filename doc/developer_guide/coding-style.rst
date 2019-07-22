_coding-style:

Coding Style
************

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
        :rtype: :py:class:`psyclone.psyGen.SubroutineGen`, :py:class:`psyclone.parser.Invokes`
    
        :raises IOError: If the filename or search path does not exist.
        :raises ParseError: If there is an error in the parsing.
        :raises RuntimeError: If there is an error in the parsing.
    
        '''

Some important details:

  #) Each section (description, parameter, return value, exceptions) must be
     separated by an empty line. Between the entries in one section there must
     not be an empty line.
  #) Sentences describing the function should start with a capital letter, and
     end with a full stop. 
  #) Sentences describing a parameter, return value or exception
     should start with a lowercase letter, and end with a full stop.
  #) If a parameter description, type, return value or exception is continued
     to the next line, there must be a '' continuation symbol at the end of
     each line. Align each continued line to start at the same column that the
     previous line starts with the text.
  #) If an argument type is a Python built-in (e.g. str, int or bool) then the
     type can be specified in-line with the argument description. However, if it
     is of a derived type then, for clarity, it should be specified in a
     separate :type my_arg: line.
  #) The type of a parameter or return value should be set in lower case
     letters, with no punctuation characters at the end. In HTML the type is
     set in parenthesis after the name of the parameter, which would include
     punctuation characters: 
     ``parent (psyclone.f2pygen.SubroutineGen.) – the parent...``
  #) The closing ''' of the interface description can be at the end of a text
     line if the overall description is short. Otherwise it should be on a
     separate line, and there should be an empty line before the '''.

