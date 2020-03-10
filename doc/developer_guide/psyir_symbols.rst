.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2017-2019, Science and Technology Facilities Council.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   * Neither the name of the copyright holder nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   -----------------------------------------------------------------------------
   Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab


PSyIR Symbols
#############

At the moment, root node schedules (e.g. `InvokeSchedules`, `KernelSchedules`
and `Containers`) have a symbol table with the definition of the symbols used
in their bodies.

.. note:: Some symbols are still hardwired as strings inside some of the PSyIR
    nodes. But these should be eventually replaced as we have no way to check
    for name clashes with these symbols.

When one wants to define a new symbol in the symbol table, in order to avoid
any clashes, the recommended way is to us the `new_symbol_name` method:

.. automethod:: psyclone.psyir.symbols.SymbolTable.new_symbol_name

However, if this variable need to be retrieved later on, one must keep track
of the returned name or use a tag to uniquely identify the variable
internally (the tag is not displayed in the generated code). Therefore, to
create a new symbol and associate it with a tag, the following lines
can be used:

.. code-block:: python

    variable = node.symbol_table.new_symbol_name("variable_name")
    node.symbol_table.add(DataSymbol(variable, DataType.INTEGER),
                          tag="variable_with_the_result_x")

Then, there are two ways to retrieve the symbol from a symbol table. Using the
`name` or using the `tag` as lookup keys. This is done with the two following
methods:

.. automethod:: psyclone.psyir.symbols.SymbolTable.lookup

.. automethod:: psyclone.psyir.symbols.SymbolTable.lookup_with_tag

Sometimes, specially in the dynamo0p3 API, we have no way of knowing if
a symbol has already been defined before it is first used. In this cases
we can use a try/catch around the `lookup_with_tag` method and if a 
KeyError is raised (the tag was not found), then proceed to declare the
symbol. Since this action is often needed, the Symbol Table provides the
following helper method that encapsulates the described behaviour:

.. automethod:: psyclone.psyir.symbols.SymbolTable.name_from_tag

.. warning:: It is recommended to avoid the `name_from_tag` method for new
    code as ideally the method will be deprecated in favor of a finer control
    of when variables are defined and used.
