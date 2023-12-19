# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab

'''This module contains the LLMTrans '''

import os
from openai import OpenAI
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import ScopingNode


class LLMTrans(Transformation):
    ''' Requests a transformation to ChatGPT '''

    def apply(self, node, options=None):
        '''Apply the LLMTrans transformation.

        :param node: node to which to apply the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with the transformations options.
        :type options: Optional[Dict[str, Any]]
        :param bool options["prompt"]: the prompt to provide to the LLM.

        '''
        if not options:
            options = {}
        if 'prompt' not in options:
            raise TransformationError("Needs a prompt")
        prompt = options['prompt']

        client = OpenAI()
        suggestion = client.chat.completions.create(
            messages=[
                {
                    "role": "user",
                    "content": f"""
                    ```fortran
                       {node.debug_string()}
                    ```
                    Given the Fortran code above, rewrite it into a valid Fortran
                    code but {prompt}:
                    """,
                }
            ],
            model="gpt-3.5-turbo",
        )
        print("Output", suggestion)
        output = node.debug_string()
        print("Output", output)
        reader = FortranReader()
        psyir = reader.psyir_from_statement(output, node.ancestor(ScopingNode).symbol_table)
        node.replace_with(psyir)

# For Sphinx AutoAPI documentation generation
__all__ = ["LLMTrans"]
