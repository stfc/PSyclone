# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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

# This is an experimental transformtion so we avoid listing
# OpenAI as a dependency in setup.py
try:
    from openai import OpenAI
except ImportError:
    OpenAI = None

from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import ScopingNode


class LLMTrans(Transformation):
    ''' Requests a transformation to ChatGPT '''

    def validate(self, node, options=None):
        '''Validate the LLMTrans transformation.

        :param node: node to which to apply the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with the transformations options.
        :type options: Optional[Dict[str, Any]]
        :param bool options["prompt"]: the prompt to provide to the LLM.

        '''
        if OpenAI is None:
            raise TransformationError(
                "LLMTrans needs the OpenAI library, it can be installed with: "
                "pip install OpenAI")
        if not options or 'prompt' not in options:
            raise TransformationError(
                "LLMTrans needs a mandatory 'promt' options entry")
        if not isinstance(options['prompt'], str):
            raise TransformationError(
                f"LLMTrans 'prompt' option must be a str but found "
                f"'{type(options['prompt'])}")

    def apply(self, node, options=None):
        '''Apply the LLMTrans transformation.

        :param node: node to which to apply the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with the transformations options.
        :type options: Optional[Dict[str, Any]]
        :param bool options["prompt"]: the prompt to provide to the LLM.

        '''
        self.validate(node, options)
        if 'prompt' not in options:
            raise TransformationError("Needs a prompt")
        prompt = options['prompt']

        client = OpenAI()
        output = client.chat.completions.create(
            messages=[
                {
                    "role": "user",
                    "content": f"""
                    ```fortran
                       {node.debug_string()}
                    ```
                    Given the Fortran code above, rewrite it into a valid
                    Fortran code but {prompt}. Respond only with the relevant
                    loop.
                    """,
                }
            ],
            model="gpt-4",
        )
        # Take the code inside the returned codeblock
        content = output.choices[0].message.content
        suggestion = content.split("```fortran")[1].split("```")[0]
        print("OpenAI suggested code:\n", suggestion)
        reader = FortranReader()
        psyir = reader.psyir_from_statement(
            suggestion, node.ancestor(ScopingNode).symbol_table)
        node.replace_with(psyir)


# For Sphinx AutoAPI documentation generation
__all__ = ["LLMTrans"]
