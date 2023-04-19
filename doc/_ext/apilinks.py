# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab.

'''
This module defines a Sphinx plugin which enables the use of a
':ref_guide:' role within Sphinx documentation. The link targets
are then generated using the 'ref_guide_base' (configuration
variable) as a base. This is set in the 'conf.py' of the documentation
and enables link targets to be configured at build time.

'''
from docutils import nodes, utils


def setup(app):
    '''
    Sets-up this Sphinx plugin.

    :param app: the Sphinx application object.
    :type app: :py:class:`sphinx.application.Sphinx`

    '''
    # The interface to the handler is defined by Sphinx and we don't
    # use all of the arguments.
    # pylint: disable=unused-argument,too-many-arguments
    def ref_link_role(role, rawtext, text, lineno, inliner, options=None,
                      content=None):
        '''
        Handler routine called when the ':ref_guide:' role is encountered.

        Modifies the supplied link text by pre-pending the text stored in
        the 'ref_guide_base' Sphinx configuration variable.

        '''
        if not options:
            options = {}
        if not content:
            content = []

        items = text.split()
        ref = app.config.ref_guide_base + items[-1]
        if len(items) > 1:
            # The link had a name associated with it so reconstruct it.
            name = ' '.join(items[:-1])
        else:
            name = utils.unescape(ref)
        # Construct the new reference.
        node = nodes.reference(rawtext, name, refuri=ref, **options)
        return [node], []

    # Define a new Sphinx configuration variable that will be used to
    # store the base URL to use when generating links. We give it a default
    # value of '' so that its value must be set in the conf.py configuration
    # file of the Sphinx document.
    app.add_config_value('ref_guide_base', default='', rebuild='env')

    # Define the new role and assign it our new handler.
    app.add_role('ref_guide', ref_link_role)
