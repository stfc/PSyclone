# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council
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
# Authors: J. Henrichs, Bureau of Meteorology
#          A. R. Porter, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

'''
This module contains the GOcean-specific implementation of the ExtractNode.
It mostly adds code to create a driver program that can read the created
output files.
'''


from __future__ import absolute_import, print_function

from psyclone.psyir.nodes import ExtractNode


class GOceanExtractNode(ExtractNode):
    '''
    This is the Gocean-specific implementation of the extract node.
    It adds a function 'generate_driver' which creates a GOcean-specific
    stand-alone driver program that can read the created output files.

    :param ast: reference into the fparser2 parse tree corresponding to \
        this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options for transformations.
    :type options: dictionary of string:values or None
    :param bool options["create-driver"]: whether or not to create a driver \
        program at code-generation time. If set, the driver will be created \
        in the current working directory with the name \
        "driver-MODULE-REGION.f90" where MODULE and REGION will be the \
        corresponding values for this region. Defaults to False.
    :param str options["prefix"]: a prefix to use for the PSyData module name \
        (``prefix_psy_data_mod``) and the PSyDataType \
        (``prefix_PSyDataType``) - a "_" will be added automatically. \
        It defaults to "extract", which is set in the base class if \
        not overwritten in the options dictionary.

    '''
    def __init__(self, ast=None, children=None, parent=None, options=None):
        # This function is only provided to document the options.
        super(GOceanExtractNode, self).__init__(ast=ast, children=children,
                                                parent=parent, options=options)

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of ExtractNode.
        :rtype: str
        '''
        return "gocean_extract_" + str(self.position)


# -------------------------------------------------------------------------
# For AutoAPI documentation generation
__all__ = ['GOceanExtractNode']
