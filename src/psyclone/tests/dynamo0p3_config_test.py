# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2020, Science and Technology Facilities Council
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
# Author I.Kavcic, Met Office

'''
Module containing tests for Dynamo0.3 (LFRic) API configuration handling.
'''

from __future__ import absolute_import

import os
import pytest

from psyclone.configuration import Config, ConfigurationError

# Valid configuration file content with only defaults and
# annexed dofs for testing purposes
_CONFIG_CONTENT = '''\
[DEFAULT]
API = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
[dynamo0.3]
COMPUTE_ANNEXED_DOFS = false
'''

def test_anx_dof_not_bool(tmpdir):
    ''' Check that we raise an error if the default API is not in
    the list of supported APIs '''
    content = re.sub(r"^COMPUTE_ANNEXED_DOFS = .*$",
                     "COMPUTE_ANNEXED_DOFS = tree",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert ("The API (invalid) is not in the list of "
                "supported APIs" in str(err.value))
