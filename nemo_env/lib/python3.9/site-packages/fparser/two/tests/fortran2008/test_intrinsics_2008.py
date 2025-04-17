# Copyright (c) 2023 Science and Technology Facilities Council.

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""Test intrinsic handling within Fortran2008. At the moment, the only
special consideration (beyond 2003) is that we can't disambiguate a
shadowed intrinsic within a submodule.

"""

import pytest
from fparser.api import get_reader
from fparser.two import Fortran2003, Fortran2008
from fparser.two.utils import walk


@pytest.mark.usefixtures("f2008_create")
def test_intrinsic_in_submodule():
    """Test that a shadowed intrinsic within a submodule is accepted
    (since a submodule shares the scope of its parent module and we
    assume that is bringing the shadowed symbol into scope).

    """
    reader = get_reader(
        """\
      submodule (foobar) bar

        contains
        subroutine my_sub
          real :: a
          a = dot_product(1,2,3)
        end subroutine my_sub
      end
      """
    )
    ast = Fortran2008.Submodule(reader)
    assert not walk(ast, Fortran2003.Intrinsic_Function_Reference)
