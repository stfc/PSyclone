# Copyright (c) 2022, Science and Technology Facilities Council.

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

"""
Test the setup performed for the fparser module.

"""
import os
import fparser


def test_fparser_get_version(monkeypatch):
    """Test the _get_version() utility routine. It doesn't make sense to
    actually check precisely which version it reports - just that it returns a
    suitable string."""
    ver1 = fparser._get_version()
    assert isinstance(ver1, str)
    assert "." in ver1

    def _broken_version(_name):
        """Broken routine with which to patch the `version` method."""
        raise fparser.PackageNotFoundError()

    try:
        from importlib import metadata

        monkeypatch.setattr(metadata, "version", _broken_version)
    except ImportError:
        # Use backport package for python <3.8
        import importlib_metadata

        monkeypatch.setattr(importlib_metadata, "version", _broken_version)
    ver2 = fparser._get_version()
    assert isinstance(ver2, str)
    assert "." in ver2


def test_fparser_logging_handler(tmpdir, caplog):
    """Test the custom error handler that is configured in the __init__.py
    file.  Invalid characters in an input file are skipped and logging
    messages are created.

    """
    content = "HELLO"
    invalid_content = "\xca".join(content)
    filepath = os.path.join(str(tmpdir), "tmp_in.f90")
    # Create the input file
    with open(filepath, "w", encoding="UTF-8") as tmp_file:
        tmp_file.write(invalid_content)
    # Specify encoding as 'ascii' to trigger errors for the non-ascii chars.
    with open(filepath, "r", errors="fparser-logging", encoding="ascii") as cfile:
        output = cfile.read()
    assert output == content
    for record in caplog.records:
        assert record.levelname != "CRITICAL"
    assert (
        "Skipped bad character in input file. Error returned was 'ascii' "
        "codec can't decode byte "
    ) in caplog.text
    # Can't check the actual value as some versions of Python3 return
    # a different value to the one above.
    assert "in position 1: ordinal not in range(128)." in caplog.text
