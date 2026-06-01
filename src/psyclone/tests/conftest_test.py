# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------

'''Tests for helpers defined in the test-suite conftest module.'''

import graphviz

from psyclone.tests import conftest as psyconftest


def test_have_graphviz_missing_executable(monkeypatch):
    '''Check that have_graphviz reports False when the graphviz executable is
    not available.

    '''
    def existing_graphviz():
        '''Returns nothing.'''
        pass

    def missing_graphviz():
        '''Raise the same exception graphviz emits when binaries are absent.'''
        raise graphviz.ExecutableNotFound("not found")

    # 'have_graphviz' is a fixture and cannot be called directly, but we can
    # do it through its `__wrapped__` method
    monkeypatch.setattr(graphviz, "version", existing_graphviz)
    assert psyconftest.have_graphviz.__wrapped__() is True
    monkeypatch.setattr(graphviz, "version", missing_graphviz)
    assert psyconftest.have_graphviz.__wrapped__() is False
