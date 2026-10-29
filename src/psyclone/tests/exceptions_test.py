# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Test exception classes to ensure consistent __repr__ & __str__ methods. '''
import inspect
import importlib
import psyclone
import pkgutil

from psyclone import errors


class DummyPSycloneError(errors.PSycloneError):
    ''' Provides a dummy PSyclone specific error class as for use in this test
    '''
    def __init__(self, value):
        super().__init__(value)
        self.value = f"Dummy PSyclone Error: {value}"


def test_exception_str_and_repr():
    ''' Test the properties of Exception classes defined by PSyclone. '''

    for module_info in pkgutil.walk_packages(psyclone.__path__,
                                             psyclone.__name__ + "."):
        module = importlib.import_module(module_info.name)

        for name, obj in inspect.getmembers(module, inspect.isclass):
            # Only classes defined in this module, not imported
            if obj.__module__ != module_info.name:
                continue
            # Only check Exceptions
            if not issubclass(obj, Exception):
                continue
            # That are not the base exception class
            if name == "PSycloneError":
                continue
            # Ensure PSyclone exceptions inherit from PSycloneError
            assert issubclass(obj, errors.PSycloneError)
            # Ensure there are __str__ & __repr__ methods implemented which
            # are not inherited from the parent Exception class
            assert obj.__str__ is not Exception.__str__
            assert obj.__repr__ is not Exception.__repr__

    # Now test that an example error class behaves as expected
    testerror = DummyPSycloneError("my msg")
    assert str(testerror) == "Dummy PSyclone Error: my msg"
    assert repr(testerror) == "DummyPSycloneError()"
