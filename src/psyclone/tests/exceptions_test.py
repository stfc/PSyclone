# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Author: A. J. Voysey, Met Office

''' Test exception classes to ensure consistent __repr__ & __str__ methods. '''

from __future__ import absolute_import

import pkgutil
import inspect
import importlib

from psyclone.errors import PSycloneError


class DummyPSycloneError(PSycloneError):
    ''' Provides a dummy PSyclone specific error class as for use in this test
    '''
    def __init__(self):
        PSycloneError.__init__(self, "")
        self.value = "Dummy PSyclone Error"


def all_sub_exceptions(expt):
    ''' Recursively find the set of all the exceptions which are subclasses of
        the given exception class. '''

    new_sub_except = [add_except for sub_except in expt.__subclasses__()
                      for add_except in all_sub_exceptions(sub_except)]
    return set(expt.__subclasses__()).union(new_sub_except)


def import_submodules(package, recursive=True):
    """ Import all submodules of a module, recursively, including subpackages

    :param package: package (name or actual module)
    :type package: str | module

    :rtype: dict[str, types.ModuleType]
    """
    if isinstance(package, str):
        package = importlib.import_module(package)
    results = {}
    for _, name, is_pkg in pkgutil.walk_packages(package.__path__):
        full_name = package.__name__ + '.' + name
        if "test" not in full_name:
            results[full_name] = importlib.import_module(full_name)
            if recursive and is_pkg:
                results.update(import_submodules(full_name))
    return results


def test_exception_repr():
    ''' Test the properties of Exception classes defined by PSyclone. '''

    modules = {}

    # Recursively walk through the psyclone module, importing sub-modules.
    # Store any class definitions we come across.

    modules = import_submodules("psyclone")
    for mod in modules:
        _ = importlib.import_module(mod)

    all_exceptions = all_sub_exceptions(Exception)
    psy_excepts = [exc for exc in all_exceptions if "psyclone." in str(exc)]
    psy_excepts.append(DummyPSycloneError)

    # Different versions of pytest behave differently with respect to their
    # handling of an exception's representation. This can lead to some tests
    # passing/failing assertions depending on which pytest version is
    # installed.
    #
    # To avoid this we will enforce the following conditions for exceptions
    # defined by psyclone: -
    # i) Exceptions will implement their own __str__ and __repr__ methods.
    # ii) These will not be the same as each other for a given exception.
    # iii) The string returned by the __str__ method will not be contained
    #      with that returned by the __repr__ method,
    #
    # When these conditions are met, assertion behaviour is consistent across
    # all pytest versions.

    for psy_except in psy_excepts:

        # Check if the exception inherits PSycloneError
        assert issubclass(psy_except, PSycloneError)

        # Ensure there are __str__ & __repr__ methods implemented which are not
        # inherited from the parent Exception class
        assert psy_except.__str__ is not Exception.__str__
        assert psy_except.__repr__ is not Exception.__repr__

        # Simulate arguments to the exception constructor
        arglist = list(inspect.getfullargspec(psy_except).args)
        args = [None for arg in arglist if arg != 'self']

        # Check that the _str__ & __repr__ do not return the same string, and
        # that one is not contained within the other
        if len(args) > 0:
            assert str(psy_except(*args)) != str(repr(psy_except(*args)))
            assert str(psy_except(*args)) not in repr(psy_except(*args))
        else:
            assert str(psy_except()) != repr(psy_except())
            assert str(psy_except()) not in repr(psy_except())
