#!/usr/bin/env python3
# ##############################################################################
#  (c) Crown copyright Met Office. All rights reserved.
#  For further details please refer to the file COPYRIGHT
#  which you should have received as part of this distribution
# ##############################################################################

'''A FAB build script for gungho_model. It relies on the FabBase class
contained in the infrastructure directory.
'''
from pathlib import Path
import sys

from fab_gungho_model import FabGungho

# We import the Apps base class, even though we don't directly
# need it. But importing LFRicAppsBase makes the core/lfric_build
# directory available, from which we import the ExtractMixin
sys.path.insert(0, str(Path(__file__).parents[2] / "build"))

from lfric_apps_base import LFRicAppsBase  # noqa: E402,F401
from extract_mixin import ExtractMixin   # noqa: E402


class FabGunghoExtract(ExtractMixin, FabGungho):
    '''This trivial class implements extraction for GungHo. The mixin
    Extract class overwrites the psyclone step (to insert its own
    step of removing private declarations first).

    There is no implementation here needed otherwise. The mixing inserts
    the required phases, and overwrites the method with which to determine
    the PSyclone script to use.

    The only other important part here is __main__, which sets a different
    fab workspace-name (and uses the class here).
    '''


# -----------------------------------------------------------------------------
if __name__ == '__main__':

    fab_gungo = FabGunghoExtract(name="gungho_model_extract",
                                 root_symbol="gungho_model")
    fab_gungo.build()
