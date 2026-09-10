#!/usr/bin/env python3
# ##############################################################################
#  (c) Crown copyright Met Office. All rights reserved.
#  For further details please refer to the file COPYRIGHT
#  which you should have received as part of this distribution
# ##############################################################################

'''
This module contains an ExtractMixin class to add support for all extration
scripts.
'''

import logging
import shutil
from pathlib import Path
from typing import Optional, Iterable

from fab.build_config import BuildConfig
from fab.artefacts import ArtefactSet
from fab.steps import run_mp, step
from fab.steps.grab.folder import grab_folder
from fab.util import file_checksum, log_or_dot, TimerLogger
from psyclone.line_length import FortLineLength

from make_public import remove_private

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class ExtractMixin:
    '''
    This is a base class for all extractions. It is important to be added
    first as base class so that the methods overwritten here will be called.
    If the derived class also needs to overwrite e.g. the psyclone method, it
    must take care to call the right base class implementation(s).
    '''
    @staticmethod
    def remove_one_private(args: tuple) -> Path:
        '''
        This static method removes the private attributes of Fortran files
        for PSyclone extraction.

        :param args: a tuple with first the config and then the file path
        '''
        state, fpath = args
        hash = file_checksum(fpath).file_hash
        no_private_fpath = (state.prebuild_folder /
                            f'no-private-{fpath.stem}.{hash}{fpath.suffix}')

        if no_private_fpath.exists():
            log_or_dot(logger,
                       f'Removing private using prebuild: \
                        {no_private_fpath}')
            shutil.copy(no_private_fpath, fpath)
        else:
            log_or_dot(logger,
                       'Removing private using fparser remove_private')

            fll = FortLineLength()
            tree = remove_private(str(fpath))
            code = fll.process(str(tree))
            with open(fpath, "wt", encoding="utf-8") as f:
                f.write(code)
            with open(no_private_fpath, "wt", encoding="utf-8") as f:
                f.write(code)

        return no_private_fpath

    @step
    def remove_private_step(self):
        '''
        This method calls the static method remove_one_private to remove
        the private attributes of Fortran files for PSyclone extraction. It
        goes through all the Fortran files in the artefact and pass their
        paths and also the config to remove_one_private.
        '''
        state = self.config
        input_files = state.artefact_store[ArtefactSet.FORTRAN_COMPILER_FILES]
        args = [(state, filename) for filename in input_files]
        with TimerLogger(f"running remove-private on {len(input_files)} "
                         f"f90 files"):
            results = run_mp(state, args, self.remove_one_private)
        # Add the cached data to the prebuilds, so that the cleanup
        # at the end of the run will not delete these files.
        state.add_current_prebuilds(results)

    def grab_files_step(self):
        '''
        This method overwrites the grab_files_step in the base class by also
        including the psydata extract folder in LFRic core repo for extraction.
        '''
        super().grab_files_step()
        grab_folder(self.config, src=self.lfric_core_root /
                    "infrastructure" / "build" / "psyclone" / "psydata"
                    / "extract", dst_label='psydata')

    def psyclone_step(
            self,
            ignore_dependencies: Optional[Iterable[str]] = None) -> None:
        '''
        This method overwrites the psyclone_step in the base class by first
        calling the remove_private_step method to remove private attributes
        from Fortran files. It then calls the base class psyclone_step method
        for PSyclone processing.
        '''
        self.remove_private_step()

        # For extraction, PSyclone must search the whole source tree
        # (for inlining of all dependencies). Provide the path
        # to the full source tree:
        super().psyclone_step(
            ignore_dependencies=ignore_dependencies,
            additional_parameters=["-d", self.config.build_output])

    def get_transformation_script(self,
                                  fpath: Path,
                                  config: BuildConfig) -> Path:
        '''
        This method overwrites the base class get_transformation_script by
        returning the path to the transformation script that PSyclone will
        use for extraction.

        :param fpath: the path to the file being processed.
        :type fpath: Path
        :param config: the FAB BuildConfig instance.
        :type config: :py:class:`fab.BuildConfig`
        :returns: the transformation script to be used by PSyclone.
        :rtype: Path
        '''
        return config.source_root / 'optimisation' / 'extract' / 'global.py'
