#!/usr/bin/env python3
"""
Reads in one (or more) PSyclone control YAML files, merges them and then runs
PSyclone on the source files selected by the control files. The output files
are written to the specified output directory, preserving the relative paths
of the input files. The script also reports the scripts selected for each
phase and source file, along with any parameters passed to the scripts. If a
phase does not have a file_pattern configured, it is skipped with a warning.
The script can be run in parallel using the specified number of worker
processes.
"""

import argparse
import logging
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from typing import Any, Optional, Sequence

from psyclone.generator import main as psyclone_main
from psyclone.scripts.psyclone_control import (
    PsycloneControl,
    PsyclonePhase,
)


LOGGER = logging.getLogger(__name__)


class RunPsyclone:
    """Process source files selected by the PSyclone control files."""

    def __init__(
            self,
            input_path: Path,
            control_files: Sequence[Path],
            optimisation: Optional[Path] = None,
            output_path: Optional[Path] = None,
            psyclone_options: Optional[str] = None,
            nprocs: int = 1) -> None:
        """Configure the input files, directories and PSyclone options.

        :param input_path: the root directory containing source files.
        :param control_files: ordered PSyclone control YAML files to merge.
        :param optimisation: the root directory containing optimisation
            scripts.
        :param output_path: the root directory for output files (defaults to
            the input directory).
        :param psyclone_options: whitespace-separated options to pass to
            PSyclone.
        :param nprocs: maximum number of worker processes for each phase.
        """
        if nprocs < 1:
            raise ValueError("nprocs must be at least 1")
        self._psyclone_options = psyclone_options
        self._nprocs = nprocs
        self._input_path = input_path.resolve()
        self._output_path = (output_path or input_path).resolve()
        self._optimisation = (optimisation or self._input_path).resolve()
        self._control_files = control_files

    def run(self) -> None:
        """
        Process the source files selected by the control files. This
        is done in parallel, using the specified number of cores, if more than
        one core is requested. The output files are written to the output
        directory, preserving the relative paths of the input files.
        """
        control = PsycloneControl(
            script_root=self._optimisation,
            base_paths=[self._input_path],
            template_data={"target": "."},
        )

        for control_file in self._control_files:
            LOGGER.info("Reading control file %s", control_file)
            control.read(control_file)

        print(control.to_yaml())

        for phase_name in control.all_phases:
            phase = control.get_info(phase_name)
            LOGGER.info("Starting phase %s", phase_name)
            if not phase.file_pattern:
                LOGGER.warning(
                    "Skipping phase %s: no file_pattern is configured",
                    phase_name,
                )
                continue

            matched_files = self.get_phase_files(phase)

            jobs = []
            for source_file in matched_files:
                script, params = phase.get_script(source_file)
                destination = (
                    self._output_path / source_file.relative_to(self._input_path)
                )
                if script == PsyclonePhase.RESULT_EXCLUDE:
                    LOGGER.info(
                        "Phase %s: excluding %s (no matching script rule)",
                        phase_name,
                        source_file,
                    )
                elif script == PsyclonePhase.RESULT_NO_SCRIPT:
                    LOGGER.info(
                        "Phase %s: processing %s without a script",
                        phase_name,
                        source_file,
                    )
                    jobs.append(
                        (source_file, destination, script, params, phase.api)
                    )
                else:
                    LOGGER.info(
                        "Phase %s: %s uses script %s with parameters %s",
                        phase_name,
                        source_file,
                        script,
                        params,
                    )
                    jobs.append(
                        (source_file, destination, script, params, phase.api)
                    )

            if self._nprocs == 1:
                for job in jobs:
                    self._execute_psyclone(*job)
            elif jobs:
                with ProcessPoolExecutor(max_workers=self._nprocs) as executor:
                    futures = [
                        executor.submit(self._execute_psyclone, *job)
                        for job in jobs
                    ]
                    for future in futures:
                        future.result()

            LOGGER.info("Finished phase %s", phase_name)

        LOGGER.info("All configured phases have been processed")

    def get_phase_files(
            self, phase: PsyclonePhase) -> list[Path]:
        """
        Determine the list of source files to work on. This default
        implementation uses the file_pattern from the phase to search
        for matching files in the input directory. It handles the

        :param phase: the information about the pase for which to
            collect the source files.

        """
        if not phase.file_pattern:
            raise ValueError("Cannot find phase files without a file_pattern")
        matched_files = sorted(
            path for path in self._input_path.rglob(phase.file_pattern)
            if path.is_file()
        )
        LOGGER.info(
            "Phase %s: found %d files matching %r",
            phase.name,
            len(matched_files),
            phase.file_pattern,
        )
        return matched_files

    def _execute_psyclone(
            self,
            input_path: Path,
            output_path: Path,
            script_name: Path,
            script_parameters: dict[str, Any],
            api: Optional[str] = None) -> None:
        """
        Executes PSyclone on the given input file, writing the output to the
        given output file. If a script is provided, it is used to transform
        the input file, otherwise PSyclone is run without a script.

        :param input_path: the source file to process.
        :param output_path: the destination file to write.
        :param script_name: the name of the script to use (if any).
        :param script_parameters: parameters for the script.
        :param api: the API to use for PSyclone.
        """

        params: list[str] = ["-l", "all",
                             "--config",
                             "/home/joerg/work/ukmo/core/applications"
                             "/skeleton/fab-workspace/skeleton-fast-debug-"
                             "mpif90-gfortran/source/psyclone_config/"
                             "psyclone.cfg"]
        if not api:
            # Transmute
            params.extend(["-o", str(output_path)])
        else:
            psy_output = output_path.with_name(f"{output_path.stem}_psy.f90")
            alg_output = output_path.with_suffix(".f90")
            params.extend(["-api", api,
                           "-opsy", str(psy_output),
                           "-oalg", str(alg_output)])
        if script_name:
            params.extend(["-s", str(script_name)])
        if script_parameters:
            # Note that psyclone expects the script parameters to be passed
            # as a single string, using key:value pairs. So, remove the curly
            # braces and pass the string representation of the dictionary
            # without the braces.
            params.extend(["--script-kwargs",
                           f"'{str(script_parameters)[1:-1]}'"])

        if self._psyclone_options:
            params.extend(self._psyclone_options.split())

        params.append(str(input_path))
        LOGGER.info(" ".join(params))
        psyclone_main(params)


def main(argv: Optional[Sequence[str]] = None) -> None:
    """Parse command-line arguments and run the phase discovery."""
    parser = argparse.ArgumentParser(
        description=(
            "Merge PSyclone control YAML files and report selected scripts."
        )
    )
    parser.add_argument(
        "--input",
        type=Path,
        required=True,
        help="Root directory to search for source files.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        required=True,
        help="Root directory for output files.",
    )
    parser.add_argument(
        "control_files",
        type=Path,
        nargs="+",
        help="Control YAML files to read and merge, in the specified order.",
    )
    parser.add_argument(
        "--optimisation",
        type=Path,
        help=(
            "Root directory containing optimisation scripts "
            "(defaults to the source directory)."
        ),
    )
    parser.add_argument(
        "--psyclone-options",
        help="Whitespace-separated options to pass to PSyclone.",
    )
    parser.add_argument(
        "--nprocs",
        type=int,
        default=1,
        help="Maximum number of worker processes per phase (default: 1).",
    )
    args = parser.parse_args(argv)
    if args.nprocs < 1:
        parser.error("--nprocs must be at least 1")

    logging.basicConfig(
        level=logging.INFO,
        format="%(levelname)s: %(message)s",
    )
    runner = RunPsyclone(
        args.input,
        args.control_files,
        args.optimisation,
        output_path=args.output,
        psyclone_options=args.psyclone_options,
        nprocs=args.nprocs,
    )
    runner.run()


if __name__ == "__main__":
    main()
