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
from collections import defaultdict
import logging
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import sys
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
            nprocs: int = 1,
            kernel_dirs: Sequence[Path] = ()) -> None:
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
        :param kernel_dirs: root directories containing kernel source code.
        """
        if nprocs < 1:
            raise ValueError("nprocs must be at least 1")
        self._psyclone_options = psyclone_options
        self._nprocs = nprocs
        self._input_path = input_path.resolve()
        self._output_path = (output_path or input_path).resolve()
        self._optimisation = (optimisation or self._input_path).resolve()
        self._control_files = control_files
        self._kernel_dirs = kernel_dirs

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
                    self._output_path /
                    source_file.relative_to(self._input_path)
                )
                if script == PsyclonePhase.RESULT_EXCLUDE:
                    LOGGER.info(
                        "Phase %s: expliclitly excluding %s",
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
                        (source_file, destination, None, params, phase.api)
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
        if phase.file_pattern == ".calling.yaml":
            # Special case to handling *.calling.yaml files. These files
            # contain list of external modules that are being used. In order
            # to find the list of files, you need to read all these yaml files
            # and combine their module information.
            return self._get_files_from_calling_yaml(matched_files)

        LOGGER.info(
            "Phase %s: found %d files matching %r",
            phase.name,
            len(matched_files),
            phase.file_pattern,
        )
        return matched_files

    def _get_files_from_calling_yaml(self, matched_files):
        self._module_routine_info = defaultdict(set)
        self._module_symbol_info = defaultdict(set)

        for path in self._input_path.rglob(f"*{suffix}"):

            with path.open("r", encoding="utf8") as stream:
                info = yaml.safe_load(stream)

            for module_name, module_info in info.items():
                self._module_routine_info[module_name].update(module_info["routines"])
                self._module_symbol_info[module_name].update(module_info["symbols"])

        mod_man = ModuleManager()
        for module in self._module_routine_info:
            mod_info = mod_man.get_module_info(module)
            file_path = Path(mod_info.filename)

    def _execute_psyclone(
            self,
            input_path: Path,
            output_path: Path,
            script_name: Optional[Path],
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

        params: list[str] = []
        if not api:
            # Transmute
            params.extend(["-o", str(output_path)])
            # Now rename the input file to .xu90, which the old
            # PSyclone makefile does, and some scripts need the
            # .x90 suffix to work correctly
            xu90 = input_path.with_suffix(".xu90")
            input_path.rename(xu90)
            input_path = xu90
        else:
            psy_output = output_path.with_name(f"{output_path.stem}_psy.f90")
            alg_output = output_path.with_suffix(".f90")
            params.extend(["-api", api,
                           "-opsy", str(psy_output),
                           "-oalg", str(alg_output)])
            for kernel_dir in self._kernel_dirs:
                params.extend(["-d", str(kernel_dir)])
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
        LOGGER.info(f"psyclone {' '.join(params)}")
        if script_name:
            # Some PSyclone scripts import files from the same
            # directory using the directory name (import my_dir/help.py
            # instead of just import help.py). To support this, add the
            # parent directory to the Python path:
            sys.path.insert(0, str(script_name.parents[1]))

        psyclone_main(params)
        if script_name:
            # Clean up Python path
            del sys.path[0]


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
        "-d",
        type=Path,
        action="append",
        default=[],
        help=(
            "Path to a root directory structure containing kernel source "
            "code. Multiple roots can be specified by using multiple -d "
            "arguments. These directories will be searched recursively."
        ),
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
        stream=sys.stdout,
    )
    runner = RunPsyclone(
        args.input,
        args.control_files,
        args.optimisation,
        output_path=args.output,
        psyclone_options=args.psyclone_options,
        nprocs=args.nprocs,
        kernel_dirs=args.d,
    )
    runner.run()


if __name__ == "__main__":
    main()
