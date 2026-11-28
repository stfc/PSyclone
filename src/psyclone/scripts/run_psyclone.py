#!/usr/bin/env python3
"""
Reads in one (or more) PSyclone control YAML files, merges them and then runs
PSyclone on the source files selected by the control files. The output files
are written to the specified output directory. The name of each output file
is determined by the `output_template` of the phase (by default preserving
the relative paths of the input files). The script also reports the scripts
selected for each phase and source file, along with any parameters passed to
the scripts. If a phase does not have a file_pattern configured, it is
skipped with a warning. The script can be run in parallel using the specified
number of worker processes.
"""

import argparse
import logging
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import sys
from typing import Any, Optional, Sequence, Union

import yaml

from psyclone.generator import main as psyclone_main
from psyclone.parse import ModuleManager
from psyclone.scripts.psyclone_control import (
    PsycloneControl,
    PsyclonePhase,
)


LOGGER = logging.getLogger(__name__)


class RunPsyclone:
    """Process source files selected by the PSyclone control files."""

    # A phase with this file_pattern does not process the matching files
    # themselves, but the source files of the modules listed in them.
    CALLING_YAML_PATTERN = "*.calling.yaml"

    def __init__(
            self,
            input_path: Path,
            control_files: Sequence[Path],
            output_path: Optional[Path] = None,
            psyclone_options: Optional[str] = None,
            nprocs: int = 1,
            kernel_dirs: Sequence[Path] = (),
            search_paths: Sequence[Path] = ()) -> None:
        """Configure the input files, directories and PSyclone options.

        :param input_path: the root directory containing source files.
        :param control_files: ordered PSyclone control YAML files to merge.
        :param output_path: the root directory for output files (defaults to
            the input directory).
        :param psyclone_options: whitespace-separated options to pass to
            PSyclone.
        :param nprocs: maximum number of worker processes for each phase.
        :param kernel_dirs: root directories containing kernel source code.
        :param search_paths: root directories to search for optimisation
            scripts, in order of precedence.
        """
        if nprocs < 1:
            raise ValueError("nprocs must be at least 1")
        self._psyclone_options = psyclone_options
        self._nprocs = nprocs
        self._input_path = input_path.resolve()
        self._output_path = (output_path or input_path).resolve()
        self._control_files = control_files
        self._kernel_dirs = kernel_dirs
        self._search_paths = [path.resolve() for path in search_paths]

    def run(self) -> None:
        """
        Process the source files selected by the control files. This
        is done in parallel, using the specified number of cores, if more than
        one core is requested. The output files are written to the output
        directory, using the output template of each phase to determine
        the name of each file.
        """
        control = PsycloneControl(script_paths=self._search_paths)

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

            jobs: list[tuple[Path, Path, Optional[Path], dict, str]] = []
            for source_file, file_kwargs in matched_files:
                script, params = phase.get_script(source_file,
                                                  self._input_path)
                # Additional information about the file (e.g. module name,
                # routines and symbols) is passed on as script parameters.
                params = {**params, **file_kwargs}
                destination = phase.get_output_path(
                    source_file, self._input_path, self._output_path)
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
            self, phase: PsyclonePhase) -> list[tuple[Path, dict[str, Any]]]:
        """
        Determine the list of source files to work on. This default
        implementation uses the file_pattern from the phase to search
        for matching files in the input directory. If the file_pattern is
        `CALLING_YAML_PATTERN`, the matching files are read, and the source
        files of the modules listed in them are returned instead.

        :param phase: the information about the phase for which to
            collect the source files.

        :returns: a list of tuples, each containing a source file and a
            dictionary of keyword arguments for this file. The dictionary is
            empty, except for files found using calling yaml files, where it
            contains the keys `module_name`, `routines` and `symbols`.

        :raises ValueError: if the phase has no file_pattern.
        """
        if not phase.file_pattern:
            raise ValueError("Cannot find phase files without a file_pattern")

        matched_files = sorted(
            path for path in self._input_path.rglob(phase.file_pattern)
            if path.is_file()
        )
        if phase.file_pattern == self.CALLING_YAML_PATTERN:
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
        return [(path, {}) for path in matched_files]

    def _get_files_from_calling_yaml(
            self, matched_files: Sequence[Path]
            ) -> list[tuple[Path, dict[str, Any]]]:
        """
        Reads the *.calling.yaml files and combines their information. Each
        file has module names as outer keys, each with a list of `routines`
        and a list of `symbols` used from that module. The information for
        modules that are listed in several files is merged. The module
        manager is then used to find the source file of each module.

        :param matched_files: the *.calling.yaml files to read.

        :returns: a list with one tuple for each module, sorted by module
            name, containing the filename and a dictionary with the keys
            `module_name`, `routines` and `symbols` (the latter two are
            sorted lists). Modules that the module manager is configured to
            ignore are skipped.

        :raises FileNotFoundError: if the source file of a module cannot be
            found.
        """

        # This is done in two steps: first, cummulate the various yaml files
        # using a dictionary for fast loopup of existing information.
        modules: dict[str, dict[str, set[str]]] = {}
        for yaml_file in matched_files:
            with open(yaml_file, "r", encoding="utf-8") as stream:
                content = yaml.safe_load(stream) or {}
            for module_name, info in content.items():
                info = info or {}
                entry = modules.setdefault(
                    module_name.lower(),
                    {"routines": set(), "symbols": set()})
                entry["routines"].update(info.get("routines") or [])
                entry["symbols"].update(info.get("symbols") or [])

        # Then convert the module names to actual file paths using the
        # # module manager.
        mod_manager = ModuleManager.get()
        mod_manager.add_search_path([self._input_path], recursive=True)

        result: list[tuple[Path, dict[str, Union[str, list[str]]]]] = []
        for module_name in sorted(modules):
            mod_info = mod_manager.get_module_info(module_name)
            if mod_info is None:
                LOGGER.info("Ignoring module '%s'", module_name)
                continue
            routines = sorted(modules[module_name]["routines"])
            symbols = sorted(modules[module_name]["symbols"])
            result.append((Path(mod_info.filename),
                           {"module_name": module_name,
                            "routines": routines,
                            "symbols": symbols}))
        return result

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
                           f"{str(script_parameters)[1:-1]}"])

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
        "--search-path",
        type=Path,
        action="append",
        default=[],
        help=(
            "Root directory to search for optimisation scripts. "
            "Can be specified several times; directories are searched in "
            "the given order."
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

    LOGGER.setLevel(logging.INFO)

    logging.basicConfig(
        level=logging.ERROR,
        format="%(levelname)s: %(message)s",
        stream=sys.stdout,
    )
    runner = RunPsyclone(
        input_path=args.input,
        control_files=args.control_files,
        output_path=args.output,
        psyclone_options=args.psyclone_options,
        nprocs=args.nprocs,
        kernel_dirs=args.d,
        search_paths=args.search_path,
    )
    runner.run()


if __name__ == "__main__":
    main()
