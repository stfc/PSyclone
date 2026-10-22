#!/usr/bin/env python3
"""Merge PSyclone control files and report the scripts selected per phase."""

import argparse
import logging
from pathlib import Path
from typing import Any, Optional, Sequence

from psyclone.generator import main as psyclone_main
from psyclone.scripts.psyclone_control import (
    PsycloneControl,
    PsyclonePhase,
)


LOGGER = logging.getLogger(__name__)


def run(
        input_path: Path,
        control_files: Sequence[Path],
        optimisation: Optional[Path] = None,
        output_path: Optional[Path] = None) -> None:
    """Merge control files and report script selection for each phase.

    :param input_path: the root directory containing source files.
    :param control_files: ordered PSyclone control YAML files to merge.
    :param optimisation: the root directory containing optimisation scripts.
    :param output_path: the root directory for output files (defaults to the
        input directory).
    """
    input_path = input_path.resolve()
    output_path = (output_path or input_path).resolve()
    script_root = (optimisation or input_path).resolve()
    control = PsycloneControl(
        script_root=script_root,
        base_paths=[input_path],
        template_data={"target": "."},
    )

    for control_file in control_files:
        LOGGER.info("Reading control file %s", control_file)
        control.read(control_file)

    print(control.to_yaml())

    for phase_name in control.all_phases:
        phase = control.get_info(phase_name)
        LOGGER.info("Starting phase %s", phase_name)
        if not phase.file_list:
            LOGGER.warning(
                "Skipping phase %s: no file_list pattern is configured",
                phase_name,
            )
            continue

        matched_files = sorted(
            path for path in input_path.rglob(phase.file_list)
            if path.is_file()
        )
        LOGGER.info(
            "Phase %s: found %d files matching %r",
            phase_name,
            len(matched_files),
            phase.file_list,
        )

        for source_file in matched_files:
            script, params = phase.get_script(source_file)
            destination = output_path / source_file.relative_to(input_path)
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
                execute_psyclone(
                    source_file, destination, script, params, phase.api)
            else:
                LOGGER.info(
                    "Phase %s: %s uses script %s with parameters %s",
                    phase_name,
                    source_file,
                    script,
                    params,
                )
                execute_psyclone(
                    source_file, destination, script, params, phase.api)

        LOGGER.info("Finished phase %s", phase_name)

    LOGGER.info("All configured phases have been processed")


def execute_psyclone(
        input_path: Path,
        output_path: Path,
        script_name: Path,
        script_parameters: dict[str, Any],
        api: Optional[str] = None) -> None:
    """Log the parameters for processing a single source file."""

    params: list[str] = ["-l", "all",
                         "--config",
                         "/home/joerg/work/ukmo/core/applications"
                         "/skeleton/fab-workspace/skeleton-fast-debug-mpif90-"
                         "gfortran/source/psyclone_config/psyclone.cfg"]
    if not api:
        # Transmute
        params.extend(["-o", str(output_path)])
    else:
        psy_output = output_path.with_name(f"{output_path.stem}_psy.f90")
        alg_output = output_path.with_suffix(".f90")
        params.extend(["-api", api,
                       "-opsy", str(psy_output),
                       "-oalg", str(alg_output),
                       "-d", "./kernel"])
    if script_name:
        params.extend(["-s", str(script_name)])
    if script_parameters:
        params.extend(["--script-kwargs", str(script_parameters)])

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
    args = parser.parse_args(argv)

    logging.basicConfig(
        level=logging.INFO,
        format="%(levelname)s: %(message)s",
    )
    run(
        args.input,
        args.control_files,
        args.optimisation,
        output_path=args.output,
    )


if __name__ == "__main__":
    main()
