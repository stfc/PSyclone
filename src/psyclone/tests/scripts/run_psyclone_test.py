"""Tests for the PSyclone control-file discovery script."""

import pytest

from psyclone.scripts import run_psyclone
from psyclone.scripts.psyclone_control import PsyclonePhase
from psyclone.scripts.run_psyclone import RunPsyclone


def test_init_rejects_invalid_nprocs(tmp_path):
    """Check the runner requires at least one worker process."""
    with pytest.raises(ValueError, match="nprocs must be at least 1"):
        RunPsyclone(tmp_path, [], nprocs=0)


def test_main_passes_psyclone_options(tmp_path, monkeypatch):
    """Check whitespace-separated CLI options reach psyclone_main."""
    source_dir = tmp_path / "source"
    source_dir.mkdir()
    source_file = source_dir / "input.x90"
    source_file.touch()
    output_dir = tmp_path / "output"
    control_file = tmp_path / "control.yaml"
    control_file.write_text(
        """
phases:
  - dsl
dsl:
  file_pattern: "*.x90"
  no_script:
    files: "*"
""",
        encoding="utf-8",
    )
    psyclone_args = []
    # pylint: disable=unnecessary-lambda
    monkeypatch.setattr(
        run_psyclone, "psyclone_main",
        lambda args: psyclone_args.extend(args),
    )

    run_psyclone.main([
        "--input", str(source_dir),
        "--output", str(output_dir),
        "--psyclone-options=--verbose -l all",
        str(control_file),
    ])

    assert psyclone_args[-4:] == [
        "--verbose", "-l", "all", str(source_file),
    ]


def test_main_rejects_invalid_nprocs(tmp_path, capsys):
    """Check the command-line parser rejects a non-positive worker count."""
    with pytest.raises(SystemExit) as err:
        run_psyclone.main([
            "--input", str(tmp_path),
            "--output", str(tmp_path / "output"),
            "--nprocs", "0",
            str(tmp_path / "control.yaml"),
        ])

    assert err.value.code == 2
    assert "--nprocs must be at least 1" in capsys.readouterr().err


def test_main_uses_nprocs_for_phase_workers(tmp_path, monkeypatch):
    """Check --nprocs sets the process limit for each phase."""
    source_dir = tmp_path / "source"
    source_dir.mkdir()
    source_files = [source_dir / f"input{index}.x90" for index in range(3)]
    for source_file in source_files:
        source_file.touch()
    control_file = tmp_path / "control.yaml"
    control_file.write_text(
        """
phases:
  - dsl
dsl:
  file_pattern: "*.x90"
  no_script:
    files: "*"
""",
        encoding="utf-8",
    )
    process_limits = []
    processed_files = []

    # The runner calls result() on each returned future.
    class CompletedFuture:
        """
        Represent a completed submission, required for ImmediateExecutor.
        """

        @staticmethod
        def result():
            """Pretend the submitted work has completed."""
            return None

    class RecordSubmissions:
        """Run submitted work inline while recording executor usage."""

        def __init__(self, max_workers):
            process_limits.append(max_workers)

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc_value, traceback):
            return False

        def submit(self, function, *args):
            """
            Simple submit implementation that just records the submitted jobs.
            """
            function(*args)
            processed_files.append(args[0])
            return CompletedFuture()

    monkeypatch.setattr(
        run_psyclone, "ProcessPoolExecutor", RecordSubmissions,
    )
    monkeypatch.setattr(run_psyclone, "psyclone_main", lambda args: None)

    run_psyclone.main([
        "--input", str(source_dir),
        "--output", str(tmp_path / "output"),
        "--nprocs", "2",
        str(control_file),
    ])

    assert process_limits == [2]
    assert set(processed_files) == set(source_files)


def test_execute_psyclone_uses_psykal_output_suffixes(
        tmp_path, caplog, monkeypatch):
    """Check PSy and algorithm output filenames derived from output_path."""
    output_path = tmp_path / "file.x90"
    caplog.set_level("INFO", logger="psyclone.scripts.run_psyclone")
    psyclone_args = []
    # pylint: disable=unnecessary-lambda
    monkeypatch.setattr(
        run_psyclone, "psyclone_main",
        lambda args: psyclone_args.extend(args),
    )

    runner = RunPsyclone(tmp_path, [])
    runner._execute_psyclone(
        tmp_path / "input.x90",
        output_path,
        tmp_path / "script.py",
        {},
        api="lfric",
    )

    assert psyclone_args[psyclone_args.index("-opsy") + 1] == str(
        tmp_path / "file_psy.f90"
    )
    assert psyclone_args[psyclone_args.index("-oalg") + 1] == str(
        tmp_path / "file.f90"
    )


def test_run_skips_phase_without_file_pattern(tmp_path, caplog, monkeypatch):
    """Check phases without a file_pattern are skipped with a warning."""
    control_file = tmp_path / "control.yaml"
    control_file.write_text(
        """
phases:
  - dsl
dsl:
  comment: No file pattern
""",
        encoding="utf-8",
    )
    psyclone_calls = []
    # pylint: disable=unnecessary-lambda
    monkeypatch.setattr(
        run_psyclone, "psyclone_main",
        lambda args: psyclone_calls.append(args),
    )
    caplog.set_level("WARNING", logger="psyclone.scripts.run_psyclone")

    runner = RunPsyclone(tmp_path, [control_file])
    runner.run()

    assert (
        "Skipping phase dsl: no file_pattern is configured"
        in caplog.text
    )
    assert not psyclone_calls


def test_get_phase_files_rejects_missing_pattern(tmp_path):
    """Check file discovery requires a configured phase pattern."""
    phase = PsyclonePhase("dsl", [], tmp_path)
    runner = RunPsyclone(tmp_path, [])

    with pytest.raises(
            ValueError,
            match="Cannot find phase files without a file_pattern"):
        runner.get_phase_files(phase)


def test_run_merges_configs_and_reports_selected_script(
        tmp_path, capsys, caplog, monkeypatch):
    """Check ordered merge, YAML output, phase matching, and script lookup."""
    source_dir = tmp_path / "source"
    source_dir.mkdir()
    output_dir = tmp_path / "output"
    optimisation_dir = tmp_path / "optimisation"
    (source_dir / "nested").mkdir()
    (source_dir / "nested" / "file.x90").touch()
    (source_dir / "nested" / "plain.x90").touch()
    (source_dir / "nested" / "skip.x90").touch()
    (source_dir / "ignore.f90").touch()
    script_path = optimisation_dir / "psykal" / "global.py"
    script_path.parent.mkdir(parents=True)
    script_path.touch()

    first_config = tmp_path / "first.yaml"
    first_config.write_text(
        """
phases:
  - dsl
dsl:
  comment: First
  file_pattern: "*x90"
  script_dir: "${target}/psykal"
  global.py:
    params:
      offload: true
    files: "*"
  no_script:
      files: "plain.x90"
  exclude:
      files: "skip.x90"
""",
        encoding="utf-8",
    )
    second_config = tmp_path / "second.yaml"
    second_config.write_text(
        """
phases:
  - dsl
dsl:
  comment: Merged
  api: lfric
""",
        encoding="utf-8",
    )

    caplog.set_level("INFO", logger="psyclone.scripts.run_psyclone")
    psyclone_calls = []
    # pylint: disable=unnecessary-lambda
    monkeypatch.setattr(
        run_psyclone, "psyclone_main",
        lambda args: psyclone_calls.append(args),
    )
    runner = RunPsyclone(
        source_dir,
        [first_config, second_config],
        optimisation=optimisation_dir,
        output_path=output_dir,
    )
    runner.run()

    stdout = capsys.readouterr().out
    assert "comment: Merged" in stdout
    assert "file_pattern: '*x90'" in stdout
    assert f"Reading control file {first_config}" in caplog.text
    assert "Phase dsl: found 3 files matching '*x90'" in caplog.text
    assert f"file.x90 uses script {script_path}" in caplog.text
    assert "'offload': True" in caplog.text
    assert "processing" in caplog.text
    assert "excluding" in caplog.text
    assert len(psyclone_calls) == 2
    assert all(
        call[call.index("-api") + 1] == "lfric"
        for call in psyclone_calls
    )
    assert {call[-1] for call in psyclone_calls} == {
        str(source_dir / "nested" / "file.x90"),
        str(source_dir / "nested" / "plain.x90"),
    }
    assert any(str(script_path) in call for call in psyclone_calls)
