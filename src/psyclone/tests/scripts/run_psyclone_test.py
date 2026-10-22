"""Tests for the PSyclone control-file discovery script."""

from psyclone.scripts import run_psyclone
from psyclone.scripts.run_psyclone import RunPsyclone


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
  file_list: "*.x90"
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


def test_run_skips_phase_without_file_list(tmp_path, caplog, monkeypatch):
    """Check phases without a file_list are skipped with a warning."""
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
        "Skipping phase dsl: no file_list pattern is configured"
        in caplog.text
    )
    assert not psyclone_calls


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
  file_list: "*x90"
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
    assert "file_list: '*x90'" in stdout
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
