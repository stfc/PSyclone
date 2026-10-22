"""Tests for the PSyclone control-file discovery script."""

from psyclone.scripts.run_psyclone import execute_psyclone, run


def test_execute_psyclone_uses_psykal_output_suffixes(tmp_path, caplog):
    """Check PSy and algorithm output filenames derived from output_path."""
    output_path = tmp_path / "file.x90"
    caplog.set_level("INFO", logger="psyclone.scripts.run_psyclone")

    execute_psyclone(
        tmp_path / "input.x90",
        output_path,
        tmp_path / "script.py",
        {},
        api="lfric",
    )

    assert f"'-opsy', '{tmp_path / 'file_psy.f90'}'" in caplog.text
    assert f"'-oalg', '{tmp_path / 'file.f90'}'" in caplog.text


def test_run_merges_configs_and_reports_selected_script(
        tmp_path, capsys, caplog):
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
    run(
        source_dir,
        [first_config, second_config],
        optimisation=optimisation_dir,
        output_path=output_dir,
    )

    stdout = capsys.readouterr().out
    assert "comment: Merged" in stdout
    assert "file_list: '*x90'" in stdout
    assert f"Reading control file {first_config}" in caplog.text
    assert "Phase dsl: found 3 files matching '*x90'" in caplog.text
    assert f"file.x90 uses script {script_path}" in caplog.text
    assert "'offload': True" in caplog.text
    # assert (
    #     f"Processing input {source_dir / 'nested' / 'file.x90'} to output "
    #     f"{output_dir / 'nested' / 'file.x90'}" in caplog.text
    # )
    assert "and API lfric" in caplog.text
    assert "processing" in caplog.text
    assert "excluding" in caplog.text
    assert caplog.text.count("Processing input") == 3
