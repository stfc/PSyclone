# Copilot instructions for the PSyclone repository

This file is intended to help future Copilot sessions (and other automated assistants) navigate the PSyclone repository and produce useful, repository-aware edits and suggestions.

---

## Quick commands (local development)

- Install for development (recommended):
  - python -m venv .venv && source .venv/bin/activate
  - pip install -e .[test,treesitter]

- Lint:
  - flake8 --count --show-source --statistics .
  - (flake8 configuration in setup.cfg)

- Run full test suite (as CI does):
  - pytest -n auto --doctest-modules --cov=psyclone --cov-report=xml src/psyclone

- Run a single test file or test:
  - pytest -q src/psyclone/tests/path/to/test_file.py
  - pytest -q src/psyclone/tests/path/to/test_file.py::test_function_name
  - pytest -q -k "substring"  # run tests matching expression

- Run examples / tutorials (used by CI):
  - make -C examples transform
  - make -C examples notebook
  - make -C tutorial/practicals transform
  - make -C tutorial/training test

- Build docs & check links (CI):
  - cd doc && make doctest
  - cd doc && make html SPHINXOPTS="-W --keep-going"
  - cd doc && make linkcheck

---

## High-level architecture (big picture)

- Purpose: PSyclone is a Python-based source-to-source Fortran tool that works with native Fortran code and implements a compiler-like architecture (frontend -> IR -> backend). It can also operate as a Domain-specific language compiler where it generates and optimises PSy-layer code from kernel-level Fortran sources. Transformations are applied to the intermediate representation (PSyIR). In DSL compiler mode it implements the PSyKAl pattern (separation of Algorithm, PSy, Kernel).

- Top-level layout (relevant):
  - src/psyclone/: main Python package
  - src/psyclone/psyir/: internal intermediate representation (PSyIR) used for analyses and transformations
  - src/psyclone/core, generator, psyGen, alg_gen, transformations: the core transformation and code-generation logic
  - src/psyclone/domain/: domain-specific front-ends (lfric, gocean) and their domain rules
  - bin/: CLI entrypoints (psyclone, psyclone-kern, psyad, psyclonefc)
  - examples/, tutorial/: transformation examples and notebooks used in CI (some require additional system deps)

- How things flow (conceptual):
  1. Kernel Fortran and algorithm driver code are parsed (fparser / tree-sitter optionally).
  2. A PSyIR is created and transformed by passes in transformations/ and generator/.
  3. Domain-specific modules (lfric/gocean/nemo) implement API-specific behaviour and tests.
  4. Generator emits transformed Fortran with chosen parallelisation/instrumentation (OpenMP/OpenACC/GPU directives, etc.).

---

## Key repository conventions and patterns

- Source layout: packages live under src/ and are installed with `pip install .` or `pip install -e .`.

- Tests: kept under `src/psyclone/tests/` and include both unit tests and doctests; CI runs `--doctest-modules`.
  - CI uses pytest-xdist (`-n auto`) to run tests in parallel; locally you can omit `-n auto` if issues arise although this can be slow.
  - Coverage is required to be 100%.

- Linting: flake8 is configured in setup.cfg. Some files and directories are explicitly excluded there (e.g., external, build and a few tutorials/examples).

- Optional parsers: the project can use the PyPI `fparser` or a submodule copy (`external/fparser`) — CI sometimes pins/use the submodule; tests may also require `tree-sitter` and `tree-sitter-fortran` extras for treesitter-based parsing.

- Doctests in modules: many modules have doctests; test runs include `--doctest-modules` so generated or modified docstrings should keep examples working.

- Domain-specific tests and workflows: there are separate workflows and test targets for LFRic, NEMO and GOcean. Changes affecting domain logic should be validated against the relevant workflows (CI names: lfric_test.yml, nemo_tests.yml, gocean/nemo_v5 tests).

- Packaging/version: dynamic version information is provided by `psyclone/version.py` (used by setuptools as configured in pyproject.toml).

- CI environment notes: GitHub Actions test matrix covers several Python versions (3.9 and 3.14 in CI) and installs extras `[test,treesitter]` and sometimes `doc` extras for documentation jobs.

- Examples & notebooks: example generation is driven by `make` targets in examples/ and tutorial/; CI runs these (sometimes silently) before running tests. For comprehensive testing, the 'compile' target to `make` should be used.

---

## When suggesting changes or helpers

- All development should be performed on a feature branch with a descriptive name that begins
  with the associated GitHub Issue number.
- Keep changes small and surgical; many modules have doctests and tight lint rules.
- If proposing test changes, include an example `pytest` command to run the new test locally (use the single-test examples above).
- For changes touching parsing, mention whether `fparser` or `tree-sitter` is required and ensure tests are run with the appropriate extras installed.
- Tag commit messages with the associated issue number.

---

## Files consulted when creating these instructions

- README.md (project purpose, structure)
- CONTRIBUTING.md (developer workflow pointers)
- pyproject.toml and setup.cfg (test & lint settings and extras)
- .github/workflows/* (how CI runs tests, linting, docs and examples)

---

If this file already exists, incorporate its content instead of replacing it. If you'd like, update this file to include any project-specific shortcuts, frequently used test selectors, or domain-only quick checks (LFRic/NEMO/GOcean) that were omitted here.
