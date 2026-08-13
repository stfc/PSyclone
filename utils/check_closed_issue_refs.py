# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
# Author: H. Frost, STFC Daresbury Lab

"""Find references to closed GitHub issues left behind in the Psyclone
codebase and report them. The CI check only catches references to issues
closed by a PR that mentions them so issues closed manually may get left
behind. This script is intended to be run by hand to find such references.

Set GITHUB_TOKEN in the environment to raise API rate limit from 60 to 5000
requests per hour.
Set before running: export GITHUB_TOKEN=<your personal access token>

Run with: python utils/check_closed_issue_refs.py
Embedded tests can be run with: pytest utils/check_closed_issue_refs.py

To generate a .txt file with all references to closed issues, run:
python utils/check_closed_issue_refs.py > audit.txt 2>&1

Exit codes: 0 = no stale references found, 1 = stale references found,
2 = error."""

import argparse
import os
import re
import sys
import collections
import concurrent.futures
import subprocess
import urllib.request
import urllib.error
import json
import fnmatch

DEFAULT_REPOSITORY = "stfc/PSyclone"

# File types searched
DEFAULT_INCLUDES = ["*.py", "*.md", "*.rst", "Makefile"]
DEFAULT_EXCLUDE_DIRS = [
    "build",
    "__pycache__",
    ".git",
    "dist",
    ".tox",
    ".mpy_cache",
    "external",
]

# Regular expression to find references to GitHub issues in the codebase
REFERENCE_PATTERN = re.compile(r"(?<![A-Za-z0-9/])#([0-9]+)\b(?![a-zA-Z])")


def run_git(root: str, *arguments: str) -> str | None:
    '''Run a git command in the given working copy
    
    :param root: the root directory of the git working copy.
    :param arguments: the git command and its arguments.
    
    :returns: the stdout of the command, or None if the command failed.
    '''
    try:
        result = subprocess.run(
            ("git", "-C", root) + arguments,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return None


def find_references(root: str, includes: list[str], exclude_dirs: list[str]) \
        -> dict[int, list[tuple[str, int, str]]]:
    '''Walk the directory tree starting at root and find all references to
    GitHub issues.

    :param root: the starting directory.
    :param includes: name patterns of filenames to include.
    :param exclude_dirs: name patterns of directories to exclude.

    :returns: a dictionary mapping issue numbers to their (path, line no,
        line text) occurrences.
    '''

    references = collections.defaultdict(list)

    for directory, subdirectories, filenames in os.walk(root):
        # skip excluded directories
        subdirectories[:] = [
            d for d in subdirectories if d not in exclude_dirs
        ]

        for filename in filenames:
            if not any(
                fnmatch.fnmatch(filename, pattern) for pattern in includes
            ):
                continue
            path = os.path.join(directory, filename)
            try:
                with open(path, "r", encoding="utf-8") as handle:
                    lines = handle.readlines()
            except (OSError, UnicodeDecodeError):
                print(
                    f"Warning: Could not read file {path}. Skipping.",
                    file=sys.stderr,
                )
                continue

            for number, line in enumerate(lines, start=1):
                for found in REFERENCE_PATTERN.finditer(line):
                    references[int(found.group(1))].append(
                        (path, number, line.strip())
                    )
    return dict(references)


def fetch_issue(repository: str, number: int, token: str | None)\
        -> dict | None:
    '''Lookup a single issue on GitHub
    
    :param repository: the GitHub repository in the form "owner/name".
    :param number: the issue or PR number to lookup.
    :param token: the GitHub API token, or None.

    :returns: the parsed JSON for the issue, or None if the issue does
    not exist or is closed (HTTP 404).
    '''

    url = (
        "https://api.github.com/repos/" + repository + "/issues/" + str(number)
    )
    request = urllib.request.Request(url)
    request.add_header("Accept", "application/vnd.github+json")
    if token:
        request.add_header("Authorization", "Bearer " + token)

    try:
        with urllib.request.urlopen(request) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        if error.code == 404:
            return None
        if error.code in (403, 429, 401):
            remaining = error.headers.get("X-RateLimit-Remaining")
            if remaining == "0":
                raise RuntimeError(
                    "GitHub API rate limit exceeded. "
                    "Set GITHUB_TOKEN in the environment to raise the limit."
                )
        raise RuntimeError(
            f"GitHub API request failed with status {error.code}: \
                {error.reason}"
        )


def classify(
    repository: str, numbers: list[int], token: str | None, workers: int
) -> tuple[dict[int, tuple[str, str]], list[int]]:
    '''Sort the given issue numbers into ones that are closed and those which
    don't exist.
    
    :param repository: the GitHub repository in the form "owner/name".
    :param numbers: the issue or PR numbers to lookup.
    :param token: the GitHub API token, or None.
    :param workers: the number of concurrent workers to use for API requests
    
    :returns: a tuple of (closed issues, missing issues) where closed issues
    is a dictionary mapping issue numbers to (kind, title) and missing issues
    is a list of issue numbers that do not exist.'''

    closed = {}
    missing = []
    done = 0

    def lookup(number: int) -> tuple[int, dict | None]:
        return number, fetch_issue(repository, number, token)

    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        for number, issue in pool.map(lookup, numbers):
            done += 1
            if issue is None:
                missing.append(number)
            elif issue.get("state") == "closed":
                kind = "PR" if "pull_request" in issue else "issue"
                closed[number] = (kind, issue.get("title", ""))
    return closed, missing


def build_parser() -> argparse.ArgumentParser:
    '''Build the command line argument parser.
    
    :returns: the configured argument parser.
    '''
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--root",
        default=".",
        help="Root directory of the Psyclone source tree to search. Default:\
            current directory.",
    )
    parser.add_argument(
        "--repository",
        default=DEFAULT_REPOSITORY,
        help="GitHub repository to check issues against. Default: "
        + DEFAULT_REPOSITORY,
    )
    parser.add_argument(
        "--include",
        action="append",
        default=None,
        metavar="GLOB",
        help="filename pattern to search: may be repeated. Default: "
        + ", ".join(DEFAULT_INCLUDES),
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=None,
        metavar="NAME",
        help="directory to exclude from search: may be repeated. Default: "
        + ", ".join(DEFAULT_EXCLUDE_DIRS),
    )
    parser.add_argument(
        "--ref",
        default=None,
        help="git reference to use in generated links. Default: current \
            branch or tag.",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=8,
        metavar="N",
        help="number of concurrent workers to use for GitHub API requests.\
            Default: 8",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    '''Main entry point for the script
    :param argv: the command line arguments, or None to use sys.argv
    
    :returns: the process exit code (0 = no stale references found, 
    1 = stale references found, 2 = error).
    '''
    arguments = build_parser().parse_args(argv)

    includes = arguments.include or DEFAULT_INCLUDES
    exclude_dirs = arguments.exclude or DEFAULT_EXCLUDE_DIRS
    repository = arguments.repository

    references = find_references(arguments.root, includes, exclude_dirs)
    if not references:
        print("No references to GitHub issues found in the codebase.")
        return 0

    total = sum(len(places) for places in references.values())
    print(
        "Found "
        + str(total)
        + " references to "
        + str(len(references))
        + " unique GitHub issues in the codebase.",
        flush=True,
    )

    ref = (
        arguments.ref
        or run_git(arguments.root, "rev-parse", "HEAD")
        or "master"
    )

    def link(path: str, lineto: int) -> str:
        '''Build a GitHub link to the given file and line number
        :param path: the file path relative to the root of the repository.
        :param lineto: the line number to link to.
        
        :returns: a URL to the file and line number on GitHub.
        '''
        relative_path = os.path.relpath(path, arguments.root)
        return (
            "https://github.com/"
            + repository
            + "/blob/"
            + ref
            + "/"
            + relative_path
            + "#L"
            + str(lineto)
        )

    token = os.environ.get("GITHUB_TOKEN")
    if not token:
        print(
            "Warning: GITHUB_TOKEN not set in the environment. GitHub API \
                rate limit is 60 requests per hour.",
            file=sys.stderr,
        )

    try:
        closed, missing = classify(
            repository,
            sorted(references),
            token,
            arguments.workers,
        )
    except RuntimeError as error:
        print("Error: " + str(error), file=sys.stderr)
        return 2

    print()
    if missing:
        print(
            "Not found in "
            + repository
            + ": "
            + ", ".join("#" + str(n) for n in missing)
        )
        print()

    if not closed:
        print("No closed issues found in " + repository)
        return 0

    print("References to closed issues: ")
    for number in sorted(closed):
        kind, title = closed[number]
        print("#" + str(number) + " (" + kind + ", closed): " + title)
        for path, line, text in references[number]:
            print("  " + link(path, line) + ": " + text)
    print()
    print(
        "Found " + str(len(closed)) + " closed issue(s) still "
        "referenced in the code."
    )
    return 1


# ---------------------------------------------------------------------------
# Embedded test suite. Run with:  pytest utils/check_closed_issue_refs.py
# ---------------------------------------------------------------------------


def test_reference_pattern_matches_genuine_references():
    """Genuine "#<number>" references are matched."""
    assert REFERENCE_PATTERN.findall("# TODO #9999999: fix this") == [
        "9999999"
    ]
    assert REFERENCE_PATTERN.findall("see (#9999999) for details") == [
        "9999999"
    ]
    assert REFERENCE_PATTERN.findall("#9999999 at line start") == ["9999999"]
    assert REFERENCE_PATTERN.findall("closes #9999999 and #8888888") == [
        "9999999",
        "8888888",
    ]


def test_reference_pattern_rejects_false_positives():
    """Things that look like references but are not."""
    assert REFERENCE_PATTERN.findall('colour = "#123abc"') == []
    assert REFERENCE_PATTERN.findall("proc#1 (1 procs)") == []
    assert REFERENCE_PATTERN.findall("when fparser#211 is fixed") == []
    assert REFERENCE_PATTERN.findall("once fparser/#211 is fixed") == []


def test_reference_pattern_full_number():
    """ "#9999999" must not be matched inside "#99999999"."""
    assert REFERENCE_PATTERN.findall("see #9999999 here") == ["9999999"]


def test_find_references_collects_numbers_and_locations(tmp_path):
    """find_references records every reference with its location."""
    (tmp_path / "a.py").write_text("x = 1  # TODO #9999999: do the thing\n")
    docs = tmp_path / "docs"
    docs.mkdir()
    (docs / "b.rst").write_text("See issue #9999999 and issue #8888888.\n")

    refs = find_references(
        str(tmp_path), DEFAULT_INCLUDES, set(DEFAULT_EXCLUDE_DIRS)
    )

    assert set(refs) == {9999999, 8888888}
    assert len(refs[9999999]) == 2
    assert len(refs[8888888]) == 1
    _, lineno, text = refs[9999999][0]
    assert lineno == 1
    assert "#9999999" in text


def test_find_references_skips_excluded_dirs(tmp_path):
    """Excluded directories are not walked."""
    (tmp_path / "kept.py").write_text("# TODO #9999999 keep\n")
    git = tmp_path / ".git"
    git.mkdir()
    (git / "config.py").write_text("# TODO #8888888 ignore\n")

    refs = find_references(
        str(tmp_path), DEFAULT_INCLUDES, set(DEFAULT_EXCLUDE_DIRS)
    )

    assert set(refs) == {9999999}


def test_find_references_ignores_unlisted_extensions(tmp_path):
    """Only the configured file types are searched."""
    (tmp_path / "seen.py").write_text("# TODO #9999999\n")
    (tmp_path / "ignored.log").write_text("# TODO #8888888 \n")

    refs = find_references(
        str(tmp_path), DEFAULT_INCLUDES, set(DEFAULT_EXCLUDE_DIRS)
    )

    assert set(refs) == {9999999}


def _fake_api(states):
    """Build a stand-in for fetch_issue from a {number: response} mapping.
    A response of None represents a 404."""

    def fetch(repository, number, token):
        return states.get(number)

    return fetch


def test_classify_separates_closed_from_missing(monkeypatch):
    """classify sorts closed issues, open issues and 404s correctly."""
    import sys as _sys

    module = _sys.modules[__name__]
    states = {
        9999999: {"state": "closed", "title": "Closed issue"},
        8888888: {"state": "open", "title": "Still open"},
        7777777: None,
        6666666: {
            "state": "closed",
            "title": "Closed PR",
            "pull_request": {},
        },
    }
    monkeypatch.setattr(module, "fetch_issue", _fake_api(states))

    closed, missing = classify(
        "owner/repo",
        [9999999, 8888888, 7777777, 6666666],
        token=None,
        workers=2,
    )

    assert set(closed) == {9999999, 6666666}
    assert closed[9999999] == ("issue", "Closed issue")
    assert closed[6666666] == ("PR", "Closed PR")
    assert missing == [7777777]


def test_main_returns_1_when_closed_reference_found(monkeypatch, tmp_path):
    """A reference to a closed issue gives exit code 1."""
    import sys as _sys

    module = _sys.modules[__name__]
    (tmp_path / "a.py").write_text("# TODO #9999999 remove me\n")
    monkeypatch.setattr(
        module,
        "fetch_issue",
        _fake_api({9999999: {"state": "closed", "title": "x"}}),
    )
    monkeypatch.setattr(module, "run_git", lambda root, *a: "deadbeef")

    assert main(["--root", str(tmp_path)]) == 1


def test_main_returns_0_when_all_open(monkeypatch, tmp_path):
    """References only to open issues give exit code 0."""
    import sys as _sys

    module = _sys.modules[__name__]
    (tmp_path / "a.py").write_text("# TODO #9999999 still needed\n")
    monkeypatch.setattr(
        module,
        "fetch_issue",
        _fake_api({9999999: {"state": "open", "title": "x"}}),
    )
    monkeypatch.setattr(module, "run_git", lambda root, *a: "deadbeef")

    assert main(["--root", str(tmp_path)]) == 0


def test_main_returns_0_when_no_references(monkeypatch, tmp_path):
    """A tree with no references gives exit code 0."""
    (tmp_path / "a.py").write_text("nothing to see here\n")
    assert main(["--root", str(tmp_path)]) == 0


if __name__ == "__main__":
    sys.exit(main())
