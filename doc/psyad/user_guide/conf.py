# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys

# -- Project information -----------------------------------------------------

project = 'PSyAD'
copyright = '2021-2025, Rupert Ford and Andrew Porter'
author = 'Rupert Ford and Andrew Porter'

# The full version, including alpha/beta/rc tags
release = '0.0.1'

docs_dir = os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__))))

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here.
sys.path.insert(0, os.path.join(docs_dir, "_ext"))

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones. The definition of 'apilinks' is in PSyclone/doc/_ext/apilinks.py.
extensions = [
    'sphinx.ext.imgmath', 'sphinx.ext.autodoc', 'sphinx.ext.intersphinx',
    'sphinx_autodoc_typehints',
    'apilinks']


# GITHUB_PR_NUMBER is set in .github/workflows/python-package.yml when
# running the document-testing job.
github_pr_num = os.environ.get('GITHUB_PR_NUMBER')

if github_pr_num:
    # Generate links to a webserver running locally if we're generating
    # documentation as part of the CI run on GHA.
    usr_guide_base = 'http://0.0.0.0:8000/user_guide/_build/html/'
    ref_guide_base = (
        'http://0.0.0.0:8000/reference_guide/build/html/autogenerated/')
else:
    # This is not a CI run so we link to the documentation hosted at
    # ReadTheDocs. We check for a RTD environment variable (see
    # https://docs.readthedocs.io/en/stable/builds.html#build-environment)
    # to decide whether we should link to the 'stable' or 'latest' versions.
    if os.environ.get('READTHEDOCS_VERSION') == "stable":
        version_txt = "stable"
    else:
        version_txt = "latest"
    usr_guide_base = f'https://psyclone.readthedocs.io/en/{version_txt}/'
    ref_guide_base = (
        f'https://psyclone-ref.readthedocs.io/en/{version_txt}/autogenerated/')

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']

# Configuration for intersphinx: allows us to link to the PSyclone User Guide.
# The base URL for this is set earlier and depends on whether the docs are
# being built as part of a CI run or not.
intersphinx_mapping = {
    'user_guide': (usr_guide_base, None)}
