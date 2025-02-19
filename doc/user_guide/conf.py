# -*- coding: utf-8 -*-

'''
 PSyclone documentation build configuration file, created by
 sphinx-quickstart on Mon Jan 27 12:50:29 2014.
'''

# This file is execfile()d with the current directory set to its
# containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

# The names of the variables in this file are Sphinx keywords so
# we can't make them uppercase as pylint demands.
# pylint: disable=invalid-name

import os
import sys

docs_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here.
sys.path.insert(0, os.path.join(docs_dir, "_ext"))

# -- General configuration ----------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
# We need version >=  1.8 for the html_css_files feature.
needs_sphinx = '1.8'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
# The definition of 'apilinks' is in PSyclone/doc/_ext/apilinks.py.
extensions = ['sphinx.ext.autodoc', 'sphinx.ext.doctest',
              'sphinx.ext.intersphinx', 'sphinx.ext.coverage',
              'sphinx.ext.imgmath', 'sphinx.ext.viewcode',
              'sphinxcontrib.bibtex', 'sphinx_tabs.tabs',
              'sphinx_autodoc_typehints',
              'apilinks',
              ]
bibtex_bibfiles = ['../bibliography/references.bib']

# GITHUB_PR_NUMBER is set in .github/workflows/python-package.yml when
# running the document-testing job.
github_pr_num = os.environ.get('GITHUB_PR_NUMBER')

if github_pr_num:
    # Generate links to a webserver running locally if we're generating
    # documentation as part of the CI run on GHA.
    dev_guide_base = 'http://0.0.0.0:8000/developer_guide/_build/html/'
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
    dev_guide_base = f'https://psyclone-dev.readthedocs.io/en/{version_txt}/'
    ref_guide_base = (f'https://psyclone-ref.readthedocs.io/en/{version_txt}/'
                      f'autogenerated/')

# Enable numbered referencing of figures (use with :numref:`my-fig-reference`)
numfig = True

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix of source filenames.
source_suffix = '.rst'

# The encoding of source files.
# source_encoding = 'utf-8-sig'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = 'PSyclone'
project_copyright = '2017-2025, STFC Daresbury Laboratory'

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
#
# We use the version module in src/psyclone. However, rather than importing
# it (which would require that PSyclone be installed first), we read it
# using execfile().
# 'version' is the short X.Y version and 'release' is the full version,
# including any alpha/beta/rc tags.
# We are in the doc/user_guide directory but need to read version.py from
# src/psyclone
BASE_PATH = os.path.dirname(os.path.abspath(__file__))
BASE_PATH = os.path.dirname(os.path.dirname(BASE_PATH))
with open(os.path.join(BASE_PATH, "src", "psyclone", "version.py"),
          encoding="utf-8") as f:
    # pylint: disable-next=exec-used
    exec(f.read())
# pylint: disable=undefined-variable
version = __SHORT_VERSION__
release = __VERSION__
# pylint: enable=undefined-variable

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
# language = None

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
# today = ''
# Else, today_fmt is used as the format for a strftime call.
# today_fmt = '%B %d, %Y'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build']

# The reST default role (used for this markup: `text`) to use for all
# documents.
# default_role = None

# If true, '()' will be appended to :func: etc. cross-reference text.
# add_function_parentheses = True

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
# add_module_names = True

# If true, sectionauthor and moduleauthor directives will be shown in the
# output. They are ignored by default.
# show_authors = False

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# A list of ignored prefixes for module index sorting.
# modindex_common_prefix = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'sphinx_rtd_theme'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
# html_theme_options = {}

# Add any paths that contain custom themes here, relative to this directory.
# html_theme_path = []

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
# html_title = None

# A shorter title for the navigation bar.  Default is the same as html_title.
# html_short_title = None

# The name of an image file (relative to this directory) to place at the top
# of the sidebar.
# html_logo = None

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
# html_favicon = None

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# These paths are either relative to html_static_path
# or fully qualified paths (eg. https://...)
html_css_files = ['theme_overrides.css']  # override wide tables in RTD theme

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
# html_last_updated_fmt = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
# html_use_smartypants = True

# Custom sidebar templates, maps document names to template names.
# html_sidebars = {}

# Additional templates that should be rendered to pages, maps page names to
# template names.
# html_additional_pages = {}

# If false, no module index is generated.
# html_domain_indices = True

# If false, no index is generated.
# html_use_index = True

# If true, the index is split into individual pages for each letter.
# html_split_index = False

# If true, links to the reST sources are added to the pages.
# html_show_sourcelink = True

# If true, "Created using Sphinx" is shown in the HTML footer. Default is True.
# html_show_sphinx = True

# If true, "(C) Copyright ..." is shown in the HTML footer. Default is True.
# html_show_copyright = True

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
# html_use_opensearch = ''

# This is the file name suffix for HTML files (e.g. ".xhtml").
# html_file_suffix = None

# Output file base name for HTML help builder.
htmlhelp_basename = 'psyclonedoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    # 'preamble': '',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author,
#  documentclass [howto/manual]).
latex_documents = [
    ('index', 'psyclone.tex', 'PSyclone User Guide',
     'Oakley Brunt, Andrew Coughtrie, Joshua Dendy, \\\\ '
     'Rupert Ford, Joerg Henrichs, Iva Kavcic, Andrew Porter, \\\\ '
     'Sergi Siso and Joseph Wallwork', 'manual'),
]

# Set maximum depth for the nested lists to prevent LaTeX
# "too deeply nested" build failures when using whitespaces instead
# of tabs in documentation (there must be an indentation of at least
# three spaces when nesting a list within another). This is a known
# Docutils failure, see e.g. here:
# https://docutils.sourceforge.io/docs/dev/todo.html
# LaTeX can have up to 6 lists (of any sort) nested, 4 "enumerate"
# environments among the set of nested lists and 4 "itemize"
# environments among the set of nested lists, see e.g. here
# https://texfaq.org/FAQ-toodeep
latex_elements = {
    'maxlistdepth': '6',
}

# The name of an image file (relative to this directory) to place at the top of
# the title page.
# latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
# latex_use_parts = False

# If true, show page references after internal links.
# latex_show_pagerefs = False

# If true, show URL addresses after external links.
# latex_show_urls = False

# Documents to append as an appendix to all manuals.
# latex_appendices = []

# If false, no module index is generated.
# latex_domain_indices = True


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'psyclone', 'PSyclone Documentation',
     ['Rupert Ford, Joerg Henrichs, Iva Kavcic, Andrew Porter, Sergi '
      'Siso and Joseph Wallwork'], 1)
]

# If true, show URL addresses after external links.
# man_show_urls = False


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    ('index', 'psyclone', 'psyclone Documentation',
     'Rupert Ford, Joerg Henrichs, Iva Kavcic, Andrew Porter, Sergi Siso, '
     'and Joseph Wallwork',
     'psyclone',
     'A domain-specific compiler for Finite-Element/Volume/Difference models.',
     'Miscellaneous'),
]

# Documents to append as an appendix to all manuals.
# texinfo_appendices = []

# If false, no module index is generated.
# texinfo_domain_indices = True

# How to display URL addresses: 'footnote', 'no', or 'inline'.
# texinfo_show_urls = 'footnote'


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = 'PSyclone'
epub_author = 'Rupert Ford, Joerg Henrichs, Iva Kavcic, Andrew Porter, ' \
    'Sergi Siso and Joseph Wallwork'
epub_publisher = 'Rupert Ford, Joerg Henrichs, Iva Kavcic, Andrew Porter, ' \
    'Sergi Siso and Joseph Wallwork'
epub_copyright = project_copyright

# The language of the text. It defaults to the language option
# or en if the language is not set.
# epub_language = ''

# The scheme of the identifier. Typical schemes are ISBN or URL.
# epub_scheme = ''

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
# epub_identifier = ''

# A unique identification for the text.
# epub_uid = ''

# A tuple containing the cover image and cover page html template filenames.
# epub_cover = ()

# HTML files that should be inserted before the pages created by sphinx.
# The format is a list of tuples containing the path and title.
# epub_pre_files = []

# HTML files shat should be inserted after the pages created by sphinx.
# The format is a list of tuples containing the path and title.
# epub_post_files = []

# A list of files that should not be packed into the epub file.
# epub_exclude_files = []

# The depth of the table of contents in toc.ncx.
# epub_tocdepth = 3

# Allow duplicate toc entries.
# epub_tocdup = True


# -- Options for linkcheck -------------------------------------------------

linkcheck_anchors = True
# We need to ignore this anchor (used a couple of times in examples.rst)
# because it seems that GitHub's JavaScript-generated page defeats the
# link checker.
linkcheck_anchors_ignore = ['user-content-netcdf-library-lfric-examples']

# MyBinder fails on a very regular basis so we skip those links.
linkcheck_ignore = [r'^https://mybinder.org/v2/gh/stfc/psyclone',
                    # Sphinx has problems with Github anchors, so we skip
                    # the links to anchors to the main README.
                    r'^https://github.com/stfc/PSyclone#',
                    # Requires authentication.
                    r'code.metoffice.gov.uk/trac/lfric/attachment/wiki/'
                    r'LFRicDocumentationPapers/lfric_documentation.pdf']


# Configuration for intersphinx: refer to the Python standard library and
# to the PSyclone Developer Guide. The base URL for this
# is set earlier and depends on whether the docs are being built as part
# of a CI run or not.
intersphinx_mapping = {
    'python': ('http://docs.python.org/', None),
    'dev_guide': (dev_guide_base, None),
    'psyad': ('https://psyclone-adjoint.readthedocs.io/en/latest', None)}
