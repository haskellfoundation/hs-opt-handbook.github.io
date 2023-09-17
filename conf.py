# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.

import os
import sys
import time
from datetime import datetime
from sphinx.util.i18n import format_date

## FIXME for the time being I've cloned relevant extensions into a submodule,
## and then hard linked the __init__.py file until my changes can be upstreamed
sys.path.insert(0, os.path.abspath('extensions'))

# -- Project information -----------------------------------------------------

project = 'Haskell Optimization Handbook'
html_title = 'Haskell Optimization Handbook'
# FIXME: https://github.com/haskellFoundation/hs-opt-handbook.github.io/issues/58
now = datetime.now()
copyright = u'2022-%s, Jeffrey Young (doyougnu)' % now.year
author = 'Jeffrey Young (doyugnu)'

# The full version, including alpha/beta/rc tags
release = '0.0.1'

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [ 'sphinx.ext.mathjax'
             , 'sphinx.ext.todo'
             , 'sphinx.ext.extlinks'
             , 'sphinx.ext.autosectionlabel'
             , 'sphinx.ext.githubpages'  ## this bypasses jekyll in the CI, if
                                         ## you remove it the output will not
                                         ## contain any .css files from any
                                         ## directory with a prefixes
                                         ## underscore
             , 'sphinxcontrib.bibtex'
             , 'sphinx_copybutton'
             # , 'sphinxcontrib.execHS.ext'
             , 'sphinx_exec_directive'
             , 'conceptual_admonitions'
             ]

# flags
todo_include_todos = False
todo_link_only     = False
autosectionlabel_prefix_document = True

## global links in the book that share a prefix that we've named.
extlinks = {'userGuide': ('https://downloads.haskell.org/~ghc/9.2.4/docs/html/users_guide/%s', '%s'),
            'ghcWiki': ('https://gitlab.haskell.org/ghc/ghc/wikis/%s', '#%s'),
            'haskellPerf': ('https://github.com/haskell-perf/%s', '%s'),
            }

# prolog for colored text and global variables
rst_prolog = """
.. include:: <s5defs.txt>

"""  + open('custom.rst', 'r').read()

# The rst file that holds the ToC. This indirection allows for a nice splash page
master_doc = "contents"

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [ '_build'
                   , 'Thumbs.db'
                   , '.DS_Store'
                   , '.dir-locals.el'
                   , '.projectile'
                   , 'README.rst'
                   , 'glossary.rst'
                   , 'custom.rst'
                   , '.direnv'
                   , 'code/lethargy/.direnv'
                   ]


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme      = "press"
html_theme_options = { "body_max_width": 1600 }
html_static_path = ['_static', 'code']
html_css_files = [ 'css/s4defs-roles.css', 'css/iframe.css', 'css/admonitions.css' ]

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']
mathjax_path = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"

# bibtex file
bibtex_bibfiles = ['bib/book.bib']
bibtex_default_style = 'unsrt'

def setup(app):
    for sheet in html_css_files:
        app.add_css_file(sheet)
