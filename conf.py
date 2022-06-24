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
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

# -- Project information -----------------------------------------------------

project = 'Haskell Optimization Handbook'
html_title = 'Haskell Optimization Handbook'
copyright = '2022, Jeffrey Young (doyougnu)'
author = 'Jeffrey Young (doyugnu)'

# The full version, including alpha/beta/rc tags
release = '0.0.1'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [ 'sphinx.ext.mathjax'
             , 'sphinx.ext.todo'
             , 'sphinx.ext.githubpages'  ## this bypasses jekyll in the CI, if
                                         ## you remove it the output will not
                                         ## contain any .css files from any
                                         ## directory with a prefixes
                                         ## underscore
             , 'sphinxcontrib.bibtex'
             ]

# flags
todo_include_todos = True
todo_link_only     = True

# The rst file that holds the ToC. This indirection allows for a nice splash page
master_doc = "contents"

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', '.dir-locals.el', '.projectile']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme      = "press"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']
mathjax_path = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"

# bibtex file
bibtex_bibfiles = ['bib/book.bib']
bibtex_default_style = 'unsrt'
