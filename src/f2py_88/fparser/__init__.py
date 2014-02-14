"""Fortran parser package.

:Author:
  Pearu Peterson <pearu.peterson@gmail.com>
:Created: September 2006

Overview
========

The Fortran parser package is a Python implementation of Fortran
66/77/90/95/2003 language parser.
The Fortran language syntax rules are defined in `Fortran2003` package,
the rules are taken from the following ISO/IEC 1539 working draft:

  http://j3-fortran.org/doc/2003_Committee_Draft/04-007.pdf

Modules
-------

.. autosummary::

  fparser.api
  fparser.Fortran2003
  fparser.readfortran
  fparser.parsefortran
  
"""
#Author: Pearu Peterson <pearu@cens.ioc.ee>
#Created: Oct 2006

import os
import logging, logging.config

# Default logging configuration file
_DEFAULT_LOG_CONFIG_PATH = os.path.join(os.path.dirname(__file__),'log.config')

# Setup loggers
logging.config.fileConfig(_DEFAULT_LOG_CONFIG_PATH)

__autodoc__ = ['api', 'Fortran2003', 'readfortran', 'parsefortran']
