==================================================================
Futility Configure, Build, Test, and Install Quick Reference Guide
==================================================================

:Author: Benjamin S Collins
:Contact: bscollin@umich.edu

:Abstract: This document contains quick reference information on how to configure, build, test, and install Futility using the TriBITS CMake build system. The primary audience are developers of Futility that need to configure and build the software.

.. NOTE: Above we can't put in newlines as the RST parser does not allow this.

.. sectnum::

.. contents::

Introduction
============

Futility description

The Futility Installation Guide provides information on how to clone the
various Futility repos and getting set up to do a basic build and test of selected
Futility components.

The set of Futility extra repos is officially defined in the file:

  `Futility/cmake/ExtraRepositoriesList.cmake <cmake/ExtraRepositoriesList.cmake>`_

Configurations of Futility tend to keep complex configuration inside of CMake
options fragment files and are read in using the TriBITS variable
``Futility_CONFIGURE_OPTIONS_FILE`` (see `Basic configuration`_ for details).
Trying to set up a basic configuration for Futility from scratch is very
difficult.  Thankfully, there are standard configuration for Futility located
in the directory:

  `Futility/build_scripts`

Futility-specific options
=========================

Below, configure options specific to Futility are given.  The later sections
give more generic options that are the same for all TriBITS projects.

First note that the location of ``TRIBITS_BASE_DIR`` in Futility is::

  TRIBITS_BASE_DIR=Futility/cmake/tribits

ToDo: Fill in Futility-specific options.
