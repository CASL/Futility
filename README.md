!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                                 Introduction                                 !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
The Futility project is a CMake-configurable package of FORTRAN utilities that
contains the following capabilities:
- Definition of kinds for intrinsic numeric data types
- Unit test harness
- Definitions for some basic extensions to the Fortran language:
 - arbitrary length strings, a parameter list construct, exception handlers,
   a command line processor, and timers
- Geometry definitions and intersection routines for:
 - point, line, plane, box, cylinder, and polyhedron
- File wrapper functions:
 - standard Fortran input/output files, binary files, HDF5 files, and VTK files
- Parallel wrapper functions:
 - MPI, and OpenMP abstraction layers, partitioning algorithms
- Math utilities:
 - BLAS, Matrix and Vector definitions, Linear Solver methods, wrappers for
   other TPLs (PETSC, MKL, etc), and pre-conditioner classes,
- A lightweight profiling interface, and
- Other miscellaneous routines:
 - random number generator, water saturation properties, sorting algorithms

The Futility project will not contain any encryption technology to preserve the
  open source license.

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                                   Citation                                   !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
Proper citation of the Futility software...


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                                    Origin                                    !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
The original software package that is now Futility was developed at the
   University of Michigan to enable efficient development and testing of the
   MPACT code. The original developers, now at ORNL and Michigan, further
   extended this library and generalized it for use with COBRA-TF and MAMBA as
   part of VERA. Further information on these original applications that
   leverage Futility can be found at www.casl.gov.

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                  Software License and Contributor Agreement                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
License Agreement:
- By downloading, copying, installing or using the software you agree to the
  Apache License v2.0, as described in LICENSE.txt.  If you do not agree to
  this license, do not download, install, copy or use the software.

Contributor Agreement:
- All contributors to the software have agreed that their contributions may be
  distributed with the license in LICENSE.txt. If you do not agree to the
  distribution of your contribution under this license, do not contribute to
  this software project.
- When a contribution is made through a pull request at github.com/CASL/Futility
  the author has the right to add an additional Copyright statement to the
  LICENSE.txt file in their pull request. The original copyright statements
  should remain followed by the new contribution.
- The administrators of the github.com/CASL/Futility project reserve the right
  to decline any pull request for any reason.

