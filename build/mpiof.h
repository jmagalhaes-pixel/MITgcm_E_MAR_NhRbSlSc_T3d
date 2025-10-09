!
! Copyright (C) 2003-2015 Intel Corporation.  All Rights Reserved.
! 
! The source code contained or described herein and all documents
! related to the source code ("Material") are owned by Intel Corporation
! or its suppliers or licensors.  Title to the Material remains with
! Intel Corporation or its suppliers and licensors.  The Material is
! protected by worldwide copyright and trade secret laws and treaty
! provisions.  No part of the Material may be used, copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed, or
! disclosed in any way without Intel's prior express written permission.
! 
! No license under any patent, copyright, trade secret or other
! intellectual property right is granted to or conferred upon you by
! disclosure or delivery of the Materials, either expressly, by
! implication, inducement, estoppel or otherwise.  Any license under
! such intellectual property rights must be express and approved by
! Intel in writing.
! 
!     Copyright (C) 1997 University of Chicago. 
! 
! 				  MPICH2 COPYRIGHT
! 
! The following is a notice of limited availability of the code, and disclaimer
! which must be included in the prologue of the code and in all source listings
! of the code.
! 
! Copyright Notice
!  + 2002 University of Chicago
! 
! Permission is hereby granted to use, reproduce, prepare derivative works, and
! to redistribute to others.  This software was authored by:
! 
! Mathematics and Computer Science Division
! Argonne National Laboratory, Argonne IL 60439
! 
! (and)
! 
! Department of Computer Science
! University of Illinois at Urbana-Champaign
! 
! 
! 			      GOVERNMENT LICENSE
! 
! Portions of this material resulted from work developed under a U.S.
! Government Contract and are subject to the following license: the Government
! is granted for itself and others acting on its behalf a paid-up, nonexclusive,
! irrevocable worldwide license in this computer software to reproduce, prepare
! derivative works, and perform publicly and display publicly.
! 
! 				  DISCLAIMER
! 
! This computer code material was prepared, in part, as an account of work
! sponsored by an agency of the United States Government.  Neither the United
! States, nor the University of Chicago, nor any of their employees, makes any
! warranty express or implied, or assumes any legal liability or responsibility
! for the accuracy, completeness, or usefulness of any information, apparatus,
! product, or process disclosed, or represents that its use would not infringe
! privately owned rights.
! 
! Portions of this code were written by Microsoft. Those portions are
! Copyright (c) 2007 Microsoft Corporation. Microsoft grants permission to
! use, reproduce, prepare derivative works, and to redistribute to
! others. The code is licensed "as is." The User bears the risk of using
! it. Microsoft gives no express warranties, guarantees or
! conditions. To the extent permitted by law, Microsoft excludes the
! implied warranties of merchantability, fitness for a particular
! purpose and non-infringement.
! 
! 
!
!     
!
! 
!    user include file for Fortran MPI-IO programs 
!
      INTEGER MPI_MODE_RDONLY, MPI_MODE_RDWR, MPI_MODE_WRONLY
      INTEGER MPI_MODE_DELETE_ON_CLOSE, MPI_MODE_UNIQUE_OPEN
      INTEGER MPI_MODE_CREATE, MPI_MODE_EXCL
      INTEGER MPI_MODE_APPEND, MPI_MODE_SEQUENTIAL
      PARAMETER (MPI_MODE_RDONLY=2, MPI_MODE_RDWR=8, MPI_MODE_WRONLY=4)
      PARAMETER (MPI_MODE_CREATE=1, MPI_MODE_DELETE_ON_CLOSE=16)
      PARAMETER (MPI_MODE_UNIQUE_OPEN=32, MPI_MODE_EXCL=64)
      PARAMETER (MPI_MODE_APPEND=128, MPI_MODE_SEQUENTIAL=256)
!
      INTEGER MPI_FILE_NULL
      PARAMETER (MPI_FILE_NULL=0)
!
      INTEGER MPI_MAX_DATAREP_STRING
      PARAMETER (MPI_MAX_DATAREP_STRING=128)
!
      INTEGER MPI_SEEK_SET, MPI_SEEK_CUR, MPI_SEEK_END
      PARAMETER (MPI_SEEK_SET=600, MPI_SEEK_CUR=602, MPI_SEEK_END=604)
!
      INTEGER MPIO_REQUEST_NULL
      PARAMETER (MPIO_REQUEST_NULL=0)
!
      integer*8 MPI_DISPLACEMENT_CURRENT
      PARAMETER (MPI_DISPLACEMENT_CURRENT=-54278278)
!
      INTEGER MPI_OFFSET_KIND
      PARAMETER (MPI_OFFSET_KIND=           8)
!
!
!
!
!
!
!
!
!
!
!
!
!
