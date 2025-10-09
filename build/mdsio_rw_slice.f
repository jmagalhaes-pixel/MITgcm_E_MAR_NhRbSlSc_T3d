C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_rw_slice.F,v 1.1 2009/09/01 19:19:10 jmc Exp $
C $Name: checkpoint62o $

C $Header: /u/gcmpack/MITgcm/pkg/mdsio/MDSIO_OPTIONS.h,v 1.8 2010/09/24 18:39:35 gforget Exp $
C $Name: checkpoint62o $








C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/CPP_OPTIONS.h,v 1.14 2004/04/06 01:26:37 jmc Exp $
C $Name: checkpoint62 $


C CPP flags controlling particular source code features

C o Shortwave heating as extra term in external_forcing.F
C Note: this should be a run-time option

C o Include/exclude phi_hyd calculation code

C o Include/exclude call to S/R CONVECT

C o Include/exclude call to S/R CALC_DIFFUSIVITY

C o Include/exclude Implicit vertical advection code

C o Include/exclude nonHydrostatic code

C o Include the flt package -> JM 2021_Oct, commented
C #define 

C o Include pressure loading code

C o Use "Exact Convervation" of fluid in Free-Surface formulation
C   so that d/dt(eta) is exactly equal to - Div.Transport

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that surface thickness (hFactors) vary with time

C o Use "OLD" UV discretisation near boundaries (*not* recommended)
C   Note - only works with  #undef NO_SLIP_LATERAL  in calc_mom_rhs.F
C          because the old code did not have no-slip BCs

C o Execution environment support options
C $Header: /u/gcmpack/MITgcm/eesupp/inc/CPP_EEOPTIONS.h,v 1.33 2010/03/04 22:01:35 jmc Exp $
C $Name: checkpoint62o $

CBOP
C     !ROUTINE: CPP_EEOPTIONS.h
C     !INTERFACE:
C     include "CPP_EEOPTIONS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP\_EEOPTIONS.h                                         |
C     *==========================================================*
C     | C preprocessor "execution environment" supporting        |
C     | flags. Use this file to set flags controlling the        |
C     | execution environment in which a model runs - as opposed |
C     | to the dynamical problem the model solves.               |
C     | Note: Many options are implemented with both compile time|
C     |       and run-time switches. This allows options to be   |
C     |       removed altogether, made optional at run-time or   |
C     |       to be permanently enabled. This convention helps   |
C     |       with the data-dependence analysis performed by the |
C     |       adjoint model compiler. This data dependency       |
C     |       analysis can be upset by runtime switches that it  |
C     |       is unable to recoginise as being fixed for the     |
C     |       duration of an integration.                        |
C     |       A reasonable way to use these flags is to          |
C     |       set all options as selectable at runtime but then  |
C     |       once an experimental configuration has been        |
C     |       identified, rebuild the code with the appropriate  |
C     |       options set at compile time.                       |
C     *==========================================================*
CEOP


C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C=== Macro related options ===
C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working set size.
C     However, on vector CRAY systems this degrades performance.
C     Enable to switch REAL4_IS_SLOW from genmake2 (with LET_RS_BE_REAL4):

C--   Control use of "double" precision constants.
C     Use D0 where it means REAL*8 but not where it means REAL*16

C--   Enable some old macro conventions for backward compatibility

C=== IO related options ===
C--   Flag used to indicate whether Fortran formatted write
C     and read are threadsafe. On SGI the routines can be thread
C     safe, on Sun it is not possible - if you are unsure then
C     undef this option.

C--   Flag used to indicate whether Binary write to Local file (i.e.,
C     a different file for each tile) and read are thread-safe.

C--   Flag to turn off the writing of error message to ioUnit zero

C--   Alternative formulation of BYTESWAP, faster than
C     compiler flag -byteswapio on the Altix.

C=== MPI, EXCH and GLOBAL_SUM related options ===
C--   Flag turns off MPI_SEND ready_to_receive polling in the
C     gather_* subroutines to speed up integrations.

C--   Control MPI based parallel processing
CXXX We no longer select the use of MPI via this file (CPP_EEOPTIONS.h)
CXXX To use MPI, use an appropriate genmake2 options file or use
CXXX genmake2 -mpi .
CXXX #undef  1
CXXX #undef  1

C--   Control use of communication that might overlap computation.
C     Under MPI selects/deselects "non-blocking" sends and receives.
C--   Control use of communication that is atomic to computation.
C     Under MPI selects/deselects "blocking" sends and receives.

C--   Control use of JAM routines for Artic network
C     These invoke optimized versions of "exchange" and "sum" that
C     utilize the programmable aspect of Artic cards.
CXXX No longer supported ; started to remove JAM routines.
CXXX #undef  LETS_MAKE_JAM
CXXX #undef  JAM_WITH_TWO_PROCS_PER_NODE

C--   Control XY periodicity in processor to grid mappings
C     Note: Model code does not need to know whether a domain is
C           periodic because it has overlap regions for every box.
C           Model assume that these values have been
C           filled in some way.

C--   Alternative way of doing global sum without MPI allreduce call
C     but instead, explicit MPI send & recv calls.

C--   Alternative way of doing global sum on a single CPU
C     to eliminate tiling-dependent roundoff errors.
C     Note: This is slow.

C=== Other options (to add/remove pieces of code) ===
C--   Flag to turn on checking for errors from all threads and procs
C     (calling S/R STOP_IF_ERROR) before stopping.

C--   Control use of communication with other component:
C     allow to import and export from/to Coupler interface.


C $Header: /u/gcmpack/MITgcm/eesupp/inc/CPP_EEMACROS.h,v 1.23 2010/08/12 21:38:58 jmc Exp $
C $Name: checkpoint62o $

CBOP
C     !ROUTINE: CPP_EEMACROS.h
C     !INTERFACE:
C     include "CPP_EEMACROS.h "
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP_EEMACROS.h
C     *==========================================================*
C     | C preprocessor "execution environment" supporting
C     | macros. Use this file to define macros for  simplifying
C     | execution environment in which a model runs - as opposed
C     | to the dynamical problem the model solves.
C     *==========================================================*
CEOP


C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C     Flag used to indicate which flavour of multi-threading
C     compiler directives to use. Only set one of these.
C     USE_SOLARIS_THREADING  - Takes directives for SUN Workshop
C                              compiler.
C     USE_KAP_THREADING      - Takes directives for Kuck and
C                              Associates multi-threading compiler
C                              ( used on Digital platforms ).
C     USE_IRIX_THREADING     - Takes directives for SGI MIPS
C                              Pro Fortran compiler.
C     USE_EXEMPLAR_THREADING - Takes directives for HP SPP series
C                              compiler.
C     USE_C90_THREADING      - Takes directives for CRAY/SGI C90
C                              system F90 compiler.






C--   Define the mapping for the _BARRIER macro
C     On some systems low-level hardware support can be accessed through
C     compiler directives here.

C--   Define the mapping for the BEGIN_CRIT() and  END_CRIT() macros.
C     On some systems we simply execute this section only using the
C     master thread i.e. its not really a critical section. We can
C     do this because we do not use critical sections in any critical
C     sections of our code!

C--   Define the mapping for the BEGIN_MASTER_SECTION() and
C     END_MASTER_SECTION() macros. These are generally implemented by
C     simply choosing a particular thread to be "the master" and have
C     it alone execute the BEGIN_MASTER..., END_MASTER.. sections.

CcnhDebugStarts
C      Alternate form to the above macros that increments (decrements) a counter each
C      time a MASTER section is entered (exited). This counter can then be checked in barrier
C      to try and detect calls to BARRIER within single threaded sections.
C      Using these macros requires two changes to Makefile - these changes are written
C      below.
C      1 - add a filter to the CPP command to kill off commented _MASTER lines
C      2 - add a filter to the CPP output the converts the string N EWLINE to an actual newline.
C      The N EWLINE needs to be changes to have no space when this macro and Makefile changes
C      are used. Its in here with a space to stop it getting parsed by the CPP stage in these
C      comments.
C      #define IF ( a .EQ. 1 ) THEN  IF ( a .EQ. 1 ) THEN  N EWLINE      CALL BARRIER_MS(a)
C      #define ENDIF    CALL BARRIER_MU(a) N EWLINE        ENDIF
C      'CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  grep -v '^[cC].*_MASTER' | cpp  -traditional -P'
C      .F.f:
C      $(CPP) $(DEFINES) $(INCLUDES) |  sed 's/N EWLINE/\n/' > $@
CcnhDebugEnds

C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working
C     set size. However, on vector CRAY systems this degrades
C     performance.
C- Note: global_sum/max macros were used to switch to  JAM routines (obsolete);
C  in addition, since only the R4 & R8 S/R are coded, GLOBAL RS & RL macros
C  enable to call the corresponding R4 or R8 S/R.



C- Note: a) exch macros were used to switch to  JAM routines (obsolete)
C        b) exch R4 & R8 macros are not practically used ; if needed,
C           will directly call the corrresponding S/R.

C--   Control use of JAM routines for Artic network (no longer supported)
C     These invoke optimized versions of "exchange" and "sum" that
C     utilize the programmable aspect of Artic cards.
CXXX No longer supported ; started to remove JAM routines.
CXXX #ifdef LETS_MAKE_JAM
CXXX #define CALL GLOBAL_SUM_R8 ( a, b) CALL GLOBAL_SUM_R8_JAM ( a, b)
CXXX #define CALL GLOBAL_SUM_R8 ( a, b ) CALL GLOBAL_SUM_R8_JAM ( a, b )
CXXX #define CALL EXCH_XY_RS ( a, b ) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define CALL EXCH_XY_RL ( a, b ) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define CALL EXCH_XYZ_RS ( a, b ) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #define CALL EXCH_XYZ_RL ( a, b ) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #endif

C--   Control use of "double" precision constants.
C     Use d0 where it means REAL*8 but not where it means REAL*16

C--   Substitue for 1.D variables
C     Sun compilers do not use 8-byte precision for literals
C     unless .Dnn is specified. CRAY vector machines use 16-byte
C     precision when they see .Dnn which runs very slowly!



C o Include/exclude code specific to the ECCO/SEALION version.
C   AUTODIFF or EXF package.
C   Currently controled by a single header file
C   For this to work, PACKAGES_CONFIG.h needs to be included!
cph#if (defined (ALLOW_AUTODIFF) || cph     defined (ALLOW_ECCO) || cph     defined (ALLOW_EXF))
cph# include "ECCO_CPPOPTIONS.h"
cph#endif



C Defining SAFE_IO stops the model from overwriting its own files



C I/O that includes tile halos in the files


C--  File mdsio_rw_slice.F: old version of MDS_READ/WRITE_SEC_XZ/YZ S/R with
C    fewer arguments (kept for backward compatibility): call new MDSIO S/R
C    with fixed additional arguments
C--   Contents
C--   o MDSREADFIELDXZ
C--   o MDSREADFIELDYZ
C--   o MDSREADFIELDXZ_LOC
C--   o MDSREADFIELDYZ_LOC
C--   o MDSWRITEFIELDXZ
C--   o MDSWRITEFIELDYZ
C--   o MDSWRITEFIELDXZ_LOC
C--   o MDSWRITEFIELDYZ_LOC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDXZ(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_XZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.FALSE.) allows to read files from
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSREADFIELDXZ is retired'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDYZ(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_YZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.FALSE.) allows to read files from
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSREADFIELDYZ is retired'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDXZ_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_XZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSREADFIELDXZ_LOC is empty'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDYZ_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_YZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSREADFIELDYZ_LOC is empty'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDXZ(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_XZ, just a way to add extra arguments
C to the argument list. 
C The 1rst new argument (useCurrentDir=.FALSE.) allows to write files to
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSWRITEFIELDXZ is retired'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDYZ(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_YZ, just a way to add extra arguments
C to the argument list. 
C The 1rst new argument (useCurrentDir=.FALSE.) allows to write files to
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSWRITEFIELDYZ is retired'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDXZ_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_XZ, just a way to add extra arguments
C to the argument list. 
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSWRITEFIELDXZ_LOC is empty'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDYZ_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_YZ, just a way to add extra arguments
C to the argument list. 
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/SIZE.h,v 1.5 2003/12/10 16:25:57 adcroft Exp $
C $Name: checkpoint56 $
C
C     /==========================================================C     | SIZE.h Declare size of underlying computational grid.    |
C     |==========================================================|
C     | The design here support a three-dimensional model grid   |
C     | with indices I,J and K. The three-dimensional domain     |
C     | is comprised of nPx*nSx blocks of size sNx along one axis|
C     | nPy*nSy blocks of size sNy along another axis and one    |
C     | block of size Nz along the final axis.                   |
C     | Blocks have overlap regions of size OLx and OLy along the|
C     | dimensions that are subdivided.                          |
C     \==========================================================/
C     Voodoo numbers controlling data layout.
C     sNx - No. X points in sub-grid.
C     sNy - No. Y points in sub-grid.
C     OLx - Overlap extent in X.
C     OLy - Overlat extent in Y.
C     nSx - No. sub-grids in X.
C     nSy - No. sub-grids in Y.
C     nPx - No. of processes to use in X.
C     nPy - No. of processes to use in Y.
C     Nx  - No. points in X for the total domain.
C     Ny  - No. points in Y for the total domain.
C     Nr  - No. points in Z for full process domain.
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  50,
     &           sNy =   1,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   60,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  200)

C     MAX_OLX  - Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      Real*8     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSWRITEFIELDYZ_LOC is empty'

      RETURN
      END
