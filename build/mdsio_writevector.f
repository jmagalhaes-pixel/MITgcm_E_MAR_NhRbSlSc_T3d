C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_writevector.F,v 1.14 2009/09/01 19:00:15 jmc Exp $
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


      SUBROUTINE MDSWRITEVECTOR(
     I   fName,
     I   filePrec,
     I   globalfile,
     I   arrType,
     I   narr,
     I   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C narr       integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(narr)
C bi         integer :: x tile index
C bj         integer :: y tile index
C irecord    integer :: record number to read
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with Real*8 and Real*8 declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)
C Changed: 05/31/00 heimbach@mit.edu
C          open(dUnit, ..., status='old', ... -> status='unknown'

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
C $Header: /u/gcmpack/MITgcm/eesupp/inc/EEPARAMS.h,v 1.29 2010/10/05 17:43:40 mlosch Exp $
C $Name: checkpoint62o $
CBOP
C     !ROUTINE: EEPARAMS.h
C     !INTERFACE:
C     include "EEPARAMS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EEPARAMS.h                                               |
C     *==========================================================*
C     | Parameters for "execution environemnt". These are used   |
C     | by both the particular numerical model and the execution |
C     | environment support routines.                            |
C     *==========================================================*
CEOP

C     ========  EESIZE.h  ========================================

C     MAX_LEN_MBUF  - Default message buffer max. size
C     MAX_LEN_FNAM  - Default file name max. size
C     MAX_LEN_PREC  - Default rec len for reading "parameter" files

      INTEGER MAX_LEN_MBUF
      PARAMETER ( MAX_LEN_MBUF = 512 )
      INTEGER MAX_LEN_FNAM
      PARAMETER ( MAX_LEN_FNAM = 512 )
      INTEGER MAX_LEN_PREC
      PARAMETER ( MAX_LEN_PREC = 200 )

C     MAX_NO_THREADS  - Maximum number of threads allowed.
C     MAX_NO_PROCS    - Maximum number of processes allowed.
C     MAX_NO_BARRIERS - Maximum number of distinct thread "barriers"
      INTEGER MAX_NO_THREADS
      PARAMETER ( MAX_NO_THREADS =  4 )
      INTEGER MAX_NO_PROCS
      PARAMETER ( MAX_NO_PROCS   =  4096 )
      INTEGER MAX_NO_BARRIERS
      PARAMETER ( MAX_NO_BARRIERS = 1 )

C     Particularly weird and obscure voodoo numbers
C     lShare  - This wants to be the length in
C               [148]-byte words of the size of
C               the address "window" that is snooped
C               on an SMP bus. By separating elements in
C               the global sum buffer we can avoid generating
C               extraneous invalidate traffic between
C               processors. The length of this window is usually
C               a cache line i.e. small O(64 bytes).
C               The buffer arrays are usually short arrays
C               and are declared REAL ARRA(lShare[148],LBUFF).
C               Setting lShare[148] to 1 is like making these arrays
C               one dimensional.
      INTEGER cacheLineSize
      INTEGER lShare1
      INTEGER lShare4
      INTEGER lShare8
      PARAMETER ( cacheLineSize = 256 )
      PARAMETER ( lShare1 =  cacheLineSize )
      PARAMETER ( lShare4 =  cacheLineSize/4 )
      PARAMETER ( lShare8 =  cacheLineSize/8 )

      INTEGER MAX_VGS
      PARAMETER ( MAX_VGS = 8192 )

C     ========  EESIZE.h  ========================================


C     Symbolic values
C     precXXXX :: precision used for I/O
      INTEGER precFloat32
      PARAMETER ( precFloat32 = 32 )
      INTEGER precFloat64
      PARAMETER ( precFloat64 = 64 )

C     UNSET_xxx :: Used to indicate variables that have not been given a value
      Real*8  UNSET_FLOAT8
      PARAMETER ( UNSET_FLOAT8 = 1.234567D5 )
      Real*4  UNSET_FLOAT4
      PARAMETER ( UNSET_FLOAT4 = 1.234567E5 )
      Real*8     UNSET_RL
      PARAMETER ( UNSET_RL     = 1.234567D5 )
      Real*8     UNSET_RS
      PARAMETER ( UNSET_RS     = 1.234567D5 )
      INTEGER UNSET_I
      PARAMETER ( UNSET_I      = 123456789  )

C     debLevX  :: used to decide when to print debug messages
      INTEGER debLevZero
      PARAMETER ( debLevZero=0 )
      INTEGER debLevA
      PARAMETER ( debLevA=1 )
      INTEGER debLevB
      PARAMETER ( debLevB=2 )

C     SQUEEZE_RIGHT       - Flag indicating right blank space removal
C                           from text field.
C     SQUEEZE_LEFT        - Flag indicating left blank space removal
C                           from text field.
C     SQUEEZE_BOTH        - Flag indicating left and right blank
C                           space removal from text field.
C     PRINT_MAP_XY        - Flag indicating to plot map as XY slices
C     PRINT_MAP_XZ        - Flag indicating to plot map as XZ slices
C     PRINT_MAP_YZ        - Flag indicating to plot map as YZ slices
C     commentCharacter    - Variable used in column 1 of parameter
C                           files to indicate comments.
C     INDEX_I             - Variable used to select an index label
C     INDEX_J               for formatted input parameters.
C     INDEX_K
C     INDEX_NONE
      CHARACTER*(*) SQUEEZE_RIGHT
      PARAMETER ( SQUEEZE_RIGHT = 'R' )
      CHARACTER*(*) SQUEEZE_LEFT
      PARAMETER ( SQUEEZE_LEFT = 'L' )
      CHARACTER*(*) SQUEEZE_BOTH
      PARAMETER ( SQUEEZE_BOTH = 'B' )
      CHARACTER*(*) PRINT_MAP_XY
      PARAMETER ( PRINT_MAP_XY = 'XY' )
      CHARACTER*(*) PRINT_MAP_XZ
      PARAMETER ( PRINT_MAP_XZ = 'XZ' )
      CHARACTER*(*) PRINT_MAP_YZ
      PARAMETER ( PRINT_MAP_YZ = 'YZ' )
      CHARACTER*(*) commentCharacter
      PARAMETER ( commentCharacter = '#' )
      INTEGER INDEX_I
      INTEGER INDEX_J
      INTEGER INDEX_K
      INTEGER INDEX_NONE
      PARAMETER ( INDEX_I    = 1,
     &            INDEX_J    = 2,
     &            INDEX_K    = 3,
     &            INDEX_NONE = 4 )

C     EXCH_IGNORE_CORNERS - Flag to select ignoring or
C     EXCH_UPDATE_CORNERS   updating of corners during
C                           an edge exchange.
      INTEGER EXCH_IGNORE_CORNERS
      INTEGER EXCH_UPDATE_CORNERS
      PARAMETER ( EXCH_IGNORE_CORNERS = 0,
     &            EXCH_UPDATE_CORNERS = 1 )

C     FORWARD_SIMULATION
C     REVERSE_SIMULATION
C     TANGENT_SIMULATION
      INTEGER FORWARD_SIMULATION
      INTEGER REVERSE_SIMULATION
      INTEGER TANGENT_SIMULATION
      PARAMETER ( FORWARD_SIMULATION = 0,
     &            REVERSE_SIMULATION = 1,
     &            TANGENT_SIMULATION = 2 )

C--   COMMON /EEPARAMS_L/ Execution environment public logical variables.
C     eeBootError    :: Flags indicating error during multi-processing
C     eeEndError     :: initialisation and termination.
C     fatalError     :: Flag used to indicate that the model is ended with an error
C     useSingleCpuIO :: When useSingleCpuIO is set, MDS_WRITE_FIELD outputs from
C                       master MPI process only. -- NOTE: read from main parameter
C                       file "data" and not set until call to INI_PARMS.
C     printMapIncludesZeros  :: Flag that controls whether character constant
C                               map code ignores exact zero values.
C     useCubedSphereExchange :: use Cubed-Sphere topology domain.
C     useCoupler     :: use Coupler for a multi-components set-up.
C     useNEST_PARENT :: use Parent Nesting interface (pkg/nest_parent)
C     useNEST_CHILD  :: use Child  Nesting interface (pkg/nest_child)
C     useOASIS       :: use OASIS-coupler for a multi-components set-up.
      COMMON /EEPARAMS_L/
     &  eeBootError, fatalError, eeEndError,
     &  useSingleCpuIO, printMapIncludesZeros,
     &  useCubedSphereExchange, useCoupler,
     &  useNEST_PARENT, useNEST_CHILD, useOASIS,
     &  useSETRLSTK, useSIGREG
      LOGICAL eeBootError
      LOGICAL eeEndError
      LOGICAL fatalError
      LOGICAL useSingleCpuIO
      LOGICAL printMapIncludesZeros
      LOGICAL useCubedSphereExchange
      LOGICAL useCoupler
      LOGICAL useNEST_PARENT
      LOGICAL useNEST_CHILD
      LOGICAL useOASIS
      LOGICAL useSETRLSTK
      LOGICAL useSIGREG

C--   COMMON /EPARAMS_I/ Execution environment public integer variables.
C     errorMessageUnit    - Fortran IO unit for error messages
C     standardMessageUnit - Fortran IO unit for informational messages
C     maxLengthPrt1D :: maximum length for printing (to Std-Msg-Unit) 1-D array
C     scrUnit1      - Scratch file 1 unit number
C     scrUnit2      - Scratch file 2 unit number
C     eeDataUnit    - Unit # for reading "execution environment" parameter file.
C     modelDataUnit - Unit number for reading "model" parameter file.
C     numberOfProcs - Number of processes computing in parallel
C     pidIO         - Id of process to use for I/O.
C     myBxLo, myBxHi - Extents of domain in blocks in X and Y
C     myByLo, myByHi   that each threads is responsble for.
C     myProcId      - My own "process" id.
C     myPx     - My X coord on the proc. grid.
C     myPy     - My Y coord on the proc. grid.
C     myXGlobalLo - My bottom-left (south-west) x-index
C                   global domain. The x-coordinate of this
C                   point in for example m or degrees is *not*
C                   specified here. A model needs to provide a
C                   mechanism for deducing that information if it
C                   is needed.
C     myYGlobalLo - My bottom-left (south-west) y-index in
C                   global domain. The y-coordinate of this
C                   point in for example m or degrees is *not*
C                   specified here. A model needs to provide a
C                   mechanism for deducing that information if it
C                   is needed.
C     nThreads    - No. of threads
C     nTx         - No. of threads in X
C     nTy         - No. of threads in Y
C                   This assumes a simple cartesian
C                   gridding of the threads which is not required elsewhere
C                   but that makes it easier.
C     ioErrorCount - IO Error Counter. Set to zero initially and increased
C                    by one every time an IO error occurs.
      COMMON /EEPARAMS_I/
     &  errorMessageUnit, standardMessageUnit, maxLengthPrt1D,
     &  scrUnit1, scrUnit2, eeDataUnit, modelDataUnit,
     &  numberOfProcs, pidIO, myProcId,
     &  myPx, myPy, myXGlobalLo, myYGlobalLo, nThreads,
     &  myBxLo, myBxHi, myByLo, myByHi,
     &  nTx, nTy, ioErrorCount
      INTEGER errorMessageUnit
      INTEGER standardMessageUnit
      INTEGER maxLengthPrt1D
      INTEGER scrUnit1
      INTEGER scrUnit2
      INTEGER eeDataUnit
      INTEGER modelDataUnit
      INTEGER ioErrorCount(MAX_NO_THREADS)
      INTEGER myBxLo(MAX_NO_THREADS)
      INTEGER myBxHi(MAX_NO_THREADS)
      INTEGER myByLo(MAX_NO_THREADS)
      INTEGER myByHi(MAX_NO_THREADS)
      INTEGER myProcId
      INTEGER myPx
      INTEGER myPy
      INTEGER myXGlobalLo
      INTEGER myYGlobalLo
      INTEGER nThreads
      INTEGER nTx
      INTEGER nTy
      INTEGER numberOfProcs
      INTEGER pidIO

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
C $Header: /u/gcmpack/MITgcm/model/inc/PARAMS.h,v 1.244 2010/11/17 21:15:08 jmc Exp $
C $Name: checkpoint62o $
C

CBOP
C     !ROUTINE: PARAMS.h
C     !INTERFACE:
C     #include PARAMS.h

C     !DESCRIPTION:
C     Header file defining model "parameters".  The values from the
C     model standard input file are stored into the variables held
C     here. Notes describing the parameters can also be found here.

CEOP

C     Macros for special grid options
C $Header: /u/gcmpack/MITgcm/model/inc/PARAMS_MACROS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name: checkpoint62o $
C
CBOP
C    !ROUTINE: PARAMS_MACROS.h
C    !INTERFACE:
C    include PARAMS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | PARAMS_MACROS.h                                           
C     *==========================================================*
C     | These macros are used to substitute definitions for       
C     | PARAMS.h variables for particular configurations.         
C     | In setting these variables the following convention       
C     | applies.                                                  
C     | define phi_CONST   - Indicates the variable phi is fixed  
C     |                      in X, Y and Z.                       
C     | define phi_FX      - Indicates the variable phi only      
C     |                      varies in X (i.e.not in X or Z).     
C     | define phi_FY      - Indicates the variable phi only      
C     |                      varies in Y (i.e.not in X or Z).     
C     | define phi_FXY     - Indicates the variable phi only      
C     |                      varies in X and Y ( i.e. not Z).     
C     *==========================================================*
C     \ev
CEOP

C $Header: /u/gcmpack/MITgcm/model/inc/FCORI_MACROS.h,v 1.4 2001/09/21 15:13:31 cnh Exp $
C $Name: checkpoint62o $
C
CBOP
C    !ROUTINE: FCORI_MACROS.h
C    !INTERFACE:
C    include FCORI_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | FCORI_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP





C--   Contants
C     Useful physical values
      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844d0   )
      Real*8 deg2rad
      PARAMETER ( deg2rad = 2.d0*PI/360.d0           )

C--   COMMON /PARM_C/ Character valued parameters used by the model.
C     buoyancyRelation :: Flag used to indicate which relation to use to
C                         get buoyancy.
C     eosType         :: choose the equation of state:
C                        LINEAR, POLY3, UNESCO, JMD95Z, JMD95P, MDJWF, IDEALGAS
C     pickupSuff      :: force to start from pickup files (even if nIter0=0)
C                        and read pickup files with this suffix (max 10 Char.)
C     mdsioLocalDir   :: read-write tiled file from/to this directory name
C                        (+ 4 digits Processor-Rank) instead of current dir.
C     tRefFile      :: File containing reference Potential Temperat.  tRef (1.D)
C     sRefFile      :: File containing reference salinity/spec.humid. sRef (1.D)
C     rhoRefFile    :: File containing reference density profile rhoRef (1.D)
C     delRFile      :: File containing vertical grid spacing delR  (1.D array)
C     delRcFile     :: File containing vertical grid spacing delRc (1.D array)
C     hybSigmFile   :: File containing hybrid-sigma vertical coord. coeff. (2x 1.D)
C     delXFile      :: File containing X-spacing grid definition (1.D array)
C     delYFile      :: File containing Y-spacing grid definition (1.D array)
C     horizGridFile :: File containing horizontal-grid definition
C                        (only when using curvilinear_grid)
C     bathyFile       :: File containing bathymetry. If not defined bathymetry
C                        is taken from inline function.
C     topoFile        :: File containing the topography of the surface (unit=m)
C                        (mainly used for the atmosphere = ground height).
C     hydrogThetaFile :: File containing initial hydrographic data (3-D)
C                        for potential temperature.
C     hydrogSaltFile  :: File containing initial hydrographic data (3-D)
C                        for salinity.
C     diffKrFile      :: File containing 3D specification of vertical diffusivity
C     viscAhDfile     :: File containing 3D specification of horizontal viscosity
C     viscAhZfile     :: File containing 3D specification of horizontal viscosity
C     viscA4Dfile     :: File containing 3D specification of horizontal viscosity
C     viscA4Zfile     :: File containing 3D specification of horizontal viscosity
C     zonalWindFile   :: File containing zonal wind data
C     meridWindFile   :: File containing meridional wind data
C     thetaClimFile   :: File containing surface theta climataology used
C                       in relaxation term -lambda(theta-theta*)
C     saltClimFile    :: File containing surface salt climataology used
C                       in relaxation term -lambda(salt-salt*)
C     surfQfile       :: File containing surface heat flux, excluding SW
C                        (old version, kept for backward compatibility)
C     surfQnetFile    :: File containing surface net heat flux
C     surfQswFile     :: File containing surface shortwave radiation
C     dQdTfile        :: File containing thermal relaxation coefficient
C     EmPmRfile       :: File containing surface fresh water flux
C           NOTE: for backward compatibility EmPmRfile is specified in
C                 m/s when using external_fields_load.F.  It is converted
C                 to kg/m2/s by multiplying by rhoConstFresh.
C     saltFluxFile    :: File containing surface salt flux
C     pLoadFile       :: File containing pressure loading
C     eddyPsiXFile    :: File containing zonal Eddy streamfunction data
C     eddyPsiYFile    :: File containing meridional Eddy streamfunction data
C     the_run_name    :: string identifying the name of the model "run"
      COMMON /PARM_C/
     &                buoyancyRelation, eosType,
     &                pickupSuff, mdsioLocalDir,
     &                tRefFile, sRefFile, rhoRefFile,
     &                delRFile, delRcFile, hybSigmFile,
     &                delXFile, delYFile, horizGridFile,
     &                bathyFile, topoFile,
     &                viscAhDfile, viscAhZfile,
     &                viscA4Dfile, viscA4Zfile,
     &                hydrogThetaFile, hydrogSaltFile, diffKrFile,
     &                zonalWindFile, meridWindFile, thetaClimFile,
     &                saltClimFile,
     &                EmPmRfile, saltFluxFile,
     &                surfQfile, surfQnetFile, surfQswFile,
     &                lambdaThetaFile, lambdaSaltFile,
     &                uVelInitFile, vVelInitFile, pSurfInitFile,
     &                dQdTfile, ploadFile,
     &                eddyPsiXFile, eddyPsiYFile,
     &                the_run_name
      CHARACTER*(MAX_LEN_FNAM) buoyancyRelation
      CHARACTER*(6)  eosType
      CHARACTER*(10) pickupSuff
      CHARACTER*(MAX_LEN_FNAM) mdsioLocalDir
      CHARACTER*(MAX_LEN_FNAM) tRefFile
      CHARACTER*(MAX_LEN_FNAM) sRefFile
      CHARACTER*(MAX_LEN_FNAM) rhoRefFile
      CHARACTER*(MAX_LEN_FNAM) delRFile
      CHARACTER*(MAX_LEN_FNAM) delRcFile
      CHARACTER*(MAX_LEN_FNAM) hybSigmFile
      CHARACTER*(MAX_LEN_FNAM) delXFile
      CHARACTER*(MAX_LEN_FNAM) delYFile
      CHARACTER*(MAX_LEN_FNAM) horizGridFile
      CHARACTER*(MAX_LEN_FNAM) bathyFile, topoFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile, hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) diffKrFile
      CHARACTER*(MAX_LEN_FNAM) viscAhDfile
      CHARACTER*(MAX_LEN_FNAM) viscAhZfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Dfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Zfile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile
      CHARACTER*(MAX_LEN_FNAM) surfQfile
      CHARACTER*(MAX_LEN_FNAM) surfQnetFile
      CHARACTER*(MAX_LEN_FNAM) surfQswFile
      CHARACTER*(MAX_LEN_FNAM) EmPmRfile
      CHARACTER*(MAX_LEN_FNAM) saltFluxFile
      CHARACTER*(MAX_LEN_FNAM) uVelInitFile
      CHARACTER*(MAX_LEN_FNAM) vVelInitFile
      CHARACTER*(MAX_LEN_FNAM) pSurfInitFile
      CHARACTER*(MAX_LEN_FNAM) dQdTfile
      CHARACTER*(MAX_LEN_FNAM) ploadFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiXFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiYFile
      CHARACTER*(MAX_LEN_FNAM) lambdaThetaFile
      CHARACTER*(MAX_LEN_FNAM) lambdaSaltFile
      CHARACTER*(MAX_LEN_PREC/2) the_run_name

C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        :: Maximum number of iterations in the
C                           two-dimensional con. grad solver.
C     cg2dChkResFreq      :: Frequency with which to check residual
C                           in con. grad solver.
C     cg2dPreCondFreq     :: Frequency for updating cg2d preconditioner
C                            (non-linear free-surf.)
C     cg3dMaxIters        :: Maximum number of iterations in the
C                           three-dimensional con. grad solver.
C     cg3dChkResFreq      :: Frequency with which to check residual
C                           in con. grad solver.
C     nIter0              :: Start time-step number of for this run
C     nTimeSteps          :: Number of timesteps to execute
C     writeStatePrec      :: Precision used for writing model state.
C     writeBinaryPrec     :: Precision used for writing binary files
C     readBinaryPrec      :: Precision used for reading binary files
C     selectCoriMap       :: select setting of Coriolis parameter map:
C                           =0 f-Plane (Constant Coriolis, = f0)
C                           =1 Beta-Plane Coriolis (= f0 + beta.y)
C                           =2 Spherical Coriolis (= 2.omega.sin(phi))
C                           =3 Read Coriolis 2-d fields from files.
C     selectSigmaCoord    :: option related to sigma vertical coordinate
C     nonlinFreeSurf      :: option related to non-linear free surface
C                           =0 Linear free surface ; >0 Non-linear
C     select_rStar        :: option related to r* vertical coordinate
C                           =0 (default) use r coord. ; > 0 use r*
C     selectNHfreeSurf    :: option for Non-Hydrostatic (free-)Surface formulation:
C                           =0 (default) hydrostatic surf. ; > 0 add NH effects.
C     selectAddFluid      :: option to add mass source/sink of fluid in the interior
C                            (3-D generalisation of oceanic real-fresh water flux)
C                           =0 off ; =1 add fluid ; =-1 virtual flux (no mass added)
C     momForcingOutAB     :: =1: take momentum forcing contribution
C                           out of (=0: in) Adams-Bashforth time stepping.
C     tracForcingOutAB    :: =1: take tracer (Temp,Salt,pTracers) forcing contribution
C                           out of (=0: in) Adams-Bashforth time stepping.
C     tempAdvScheme       :: Temp. Horiz.Advection scheme selector
C     tempVertAdvScheme   :: Temp. Vert. Advection scheme selector
C     saltAdvScheme       :: Salt. Horiz.advection scheme selector
C     saltVertAdvScheme   :: Salt. Vert. Advection scheme selector
C     selectKEscheme      :: Kinetic Energy scheme selector (Vector Inv.)
C     selectVortScheme    :: Scheme selector for Vorticity term (Vector Inv.)
C     monitorSelect       :: select group of variables to monitor
C                            =1 : dynvars ; =2 : + vort ; =3 : + surface
C-    debugLevel          :: controls printing of algorithm intermediate results
C                            and statistics ; higher -> more writing

      COMMON /PARM_I/
     &        cg2dMaxIters,
     &        cg2dChkResFreq, cg2dPreCondFreq,
     &        cg3dMaxIters,
     &        cg3dChkResFreq,
     &        nIter0, nTimeSteps, nEndIter,
     &        writeStatePrec,
     &        writeBinaryPrec, readBinaryPrec,
     &        selectCoriMap,
     &        selectSigmaCoord,
     &        nonlinFreeSurf, select_rStar,
     &        selectNHfreeSurf,
     &        selectAddFluid,
     &        momForcingOutAB, tracForcingOutAB,
     &        tempAdvScheme, tempVertAdvScheme,
     &        saltAdvScheme, saltVertAdvScheme,
     &        selectKEscheme, selectVortScheme,
     &        monitorSelect, debugLevel
      INTEGER cg2dMaxIters
      INTEGER cg2dChkResFreq
      INTEGER cg2dPreCondFreq
      INTEGER cg3dMaxIters
      INTEGER cg3dChkResFreq
      INTEGER nIter0
      INTEGER nTimeSteps
      INTEGER nEndIter
      INTEGER writeStatePrec
      INTEGER writeBinaryPrec
      INTEGER readBinaryPrec
      INTEGER selectCoriMap
      INTEGER selectSigmaCoord
      INTEGER nonlinFreeSurf
      INTEGER select_rStar
      INTEGER selectNHfreeSurf
      INTEGER selectAddFluid
      INTEGER momForcingOutAB, tracForcingOutAB
      INTEGER tempAdvScheme, tempVertAdvScheme
      INTEGER saltAdvScheme, saltVertAdvScheme
      INTEGER selectKEscheme
      INTEGER selectVortScheme
      INTEGER monitorSelect
      INTEGER debugLevel

C--   COMMON /PARM_L/ Logical valued parameters used by the model.
C- Coordinate + Grid params:
C     fluidIsAir       :: Set to indicate that the fluid major constituent
C                        is air
C     fluidIsWater     :: Set to indicate that the fluid major constituent
C                        is water
C     usingPCoords     :: Set to indicate that we are working in a pressure
C                        type coordinate (p or p*).
C     usingZCoords     :: Set to indicate that we are working in a height
C                        type coordinate (z or z*)
C     useDynP_inEos_Zc :: use the dynamical pressure in EOS (with Z-coord.)
C                         this requires specific code for restart & exchange
C     usingCartesianGrid :: If TRUE grid generation will be in a cartesian
C                          coordinate frame.
C     usingSphericalPolarGrid :: If TRUE grid generation will be in a
C                               spherical polar frame.
C     rotateGrid      :: rotate grid coordinates to geographical coordinates
C                        according to Euler angles phiEuler, thetaEuler, psiEuler
C     usingCurvilinearGrid :: If TRUE, use a curvilinear grid (to be provided)
C     usingCylindricalGrid :: If TRUE grid generation will be Cylindrical
C     deepAtmosphere :: deep model (drop the shallow-atmosphere approximation)
C     setInterFDr    :: set Interface depth (put cell-Center at the middle)
C     setCenterDr    :: set cell-Center depth (put Interface at the middle)
C- Momentum params:
C     no_slip_sides  :: Impose "no-slip" at lateral boundaries.
C     no_slip_bottom :: Impose "no-slip" at bottom boundary.
C     useFullLeith   :: Set to true to use full Leith viscosity(may be unstable
C                       on irregular grids)
C     useStrainTensionVisc:: Set to true to use Strain-Tension viscous terms
C     useAreaViscLength :: Set to true to use old scaling for viscous lengths,
C                          e.g., L2=Raz.  May be preferable for cube sphere.
C     momViscosity  :: Flag which turns momentum friction terms on and off.
C     momAdvection  :: Flag which turns advection of momentum on and off.
C     momForcing    :: Flag which turns external forcing of momentum on
C                      and off.
C     momPressureForcing :: Flag which turns pressure term in momentum equation
C                          on and off.
C     metricTerms   :: Flag which turns metric terms on or off.
C     useNHMTerms   :: If TRUE use non-hydrostatic metric terms.
C     useCoriolis   :: Flag which turns the coriolis terms on and off.
C     use3dCoriolis :: Turns the 3-D coriolis terms (in Omega.cos Phi) on - off
C     useCDscheme   :: use CD-scheme to calculate Coriolis terms.
C     vectorInvariantMomentum :: use Vector-Invariant form (mom_vecinv package)
C                                (default = F = use mom_fluxform package)
C     useJamartWetPoints :: Use wet-point method for Coriolis (Jamart & Ozer 1986)
C     useJamartMomAdv :: Use wet-point method for V.I. non-linear term
C     upwindVorticity :: bias interpolation of vorticity in the Coriolis term
C     highOrderVorticity :: use 3rd/4th order interp. of vorticity (V.I., advection)
C     useAbsVorticity :: work with f+zeta in Coriolis terms
C     upwindShear        :: use 1rst order upwind interp. (V.I., vertical advection)
C     momStepping    :: Turns momentum equation time-stepping off
C- Temp. & Salt params:
C     tempStepping   :: Turns temperature equation time-stepping off
C     saltStepping   :: Turns salinity equation time-stepping off
C     tempAdvection  :: Flag which turns advection of temperature on and off.
C     tempIsActiveTr :: Pot.Temp. is a dynamically active tracer
C     tempForcing    :: Flag which turns external forcing of temperature on
C                       and off.
C     saltAdvection  :: Flag which turns advection of salinity on and off.
C     saltIsActiveTr :: Salinity  is a dynamically active tracer
C     saltForcing    :: Flag which turns external forcing of salinity on
C                       and off.
C     maskIniTemp    :: apply mask to initial Pot.Temp.
C     maskIniSalt    :: apply mask to initial salinity
C     checkIniTemp   :: check for points with identically zero initial Pot.Temp.
C     checkIniSalt   :: check for points with identically zero initial salinity
C     useRealFreshWaterFlux :: if True (=Natural BCS), treats P+R-E flux
C                         as a real Fresh Water (=> changes the Sea Level)
C                         if F, converts P+R-E to salt flux (no SL effect)
C- Pressure solver related parameters (PARM02)
C     useSRCGSolver  :: Set to true to use conjugate gradient
C                       solver with single reduction (only one call of
C                       s/r mpi_allreduce), default is false
C- Time-stepping params:
C     rigidLid            :: Set to true to use rigid lid
C     implicitFreeSurface :: Set to true to use implicit free surface
C     exactConserv        :: Set to true to conserve exactly the total Volume
C     linFSConserveTr     :: Set to true to correct source/sink of tracer
C                            at the surface due to Linear Free Surface
C     uniformLin_PhiSurf  :: Set to true to use a uniform Bo_surf in the
C                            linear relation Phi_surf = Bo_surf*eta
C     quasiHydrostatic :: Using non-hydrostatic terms in hydrostatic algorithm
C     nonHydrostatic   :: Using non-hydrostatic algorithm
C     use3Dsolver      :: set to true to use 3-D pressure solver
C     implicitIntGravWave :: treat Internal Gravity Wave implicitly
C     staggerTimeStep   :: enable a Stagger time stepping U,V (& W) then T,S
C     implicitDiffusion :: Turns implicit vertical diffusion on
C     implicitViscosity :: Turns implicit vertical viscosity on
C     tempImplVertAdv :: Turns on implicit vertical advection for Temperature
C     saltImplVertAdv :: Turns on implicit vertical advection for Salinity
C     momImplVertAdv  :: Turns on implicit vertical advection for Momentum
C     multiDimAdvection :: Flag that enable multi-dimension advection
C     useMultiDimAdvec  :: True if multi-dim advection is used at least once
C     momDissip_In_AB   :: if False, put Dissipation tendency contribution
C                          out off Adams-Bashforth time stepping.
C     doAB_onGtGs       :: if the Adams-Bashforth time stepping is used, always
C                          apply AB on tracer tendencies (rather than on Tracer)
C- Other forcing params -
C     balanceEmPmR    :: substract global mean of EmPmR at every time step
C     balanceQnet     :: substract global mean of Qnet at every time step
C     balancePrintMean:: print substracted global means to STDOUT
C     doThetaClimRelax :: Set true if relaxation to temperature
C                        climatology is required.
C     doSaltClimRelax  :: Set true if relaxation to salinity
C                        climatology is required.
C     allowFreezing  :: Allows surface water to freeze and form ice
C     useOldFreezing :: use the old version (before checkpoint52a_pre, 2003-11-12)
C     periodicExternalForcing :: Set true if forcing is time-dependant
C- I/O parameters -
C     globalFiles    :: Selects between "global" and "tiled" files.
C                       On some platforms with MPI, option globalFiles is either
C                       slow or does not work. Use useSingleCpuIO instead.
C     useSingleCpuIO :: moved to EEPARAMS.h
C     pickupStrictlyMatch :: check and stop if pickup-file do not stricly match
C     startFromPickupAB2 :: with AB-3 code, start from an AB-2 pickup
C     usePickupBeforeC54 :: start from old-pickup files, generated with code from
C                           before checkpoint-54a, Jul 06, 2004.
C     pickup_write_mdsio :: use mdsio to write pickups
C     pickup_read_mdsio  :: use mdsio to read  pickups
C     pickup_write_immed :: echo the pickup immediately (for conversion)
C     writePickupAtEnd   :: write pickup at the last timestep
C     timeave_mdsio      :: use mdsio for timeave output
C     snapshot_mdsio     :: use mdsio for "snapshot" (dumpfreq/diagfreq) output
C     monitor_stdio      :: use stdio for monitor output
C     dumpInitAndLast :: dumps model state to files at Initial (nIter0)
C                        & Last iteration, in addition multiple of dumpFreq iter.
C     debugMode       :: controls printing of debug msg (sequence of S/R calls).

      COMMON /PARM_L/
     & fluidIsAir, fluidIsWater,
     & usingPCoords, usingZCoords, useDynP_inEos_Zc,
     & usingCartesianGrid, usingSphericalPolarGrid, rotateGrid,
     & usingCurvilinearGrid, usingCylindricalGrid,
     & deepAtmosphere, setInterFDr, setCenterDr,
     & no_slip_sides, no_slip_bottom,
     & useFullLeith, useStrainTensionVisc, useAreaViscLength,
     & momViscosity, momAdvection, momForcing,
     & momPressureForcing, metricTerms, useNHMTerms,
     & useCoriolis, use3dCoriolis,
     & useCDscheme, vectorInvariantMomentum,
     & useEnergyConservingCoriolis, useJamartWetPoints, useJamartMomAdv,
     & upwindVorticity, highOrderVorticity,
     & useAbsVorticity, upwindShear,
     & momStepping, tempStepping, saltStepping,
     & tempAdvection, tempIsActiveTr, tempForcing,
     & saltAdvection, saltIsActiveTr, saltForcing,
     & maskIniTemp, maskIniSalt, checkIniTemp, checkIniSalt,
     & useRealFreshWaterFlux,
     & rigidLid, implicitFreeSurface, exactConserv, linFSConserveTr,
     & uniformLin_PhiSurf,
     & quasiHydrostatic, nonHydrostatic,
     & use3Dsolver, implicitIntGravWave, staggerTimeStep,
     & implicitDiffusion, implicitViscosity,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & multiDimAdvection, useMultiDimAdvec,
     & momDissip_In_AB, doAB_onGtGs,
     & balanceEmPmR, balanceQnet, balancePrintMean,
     & doThetaClimRelax, doSaltClimRelax,
     & allowFreezing, useOldFreezing,
     & periodicExternalForcing,
     & globalFiles,
     & pickupStrictlyMatch, usePickupBeforeC54, startFromPickupAB2,
     & pickup_read_mdsio, pickup_write_mdsio, pickup_write_immed,
     & writePickupAtEnd,
     & timeave_mdsio, snapshot_mdsio, monitor_stdio,
     & outputTypesInclusive, dumpInitAndLast, debugMode,
     & inAdMode, inAdTrue, inAdFalse, inAdExact,
     & useSRCGSolver

      LOGICAL fluidIsAir
      LOGICAL fluidIsWater
      LOGICAL usingPCoords
      LOGICAL usingZCoords
      LOGICAL useDynP_inEos_Zc
      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid, rotateGrid
      LOGICAL usingCylindricalGrid
      LOGICAL usingCurvilinearGrid
      LOGICAL deepAtmosphere
      LOGICAL setInterFDr
      LOGICAL setCenterDr
      LOGICAL useNHMTerms
      LOGICAL no_slip_sides
      LOGICAL no_slip_bottom
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL momPressureForcing
      LOGICAL useCoriolis
      LOGICAL vectorInvariantMomentum
      LOGICAL tempAdvection
      LOGICAL tempIsActiveTr
      LOGICAL tempForcing
      LOGICAL saltAdvection
      LOGICAL saltIsActiveTr
      LOGICAL saltForcing
      LOGICAL maskIniTemp
      LOGICAL maskIniSalt
      LOGICAL checkIniTemp
      LOGICAL checkIniSalt
      LOGICAL useRealFreshWaterFlux
      LOGICAL useFullLeith
      LOGICAL useStrainTensionVisc
      LOGICAL useAreaViscLength
      LOGICAL rigidLid
      LOGICAL implicitFreeSurface
      LOGICAL exactConserv
      LOGICAL linFSConserveTr
      LOGICAL uniformLin_PhiSurf
      LOGICAL quasiHydrostatic
      LOGICAL nonHydrostatic
      LOGICAL use3Dsolver
      LOGICAL implicitIntGravWave
      LOGICAL staggerTimeStep
      LOGICAL momStepping
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL metricTerms
      LOGICAL use3dCoriolis
      LOGICAL useCDscheme
      LOGICAL useEnergyConservingCoriolis
      LOGICAL useJamartWetPoints
      LOGICAL useJamartMomAdv
      LOGICAL upwindVorticity
      LOGICAL highOrderVorticity
      LOGICAL useAbsVorticity
      LOGICAL upwindShear
      LOGICAL implicitDiffusion
      LOGICAL implicitViscosity
      LOGICAL tempImplVertAdv
      LOGICAL saltImplVertAdv
      LOGICAL momImplVertAdv
      LOGICAL multiDimAdvection
      LOGICAL useMultiDimAdvec
      LOGICAL momDissip_In_AB
      LOGICAL doAB_onGtGs
      LOGICAL balanceEmPmR
      LOGICAL balanceQnet
      LOGICAL balancePrintMean
      LOGICAL doThetaClimRelax
      LOGICAL doSaltClimRelax
      LOGICAL allowFreezing
      LOGICAL useOldFreezing
      LOGICAL periodicExternalForcing
      LOGICAL globalFiles
      LOGICAL pickupStrictlyMatch
      LOGICAL usePickupBeforeC54
      LOGICAL startFromPickupAB2
      LOGICAL pickup_read_mdsio, pickup_write_mdsio
      LOGICAL pickup_write_immed, writePickupAtEnd
      LOGICAL timeave_mdsio, snapshot_mdsio, monitor_stdio
      LOGICAL outputTypesInclusive
      LOGICAL dumpInitAndLast
      LOGICAL debugMode
      LOGICAL inAdMode, inAdTrue, inAdFalse, inAdExact
      LOGICAL useSRCGSolver

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     cg2dTargetResidual
C          :: Target residual for cg2d solver; no unit (RHS normalisation)
C     cg2dTargetResWunit
C          :: Target residual for cg2d solver; W unit (No RHS normalisation)
C     cg3dTargetResidual
C               :: Target residual for cg3d solver.
C     cg2dpcOffDFac :: Averaging weight for preconditioner off-diagonal.
C     Note. 20th May 1998
C           I made a weird discovery! In the model paper we argue
C           for the form of the preconditioner used here ( see
C           A Finite-volume, Incompressible Navier-Stokes Model
C           ...., Marshall et. al ). The algebra gives a simple
C           0.5 factor for the averaging of ac and aCw to get a
C           symmettric pre-conditioner. By using a factor of 0.51
C           i.e. scaling the off-diagonal terms in the
C           preconditioner down slightly I managed to get the
C           number of iterations for convergence in a test case to
C           drop form 192 -> 134! Need to investigate this further!
C           For now I have introduced a parameter cg2dpcOffDFac which
C           defaults to 0.51 but can be set at runtime.
C     delR      :: Vertical grid spacing ( units of r ).
C     delRc     :: Vertical grid spacing between cell centers (r unit).
C     delX      :: Separation between cell faces (m) or (deg), depending
C     delY        on input flags.
C     gravity   :: Accel. due to gravity ( m/s^2 )
C     recip_gravity and its inverse
C     gBaro     :: Accel. due to gravity used in barotropic equation ( m/s^2 )
C     rhoNil    :: Reference density for the linear equation of state
C     rhoConst  :: Vertically constant reference density
C     rhoFacC   :: normalized (by rhoConst) reference density at cell-Center
C     rhoFacF   :: normalized (by rhoConst) reference density at cell-interFace
C     rhoConstFresh :: Constant reference density for fresh water (rain)
C     rho1Ref   :: reference vertical profile for density
C     tRef      :: reference vertical profile for potential temperature
C     sRef      :: reference vertical profile for salinity/specific humidity
C     phiRef    :: reference potential (pressure/rho, geopotential) profile
C     dBdrRef   :: vertical gradient of reference boyancy  [(m/s/r)^2)]:
C               :: z-coord: = N^2_ref = Brunt-Vaissala frequency [s^-2]
C               :: p-coord: = -(d.alpha/dp)_ref          [(m^2.s/kg)^2]
C     rVel2wUnit :: units conversion factor (Non-Hydrostatic code),
C                :: from r-coordinate vertical velocity to vertical velocity [m/s].
C                :: z-coord: = 1 ; p-coord: wSpeed [m/s] = rVel [Pa/s] * rVel2wUnit
C     wUnit2rVel :: units conversion factor (Non-Hydrostatic code),
C                :: from vertical velocity [m/s] to r-coordinate vertical velocity.
C                :: z-coord: = 1 ; p-coord: rVel [Pa/s] = wSpeed [m/s] * wUnit2rVel
C     mass2rUnit :: units conversion factor (surface forcing),
C                :: from mass per unit area [kg/m2] to vertical r-coordinate unit.
C                :: z-coord: = 1/rhoConst ( [kg/m2] / rho = [m] ) ;
C                :: p-coord: = gravity    ( [kg/m2] *  g = [Pa] ) ;
C     rUnit2mass :: units conversion factor (surface forcing),
C                :: from vertical r-coordinate unit to mass per unit area [kg/m2].
C                :: z-coord: = rhoConst  ( [m] * rho = [kg/m2] ) ;
C                :: p-coord: = 1/gravity ( [Pa] /  g = [kg/m2] ) ;
C     xgOrigin   :: Origin of the X-axis (Cartesian Grid) / Longitude of Western
C                :: most cell face (Lat-Lon grid) (Note: this is an "inert"
C                :: parameter but it makes geographical references simple.)
C     ygOrigin   :: Origin of the Y-axis (Cartesian Grid) / Latitude of Southern
C                :: most face (Lat-Lon grid).
C     rSphere    :: Radius of sphere for a spherical polar grid ( m ).
C     recip_rSphere  :: Reciprocal radius of sphere ( m ).
C     f0         :: Reference coriolis parameter ( 1/s )
C                   ( Southern edge f for beta plane )
C     beta       :: df/dy ( s^-1.m^-1 )
C     fPrime     :: Second Coriolis parameter ( 1/s ), related to Y-component
C                   of rotation (reference value = 2.Omega.Cos(Phi))
C     omega      :: Angular velocity ( rad/s )
C     rotationPeriod :: Rotation period (s) (= 2.pi/omega)
C     viscArNr   :: vertical profile of Eddy viscosity coeff.
C                   for vertical mixing of momentum ( units of r^2/s )
C     viscAh     :: Eddy viscosity coeff. for mixing of
C                   momentum laterally ( m^2/s )
C     viscAhW    :: Eddy viscosity coeff. for mixing of vertical
C                   momentum laterally, no effect for hydrostatic
C                   model, defaults to viscAh if unset ( m^2/s )
C                   Not used if variable horiz. viscosity is used.
C     viscA4     :: Biharmonic viscosity coeff. for mixing of
C                   momentum laterally ( m^4/s )
C     viscA4W    :: Biharmonic viscosity coeff. for mixing of vertical
C                   momentum laterally, no effect for hydrostatic
C                   model, defaults to viscA4 if unset ( m^2/s )
C                   Not used if variable horiz. viscosity is used.
C     viscAhD    :: Eddy viscosity coeff. for mixing of momentum laterally
C                   (act on Divergence part) ( m^2/s )
C     viscAhZ    :: Eddy viscosity coeff. for mixing of momentum laterally
C                   (act on Vorticity  part) ( m^2/s )
C     viscA4D    :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                   (act on Divergence part) ( m^4/s )
C     viscA4Z    :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                   (act on Vorticity  part) ( m^4/s )
C     viscC2leith  :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC2leithD :: Modified Leith non-dimensional visc. factor (grad(div))
C     viscC4leith  :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC4leithD :: Modified Leith non-dimensional viscosity factor (grad(div))
C     viscC2smag   :: Smagorinsky non-dimensional viscosity factor (harmonic)
C     viscC4smag   :: Smagorinsky non-dimensional viscosity factor (biharmonic)
C     viscAhMax    :: Maximum eddy viscosity coeff. for mixing of
C                    momentum laterally ( m^2/s )
C     viscAhReMax  :: Maximum gridscale Reynolds number for eddy viscosity
C                     coeff. for mixing of momentum laterally (non-dim)
C     viscAhGrid   :: non-dimensional grid-size dependent viscosity
C     viscAhGridMax:: maximum and minimum harmonic viscosity coefficients ...
C     viscAhGridMin::  in terms of non-dimensional grid-size dependent visc.
C     viscA4Max    :: Maximum biharmonic viscosity coeff. for mixing of
C                     momentum laterally ( m^4/s )
C     viscA4ReMax  :: Maximum Gridscale Reynolds number for
C                     biharmonic viscosity coeff. momentum laterally (non-dim)
C     viscA4Grid   :: non-dimensional grid-size dependent bi-harmonic viscosity
C     viscA4GridMax:: maximum and minimum biharmonic viscosity coefficients ...
C     viscA4GridMin::  in terms of non-dimensional grid-size dependent viscosity
C     diffKhT   :: Laplacian diffusion coeff. for mixing of
C                 heat laterally ( m^2/s )
C     diffKrNrT :: vertical profile of Laplacian diffusion coeff.
C                 for mixing of heat vertically ( units of r^2/s )
C     diffK4T   :: Biharmonic diffusion coeff. for mixing of
C                 heat laterally ( m^4/s )
C     diffKhS  ::  Laplacian diffusion coeff. for mixing of
C                 salt laterally ( m^2/s )
C     diffKrNrS :: vertical profile of Laplacian diffusion coeff.
C                 for mixing of salt vertically ( units of r^2/s ),
C     diffK4S   :: Biharmonic diffusion coeff. for mixing of
C                 salt laterally ( m^4/s )
C     diffKrBL79surf :: T/S surface diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79deep :: T/S deep diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79scl  :: depth scale for arctan fn (m) Bryan and Lewis, 1979
C     diffKrBL79Ho   :: depth offset for arctan fn (m) Bryan and Lewis, 1979
C     BL79LatVary    :: polarwise of this latitude diffKrBL79 is applied with
C                       gradual transition to diffKrBLEQ towards Equator
C     diffKrBLEQsurf :: same as diffKrBL79surf but at Equator
C     diffKrBLEQdeep :: same as diffKrBL79deep but at Equator
C     diffKrBLEQscl  :: same as diffKrBL79scl but at Equator
C     diffKrBLEQHo   :: same as diffKrBL79Ho but at Equator
C     deltaT    :: Default timestep ( s )
C     deltaTClock  :: Timestep used as model "clock". This determines the
C                    IO frequencies and is used in tagging output. It can
C                    be totally different to the dynamical time. Typically
C                    it will be the deep-water timestep for accelerated runs.
C                    Frequency of checkpointing and dumping of the model state
C                    are referenced to this clock. ( s )
C     deltaTMom    :: Timestep for momemtum equations ( s )
C     dTtracerLev  :: Timestep for tracer equations ( s ), function of level k
C     deltaTfreesurf :: Timestep for free-surface equation ( s )
C     freesurfFac  :: Parameter to turn implicit free surface term on or off
C                    freesurfac = 1. uses implicit free surface
C                    freesurfac = 0. uses rigid lid
C     abEps        :: Adams-Bashforth-2 stabilizing weight
C     alph_AB      :: Adams-Bashforth-3 primary factor
C     beta_AB      :: Adams-Bashforth-3 secondary factor
C     implicSurfPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Surface Pressure Gradient ( 0-1 )
C     implicDiv2Dflow :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of barotropic flow Divergence ( 0-1 )
C     implicitNHPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Non-Hydrostatic Pressure Gradient ( 0-1 )
C     hFacMin      :: Minimum fraction size of a cell (affects hFacC etc...)
C     hFacMinDz    :: Minimum dimensional size of a cell (affects hFacC etc..., m)
C     hFacMinDp    :: Minimum dimensional size of a cell (affects hFacC etc..., Pa)
C     hFacMinDr    :: Minimum dimensional size of a cell (-> hFacC etc..., r units)
C     hFacInf      :: Threshold (inf and sup) for fraction size of surface cell
C     hFacSup          that control vanishing and creating levels
C     tauCD         :: CD scheme coupling timescale ( s )
C     rCD           :: CD scheme normalised coupling parameter (= 1 - deltaT/tauCD)
C     epsAB_CD      :: Adams-Bashforth-2 stabilizing weight used in CD scheme
C     baseTime      :: model base time (time origin) = time @ iteration zero
C     startTime     :: Starting time for this integration ( s ).
C     endTime       :: Ending time for this integration ( s ).
C     chkPtFreq     :: Frequency of rolling check pointing ( s ).
C     pChkPtFreq    :: Frequency of permanent check pointing ( s ).
C     dumpFreq      :: Frequency with which model state is written to
C                     post-processing files ( s ).
C     diagFreq      :: Frequency with which model writes diagnostic output
C                     of intermediate quantities.
C     afFacMom      :: Advection of momentum term tracer parameter
C     vfFacMom      :: Momentum viscosity tracer parameter
C     pfFacMom      :: Momentum pressure forcing tracer parameter
C     cfFacMom      :: Coriolis term tracer parameter
C     foFacMom      :: Momentum forcing tracer parameter
C     mtFacMom      :: Metric terms tracer parameter
C     cosPower      :: Power of cosine of latitude to multiply viscosity
C     cAdjFreq      :: Frequency of convective adjustment
C
C     taveFreq      :: Frequency with which time-averaged model state
C                      is written to post-processing files ( s ).
C     tave_lastIter :: (for state variable only) fraction of the last time
C                      step (of each taveFreq period) put in the time average.
C                      (fraction for 1rst iter = 1 - tave_lastIter)
C     tauThetaClimRelax :: Relaxation to climatology time scale ( s ).
C     tauSaltClimRelax :: Relaxation to climatology time scale ( s ).
C     latBandClimRelax :: latitude band where Relaxation to Clim. is applied,
C                         i.e. where |yC| <= latBandClimRelax
C     externForcingPeriod :: Is the period of which forcing varies (eg. 1 month)
C     externForcingCycle :: Is the repeat time of the forcing (eg. 1 year)
C                          (note: externForcingCycle must be an integer
C                           number times externForcingPeriod)
C     convertFW2Salt :: salinity, used to convert Fresh-Water Flux to Salt Flux
C                       (use model surface (local) value if set to -1)
C     temp_EvPrRn :: temperature of Rain & Evap.
C     salt_EvPrRn :: salinity of Rain & Evap.
C     temp_addMass :: temperature of addMass array
C     salt_addMass :: salinity of addMass array
C        (notes: a) tracer content of Rain/Evap only used if both
C                     NonLin_FrSurf & useRealFreshWater are set.
C                b) use model surface (local) value if set to UNSET_RL)
C     hMixCriteria:: criteria for mixed-layer diagnostic
C     dRhoSmall   :: parameter for mixed-layer diagnostic
C     hMixSmooth  :: Smoothing parameter for mixed-layer diag (default=0=no smoothing)
C     ivdc_kappa  :: implicit vertical diffusivity for convection [m^2/s]
C     Ro_SeaLevel :: standard position of Sea-Level in "R" coordinate, used as
C                    starting value (k=1) for vertical coordinate (rf(1)=Ro_SeaLevel)
C     rSigmaBnd   :: vertical position (in r-unit) of r/sigma transition (Hybrid-Sigma)
C     sideDragFactor     :: side-drag scaling factor (used only if no_slip_sides)
C                           (default=2: full drag ; =1: gives half-slip BC)
C     bottomDragLinear    :: Linear    bottom-drag coefficient (units of [r]/s)
C     bottomDragQuadratic :: Quadratic bottom-drag coefficient (units of [r]/m)
C               (if using zcoordinate, units becomes linear: m/s, quadratic: [-])
C     smoothAbsFuncRange :: 1/2 of interval around zero, for which FORTRAN ABS
C                           is to be replace by a smoother function
C                           (affects myabs, mymin, mymax)
C     nh_Am2        :: scales the non-hydrostatic terms and changes internal scales
C                      (i.e. allows convection at different Rayleigh numbers)
C     phiEuler      :: Euler angle, rotation about original z-axis
C     thetaEuler    :: Euler angle, rotation about new x-axis
C     psiEuler      :: Euler angle, rotation about new z-axis
      COMMON /PARM_R/ cg2dTargetResidual, cg2dTargetResWunit,
     & cg2dpcOffDFac, cg3dTargetResidual,
     & delR, delRc, delX, delY,
     & deltaT, deltaTmom, dTtracerLev, deltaTfreesurf, deltaTClock,
     & abEps, alph_AB, beta_AB,
     & xgOrigin, ygOrigin, rSphere, recip_rSphere,
     & f0, beta, fPrime, omega, rotationPeriod,
     & viscFacAdj, viscAh, viscAhW, viscAhMax,
     & viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC2leithD,
     & viscC2smag, viscC4smag,
     & viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W, viscA4Max,
     & viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscAhReMax, viscA4ReMax,
     & viscC4leith, viscC4leithD, viscArNr,
     & diffKhT, diffK4T, diffKrNrT,
     & diffKhS, diffK4S, diffKrNrS,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & BL79LatVary,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & tauCD, rCD, epsAB_CD,
     & freeSurfFac, implicSurfPress, implicDiv2Dflow, implicitNHPress,
     & hFacMin, hFacMinDz, hFacInf, hFacSup,
     & gravity, recip_gravity, gBaro,
     & rhonil, recip_rhonil, rhoConst, recip_rhoConst,
     & rhoFacC, recip_rhoFacC, rhoFacF, recip_rhoFacF,
     & rhoConstFresh, rho1Ref, tRef, sRef, phiRef, dBdrRef,
     & rVel2wUnit, wUnit2rVel, mass2rUnit, rUnit2mass,
     & baseTime, startTime, endTime,
     & chkPtFreq, pChkPtFreq, dumpFreq, adjDumpFreq,
     & diagFreq, taveFreq, tave_lastIter, monitorFreq, adjMonitorFreq,
     & afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,
     & cosPower, cAdjFreq,
     & tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,
     & externForcingCycle, externForcingPeriod,
     & convertFW2Salt, temp_EvPrRn, salt_EvPrRn,
     & temp_addMass, salt_addMass, hFacMinDr, hFacMinDp,
     & ivdc_kappa, hMixCriteria, dRhoSmall, hMixSmooth,
     & Ro_SeaLevel, rSigmaBnd,
     & sideDragFactor, bottomDragLinear, bottomDragQuadratic, nh_Am2,
     & smoothAbsFuncRange,
     & tCylIn, tCylOut,
     & phiEuler, thetaEuler, psiEuler

      Real*8 cg2dTargetResidual
      Real*8 cg2dTargetResWunit
      Real*8 cg3dTargetResidual
      Real*8 cg2dpcOffDFac
      Real*8 delR(Nr)
      Real*8 delRc(Nr+1)
      Real*8 delX(Nx)
      Real*8 delY(Ny)
      Real*8 deltaT
      Real*8 deltaTClock
      Real*8 deltaTmom
      Real*8 dTtracerLev(Nr)
      Real*8 deltaTfreesurf
      Real*8 abEps, alph_AB, beta_AB
      Real*8 xgOrigin
      Real*8 ygOrigin
      Real*8 rSphere
      Real*8 recip_rSphere
      Real*8 f0
      Real*8 beta
      Real*8 fPrime
      Real*8 omega
      Real*8 rotationPeriod
      Real*8 freeSurfFac
      Real*8 implicSurfPress
      Real*8 implicDiv2Dflow
      Real*8 implicitNHPress
      Real*8 hFacMin
      Real*8 hFacMinDz
      Real*8 hFacMinDp
      Real*8 hFacMinDr
      Real*8 hFacInf
      Real*8 hFacSup
      Real*8 viscArNr(Nr)
      Real*8 viscFacAdj
      Real*8 viscAh
      Real*8 viscAhW
      Real*8 viscAhD
      Real*8 viscAhZ
      Real*8 viscAhMax
      Real*8 viscAhReMax
      Real*8 viscAhGrid, viscAhGridMax, viscAhGridMin
      Real*8 viscC2leith
      Real*8 viscC2leithD
      Real*8 viscC2smag
      Real*8 viscA4
      Real*8 viscA4W
      Real*8 viscA4D
      Real*8 viscA4Z
      Real*8 viscA4Max
      Real*8 viscA4ReMax
      Real*8 viscA4Grid, viscA4GridMax, viscA4GridMin
      Real*8 viscC4leith
      Real*8 viscC4leithD
      Real*8 viscC4smag
      Real*8 diffKhT
      Real*8 diffKrNrT(Nr)
      Real*8 diffK4T
      Real*8 diffKhS
      Real*8 diffKrNrS(Nr)
      Real*8 diffK4S
      Real*8 diffKrBL79surf
      Real*8 diffKrBL79deep
      Real*8 diffKrBL79scl
      Real*8 diffKrBL79Ho
      Real*8 BL79LatVary
      Real*8 diffKrBLEQsurf
      Real*8 diffKrBLEQdeep
      Real*8 diffKrBLEQscl
      Real*8 diffKrBLEQHo
      Real*8 tauCD, rCD, epsAB_CD
      Real*8 gravity
      Real*8 recip_gravity
      Real*8 gBaro
      Real*8 rhonil,        recip_rhonil
      Real*8 rhoConst,      recip_rhoConst
      Real*8 rhoFacC(Nr),   recip_rhoFacC(Nr)
      Real*8 rhoFacF(Nr+1), recip_rhoFacF(Nr+1)
      Real*8 rhoConstFresh
      Real*8 rho1Ref(Nr)
      Real*8 tRef(Nr)
      Real*8 sRef(Nr)
      Real*8 phiRef(2*Nr+1)
      Real*8 dBdrRef(Nr)
      Real*8 rVel2wUnit(Nr+1), wUnit2rVel(Nr+1)
      Real*8 mass2rUnit, rUnit2mass
      Real*8 baseTime
      Real*8 startTime
      Real*8 endTime
      Real*8 chkPtFreq
      Real*8 pChkPtFreq
      Real*8 dumpFreq
      Real*8 adjDumpFreq
      Real*8 diagFreq
      Real*8 taveFreq
      Real*8 tave_lastIter
      Real*8 monitorFreq
      Real*8 adjMonitorFreq
      Real*8 afFacMom
      Real*8 vfFacMom
      Real*8 pfFacMom
      Real*8 cfFacMom
      Real*8 foFacMom
      Real*8 mtFacMom
      Real*8 cosPower
      Real*8 cAdjFreq
      Real*8 tauThetaClimRelax
      Real*8 tauSaltClimRelax
      Real*8 latBandClimRelax
      Real*8 externForcingCycle
      Real*8 externForcingPeriod
      Real*8 convertFW2Salt
      Real*8 temp_EvPrRn
      Real*8 salt_EvPrRn
      Real*8 temp_addMass
      Real*8 salt_addMass
      Real*8 ivdc_kappa
      Real*8 hMixCriteria
      Real*8 dRhoSmall
      Real*8 hMixSmooth
      Real*8 Ro_SeaLevel
      Real*8 rSigmaBnd
      Real*8 sideDragFactor
      Real*8 bottomDragLinear
      Real*8 bottomDragQuadratic
      Real*8 smoothAbsFuncRange
      Real*8 nh_Am2
      Real*8 tCylIn
      Real*8 tCylOut
      Real*8 phiEuler, thetaEuler, psiEuler

C--   COMMON /PARM_A/ Thermodynamics constants ?
      COMMON /PARM_A/ HeatCapacity_Cp,recip_Cp
      Real*8 HeatCapacity_Cp
      Real*8 recip_Cp

C--   COMMON /PARM_ATM/ Atmospheric physical parameters (Ideal Gas EOS, ...)
C     celsius2K :: convert centigrade (Celsius) degree to Kelvin
C     atm_Po    :: standard reference pressure
C     atm_Cp    :: specific heat (Cp) of the (dry) air at constant pressure
C     atm_Rd    :: gas constant for dry air
C     atm_kappa :: kappa = R/Cp (R: constant of Ideal Gas EOS)
C     atm_Rq    :: water vapour specific volume anomaly relative to dry air
C                  (e.g. typical value = (29/18 -1) 10^-3 with q [g/kg])
C     integr_GeoPot :: option to select the way we integrate the geopotential
C                     (still a subject of discussions ...)
C     selectFindRoSurf :: select the way surf. ref. pressure (=Ro_surf) is
C             derived from the orography. Implemented: 0,1 (see INI_P_GROUND)
      COMMON /PARM_ATM/
     &            celsius2K,
     &            atm_Cp, atm_Rd, atm_kappa, atm_Rq, atm_Po,
     &            integr_GeoPot, selectFindRoSurf
      Real*8 celsius2K
      Real*8 atm_Po, atm_Cp, atm_Rd, atm_kappa, atm_Rq
      INTEGER integr_GeoPot, selectFindRoSurf

C Logical flags for selecting packages
      LOGICAL useGAD
      LOGICAL useOBCS
      LOGICAL useSHAP_FILT
      LOGICAL useZONAL_FILT
      LOGICAL useOPPS
      LOGICAL usePP81
      LOGICAL useMY82
      LOGICAL useGGL90
      LOGICAL useKPP
      LOGICAL useGMRedi
      LOGICAL useDOWN_SLOPE
      LOGICAL useCAL
      LOGICAL useEXF
      LOGICAL useBulkForce
      LOGICAL useEBM
      LOGICAL useCheapAML
      LOGICAL useGrdchk
      LOGICAL useSMOOTH
      LOGICAL useECCO
      LOGICAL useSBO
      LOGICAL useFLT
      LOGICAL usePTRACERS
      LOGICAL useGCHEM
      LOGICAL useRBCS
      LOGICAL useOffLine
      LOGICAL useMATRIX
      LOGICAL useSEAICE
      LOGICAL useSALT_PLUME
      LOGICAL useShelfIce
      LOGICAL useICEFRONT
      LOGICAL useThSIce
      LOGICAL useATM2d
      LOGICAL useAIM
      LOGICAL useLand
      LOGICAL useFizhi
      LOGICAL useGridAlt
      LOGICAL useDiagnostics
      LOGICAL useREGRID
      LOGICAL useLayers
      LOGICAL useMNC
      LOGICAL useRunClock
      LOGICAL useEMBED_FILES
      LOGICAL useMYPACKAGE
      COMMON /PARM_PACKAGES/
     &        useGAD, useOBCS, useSHAP_FILT, useZONAL_FILT,
     &        useOPPS, usePP81, useMY82, useGGL90, useKPP,
     &        useGMRedi, useDOWN_SLOPE,
     &        useCAL, useEXF, useBulkForce, useEBM, useCheapAML,
     &        useGrdchk, useSMOOTH, useECCO, useSBO, useFLT,
     &        usePTRACERS, useGCHEM, useRBCS, useOffLine, useMATRIX,
     &        useSEAICE, useSALT_PLUME, useShelfIce,
     &        useICEFRONT, useThSIce,
     &        useATM2D, useAIM, useLand, useFizhi, useGridAlt,
     &        useDiagnostics, useREGRID, useLayers, useMNC,
     &        useRunClock, useEMBED_FILES,
     &        useMYPACKAGE
CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
C $Header: /u/gcmpack/MITgcm/eesupp/inc/EESUPPORT.h,v 1.10 2009/04/21 16:00:53 jmc Exp $
C $Name: checkpoint62o $
CBOP
C     !ROUTINE: EESUPPORT.h
C     !INTERFACE:
C     include "EESUPPORT.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EESUPPORT.h                                              |
C     *==========================================================*
C     | Support data structures for the MITgcm UV ``execution    |
C     | environment'' code. This data should be private to the   |
C     | execution environment routines. Data which needs to be   |
C     | accessed directly by a numerical model goes in           |
C     | EEPARAMS.h.                                              |
C     *==========================================================*
CEOP

C     ERROR_HEADER        - String which prefixes error messages
      CHARACTER*(*) ERROR_HEADER
      PARAMETER ( ERROR_HEADER = ' *** ERROR ***' )
C     PROCESS_HEADER      - String which prefixes processor number
      CHARACTER*(*) PROCESS_HEADER
      PARAMETER ( PROCESS_HEADER = 'PID.TID' )

C     MAX_NUM_COMM_MODES - Maximum number of communication modes
C     COMM_NONE       - No edge communication
C     COMM_MSG        - Use messages to communicate edges
C     COMM_PUT        - Use put to communicate edges
C     COMM_GET        - Use get to communicate edges
C     Note - commName holds an identifying name for each communication
C            mode. The COMM_ parameters are used to index commName
C            so the COMM_ parameters need to be in the range
C            1 : MAX_NUM_COMM_MODES.
      INTEGER MAX_NUM_COMM_MODES
      PARAMETER ( MAX_NUM_COMM_MODES = 4 )
      INTEGER COMM_NONE
      PARAMETER ( COMM_NONE   =   1 )
      INTEGER COMM_MSG
      PARAMETER ( COMM_MSG    =   2 )
      INTEGER COMM_PUT
      PARAMETER ( COMM_PUT    =   3 )
      INTEGER COMM_GET
      PARAMETER ( COMM_GET    =   4 )
      COMMON /EESUPP_COMMNAME/ commName
      CHARACTER*10 commName(MAX_NUM_COMM_MODES)

C     Tile identifiers
C     Tiles have a number that is unique over the global domain.
C     A tile that is not there has its number set to NULL_TILE
      INTEGER NULL_TILE
      PARAMETER ( NULL_TILE = -1 )


C--   COMMON /EESUPP_C/ Execution environment support character variables
C     myProcessStr - String identifying my process number
      COMMON /EESUPP_C/ myProcessStr
      CHARACTER*128 myProcessStr

C--   COMMON /EESUPP_L/ Execution environment support logical variables
C     initMPError - Flag indicating error during multi-processing
C                   initialisation.
C     finMPError  - Flag indicating error during multi-processing
C                   termination.
C     ThError     - Thread detected an error.
C     usingMPI    - Flag controlling use of MPI routines. This flag
C                   allows either MPI or threads to be used in a
C                   shared memory environment which can be a useful
C                   debugging/performance analysis tool.
C     usingSyncMessages - Flag that causes blocking communication to be used
C                         if possible. When false non-blocking EXCH routines
C                         will be used if possible.
C     notUsingXPeriodicity - Flag indicating no X/Y boundary wrap around
C     notUsingYPeriodicity   This affects the communication routines but
C                            is generally ignored in the numerical model
C                            code.
C     threadIsRunning, threadIsComplete - Flags used to check for correct behaviour
C                                         of multi-threaded code.
C                                         threadIsRunning is used to check that the
C                                         threads we need are running. This catches the
C                                         situation where a program eedata file has nTthreads
C                                         greater than the setenv PARALLEL or NCPUS variable.
C                                         threadIsComplete is used to flag that a thread has
C                                         reached the end of the model. This is used as a check to
C                                         trap problems that might occur if one thread "escapes"
C                                         the main.F master loop. This should not happen
C                                         if the multi-threading compilation tools works right.
C                                         But (see for example KAP) this is not always the case!
      COMMON /EESUPP_L/ thError, threadIsRunning, threadIsComplete,
     & allMyEdgesAreSharedMemory, usingMPI, usingSyncMessages,
     & notUsingXPeriodicity, notUsingYPeriodicity
      LOGICAL thError(MAX_NO_THREADS)
      LOGICAL threadIsRunning(MAX_NO_THREADS)
      LOGICAL threadIsComplete(MAX_NO_THREADS)
      LOGICAL allMyEdgesAreSharedMemory(MAX_NO_THREADS)
      LOGICAL usingMPI
      LOGICAL usingSyncMessages
      LOGICAL notUsingXPeriodicity
      LOGICAL notUsingYPeriodicity

C--   COMMON /EESUPP_I/ Parallel support integer globals
C     pidW   -  Process  ID of neighbor to West
C     pidE   -           ditto             East
C     pidN   -           ditto             North
C     pidS   -           ditto             South
C              Note: pid[XY] is not necessairily the UNIX
C                    process id - it is just an identifying
C                    number.
C     myPid  - My own process id
C     nProcs - Number of processes
C     westCommunicationMode  - Mode of communication for each tile face
C     eastCommunicationMode
C     northCommunicationMode
C     southCommunicationMode
C     bi0   - Low cartesian tile index for this process
C     bj0     Note - In a tile distribution with holes bi0 and bj0
C                    are not useful. Neighboring tile indices must
C                    be derived some other way.
C     tileNo       - Tile identification number for my tile and
C     tileNo[WENS]   my N,S,E,W neighbor tiles.
C     tilePid[WENS] - Process identification number for
C                     my N,S,E,W neighbor tiles.
C     nTx, nTy    - No. threads in X and Y. This assumes a simple
C                   cartesian gridding of the threads which is not
C                   required elsewhere but that makes it easier.
      COMMON /EESUPP_I/
     & myPid, nProcs, pidW, pidE, pidN, pidS,
     & tileCommModeW,  tileCommModeE,
     & tileCommModeN,  tileCommModeS,
     & tileNo, tileNoW, tileNoE, tileNoS, tileNoN,
     &  tilePidW, tilePidE, tilePidS, tilePidN,
     &  tileBiW, tileBiE, tileBiS, tileBiN,
     & tileBjW, tileBjE, tileBjS, tileBjN,
     & tileTagSendW, tileTagSendE, tileTagSendS, tileTagSendN,
     & tileTagRecvW, tileTagRecvE, tileTagRecvS, tileTagRecvN
      INTEGER myPid
      INTEGER nProcs
      INTEGER pidW
      INTEGER pidE
      INTEGER pidN
      INTEGER pidS
      INTEGER tileCommModeW ( nSx, nSy )
      INTEGER tileCommModeE ( nSx, nSy )
      INTEGER tileCommModeN ( nSx, nSy )
      INTEGER tileCommModeS ( nSx, nSy )
      INTEGER tileNo( nSx, nSy )
      INTEGER tileNoW( nSx, nSy )
      INTEGER tileNoE( nSx, nSy )
      INTEGER tileNoN( nSx, nSy )
      INTEGER tileNoS( nSx, nSy )
      INTEGER tilePidW( nSx, nSy )
      INTEGER tilePidE( nSx, nSy )
      INTEGER tilePidN( nSx, nSy )
      INTEGER tilePidS( nSx, nSy )
      INTEGER tileBiW( nSx, nSy )
      INTEGER tileBiE( nSx, nSy )
      INTEGER tileBiN( nSx, nSy )
      INTEGER tileBiS( nSx, nSy )
      INTEGER tileBjW( nSx, nSy )
      INTEGER tileBjE( nSx, nSy )
      INTEGER tileBjN( nSx, nSy )
      INTEGER tileBjS( nSx, nSy )
      INTEGER tileTagSendW( nSx, nSy )
      INTEGER tileTagSendE( nSx, nSy )
      INTEGER tileTagSendN( nSx, nSy )
      INTEGER tileTagSendS( nSx, nSy )
      INTEGER tileTagRecvW( nSx, nSy )
      INTEGER tileTagRecvE( nSx, nSy )
      INTEGER tileTagRecvN( nSx, nSy )
      INTEGER tileTagRecvS( nSx, nSy )

C--   Include MPI standard Fortran header file
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
!      
!      (C) 2001 by Argonne National Laboratory.
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
!      DO NOT EDIT
!      This file created by buildiface 
!      
       INTEGER MPI_SOURCE, MPI_TAG, MPI_ERROR
       PARAMETER (MPI_SOURCE=3,MPI_TAG=4,MPI_ERROR=5)
       INTEGER MPI_STATUS_SIZE
       PARAMETER (MPI_STATUS_SIZE=5)
       INTEGER MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
       INTEGER MPI_STATUSES_IGNORE(MPI_STATUS_SIZE,1)
       INTEGER MPI_ERRCODES_IGNORE(1)
       CHARACTER*1 MPI_ARGVS_NULL(1,1)
       CHARACTER*1 MPI_ARGV_NULL(1)
       INTEGER MPI_SUCCESS
       PARAMETER (MPI_SUCCESS=0)
       INTEGER MPI_ERR_OTHER
       PARAMETER (MPI_ERR_OTHER=15)
       INTEGER MPI_ERR_COUNT
       PARAMETER (MPI_ERR_COUNT=2)
       INTEGER MPI_ERR_SPAWN
       PARAMETER (MPI_ERR_SPAWN=42)
       INTEGER MPI_ERR_LOCKTYPE
       PARAMETER (MPI_ERR_LOCKTYPE=47)
       INTEGER MPI_ERR_OP
       PARAMETER (MPI_ERR_OP=9)
       INTEGER MPI_ERR_DUP_DATAREP
       PARAMETER (MPI_ERR_DUP_DATAREP=24)
       INTEGER MPI_ERR_UNSUPPORTED_DATAREP
       PARAMETER (MPI_ERR_UNSUPPORTED_DATAREP=43)
       INTEGER MPI_ERR_TRUNCATE
       PARAMETER (MPI_ERR_TRUNCATE=14)
       INTEGER MPI_ERR_INFO_NOKEY
       PARAMETER (MPI_ERR_INFO_NOKEY=31)
       INTEGER MPI_ERR_ASSERT
       PARAMETER (MPI_ERR_ASSERT=53)
       INTEGER MPI_ERR_FILE_EXISTS
       PARAMETER (MPI_ERR_FILE_EXISTS=25)
       INTEGER MPI_ERR_PENDING
       PARAMETER (MPI_ERR_PENDING=18)
       INTEGER MPI_ERR_COMM
       PARAMETER (MPI_ERR_COMM=5)
       INTEGER MPI_ERR_KEYVAL
       PARAMETER (MPI_ERR_KEYVAL=48)
       INTEGER MPI_ERR_NAME
       PARAMETER (MPI_ERR_NAME=33)
       INTEGER MPI_ERR_REQUEST
       PARAMETER (MPI_ERR_REQUEST=19)
       INTEGER MPI_ERR_TYPE
       PARAMETER (MPI_ERR_TYPE=3)
       INTEGER MPI_ERR_INFO_VALUE
       PARAMETER (MPI_ERR_INFO_VALUE=30)
       INTEGER MPI_ERR_RMA_SYNC
       PARAMETER (MPI_ERR_RMA_SYNC=50)
       INTEGER MPI_ERR_NO_MEM
       PARAMETER (MPI_ERR_NO_MEM=34)
       INTEGER MPI_ERR_BAD_FILE
       PARAMETER (MPI_ERR_BAD_FILE=22)
       INTEGER MPI_ERR_QUOTA
       PARAMETER (MPI_ERR_QUOTA=39)
       INTEGER MPI_ERR_ROOT
       PARAMETER (MPI_ERR_ROOT=7)
       INTEGER MPI_ERR_SERVICE
       PARAMETER (MPI_ERR_SERVICE=41)
       INTEGER MPI_ERR_IO
       PARAMETER (MPI_ERR_IO=32)
       INTEGER MPI_ERR_RMA_FLAVOR
       PARAMETER (MPI_ERR_RMA_FLAVOR=58)
       INTEGER MPI_ERR_ACCESS
       PARAMETER (MPI_ERR_ACCESS=20)
       INTEGER MPI_ERR_NO_SPACE
       PARAMETER (MPI_ERR_NO_SPACE=36)
       INTEGER MPI_ERR_CONVERSION
       PARAMETER (MPI_ERR_CONVERSION=23)
       INTEGER MPI_ERR_WIN
       PARAMETER (MPI_ERR_WIN=45)
       INTEGER MPI_ERR_FILE
       PARAMETER (MPI_ERR_FILE=27)
       INTEGER MPI_ERR_RMA_SHARED
       PARAMETER (MPI_ERR_RMA_SHARED=57)
       INTEGER MPI_ERR_BASE
       PARAMETER (MPI_ERR_BASE=46)
       INTEGER MPI_ERR_RMA_CONFLICT
       PARAMETER (MPI_ERR_RMA_CONFLICT=49)
       INTEGER MPI_ERR_IN_STATUS
       PARAMETER (MPI_ERR_IN_STATUS=17)
       INTEGER MPI_ERR_INFO_KEY
       PARAMETER (MPI_ERR_INFO_KEY=29)
       INTEGER MPI_ERR_ARG
       PARAMETER (MPI_ERR_ARG=12)
       INTEGER MPI_ERR_READ_ONLY
       PARAMETER (MPI_ERR_READ_ONLY=40)
       INTEGER MPI_ERR_SIZE
       PARAMETER (MPI_ERR_SIZE=51)
       INTEGER MPI_ERR_BUFFER
       PARAMETER (MPI_ERR_BUFFER=1)
       INTEGER MPI_ERR_LASTCODE
       PARAMETER (MPI_ERR_LASTCODE=1073741823)
       INTEGER MPI_ERR_DISP
       PARAMETER (MPI_ERR_DISP=52)
       INTEGER MPI_ERR_PORT
       PARAMETER (MPI_ERR_PORT=38)
       INTEGER MPI_ERR_GROUP
       PARAMETER (MPI_ERR_GROUP=8)
       INTEGER MPI_ERR_TOPOLOGY
       PARAMETER (MPI_ERR_TOPOLOGY=10)
       INTEGER MPI_ERR_TAG
       PARAMETER (MPI_ERR_TAG=4)
       INTEGER MPI_ERR_NOT_SAME
       PARAMETER (MPI_ERR_NOT_SAME=35)
       INTEGER MPI_ERR_INFO
       PARAMETER (MPI_ERR_INFO=28)
       INTEGER MPI_ERR_UNKNOWN
       PARAMETER (MPI_ERR_UNKNOWN=13)
       INTEGER MPI_ERR_FILE_IN_USE
       PARAMETER (MPI_ERR_FILE_IN_USE=26)
       INTEGER MPI_ERR_RMA_ATTACH
       PARAMETER (MPI_ERR_RMA_ATTACH=56)
       INTEGER MPI_ERR_UNSUPPORTED_OPERATION
       PARAMETER (MPI_ERR_UNSUPPORTED_OPERATION=44)
       INTEGER MPI_ERR_AMODE
       PARAMETER (MPI_ERR_AMODE=21)
       INTEGER MPI_ERR_RANK
       PARAMETER (MPI_ERR_RANK=6)
       INTEGER MPI_ERR_DIMS
       PARAMETER (MPI_ERR_DIMS=11)
       INTEGER MPI_ERR_NO_SUCH_FILE
       PARAMETER (MPI_ERR_NO_SUCH_FILE=37)
       INTEGER MPI_ERR_RMA_RANGE
       PARAMETER (MPI_ERR_RMA_RANGE=55)
       INTEGER MPI_ERR_INTERN
       PARAMETER (MPI_ERR_INTERN=16)
       INTEGER MPI_ERRORS_ARE_FATAL
       PARAMETER (MPI_ERRORS_ARE_FATAL=1409286144)
       INTEGER MPI_ERRORS_RETURN
       PARAMETER (MPI_ERRORS_RETURN=1409286145)
       INTEGER MPI_IDENT
       PARAMETER (MPI_IDENT=0)
       INTEGER MPI_CONGRUENT
       PARAMETER (MPI_CONGRUENT=1)
       INTEGER MPI_SIMILAR
       PARAMETER (MPI_SIMILAR=2)
       INTEGER MPI_UNEQUAL
       PARAMETER (MPI_UNEQUAL=3)
       INTEGER MPI_WIN_FLAVOR_CREATE
       PARAMETER (MPI_WIN_FLAVOR_CREATE=1)
       INTEGER MPI_WIN_FLAVOR_ALLOCATE
       PARAMETER (MPI_WIN_FLAVOR_ALLOCATE=2)
       INTEGER MPI_WIN_FLAVOR_DYNAMIC
       PARAMETER (MPI_WIN_FLAVOR_DYNAMIC=3)
       INTEGER MPI_WIN_FLAVOR_SHARED
       PARAMETER (MPI_WIN_FLAVOR_SHARED=4)
       INTEGER MPI_WIN_SEPARATE
       PARAMETER (MPI_WIN_SEPARATE=1)
       INTEGER MPI_WIN_UNIFIED
       PARAMETER (MPI_WIN_UNIFIED=2)
       INTEGER MPI_MAX
       PARAMETER (MPI_MAX=1476395009)
       INTEGER MPI_MIN
       PARAMETER (MPI_MIN=1476395010)
       INTEGER MPI_SUM
       PARAMETER (MPI_SUM=1476395011)
       INTEGER MPI_PROD
       PARAMETER (MPI_PROD=1476395012)
       INTEGER MPI_LAND
       PARAMETER (MPI_LAND=1476395013)
       INTEGER MPI_BAND
       PARAMETER (MPI_BAND=1476395014)
       INTEGER MPI_LOR
       PARAMETER (MPI_LOR=1476395015)
       INTEGER MPI_BOR
       PARAMETER (MPI_BOR=1476395016)
       INTEGER MPI_LXOR
       PARAMETER (MPI_LXOR=1476395017)
       INTEGER MPI_BXOR
       PARAMETER (MPI_BXOR=1476395018)
       INTEGER MPI_MINLOC
       PARAMETER (MPI_MINLOC=1476395019)
       INTEGER MPI_MAXLOC
       PARAMETER (MPI_MAXLOC=1476395020)
       INTEGER MPI_REPLACE
       PARAMETER (MPI_REPLACE=1476395021)
       INTEGER MPI_NO_OP
       PARAMETER (MPI_NO_OP=1476395022)
       INTEGER MPI_COMM_WORLD
       PARAMETER (MPI_COMM_WORLD=1140850688)
       INTEGER MPI_COMM_SELF
       PARAMETER (MPI_COMM_SELF=1140850689)
       INTEGER MPI_GROUP_EMPTY
       PARAMETER (MPI_GROUP_EMPTY=1207959552)
       INTEGER MPI_COMM_NULL
       PARAMETER (MPI_COMM_NULL=67108864)
       INTEGER MPI_WIN_NULL
       PARAMETER (MPI_WIN_NULL=536870912)
       INTEGER MPI_FILE_NULL
       PARAMETER (MPI_FILE_NULL=0)
       INTEGER MPI_GROUP_NULL
       PARAMETER (MPI_GROUP_NULL=134217728)
       INTEGER MPI_OP_NULL
       PARAMETER (MPI_OP_NULL=402653184)
       INTEGER MPI_DATATYPE_NULL
       PARAMETER (MPI_DATATYPE_NULL=201326592)
       INTEGER MPI_REQUEST_NULL
       PARAMETER (MPI_REQUEST_NULL=738197504)
       INTEGER MPI_ERRHANDLER_NULL
       PARAMETER (MPI_ERRHANDLER_NULL=335544320)
       INTEGER MPI_INFO_NULL
       PARAMETER (MPI_INFO_NULL=469762048)
       INTEGER MPI_INFO_ENV
       PARAMETER (MPI_INFO_ENV=1543503873)
       INTEGER MPI_TAG_UB
       PARAMETER (MPI_TAG_UB=1681915906)
       INTEGER MPI_HOST
       PARAMETER (MPI_HOST=1681915908)
       INTEGER MPI_IO
       PARAMETER (MPI_IO=1681915910)
       INTEGER MPI_WTIME_IS_GLOBAL
       PARAMETER (MPI_WTIME_IS_GLOBAL=1681915912)
       INTEGER MPI_UNIVERSE_SIZE
       PARAMETER (MPI_UNIVERSE_SIZE=1681915914)
       INTEGER MPI_LASTUSEDCODE
       PARAMETER (MPI_LASTUSEDCODE=1681915916)
       INTEGER MPI_APPNUM
       PARAMETER (MPI_APPNUM=1681915918)
       INTEGER MPI_WIN_BASE
       PARAMETER (MPI_WIN_BASE=1711276034)
       INTEGER MPI_WIN_SIZE
       PARAMETER (MPI_WIN_SIZE=1711276036)
       INTEGER MPI_WIN_DISP_UNIT
       PARAMETER (MPI_WIN_DISP_UNIT=1711276038)
       INTEGER MPI_WIN_CREATE_FLAVOR
       PARAMETER (MPI_WIN_CREATE_FLAVOR=1711276040)
       INTEGER MPI_WIN_MODEL
       PARAMETER (MPI_WIN_MODEL=1711276042)
       INTEGER MPI_MAX_ERROR_STRING
       PARAMETER (MPI_MAX_ERROR_STRING=512-1)
       INTEGER MPI_MAX_PORT_NAME
       PARAMETER (MPI_MAX_PORT_NAME=255)
       INTEGER MPI_MAX_OBJECT_NAME
       PARAMETER (MPI_MAX_OBJECT_NAME=127)
       INTEGER MPI_MAX_INFO_KEY
       PARAMETER (MPI_MAX_INFO_KEY=254)
       INTEGER MPI_MAX_INFO_VAL
       PARAMETER (MPI_MAX_INFO_VAL=1023)
       INTEGER MPI_MAX_PROCESSOR_NAME
       PARAMETER (MPI_MAX_PROCESSOR_NAME=128-1)
       INTEGER MPI_MAX_DATAREP_STRING
       PARAMETER (MPI_MAX_DATAREP_STRING=127)
       INTEGER MPI_MAX_LIBRARY_VERSION_STRING
       PARAMETER (MPI_MAX_LIBRARY_VERSION_STRING=8192-1)
       INTEGER MPI_UNDEFINED
       PARAMETER (MPI_UNDEFINED=(-32766))
       INTEGER MPI_KEYVAL_INVALID
       PARAMETER (MPI_KEYVAL_INVALID=603979776)
       INTEGER MPI_BSEND_OVERHEAD
       PARAMETER (MPI_BSEND_OVERHEAD=95)
       INTEGER MPI_PROC_NULL
       PARAMETER (MPI_PROC_NULL=-1)
       INTEGER MPI_ANY_SOURCE
       PARAMETER (MPI_ANY_SOURCE=-2)
       INTEGER MPI_ANY_TAG
       PARAMETER (MPI_ANY_TAG=-1)
       INTEGER MPI_ROOT
       PARAMETER (MPI_ROOT=-3)
       INTEGER MPI_GRAPH
       PARAMETER (MPI_GRAPH=1)
       INTEGER MPI_CART
       PARAMETER (MPI_CART=2)
       INTEGER MPI_DIST_GRAPH
       PARAMETER (MPI_DIST_GRAPH=3)
       INTEGER MPI_VERSION
       PARAMETER (MPI_VERSION=3)
       INTEGER MPI_SUBVERSION
       PARAMETER (MPI_SUBVERSION=0)
       INTEGER MPI_LOCK_EXCLUSIVE
       PARAMETER (MPI_LOCK_EXCLUSIVE=234)
       INTEGER MPI_LOCK_SHARED
       PARAMETER (MPI_LOCK_SHARED=235)
       INTEGER MPI_COMPLEX
       PARAMETER (MPI_COMPLEX=1275070494)
       INTEGER MPI_DOUBLE_COMPLEX
       PARAMETER (MPI_DOUBLE_COMPLEX=1275072546)
       INTEGER MPI_LOGICAL
       PARAMETER (MPI_LOGICAL=1275069469)
       INTEGER MPI_REAL
       PARAMETER (MPI_REAL=1275069468)
       INTEGER MPI_DOUBLE_PRECISION
       PARAMETER (MPI_DOUBLE_PRECISION=1275070495)
       INTEGER MPI_INTEGER
       PARAMETER (MPI_INTEGER=1275069467)
       INTEGER MPI_2INTEGER
       PARAMETER (MPI_2INTEGER=1275070496)
       INTEGER MPI_2DOUBLE_PRECISION
       PARAMETER (MPI_2DOUBLE_PRECISION=1275072547)
       INTEGER MPI_2REAL
       PARAMETER (MPI_2REAL=1275070497)
       INTEGER MPI_CHARACTER
       PARAMETER (MPI_CHARACTER=1275068698)
       INTEGER MPI_BYTE
       PARAMETER (MPI_BYTE=1275068685)
       INTEGER MPI_UB
       PARAMETER (MPI_UB=1275068433)
       INTEGER MPI_LB
       PARAMETER (MPI_LB=1275068432)
       INTEGER MPI_PACKED
       PARAMETER (MPI_PACKED=1275068687)
       INTEGER MPI_INTEGER1
       PARAMETER (MPI_INTEGER1=1275068717)
       INTEGER MPI_INTEGER2
       PARAMETER (MPI_INTEGER2=1275068975)
       INTEGER MPI_INTEGER4
       PARAMETER (MPI_INTEGER4=1275069488)
       INTEGER MPI_INTEGER8
       PARAMETER (MPI_INTEGER8=1275070513)
       INTEGER MPI_INTEGER16
       PARAMETER (MPI_INTEGER16=MPI_DATATYPE_NULL)
       INTEGER MPI_REAL4
       PARAMETER (MPI_REAL4=1275069479)
       INTEGER MPI_REAL8
       PARAMETER (MPI_REAL8=1275070505)
       INTEGER MPI_REAL16
       PARAMETER (MPI_REAL16=1275072555)
       INTEGER MPI_COMPLEX8
       PARAMETER (MPI_COMPLEX8=1275070504)
       INTEGER MPI_COMPLEX16
       PARAMETER (MPI_COMPLEX16=1275072554)
       INTEGER MPI_COMPLEX32
       PARAMETER (MPI_COMPLEX32=1275076652)
       INTEGER MPI_ADDRESS_KIND
       PARAMETER (MPI_ADDRESS_KIND=8)
       INTEGER MPI_OFFSET_KIND
       PARAMETER (MPI_OFFSET_KIND=8)
       INTEGER MPI_COUNT_KIND
       PARAMETER (MPI_COUNT_KIND=8)
       INTEGER MPI_INTEGER_KIND
       PARAMETER (MPI_INTEGER_KIND=4)
       INTEGER MPI_CHAR
       PARAMETER (MPI_CHAR=1275068673)
       INTEGER MPI_SIGNED_CHAR
       PARAMETER (MPI_SIGNED_CHAR=1275068696)
       INTEGER MPI_UNSIGNED_CHAR
       PARAMETER (MPI_UNSIGNED_CHAR=1275068674)
       INTEGER MPI_WCHAR
       PARAMETER (MPI_WCHAR=1275069454)
       INTEGER MPI_SHORT
       PARAMETER (MPI_SHORT=1275068931)
       INTEGER MPI_UNSIGNED_SHORT
       PARAMETER (MPI_UNSIGNED_SHORT=1275068932)
       INTEGER MPI_INT
       PARAMETER (MPI_INT=1275069445)
       INTEGER MPI_UNSIGNED
       PARAMETER (MPI_UNSIGNED=1275069446)
       INTEGER MPI_LONG
       PARAMETER (MPI_LONG=1275070471)
       INTEGER MPI_UNSIGNED_LONG
       PARAMETER (MPI_UNSIGNED_LONG=1275070472)
       INTEGER MPI_FLOAT
       PARAMETER (MPI_FLOAT=1275069450)
       INTEGER MPI_DOUBLE
       PARAMETER (MPI_DOUBLE=1275070475)
       INTEGER MPI_LONG_DOUBLE
       PARAMETER (MPI_LONG_DOUBLE=1275072524)
       INTEGER MPI_LONG_LONG_INT
       PARAMETER (MPI_LONG_LONG_INT=1275070473)
       INTEGER MPI_UNSIGNED_LONG_LONG
       PARAMETER (MPI_UNSIGNED_LONG_LONG=1275070489)
       INTEGER MPI_LONG_LONG
       PARAMETER (MPI_LONG_LONG=1275070473)
       INTEGER MPI_FLOAT_INT
       PARAMETER (MPI_FLOAT_INT=-1946157056)
       INTEGER MPI_DOUBLE_INT
       PARAMETER (MPI_DOUBLE_INT=-1946157055)
       INTEGER MPI_LONG_INT
       PARAMETER (MPI_LONG_INT=-1946157054)
       INTEGER MPI_SHORT_INT
       PARAMETER (MPI_SHORT_INT=-1946157053)
       INTEGER MPI_2INT
       PARAMETER (MPI_2INT=1275070486)
       INTEGER MPI_LONG_DOUBLE_INT
       PARAMETER (MPI_LONG_DOUBLE_INT=-1946157052)
       INTEGER MPI_INT8_T
       PARAMETER (MPI_INT8_T=1275068727)
       INTEGER MPI_INT16_T
       PARAMETER (MPI_INT16_T=1275068984)
       INTEGER MPI_INT32_T
       PARAMETER (MPI_INT32_T=1275069497)
       INTEGER MPI_INT64_T
       PARAMETER (MPI_INT64_T=1275070522)
       INTEGER MPI_UINT8_T
       PARAMETER (MPI_UINT8_T=1275068731)
       INTEGER MPI_UINT16_T
       PARAMETER (MPI_UINT16_T=1275068988)
       INTEGER MPI_UINT32_T
       PARAMETER (MPI_UINT32_T=1275069501)
       INTEGER MPI_UINT64_T
       PARAMETER (MPI_UINT64_T=1275070526)
       INTEGER MPI_C_BOOL
       PARAMETER (MPI_C_BOOL=1275068735)
       INTEGER MPI_C_FLOAT_COMPLEX
       PARAMETER (MPI_C_FLOAT_COMPLEX=1275070528)
       INTEGER MPI_C_COMPLEX
       PARAMETER (MPI_C_COMPLEX=1275070528)
       INTEGER MPI_C_DOUBLE_COMPLEX
       PARAMETER (MPI_C_DOUBLE_COMPLEX=1275072577)
       INTEGER MPI_C_LONG_DOUBLE_COMPLEX
       PARAMETER (MPI_C_LONG_DOUBLE_COMPLEX=1275076674)
       INTEGER MPI_AINT
       PARAMETER (MPI_AINT=1275070531)
       INTEGER MPI_OFFSET
       PARAMETER (MPI_OFFSET=1275070532)
       INTEGER MPI_COUNT
       PARAMETER (MPI_COUNT=1275070533)
       INTEGER MPI_CXX_BOOL
       PARAMETER (MPI_CXX_BOOL=1275068723)
       INTEGER MPI_CXX_FLOAT_COMPLEX
       PARAMETER (MPI_CXX_FLOAT_COMPLEX=1275070516)
       INTEGER MPI_CXX_DOUBLE_COMPLEX
       PARAMETER (MPI_CXX_DOUBLE_COMPLEX=1275072565)
       INTEGER MPI_CXX_LONG_DOUBLE_COMPLEX
       PARAMETER (MPI_CXX_LONG_DOUBLE_COMPLEX=1275076662)
       INTEGER MPI_COMBINER_NAMED
       PARAMETER (MPI_COMBINER_NAMED=1)
       INTEGER MPI_COMBINER_DUP
       PARAMETER (MPI_COMBINER_DUP=2)
       INTEGER MPI_COMBINER_CONTIGUOUS
       PARAMETER (MPI_COMBINER_CONTIGUOUS=3)
       INTEGER MPI_COMBINER_VECTOR
       PARAMETER (MPI_COMBINER_VECTOR=4)
       INTEGER MPI_COMBINER_HVECTOR_INTEGER
       PARAMETER (MPI_COMBINER_HVECTOR_INTEGER=5)
       INTEGER MPI_COMBINER_HVECTOR
       PARAMETER (MPI_COMBINER_HVECTOR=6)
       INTEGER MPI_COMBINER_INDEXED
       PARAMETER (MPI_COMBINER_INDEXED=7)
       INTEGER MPI_COMBINER_HINDEXED_INTEGER
       PARAMETER (MPI_COMBINER_HINDEXED_INTEGER=8)
       INTEGER MPI_COMBINER_HINDEXED
       PARAMETER (MPI_COMBINER_HINDEXED=9)
       INTEGER MPI_COMBINER_INDEXED_BLOCK
       PARAMETER (MPI_COMBINER_INDEXED_BLOCK=10)
       INTEGER MPI_COMBINER_STRUCT_INTEGER
       PARAMETER (MPI_COMBINER_STRUCT_INTEGER=11)
       INTEGER MPI_COMBINER_STRUCT
       PARAMETER (MPI_COMBINER_STRUCT=12)
       INTEGER MPI_COMBINER_SUBARRAY
       PARAMETER (MPI_COMBINER_SUBARRAY=13)
       INTEGER MPI_COMBINER_DARRAY
       PARAMETER (MPI_COMBINER_DARRAY=14)
       INTEGER MPI_COMBINER_F90_REAL
       PARAMETER (MPI_COMBINER_F90_REAL=15)
       INTEGER MPI_COMBINER_F90_COMPLEX
       PARAMETER (MPI_COMBINER_F90_COMPLEX=16)
       INTEGER MPI_COMBINER_F90_INTEGER
       PARAMETER (MPI_COMBINER_F90_INTEGER=17)
       INTEGER MPI_COMBINER_RESIZED
       PARAMETER (MPI_COMBINER_RESIZED=18)
       INTEGER MPI_COMBINER_HINDEXED_BLOCK
       PARAMETER (MPI_COMBINER_HINDEXED_BLOCK=19)
       INTEGER MPI_TYPECLASS_REAL
       PARAMETER (MPI_TYPECLASS_REAL=1)
       INTEGER MPI_TYPECLASS_INTEGER
       PARAMETER (MPI_TYPECLASS_INTEGER=2)
       INTEGER MPI_TYPECLASS_COMPLEX
       PARAMETER (MPI_TYPECLASS_COMPLEX=3)
       INTEGER MPI_MODE_NOCHECK
       PARAMETER (MPI_MODE_NOCHECK=1024)
       INTEGER MPI_MODE_NOSTORE
       PARAMETER (MPI_MODE_NOSTORE=2048)
       INTEGER MPI_MODE_NOPUT
       PARAMETER (MPI_MODE_NOPUT=4096)
       INTEGER MPI_MODE_NOPRECEDE
       PARAMETER (MPI_MODE_NOPRECEDE=8192)
       INTEGER MPI_MODE_NOSUCCEED
       PARAMETER (MPI_MODE_NOSUCCEED=16384)
       INTEGER MPI_COMM_TYPE_SHARED
       PARAMETER (MPI_COMM_TYPE_SHARED=1)
       INTEGER MPI_MESSAGE_NULL
       PARAMETER (MPI_MESSAGE_NULL=(MPI_REQUEST_NULL))
       INTEGER MPI_MESSAGE_NO_PROC
       PARAMETER (MPI_MESSAGE_NO_PROC=1811939328)
       INTEGER MPI_THREAD_SINGLE
       PARAMETER (MPI_THREAD_SINGLE=0)
       INTEGER MPI_THREAD_FUNNELED
       PARAMETER (MPI_THREAD_FUNNELED=1)
       INTEGER MPI_THREAD_SERIALIZED
       PARAMETER (MPI_THREAD_SERIALIZED=2)
       INTEGER MPI_THREAD_MULTIPLE
       PARAMETER (MPI_THREAD_MULTIPLE=3)
       INTEGER MPI_MODE_RDONLY
       PARAMETER (MPI_MODE_RDONLY=2)
       INTEGER MPI_MODE_RDWR
       PARAMETER (MPI_MODE_RDWR=8)
       INTEGER MPI_MODE_WRONLY
       PARAMETER (MPI_MODE_WRONLY=4)
       INTEGER MPI_MODE_DELETE_ON_CLOSE
       PARAMETER (MPI_MODE_DELETE_ON_CLOSE=16)
       INTEGER MPI_MODE_UNIQUE_OPEN
       PARAMETER (MPI_MODE_UNIQUE_OPEN=32)
       INTEGER MPI_MODE_CREATE
       PARAMETER (MPI_MODE_CREATE=1)
       INTEGER MPI_MODE_EXCL
       PARAMETER (MPI_MODE_EXCL=64)
       INTEGER MPI_MODE_APPEND
       PARAMETER (MPI_MODE_APPEND=128)
       INTEGER MPI_MODE_SEQUENTIAL
       PARAMETER (MPI_MODE_SEQUENTIAL=256)
       INTEGER MPI_SEEK_SET
       PARAMETER (MPI_SEEK_SET=600)
       INTEGER MPI_SEEK_CUR
       PARAMETER (MPI_SEEK_CUR=602)
       INTEGER MPI_SEEK_END
       PARAMETER (MPI_SEEK_END=604)
       INTEGER MPI_ORDER_C
       PARAMETER (MPI_ORDER_C=56)
       INTEGER MPI_ORDER_FORTRAN
       PARAMETER (MPI_ORDER_FORTRAN=57)
       INTEGER MPI_DISTRIBUTE_BLOCK
       PARAMETER (MPI_DISTRIBUTE_BLOCK=121)
       INTEGER MPI_DISTRIBUTE_CYCLIC
       PARAMETER (MPI_DISTRIBUTE_CYCLIC=122)
       INTEGER MPI_DISTRIBUTE_NONE
       PARAMETER (MPI_DISTRIBUTE_NONE=123)
       INTEGER MPI_DISTRIBUTE_DFLT_DARG
       PARAMETER (MPI_DISTRIBUTE_DFLT_DARG=-49767)
       integer*8 MPI_DISPLACEMENT_CURRENT
       PARAMETER (MPI_DISPLACEMENT_CURRENT=-54278278)
       LOGICAL MPI_SUBARRAYS_SUPPORTED
       PARAMETER(MPI_SUBARRAYS_SUPPORTED=.FALSE.)
       LOGICAL MPI_ASYNC_PROTECTS_NONBLOCKING
       PARAMETER(MPI_ASYNC_PROTECTS_NONBLOCKING=.FALSE.)
       INTEGER*4 MPI_BOTTOM, MPI_IN_PLACE, MPI_UNWEIGHTED
       INTEGER MPI_WEIGHTS_EMPTY
       EXTERNAL MPI_DUP_FN, MPI_NULL_DELETE_FN, MPI_NULL_COPY_FN
       EXTERNAL MPI_WTIME, MPI_WTICK
       EXTERNAL PMPI_WTIME, PMPI_WTICK
       EXTERNAL MPI_COMM_DUP_FN, MPI_COMM_NULL_DELETE_FN
       EXTERNAL MPI_COMM_NULL_COPY_FN
       EXTERNAL MPI_WIN_DUP_FN, MPI_WIN_NULL_DELETE_FN
       EXTERNAL MPI_WIN_NULL_COPY_FN
       EXTERNAL MPI_TYPE_DUP_FN, MPI_TYPE_NULL_DELETE_FN
       EXTERNAL MPI_TYPE_NULL_COPY_FN
       EXTERNAL MPI_CONVERSION_FN_NULL
       REAL*8 MPI_WTIME, MPI_WTICK
       REAL*8 PMPI_WTIME, PMPI_WTICK


       COMMON /MPIFCMB5/ MPI_UNWEIGHTED
       COMMON /MPIFCMB9/ MPI_WEIGHTS_EMPTY
       SAVE /MPIFCMB5/
       SAVE /MPIFCMB9/

       COMMON /MPIPRIV1/ MPI_BOTTOM, MPI_IN_PLACE, MPI_STATUS_IGNORE

       COMMON /MPIPRIV2/ MPI_STATUSES_IGNORE, MPI_ERRCODES_IGNORE
       SAVE /MPIPRIV1/,/MPIPRIV2/

       COMMON /MPIPRIVC/ MPI_ARGVS_NULL, MPI_ARGV_NULL
       SAVE   /MPIPRIVC/

C--   COMMON /EESUPP_MPI_I/ MPI parallel support integer globals
C     mpiPidW   - MPI process id for west neighbor.
C     mpiPidE   - MPI process id for east neighbor.
C     mpiPidN   - MPI process id for north neighbor.
C     mpiPidS   - MPI process id for south neighbor.
C     mpiPidNW  - MPI process id for northwest neighbor.
C     mpiPidNE  - MPI process id for northeast neighbor.
C     mpiPidSW  - MPI process id for southwest neighbor.
C     mpiPidSE  - MPI process id for southeast neighbor.
C     mpiPidIO  - MPI process to use for IO.
C     mpiNprocs - No. of MPI processes.
C     mpiMyId   - MPI process id of me.
C     mpiComm   - MPI communicator to use.
C     mpiPx     - My MPI proc. grid X coord
C     mpiPy     - My MPI proc. grid Y coord
C     mpiXGlobalLo - My bottom-left (south-west) x-coordinate in
C                    global domain.
C     mpiYGlobalLo - My bottom-left (south-west) y-coordinate in
C                    global domain.
C     mpiTypeXFaceBlock_xy_r4  - Primitives for communicating edge
C     mpiTypeXFaceBlock_xy_r8    of a block.
C     mpiTypeYFaceBlock_xy_r4    XFace is used in east-west transfer
C     mpiTypeYFaceBlock_xy_r8    YFace is used in nrth-south transfer
C     mpiTypeXFaceBlock_xyz_r4   xy is used in two-dimensional arrays
C     mpiTypeXFaceBlock_xyz_r8   xyz is used with three-dimensional arrays
C     mpiTypeYFaceBlock_xyz_r4   r4 is used for real*4 data
C     mpiTypeYFaceBlock_xyz_r8   r8 is used for real*8 data
C     mpiTypeXFaceThread_xy_r4  - Composites of the above primitives
C     mpiTypeXFaceThread_xy_r8    for communicating edges of all blocks
C     mpiTypeYFaceThread_xy_r4    owned by a thread.
C     mpiTypeYFaceThread_xy_r8
C     mpiTypeXFaceThread_xyz_r4
C     mpiTypeXFaceThread_xyz_r8
C     mpiTypeYFaceThread_xyz_r4
C     mpiTypeYFaceBlock_xyz_r8
C     mpiTagE       - Tags are needed to mark requests when MPI is running
C     mpiTagW         between multithreaded processes or when the same process.
C     mpiTagS         is a neighbor in more than one direction. The tags ensure that
C     mpiTagN         a thread will get the message it is looking for.
C     mpiTagSW        The scheme adopted is to tag messages according to
C     mpiTagSE        the direction they are travelling. Thus a message
C     mpiTagNW        travelling east is tagged mpiTagE. However, in a
C     mpiTagNE        multi-threaded environemnt several messages could
C                     be travelling east from the same process at the
C                     same time. The tag is therefore modified to
C                     be mpiTag[EWS...]*nThreads+myThid. This requires that
C                     each thread also know the thread ids of its "neighbor"
C                     threads.
      COMMON /EESUPP_MPI_I/
     & mpiPidW,  mpiPidE,  mpiPidS,  mpiPidN,
     & mpiPidSE, mpiPidSW, mpiPidNE, mpiPidNW,
     & mpiPidIo, mpiMyId, mpiNProcs, mpiComm,
     & mpiPx, mpiPy, mpiXGlobalLo, mpiYGlobalLo,
     & mpiTypeXFaceBlock_xy_r4, mpiTypeXFaceBlock_xy_r8,
     & mpiTypeYFaceBlock_xy_r4, mpiTypeYFaceBlock_xy_r8,
     & mpiTypeXFaceBlock_xyz_r4, mpiTypeXFaceBlock_xyz_r8,
     & mpiTypeYFaceBlock_xyz_r4, mpiTypeYFaceBlock_xyz_r8,
     & mpiTypeXFaceThread_xy_r4, mpiTypeXFaceThread_xy_r8,
     & mpiTypeYFaceThread_xy_r4, mpiTypeYFaceThread_xy_r8,
     & mpiTypeXFaceThread_xyz_r4, mpiTypeXFaceThread_xyz_r8,
     & mpiTypeYFaceThread_xyz_r4, mpiTypeYFaceThread_xyz_r8,
     & mpiTagE, mpiTagW, mpiTagN, mpiTagS,
     & mpiTagSE, mpiTagSW, mpiTagNW, mpiTagNE

      INTEGER mpiPidW
      INTEGER mpiPidE
      INTEGER mpiPidS
      INTEGER mpiPidN
      INTEGER mpiPidSW
      INTEGER mpiPidSE
      INTEGER mpiPidNW
      INTEGER mpiPidNE
      INTEGER mpiPidIO
      INTEGER mpiMyId
      INTEGER mpiNProcs
      INTEGER mpiComm
      INTEGER mpiPx
      INTEGER mpiPy
      INTEGER mpiXGlobalLo
      INTEGER mpiYGlobalLo
      INTEGER mpiTypeXFaceBlock_xy_r4
      INTEGER mpiTypeXFaceBlock_xy_r8
      INTEGER mpiTypeYFaceBlock_xy_r4
      INTEGER mpiTypeYFaceBlock_xy_r8
      INTEGER mpiTypeXFaceBlock_xyz_r4
      INTEGER mpiTypeXFaceBlock_xyz_r8
      INTEGER mpiTypeYFaceBlock_xyz_r4
      INTEGER mpiTypeYFaceBlock_xyz_r8
      INTEGER mpiTypeXFaceThread_xy_r4(MAX_NO_THREADS)
      INTEGER mpiTypeXFaceThread_xy_r8(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xy_r4(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xy_r8(MAX_NO_THREADS)
      INTEGER mpiTypeXFaceThread_xyz_r4(MAX_NO_THREADS)
      INTEGER mpiTypeXFaceThread_xyz_r8(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xyz_r4(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xyz_r8(MAX_NO_THREADS)
      INTEGER mpiTagNW
      INTEGER mpiTagNE
      INTEGER mpiTagSW
      INTEGER mpiTagSE
      INTEGER mpiTagW
      INTEGER mpiTagE
      INTEGER mpiTagN
      INTEGER mpiTagS

C--   COMMON /MPI_FULLMAP_I/ holds integer arrays of the full list of MPI process
C     mpi_myXGlobalLo :: List of all processors bottom-left X-index in global domain
C     mpi_myYGlobalLo :: List of all processors bottom-left Y-index in global domain
C                        Note: needed for mpi gather/scatter routines & singleCpuIO.
      COMMON /MPI_FULLMAP_I/
     &        mpi_myXGlobalLo, mpi_myYGlobalLo
      INTEGER mpi_myXGlobalLo(nPx*nPy)
      INTEGER mpi_myYGlobalLo(nPx*nPy)

C MPI communicator describing this model realization
      COMMON /MPI_COMMS/
     &        MPI_COMM_MODEL
      INTEGER MPI_COMM_MODEL


C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalfile
      CHARACTER*(2) arrType
      INTEGER narr
      Real*8 arr(narr)
      INTEGER bi,bj
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

      STOP 'ABNORMAL END: S/R MDSWRITEVECTOR is empty'

C     ------------------------------------------------------------------
      RETURN
      END
