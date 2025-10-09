C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/obcs_calc.F,v 1.6 2009/12/15 17:03:29 jahn Exp $
C $Name: checkpoint62 $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CALC( futureTime, futureIter,
     &                      uVel, vVel, wVel, theta, salt, 
     &                      myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_CALC                                     |
C     | o Calculate future boundary data at open boundaries      |
C     |   at time = futureTime                                   |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "OBCS.h"

C     == Routine arguments ==
      INTEGER futureIter
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER bi, bj
      INTEGER I, J ,K, JJ

#include "GRID.h"
      _RL Uinflow,rampTime2
      _RL omegaF,ftide,facdmp,dumvar
      _RL omgF(1:8),amplF(1:8),phsF(1:8)

c
c      Uinflow  = 0.06667
c      omegaF   = 1.4053 _d -4  !M2 tidal frequency in rad/s

C     == forcing characteristics 
      omgF(1) = 0.000140518900
      omgF(2) = 0.000145444100
      omgF(3) = 0.000137879700
      omgF(4) = 0.000145842300
      omgF(5) = 0.000072921170
      omgF(6) = 0.000067597740
      omgF(7) = 0.000072522950
      omgF(8) = 0.000064958540
      amplF(1) = 0.029840145660
      amplF(2) = 0.017675093411
      amplF(3) = 0.004950825738
      amplF(4) = 0.005995579943
      amplF(5) = 0.009220553631
      amplF(6) = 0.005606491338
      amplF(7) = 0.002531745179
      amplF(8) = 0.001195082500
CC model start time: t=2009     2     2    11    30     0
      phsF(1) = -0.354637125401
      phsF(2) = -2.882378603450
      phsF(3) = -1.498207132120
      phsF(4) = -1.504070929408
      phsF(5) = -2.197846922123
      phsF(6) = 2.575411209660
      phsF(7) = -0.637532956749
      phsF(8) = 1.580049693138

C *NOTE* I have commented out the ramp function below
C just to speed things up. You will probably want to use it
C for smoother looking solutions.
      rampTime2 = 1. _d 4
      facdmp = 1. _d 0 - exp(-1. _d 0/rampTime2*futureTime)

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C     Eastern OB
      IF (useOrlanskiEast) THEN
        CALL ORLANSKI_EAST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            dumvar = 0. _d 0
            DO JJ=1,8
              dumvar = dumvar
     &        +amplF(JJ)*cos(omgF(JJ)*futureTime-phsF(JJ))*facdmp
            ENDDO
            OBEu(J,K,bi,bj)=dumvar - 0.0141 !JM add mean U
            OBEv(J,K,bi,bj)=0.
            OBEt(J,K,bi,bj)=tRef(K)
            OBEs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(J,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C     Western OB
      IF (useOrlanskiWest) THEN
        CALL ORLANSKI_WEST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            dumvar = 0. _d 0
            DO JJ=1,8
              dumvar = dumvar
     &        +amplF(JJ)*cos(omgF(JJ)*futureTime-phsF(JJ))*facdmp
            ENDDO
            OBWu(J,K,bi,bj)=dumvar - 0.0141 !JM add mean U
            OBWv(J,K,bi,bj)=0.
            OBWt(J,K,bi,bj)=tRef(K)
            OBWs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBWw(J,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C         Northern OB, template for forcing
      IF (useOrlanskiNorth) THEN
        CALL ORLANSKI_NORTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBNv(I,K,bi,bj)=0.
            OBNu(I,K,bi,bj)=0.
            OBNt(I,K,bi,bj)=tRef(K)
            OBNs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C         Southern OB, template for forcing
      IF (useOrlanskiSouth) THEN   
        CALL ORLANSKI_SOUTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBSu(I,K,bi,bj)=0.
            OBSv(I,K,bi,bj)=0.
            OBSt(I,K,bi,bj)=tRef(K)
            OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C--   end bi,bj loops.
      ENDDO 
      ENDDO

#endif /* ALLOW_OBCS */
      RETURN
      END
