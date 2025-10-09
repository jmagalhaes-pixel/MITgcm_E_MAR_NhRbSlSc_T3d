C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_calc.F,v 1.3 2005/04/06 18:45:20 jmc Exp $
C $Name:  $

#include "PP81_OPTIONS.h"

CBOP
C !ROUTINE: PP81_CALC

C !INTERFACE: =======================================================
      subroutine PP81_CALC(
     I     bi, bj, myTime, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE PP81_CALC                                     |
C     | o Compute all PP81 fields defined in PP81.h              |
C     |==========================================================|
C     | This subroutine is based on SPEM code                    |
C     \==========================================================/
      IMPLICIT NONE
C
C--------------------------------------------------------------------

C Maarten Buijsman, GFDL, 2011-1-31; modified version of Jody Klymak's pp81 code
C 
C global parameters updated by pp_calc
C     PPviscAz   - PP eddy viscosity coefficient              (m^2/s)
C     PPdiffKzT  - PP diffusion coefficient for temperature   (m^2/s)
C
C \ev

C !USES: ============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "DYNVARS.h"
#include "PP81.h"
#include "FFIELDS.h"
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#else /* ALLOW_AUTODIFF_TAMC */
      integer ikppkey
#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER bi, bj
      INTEGER myThid
      _RL     myTime

#ifdef ALLOW_PP81

C !LOCAL VARIABLES: ====================================================
c Local constants
C     imin, imax, jmin, jmax  - array computation indices
C     RiNumber                - Richardson Number
      INTEGER I, J, K, Km1, JJ
      INTEGER   iMin ,iMax ,jMin ,jMax
      _RL     denom, PPviscTmp, buoyFreq,rhot,tempu,tempv,oldK,Ri
      _RL     buoyFreqf, DZ2a
      _RL     rhoS(1:Nr),RS(1:Nr)
!      _RL     bF(1:Nr)
!      _RL     theta_mcb(1:Nr),theta_mcb3(1:Nr)
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(1024) msgBuf

CEOP
      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

C      DO K = 2, Nr

C ---------------------------------------------------------------------------
C replace theta with theta_mcb
C make linear temp profile
C ---------------------------------------------------------------------------
!       DO K=1,Nr
!         theta_mcb(K) = 20./3000. * rC(K) + 30.
!       ENDDO
!       theta_mcb3 = theta_mcb

C reverse surface layers
!       DO K=1,10
!          theta_mcb3(K+10) = theta_mcb(K)
!          theta_mcb3(K)    = theta_mcb(K+10)
!       ENDDO
C only reverse first layer
!       theta_mcb3(1+1) = theta_mcb(1)
!       theta_mcb3(1)   = theta_mcb(1+1)
!       theta_mcb= theta_mcb3 !switch

C Code by Jody Klymak has changed
C possible bugs
C 1) rhot = -tAlpha*(theta(I,J,K,bi,bj))*rhoNil
C K is not assigned and rho0 = 1024.0 is missing (?) This gives a  very low surface density 
C => the surface is always stable => causing a really high buoyancy freq (buoyFreq) 
C => giving a really high viscosity K in the case of near surface overturns.
C It almost seems if this is all purposely done, but I do not know why?
C 2) then something goes wrong with the sorting loop (I did not nail it), 
C because DZ is not equal over the sorted overturn 
C => -90 m at the surface and 100 m in the lower layer => gives unequal K
C 3) this incorrect soring causes a jump in the vertical density gradient, 
C which should be constant => gives a jump in N => gives a jump in K C
C 
C K will be 0.2*dz^2 N where dz is the displacement of water from its
C sorted depth and N is the brunt-vaisala frequency of the sorted profile.

      DO J=jMin,jMax
         DO I=iMin,iMax

C$$$            CALL FIND_RHO(
C$$$     I           bi, bj, I, I, J, J, 1,1,
C$$$     I           theta, salt,
C$$$     O           rhot,
C$$$     I           myThid )
C Sort density profile...
            rhot = 1024.0-tAlpha*(theta(I,J,1,bi,bj))*rhoNil
!            rhot = 1024.0-tAlpha*(theta_mcb(1))*rhoNil
            rhoS(1)=rhot
            RS(1)=rC(1)

            DO K=2,Nr
C$$$               CALL FIND_RHO(
C$$$     I              bi, bj, I, I, J, J, K,K,
C$$$     I              theta, salt,
C$$$     O              rhot,
C$$$     I              myThid )
               rhot = 1024.0-tAlpha*(theta(I,J,K,bi,bj))*rhoNil
!               rhot = 1024.0-tAlpha*(theta_mcb(K))*rhoNil

               rhoS(K)=rhot
               RS(K)=rC(K)

C$$$               WRITE(msgBuf, '(A,I10.10,A,E10.4,A,E10.4)') 'Hellok ', K
C$$$     $              -1,' ',theta(I,J,K,bi,bj),' ',rhot
C$$$               CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
C$$$     &              SQUEEZE_RIGHT , 1)

               IF ( (rhoS(K).LT.rhoS(K-1)).AND.(maskC(I,J,K,bi
     &              ,bj).GT.0)) THEN
                  JJ=K-1
                  DO WHILE ( (JJ.GT.0).AND.(rhoS(K).LT.rhoS(JJ)) )
!                     write(*,*) K,JJ,rhoS(K),rhoS(JJ)   
                     JJ=JJ-1
                  ENDDO
                  rhoS(JJ+1:K)=cshift(rhoS(JJ+1:K),-1)
                  RS(JJ+1:K)=cshift(RS(JJ+1:K),-1)
               ENDIF

            ENDDO

!             write(*,*) '---------------------------------------------'
!             DO K=1,Nr
!                write(*,*) K,rhoS(K),RS(K),diffKrNrT(K)
!             ENDDO

C similar to N compute DZ at faces, as well as K   
C RS-R is dz....
C recip_drC=inverse distanance between centers, 
C first is between surface and first center
C diffKrNrT(K) = viscArNr(K) = background value   
            PPviscAr(I,J,1,bi,bj) = viscArNr(1)
            PPdiffKr(I,J,1,bi,bj) = MAX(PPviscAr(I,J,1,bi,bj),
     &              diffKrNrT(1))
            PPeps(I,J,1,bi,bj) = 0.0

            DO K=2,Nr
C N2 at K-2 face
               buoyFreqf = -gravity*mass2rUnit*
     &              (rhoS(K-1) - rhoS(K))*recip_drC(K)
               buoyFreqf = MAX(buoyFreqf,0. _d 0) ! not < 0
                
C DZ, first square, then average
    	       DZ2a = 0.5*( (RS(K-1) - rC(K-1))*(RS(K-1) - rC(K-1))+
     &                      (RS(K)   - rC(K)  )*(RS(K)   - rC(K)  ) )
C then viscosity
               PPviscAr(I,J,K,bi,bj) = MAX( viscArNr(K), 
     &                                        0.2*DZ2a*sqrt(buoyFreqf) ) 
C avoid large values
               PPviscAr(I,J,K,bi,bj) = MIN( 300. _d 0, 
     &                                  PPviscAr(I,J,K,bi,bj) )

               PPdiffKr(I,J,K,bi,bj) = MAX( PPviscAr(I,J,K,bi,bj),
     &                                      diffKrNrT(K) )

C Get epsilon, but square before averaging 
               Km1=K-1
               tempu=.5*((uVel(I,J,Km1,bi,bj)-uVel(I,J,K,bi,bj))*
     &                   (uVel(I,J,Km1,bi,bj)-uVel(I,J,K,bi,bj))*
     &                   recip_drC(K)*recip_drC(K)+
     &                   (uVel(I+1,J,Km1,bi,bj)-uVel(I+1,J,K,bi ,bj))*
     &                   (uVel(I+1,J,Km1,bi,bj)-uVel(I+1,J,K,bi ,bj))*
     &                   recip_drC(K)*recip_drC(K))

               tempv=.5*((vVel(I,J,Km1,bi,bj)-vVel(I,J,K,bi,bj))*
     &                   (vVel(I,J,Km1,bi,bj)-vVel(I,J,K,bi,bj))*
     &                   recip_drC(K)*recip_drC(K)+
     &                   (vVel(I,J+1,Km1,bi,bj)-vVel(I,J+1,K,bi ,bj))*
     &                   (vVel(I,J+1,Km1,bi,bj)-vVel(I,J+1,K,bi ,bj))*
     &                   recip_drC(K)*recip_drC(K))

                PPeps(I,J,K,bi,bj)=(tempu+tempv)*PPviscAr(I,J,K,bi,bj)
C Get epsilon in the old way, . 
!                tempu=.5*(uVel(I,J,Km1,bi,bj)+uVel(I+1,J,Km1,bi,bj)-
!      &                  (uVel(I,J,K  ,bi,bj)+uVel(I+1,J,K  ,bi ,bj)))
!      &                   *recip_drC(K)
!                tempv=.5*(vVel(I,J,Km1,bi,bj)+vVel(I,J+1,Km1,bi,bj)-
!      &                  (vVel(I,J,K,bi,bj)+vVel(I,J+1,K,bi ,bj)))
!      &              *recip_drC(K)
!                PPeps(I,J,K,bi,bj)=(tempu*tempu+tempv*tempv) 
!      &                             *PPviscAr(I,J,K,bi,bj)

!               write(*,*) K,RS(K)-rC(K),buoyFreqc,PPviscAr(I,J,K,bi,bj)

            ENDDO

         ENDDO
      ENDDO

#endif /* ALLOW_PP81 */

      RETURN
      END
