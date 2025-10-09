C JM 2021_Oct from MIT verif. exp.
C CPP options file for FLT package

#ifndef FLT_OPTIONS_H
#define FLT_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_FLT
C     Package-specific Options & Macros go here

C Include/Exclude part that allows 3-dimensional advection of floats
#define ALLOW_3D_FLT

C Use the alternative method of adding random noise to float advection
C #define USE_FLT_ALT_NOISE

C Add noise also to the vertical velocity of 3D floats
C #ifdef ALLOW_3D_FLT
C #define ALLOW_FLT_3D_NOISE
C #endif

C Define this to revert to old second-order Runge-Kutta integration
C #define FLT_SECOND_ORDER_RUNGE_KUTTA

C Prevent floats to re-enter the opposite side of a periodic domain (stop instead)
C #undef FLT_WITHOUT_X_PERIODICITY
C #undef FLT_WITHOUT_Y_PERIODICITY

#endif /* ALLOW_FLT */
#endif /* FLT_OPTIONS_H */
