!*==SGRND.spg  processed by SPAG 6.72Dc at 15:54 on 26 Jun 2023
! A C-program for MT19937: Real number version
!   genrand() generates one pseudorandom real number (double)
! which is uniformly distributed on [0,1]-interval, for each
! call. sgenrand(seed) set initial values to the working area
! of 624 words. Before genrand(), sgenrand(seed) must be
! called once. (seed is any 32-bit integer except for 0).
! Integer generator is obtained by modifying two lines.
!   Coded by Takuji Nishimura, considering the suggestions by
! Topher Cooper and Marc Rieffel in July-Aug. 1997.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later
! version.
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU Library General Public License for more details.
! You should have received a copy of the GNU Library General
! Public License along with this library; if not, write to the
! Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
! 02111-1307  USA
!
! Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
! When you use this, send an email to: matumoto@math.keio.ac.jp
! with an appropriate reference to your work.
!
!***********************************************************************
! Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
!
!   genrand()      -> double precision function grnd()
!   sgenrand(seed) -> subroutine sgrnd(seed)
!                     integer seed
!
! This program uses the following non-standard intrinsics.
!   ishft(i,n): If n>0, shifts bits in i by n positions to left.
!               If n<0, shifts bits in i by n positions to right.
!   iand (i,j): Performs logical AND on corresponding bits of i and j.
!   ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
!   ieor (i,j): Performs exclusive OR on corresponding bits of i and j.
!
!***********************************************************************
!***********************************************************************
module mt19937
    contains
      SUBROUTINE SGRND(Seed)
!
      IMPLICIT NONE
!*--SGRND50
!*** Start of declarations inserted by SPAG
      INTEGER MT , MTI , N , Seed
!*** End of declarations inserted by SPAG
!
! Period parameters
      PARAMETER (N=624)
!
      DIMENSION MT(0:N-1)
!                     the array for the state vector
      COMMON /BLOCK / MTI , MT
      SAVE /BLOCK / 
!
!      setting initial seeds to mt[N] using
!      the generator Line 25 of Table 1 in
!      [KNUTH 1981, The Art of Computer Programming
!         Vol. 2 (2nd Ed.), pp102]
!
      MT(0) = IAND(Seed,-1)
      DO MTI = 1 , N - 1
         MT(MTI) = IAND(69069*MT(MTI-1),-1)
      ENDDO
!
      END
!*==GRND.spg  processed by SPAG 6.72Dc at 15:54 on 26 Jun 2023
!***********************************************************************
      DOUBLE PRECISION FUNCTION GRND()
!
      IMPLICIT NONE
!*--GRND79
!*** Start of declarations inserted by SPAG
      INTEGER kk , LMASK , M , mag01 , MATA , MT , MTI , N , N1 ,       &
            & TMASKB , TMASKC , TSHFTL , TSHFTS , TSHFTT , TSHFTU ,     &
            & UMASK , y
!*** End of declarations inserted by SPAG
!
! Period parameters
      PARAMETER (N=624)
      PARAMETER (N1=N+1)
      PARAMETER (M=397)
      PARAMETER (MATA=-1727483681)
!                                    constant vector a
      PARAMETER (UMASK=-214748364)
!                                    most significant w-r bits
      PARAMETER (LMASK=214748364)
!                                    least significant r bits
! Tempering parameters
      PARAMETER (TMASKB=-1658038656)
      PARAMETER (TMASKC=-272236544)
!
      DIMENSION MT(0:N-1)
!                     the array for the state vector
      COMMON /BLOCK / MTI , MT
      SAVE /BLOCK / 
      DATA MTI/N1/
!                     mti==N+1 means mt[N] is not initialized
!
      DIMENSION mag01(0:1)
      DATA mag01/0 , MATA/
      SAVE mag01
!                        mag01(x) = x * MATA for x=0,1
!
      TSHFTU(y) = ISHFT(y,-11)
      TSHFTS(y) = ISHFT(y,7)
      TSHFTT(y) = ISHFT(y,15)
      TSHFTL(y) = ISHFT(y,-18)
!
      IF ( MTI.GE.N ) THEN
!                       generate N words at one time
!                            if sgrnd() has not been called,
!                              a default initial seed is used
         IF ( MTI.EQ.N+1 ) CALL SGRND(4357)
!
         DO kk = 0 , N - M - 1
            y = IOR(IAND(MT(kk),UMASK),IAND(MT(kk+1),LMASK))
            MT(kk) = IEOR(IEOR(MT(kk+M),ISHFT(y,-1)),mag01(IAND(y,1)))
         ENDDO
         DO kk = N - M , N - 2
            y = IOR(IAND(MT(kk),UMASK),IAND(MT(kk+1),LMASK))
            MT(kk) = IEOR(IEOR(MT(kk+(M-N)),ISHFT(y,-1)),               &
                   & mag01(IAND(y,1)))
         ENDDO
         y = IOR(IAND(MT(N-1),UMASK),IAND(MT(0),LMASK))
         MT(N-1) = IEOR(IEOR(MT(M-1),ISHFT(y,-1)),mag01(IAND(y,1)))
         MTI = 0
      ENDIF
!
      y = MT(MTI)
      MTI = MTI + 1
      y = IEOR(y,TSHFTU(y))
      y = IEOR(y,IAND(TSHFTS(y),TMASKB))
      y = IEOR(y,IAND(TSHFTT(y),TMASKC))
      y = IEOR(y,TSHFTL(y))
!
      IF ( y.LT.0 ) THEN
         GRND = (DBLE(y)+2.0D0**32)/(2.0D0**32-1.0D0)
      ELSE
         GRND = DBLE(y)/(2.0D0**32-1.0D0)
      ENDIF
!
      END
    end module mt19937
