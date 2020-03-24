   !-------------------------------------------------------
   !  
   ! bulk.f90
   !
   ! Description:
   ! ============
   ! Code to compute bulk scattering properties 
   ! from BHMIE output based on a size grid from
   ! grid.txt . The numerical quadrature is per-
   ! formed using the Trapezoidal Rule and the 
   ! PSD is assumed to be a Gamma distribution.
   !
   ! Record of Revisions:
   ! ====================
   !
   ! Date:        Author:           Description:
   ! =====        =======           ============
   ! 2020-03-12   P. Stegmann       Original Code
   !
   !
   !
   !
   ! Copyright © 2020 Patrick Stegmann
   !
   ! This file is part of ScatteringBase.
   !
   ! ScatteringBase is free software:
   ! you can redistribute it and/or modify it under 
   ! the terms of the Apache License as published by
   ! the Apache Software Foundation, either version 2.0
   ! of the License, or (at your option) any later version.
   !
   ! This program is distributed in the hope that it will 
   ! be useful,
   ! but WITHOUT ANY WARRANTY; without even the implied 
   ! warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
   ! PURPOSE.  See the Apache License for more details.
   !
   ! You should have received a copy of the Apache 2.0 
   ! License along with this program. If not, 
   ! see <https://www.apache.org/licenses/LICENSE-2.0>.
   !
   !-------------------------------------------------------

   PROGRAM MAIN

      IMPLICIT NONE

      ! Data Dictionary:

      ! Computation Parameters:
      INTEGER(KIND=4), PARAMETER :: points = 100
      INTEGER(KIND=4), PARAMETER :: nangles = 1999
      INTEGER(KIND=4), PARAMETER :: nStokes = 4
      ! Physical Parameteers:
      REAL(KIND=8), PARAMETER :: pi = 4.d0*DATAN(1.d0)
      REAL(KIND=8), PARAMETER :: veff = 0.3
      REAL(KIND=8), DIMENSION(points) :: rs
      REAL(KIND=8), DIMENSION(nangles) :: scad
      REAL(KIND=8), DIMENSION(nStokes,nangles) :: phf
      REAL(KIND=8), DIMENSION(nStokes,nangles) :: phfi
      REAL(KIND=8) :: p11, p12, p33, p43
      REAL(KIND=8) :: puf
      CHARACTER(LEN=19), DIMENSION(points) :: chas
      CHARACTER(LEN=55) :: dir
      CHARACTER(LEN=55) :: buf
      CHARACTER(LEN=55) :: Reffchar
      REAL(KIND=8) :: Reff
      REAL(KIND=8) :: cexti
      REAL(KIND=8) :: cscai
      REAL(KIND=8) :: cext
      REAL(KIND=8) :: csca
      REAL(KIND=8) :: asym
      REAL(KIND=8) :: asymi
      REAL(KIND=8) :: deltar 
      REAL(KIND=8) :: dpl
      REAL(KIND=8) :: rci
      REAL(KIND=8) :: rc
      REAL(KIND=8) :: csc
      INTEGER(KIND=4) :: ii, jj, isca

      ! Instructions:

      ! Read effective radius as command line argument:
      CALL GET_COMMAND_ARGUMENT(1,Reffchar)
      READ(Reffchar,*) Reff

      ! Read the quadrature grid of the particle size from file 
      OPEN(10,FILE='grid.txt',ACTION='READ',STATUS='OLD')
      READ(10,*) chas
      REWIND(10)
      READ(10,*) rs 
      CLOSE(10)

      ! Read the scattering angles
      dir = TRIM(ADJUSTL(chas(1)))//'/mie_output.txt'
      !dir = 'mie_output.txt'
	  OPEN(12,FILE=dir,ACTION='READ',STATUS='OLD')
      ! Read the header to blank
      DO isca = 1, 6
         READ(12,*)
      END DO 
      ! Read the actual scattering angle table
      DO isca = 1, nangles
         READ(12,*) scad(isca)
      END DO 
      CLOSE(12)

      rc = 0.d0
      phf = 0.d0
      cext = 0.d0
      csca = 0.d0
      asym = 0.d0
      
      ! Loop over all particle radii (quadrature points)
      ! for one effective radius:
      DO ii = 1, points
         ! Compute the abscissa spacing 
         ! delta_r of the quadrature.      
         IF (ii .EQ. 1) THEN
            deltar = rs(1)
         ELSE
            deltar = rs(ii) - rs(ii-1)
         ENDIF
         ! Gamma Dist. PSD integration factor:
         dpl = 1.d0/(rs(ii)**4.d0)
         dpl = rs(ii)**((1-3.*veff)/veff)*EXP(-rs(ii)/(veff*Reff))
         ! Total integration factor:
         rci = deltar * dpl
         rc = rc + rci
         ! Open single-scattering results:
         dir = TRIM(ADJUSTL(chas(ii)))//'/mie_output.txt'
         !dir = 'mie_output.txt'
		 OPEN(11,FILE=dir,ACTION='READ',STATUS='OLD')
         ! Skip header:
         DO isca = 1, 3
            READ(11,*)
         END DO 
         ! Read extinction, scattering efficiency,
         ! and asymmetry parameter:
         READ(11,*) buf, cexti, buf, cscai, buf, puf, buf, asymi
         ! Convert efficiency factors to cross-sections:
         cexti = cexti*pi*rs(ii)**2
         cscai = cscai*pi*rs(ii)**2
         ! Average of extinction and scattering cross-sections
         cext = cext + rci * cexti
         csca = csca + rci * cscai
         asym = asym + rci * asymi * cscai
         rci = rci * cscai
         ! Read the phase matrix header to blank:
         DO isca = 1, 2
            read(11,*)
         END DO 
         ! Read the entire single-scattering phase matrix from file:
         DO isca = 1, nangles
            READ(11,*) puf, phfi(1:4,isca)
         END DO 
         CLOSE(11)
         ! Normalization of phase function
         csc = 0.d0
         DO isca = 1, (nangles-1)
            csc = csc + 0.25d0 * (cos(scad(isca)*pi/180.d0)&
                      - cos(scad(isca+1)*pi/180.d0))*(phfi(1,isca)&
                      + phfi(1,isca+1))
         END DO 
         phfi(2,:) = phfi(2,:)*phfi(1,:)         
         phfi = phfi / csc
         ! WRITE(*,*) phfi(1,1), csc, rci
         DO isca = 1, nangles
            phf(1:4, isca) = phf(1:4, isca) + rci * phfi(1:4, isca)
         END DO
      END DO     
!
      cext = cext/rc
      csca = csca/rc
      phf = phf/rc 
      phf = phf/csca
      asym = asym/rc
      csc = 0.d0
      DO isca = 1, (nangles-1)
         csc = csc + 0.25d0 * (cos(scad(isca)*pi/180.d0)&
                   - cos(scad(isca+1)*pi/180.d0))*(phf(1,isca)&
                   + phf(1,isca+1))
      END DO 
      !WRITE(*,*) csc, csca, cext

      DO isca = 1, nangles
         phf(2:4,isca) = phf(2:4,isca)/phf(1,isca)
      END DO 

      ! Write results to file:
      OPEN(13,FILE='sca_mie.dat',ACTION='WRITE',STATUS='UNKNOWN')
      WRITE(13,'(E16.8,1X,A)') cext, 'extinction coefficient'
      WRITE(13,'(E16.8,1X,A)') csca/cext, 'single-scattering albedo'
      WRITE(13,'(E16.8,1X,A)') asym/csca, 'asymmetry factor'
      DO isca = 1, nangles
         WRITE(13,'(7(E14.6,1X))') scad(isca),phf(1,isca),phf(2,isca),&
                                              phf(3,isca),phf(4,isca)
      END DO 
      CLOSE(13)

   END PROGRAM MAIN 

