!-------------------------------------------------------
!
! main.f90
!
! Description:
! ------------
! Fortran snippet to produce a grid of real
! numbers with logarithmically equal spacing.
!
! Record of Revisions:
! --------------------
!
! Date:            Author:         Description:
! -----            -------         ------------
! 2020-03-06       P. Stegmann     Original Code
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
	REAL(KIND=8), PARAMETER :: start = 0.1
	REAL(KIND=8), PARAMETER :: fin = 3000.
	INTEGER(KIND=4), PARAMETER :: steps = 100
	REAL(KIND=8),DIMENSION(steps) :: grid
    INTEGER(KIND=4), PARAMETER :: FileUnit = 444

	! Instructions:

	! Compute the logequal grid:
	grid = LOGEQUAL(start,fin,steps)

	! Output grid to file:
	OPEN(UNIT=FileUnit, &
		 FILE="grid.txt", &
		 ACTION="WRITE", &
		 STATUS="REPLACE")
	WRITE(FileUnit,*) grid
	CLOSE(FileUnit)

CONTAINS

	PURE FUNCTION LOGEQUAL(start,fin,steps) RESULT(grid)
		! Data Dictionary
		REAL(KIND=8), INTENT(IN) :: start
		REAL(KIND=8), INTENT(IN) :: fin
		INTEGER(KIND=4), INTENT(IN) :: steps
		REAL(KIND=8),DIMENSION(steps) :: grid
		INTEGER(KIND=4) :: ii
		REAL(KIND=8) :: tmp
		! Function Instructions:
		tmp = LOG(start)
		grid_loop: DO ii = 1, steps
			tmp = (LOG(fin)-LOG(start))/steps + tmp
			grid(ii) = EXP(tmp) 
		END DO grid_loop
		RETURN
	END FUNCTION LOGEQUAL

END PROGRAM MAIN
