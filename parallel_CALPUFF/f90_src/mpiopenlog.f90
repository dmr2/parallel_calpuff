
SUBROUTINE mpiopenlog(log_unit, log_name)

!---------------------------------------------------------
! Purpose:  open log file for each processor
! Revised:  12 Jul 2011 Original version. (D.Yin)
!---------------------------------------------------------

USE mpiranktasks 
IMPLICIT none

!arguments and their descriptions

INTEGER, INTENT(IN) :: log_unit
CHARACTER(LEN=*), INTENT(IN) :: log_name

!local variables and their descriptions 

INTEGER :: length 
CHARACTER (LEN=LEN(log_name)) :: file_name


!start of the executable code 
file_name=log_name
length=LEN_TRIM(log_name)

IF (mpinumtasks.LT.100) THEN
   WRITE(file_name(length+1:length+4),'(I2.2,I2.2)') mpinumtasks,mpirank
ELSE 
   WRITE(file_name(length+1:length+5),'(I5.5)') mpirank
ENDIF

OPEN(log_unit,FILE=file_name,STATUS='UNKNOWN')
   
END SUBROUTINE mpiopenlog



