PROGRAM test
IMPLICIT NONE

INTEGER, PARAMETER :: STUDENT_COUNT = 7
INTEGER, PARAMETER :: MAX_VACANCY = 3

INTEGER, DIMENSION(STUDENT_COUNT, MAX_VACANCY) :: VACANCY
INTEGER, DIMENSION(STUDENT_COUNT) :: schedule = 0
INTEGER :: solutionCount = 0

VACANCY = transpose(reshape( &
                 (/2,4,0, &
                   1,6,0, &
                   3,7,0, &
                   5,0,0, &
                   1,4,6, &
                   2,5,0, &
                   3,6,7/), &
                 (/MAX_VACANCY, STUDENT_COUNT/)))

!write(*,*) VACANCY(2,3)

CALL assignStudent(1)
WRITE(*,*) "Solutions: ", solutionCount

CONTAINS

RECURSIVE SUBROUTINE assignStudent(studentIndex)
    INTEGER, INTENT(IN) :: studentIndex
    INTEGER :: i, j, vc

    TEST_VACANCY: DO i = 1,MAX_VACANCY
        vc = VACANCY(studentIndex, i)
        IF (vc == 0) EXIT TEST_VACANCY
        ! sanity check
        DO j = 1,studentIndex - 1
            ! DOW is occupied
            IF (schedule(j) == vc) CYCLE TEST_VACANCY
        END DO
        ! so far, so good
        schedule(studentIndex) = vc
        IF (studentIndex >= STUDENT_COUNT) THEN
            ! the solution is complete
            DO j = 1, STUDENT_COUNT
                write(*,"(I4)",advance="no") schedule(j)
            END DO
            write(*,*)
            solutionCount = solutionCount + 1
            CYCLE TEST_VACANCY
        END IF
        ! the solution is not complete
        ! go to next level
        CALL assignStudent(studentIndex + 1)        
    END DO TEST_VACANCY
END SUBROUTINE assignStudent

END PROGRAM test
