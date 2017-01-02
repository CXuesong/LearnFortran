program manipulate_student

    implicit none

    integer, parameter :: STUDENT_COUNT = 10

    type record
        integer id
        real score
    end type

    type(record), dimension(STUDENT_COUNT) :: records
    integer :: i

    ! read the records
    open(unit=10, file="students.txt", status="old")
    do i = 1, STUDENT_COUNT
        read (10, *) records(i)%id, records(i)%score
    end do
    close(10)

    ! manipulate the poor guy's record
    records(4)%score = 95d0

    ! write into new file
    open(unit=10, file="students_new.txt", status="replace")
    do i = 1, STUDENT_COUNT
        write (10, *) records(i)%id, records(i)%score
    end do
    close(10)

end program
