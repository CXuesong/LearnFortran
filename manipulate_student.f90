program manipulate_student

    implicit none

    integer, parameter :: STUDENT_COUNT = 10
    character(len=*), parameter :: RECORD_FORMAT = "(I10, F8.2)"

    type record
        integer id
        real score
    end type

    type(record), dimension(STUDENT_COUNT) :: records
    integer :: i

10  format(I10, F8.2)

    ! read the records
    open(unit=10, file="students.txt", status="old")
    do i = 1, STUDENT_COUNT
        read (10, *) records(i)%id, records(i)%score
    end do
    close(10)

    ! write into new file
    open(unit=10, file="students_new.txt", status="replace", access="direct", form="formatted", recl=20)
    do i = 1, STUDENT_COUNT
        write (10, RECORD_FORMAT, rec=i) records(i)%id, records(i)%score
    end do
    
    ! manipulate the poor guy's record
    records(4)%score = 95d0
    write (10, 10, rec=4) records(4)%id, records(4)%score

    ! sort
    call sort_records(10, STUDENT_COUNT)

    close(10)

    contains

    ! Sorts all the records in-place in a file.
    subroutine sort_records(unit, count)
        integer, intent(in) :: unit, count
        logical             :: swapped
        integer             :: i, j
        type(record)        :: a, b

        ! basic bubbling sort

        do i = 2, count
            swapped = .false.
            do j = 2, count
                read (unit, RECORD_FORMAT, rec=j - 1) a%id, a%score
                read (unit, RECORD_FORMAT, rec=j) b%id, b%score
                if (b%score > a%score) then
                    write (unit, RECORD_FORMAT, rec=j - 1) b%id, b%score
                    write (unit, RECORD_FORMAT, rec=j) a%id, a%score
                    swapped = .true.
                end if
            end do
            if (.not. swapped) then
                exit
            end if
        end do

    end subroutine

end program
