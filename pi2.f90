module PI

    
    implicit none

    contains

    function factorial(n) result (y)
        integer, intent(in) :: n
        integer             :: y
        
        integer             :: i

        y = 1
        do i = 2, n
            y = y*i
        end do
    end function

    ! n should be less than 17, or it will overflow.
    function sint(n, x) result (y)
        integer, intent(in) :: n
        real, intent(in)    :: x
        real                :: y

        integer             :: i
        real                :: coeff
        y = x
        coeff = x
        do i = 3, n, 2
            coeff = -coeff*x*x
            !print *, i, coeff, factorial(i)
            y = y + coeff/factorial(i)
        end do
    end function
end module PI

program test
    
    use PI

    implicit none

    call evalSine()

    contains

    subroutine evalSine()
        real, parameter :: PIC = 4*atan(1d0)
        integer         :: i

        open(unit=10, file="sinx_Taylor_5.txt", status="replace")
        open(unit=11, file="sinx_Taylor_10.txt", status="replace")
        do i = 0, 200
            write (10, "(F16.9)"), sint(5, PIC/200*i)
            write (11, "(F16.9)"), sint(10, PIC/200*i)
        end do
        close(10)
        close(11)
    end subroutine

end program

