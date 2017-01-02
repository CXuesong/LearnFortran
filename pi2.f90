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
    real, parameter     :: PIC = 4*atan(1d0)
    real, parameter     :: EVAL_MAX = 2*PIC
    integer, parameter  :: EVAL_SEGMENTS = 200

    call evalSine()
    call compareSine()

    contains

    subroutine evalSine()
        integer         :: i
        real            :: x

10      format(F16.9, F16.9)

        open(unit=10, file="sinx_Taylor_5.txt", status="replace")
        open(unit=11, file="sinx_Taylor_10.txt", status="replace")
        do i = 0, EVAL_SEGMENTS
            x = EVAL_MAX/EVAL_SEGMENTS*i
            write (10, 10), x, sint(5, x)
            write (11, 10), x, sint(10, x)
        end do
        close(10)
        close(11)
    end subroutine

    subroutine compareSine()
        integer         :: i
        real, dimension(EVAL_SEGMENTS + 1) :: x, y5, y10
        real            :: dummy

        open(unit=10, file="sinx_Taylor_5.txt", status="old")
        open(unit=11, file="sinx_Taylor_10.txt", status="old")
        do i = 1, EVAL_SEGMENTS + 1
            read (10, *), x(i), y5(i)
            read (11, *), dummy, y10(i)
        end do
        close(10)
        close(11)

        open(unit=12, file="sinx_Taylor_5_10_error.txt", status="replace")
        do i = 1, EVAL_SEGMENTS + 1
            write (12, "(F12.6, F12.6)"), x(i), y10(i) - y5(i)
        end do
        close(12)

    end subroutine

end program

