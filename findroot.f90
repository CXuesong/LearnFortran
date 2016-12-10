program findroot

    implicit none

    print *, "x0 = ", root(F, 1.5, 2.5, 1e-12)
    
    contains

    function F(x)
        real, intent(in) :: x
        real F
        F = (x - 2)*(x + 1)
    end function

    function root(func, a, b, eps)
        interface
            function func(x) result(y)
                real, intent(in) :: x
                real :: y
            end function func
        end interface
        real, intent(in) :: a, b, eps
        real :: root
        
        real :: l, r, m
        real :: lv, rv, mv

        l = a; r = b
        lv = func(l); rv = func(r)
        
        if (lv*rv > 0) stop -1   ! invalid input
        
        do while (l - r > eps)
            m = (l + r)/2
            mv = func(m)
            if (mv*lv > 0) then
                l = m
            else if (mv*rv > 0) then
                r = m
            else
                exit                       ! now we must have mv == 0
            end if
        end do

        root = (l + r)/2
    end function root

end program findroot

