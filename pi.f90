program piEvaluator

    use mpmodule
 
    implicit none
   
    ! c.f. https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm

    type(mp_real) :: a, b, t, p

    call mpinit()

    a = mpreal(1d0, 1)
    b = sqrt(mpreal(0.5d0, 1))
    t = mpreal(0.25d0, 1)
    p = mpreal(1d0, 1)

    print *, a

end program piEvaluator
