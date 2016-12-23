program piEvaluator

    use mpmodule

    implicit none

    ! c.f. https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm

     type(mp_real) a, b, t, p

     call mpinit(10)

     !a = mpreal(1d0)
     !b = sqrt(mpreal(0.5d0))
     !t = mpreal(0.25d0)
     !p = mpreal(1d0)
     
    
    a = mppi()

    call mpwrite (6, 300, 100, a)

end program piEvaluator
