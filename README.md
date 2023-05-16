# guia5

trapecio:

    h = (b-a)/n
    It = (f(a)+f(b))/2._wp
    do i = 1, n-1
      x = a + i*h
      It = It + f(xi)
    end do
    It = It*h
    
 ---------------------------------------   
    h = (b-a)/n
    It = (f(a)+f(b))/2._wp
    do i = 1, n-1
      !x = a + i*h
      It = It + f(a + i*h)
    end do
    It = It*h
    
a lo experimental:    
---------------------------------------    
    h = (xn-xo)/n
    It=(y0+yn)/2._wp
    do i=1,n-1
        It=It+yi
    end do
    It=It*h
