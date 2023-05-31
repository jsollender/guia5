module integracion

use mod_prec 
implicit none

!realice un programa que encuentre las aproximaciones numericas Sm, 
!St y Ss a la integral I (integral entre a y b de f de x, dif x) 
!utilizando la regla compuesta del punto medio,trapecio y la de 
!Simpson, respectivamente.

!para los dos métodos se debe evaluar integrando f(x) en n+1 puntos 
!equiespaciados xi (i=0,1,...,n)c con espaciamiento h=(b-a)/n.

!en caso del punto medio se evalua en los xi + h/2 (I=0,...1n-1).

!el prog debe utilizar unmod de presicion y un mod con tres 
!subrutinas:

!pmedio
!trapecio
!simpson

!las subr deben incluir entre sus arg. a los limites de integracion 
!y el nº de puntos o intervalos empleados. 

contains
	subroutine pmedio (a, b, n, ti)
	use mod_prec
	implicit none
	end subroutine pmedio
	
	subroutine trapecio (a, b, n, It, xi)
	use mod_prec
	implicit none
	
	
	end subroutine trapecio
	
	subroutine simpson (a, b, n, ti)
	use mod_prec
	implicit none
	end subroutine simpson
	
		

end module
