PROGRAM secante
IMPLICIT NONE
REAL::f,ea=6,es=1,x0,x1,x2
INTEGER::c=0


PRINT *,'Insira o intervalo inicial'
read(*,*)x0,x1

DO WHILE(ea>es)
x2=x1-((x1-x0)/((f(x1))-(f(x0))))*f(x1)
ea=abs(((x1-x0)/x1)*100)

x0=x1
x1=x2
c=c+1

IF(c>50) EXIT


WRITE(1,*) x1
END DO

IF(x1 > 3E+38 .OR. x1 < -3E+38) THEN

PRINT *,'Intervalo inv lido'
ELSE
PRINT *,'Raiz final encontrada ‚',x1
END IF

pause

END PROGRAM secante

REAL function f(x1)

REAL::x1

f=4*sin(x1) - exp(x1)

RETURN
END FUNCTION f
