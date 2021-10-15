
PROGRAM NewtonRaphson

   IMPLICIT NONE

   REAL, EXTERNAL :: f, f_prime
   INTEGER :: max_iter, error
   REAL :: epsilon, start, root
   REAL :: f_val

   PRINT *, "Valor inicial:"
   READ *, start


   epsilon = 0.000001
   max_iter = 100
   CALL Newton_Raphson(f, f_prime, start, epsilon, max_iter, root, error)

   f_val = f(root)
   PRINT '(2(A,E15.6))',"Raiz encontrada em x=", root,"  f(x) =", f_val
   pause


END PROGRAM NewtonRaphson

REAL FUNCTION f(x)
   IMPLICIT NONE
   REAL, INTENT(IN) :: x
   f= 63*(x**3) - 183*(x**2) + 97*x + 55

END FUNCTION f

REAL FUNCTION f_prime(x)
   IMPLICIT NONE
   REAL, INTENT(IN) :: x
   f_prime = 189*(x**2) - 372*x + 97

END FUNCTION f_prime


! ***************************************************************************

SUBROUTINE Newton_Raphson(f, f_prime, start, epsilon, max_iter, root, error)

   IMPLICIT NONE

   REAL, EXTERNAL :: f, f_prime
   REAL, INTENT(IN) :: start, epsilon
   INTEGER, INTENT(IN) :: max_iter

   REAL, INTENT(INOUT) :: root
   INTEGER, INTENT(OUT) :: error

   INTEGER :: i
   REAL :: f_val, f_der


   IF(epsilon <= 0.0) THEN
     error = -3
     root = HUGE(root)
     RETURN
   END IF


   root = start

   DO i = 1, max_iter
      f_val = f(root)
      IF(ABS(f_val ) <= epsilon) THEN

        error = 0
        RETURN
      END IF
      f_der = f_prime(root)
      IF(f_der == 0.0) THEN

        error = -2
        RETURN
      END IF

      root = root - f_val/f_der

   END DO

   error = -1

END SUBROUTINE Newton_Raphson


