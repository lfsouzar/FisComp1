program bisseccao
implicit none
real:: a,b,c,error,f,k=0
error=1.0e-06
write(*,*)"Insira dois n£meros a e b para o intervalo da funá∆o"

10 read(*,*) a,b
15 if (f(a)*f(b) .lt. 0) then
   c=(a+b)/2.0
else
    write(*,*)"Sem Raiz £nica no intervalo, tente novamente"
    goto 10
end if

if (f(a)*f(c) .lt. 0) then
   b=c
else
    a=c
end if

k = k+1
if (abs(b-a) .gt. error) goto 15

write(*,*)"A raiz Ç",c, "para precis∆o de ",error, "com ", k, "iteraá‰es"
pause
end program bisseccao

real function f(x)
implicit none
real::x
f= 63*(x**3) - 183*(x**2) + 97*x + 55
end function f
