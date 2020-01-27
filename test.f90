module test
implicit none
private
public fib

contains

! Fibonacci function
integer recursive function fib(n) result(r)
    integer, intent(in) :: n
    if (n < 2) then
        r = n
    else
        r = fib(n-1) + fib(n-2)
    end if
end function  ! end of Fibonacci function
end module

program fibonacci
use test, only: fib
implicit none
integer :: r,i 
integer :: n = 1e09
real(8) :: start, finish, cum_time

cum_time=0
do i= 1,n 
    call cpu_time(start)
    r = fib(20)
    call cpu_time(finish) 
    cum_time = cum_time + (finish - start)
    if (cum_time >0.5) exit
enddo  

print*,i,'runs, average elapsed time is', cum_time/i/1e-06, 'us'
read(*,*)
end program

