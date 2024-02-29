module m
	use iso_fortran_env
	use fftpack, only: dffti, dfftf, dfftb
	use fftpack_kind, only: rk
	implicit none

	contains

	subroutine fourier
	
	implicit none
	real    :: n ,f, pi=acos(-1.)
	integer :: i
	integer, parameter :: M=100
	real(rk), dimension(M) :: r 
	real(rk), dimension(2*M+15) :: wsave, inverse

	print*, "initialisation"	
	
	call dffti(M, wsave)

	open(unit=20, file="cos.txt")
 	! result files to plot with gnuplot
	open(unit=30, file="result.txt")
	open(unit=40, file="result2.txt")
	
	n=0
	
	print*, "filling cos(x)"
	
	do i=1,M
		r(i)=cos(n+2*pi/20)
		n=n+pi/10
		write (20,*) r(i)
	enddo
	
	print*, "transform"
	
	call dfftf(M,r,wsave)
	do i=1,M
		write(30,*) wsave(i)
	enddo
	
	print*, "inverse"
	
	call dfftb(M,r,wsave)
	
	print*, "writing inverse"
	
	do i=1,M
		write(40,*) wsave(i)
	enddo
	
	close(20)
	close(30)
	close(40)
	
	end subroutine
end module 
!compile with gfortran -o ft ft.f90 -ldfftpack -Lfftpack/src -Ifftpack/src
program cos_fourier_transform
	use m
	
	print*, "start"

	call fourier()

	print*, "end"

end program
