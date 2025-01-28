program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  implicit none

  integer(kind=i8) :: d, N, i, m
  integer(kind=i4), allocatable :: rede1d(:), rede2d(:,:), rede3d(:,:,:), dados(:,:)
  
  d = 2
  N = 10

  allocate(rede2d(0:(N-1),0:(N-1)))

  allocate(dados(0:1,0:(N-1)))

  rede2d = -1

  !do m = 1, (N-1)
  !  
  !end do
end program main
