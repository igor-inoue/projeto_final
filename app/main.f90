program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  use rndgen_mod
  use m_emq
  implicit none

  integer(kind=i4) :: seed = 213123
  type(rndgen) :: gerador

  integer(kind=i4) :: d, N, m, l, c, ang, den
  integer(kind=i4), allocatable :: rede1d(:), rede2d(:,:), rede3d(:,:,:), dados(:,:)
  real(kind=sp) :: r0, r1
  
  d = 2
  N = 10
  den = 3

  allocate(rede2d(0:(N-1),0:(N-1)))

  allocate(dados(0:1,0:(N-1)))

  rede2d = -1

  call gerador%init(seed)

  m = 0

  do while (m < den)
    l = int(N * gerador%rnd())
    c = int(N * gerador%rnd())

    if (rede2d(l,c) .ne. 1) then
      rede2d(l,c) = 1
      m = m + 1
    end if
  end do

  call emq(rede2d, N)

  do m = 0, (N-1)
    l = int(N * gerador%rnd())
    c = int(N * gerador%rnd())

    ang = int(4 * gerador%rnd())

    rede2d(l,c) = rede2d(l + cos(ang * asin(1.0)), c + sin(ang * asin(1.0)))

    print*, l, c, rede2d(l,c)
  end do

  call emq(rede2d, N)
end program main
