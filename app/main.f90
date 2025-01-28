program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  use rndgen_mod
  use m_emq
  use m_uni
  use m_vizinho
  implicit none

  integer(kind=i4) :: seed = 213123
  type(rndgen) :: gerador

  integer(kind=i4) :: d, N, i, l, c, den, l2, c2, m
  integer(kind=i4), allocatable :: rede1d(:), rede2d(:,:), rede3d(:,:,:), dados(:,:)
  integer(kind=i8) :: t
  real(kind=sp) :: r0, r1, ang
  
  d = 2
  N = 10
  den = 45

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

  t = 0

  call emq(rede2d, N)

  do while (uni(rede2d,N) .eqv. .false.)
    l = int(N * gerador%rnd())
    c = int(N * gerador%rnd())

    rede2d(l,c) = vizinho(rede2d,N,l,c)
    t = t + 1
    if (t > 20000) then
      call emq(rede2d,N)
      exit
    end if
  end do
end program main
