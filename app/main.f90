program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  use rndgen_mod
  use m_emq
  use m_uni
  implicit none

  integer(kind=i4) :: seed = 213123
  type(rndgen) :: gerador

  integer(kind=i4) :: d, N, i, l, c, den, l2, c2
  integer(kind=i4), allocatable :: rede1d(:), rede2d(:,:), rede3d(:,:,:), dados(:,:)
  integer(kind=i8) :: t, tsoma
  real(kind=sp) :: r0, r1, ang
  real(kind=dp) :: tmed
  
  d = 2
  N = 10

  allocate(rede2d(0:(N-1),0:(N-1)))

  allocate(dados(0:1,0:(N-1)))

  call gerador%init(seed)

  tsoma = 0

  do den = 0, N ** 2
    do i = 1, 20
      rede2d = -1

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

      do while (uni(rede2d,N) .eqv. .false.)
        l = int(N * gerador%rnd())
        c = int(N * gerador%rnd())

        l2 = -1
        c2 = -1

        do while (l2 < 0 .or. l2 > (N - 1) .or. c2 < 0 .or. c2 > (N - 1))
          ang = int(4 * gerador%rnd()) * asin(1.0)

          l2 = l + int(cos(ang))
          c2 = c + int(sin(ang))
        end do
        rede2d(l,c) = rede2d(l2,c2)
        t = t + 1
      end do
    end do
    tsoma = tsoma + t
  end do

  tmed = tsoma / ((N**2 + 1) * 20)

  print*, N, tmed
end program main
