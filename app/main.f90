program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  use rndgen_mod
  use m_emq
  use m_uni
  implicit none

  integer(kind=i4) :: seed = 1!213123
  type(rndgen) :: gerador

  integer(kind=i4) :: arquivo1, arquivo2
  integer(kind=i4) :: d, N, i, l, c, den, l2, c2, Nmax, m
  integer(kind=i4), allocatable :: rede1d(:), rede2d(:,:), rede3d(:,:,:), dados(:,:)
  integer(kind=i8) :: t, tsoma, soma1
  real(kind=sp) :: r0, r1, ang
  real(kind=dp) :: tmed, prob1
  
  d = 2
  Nmax = 12

  allocate(rede2d(0:(Nmax-1),0:(Nmax-1)))

  call gerador%init(seed)

  open(newunit=arquivo1, file='dados.dat')
  write(arquivo1,*) "N  t_med"
  open(newunit=arquivo2, file='dados2.dat')

  do N = 1, Nmax
    tsoma = 0

    do den = 0, N ** 2
      soma1 = 0
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
        if ( rede2d(0,0) .eq. 1 ) then
          soma1 = soma1 + 1  
        end if
      end do
      tsoma = tsoma + t
      prob1 = soma1 / 20.0
      write(arquivo2,*) N, den, prob1
    end do

    tmed = tsoma / ((N**2 + 1) * 20.0)

    write(arquivo1,*) N, tmed

  end do

  close(arquivo1)
end program main
