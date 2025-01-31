program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  use rndgen_mod
  use m_emq
  use m_uni
  use m_vizinho
  implicit none

  integer(kind=i4) :: seed = 1!213123
  type(rndgen) :: gerador

  integer(kind=i4) :: arquivo1, arquivo2
  integer(kind=i4) :: d, N, k, i, j, l, c, den, l2, c2, Nmax, m
  integer(kind=i4), allocatable :: rede2d(:,:), parcial2d(:,:)
  integer(kind=i8) :: t, tsoma, soma1
  real(kind=sp) :: r0, r1, ang
  real(kind=dp) :: tmed, prob1
  
  d = 2
  Nmax = 15

  allocate(rede2d(0:(Nmax-1),0:(Nmax-1)))
  allocate(parcial2d(0:(Nmax-1),0:(Nmax-1)))

  call gerador%init(seed)


  open(newunit=arquivo1, file='dados.dat')
  open(newunit=arquivo2, file='dados2.dat')

  do N = 5, Nmax
    tsoma = 0

    do den = 1, (N ** 2 - 1)
      soma1 = 0
      do k = 1, 20
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
          do i = 0, (N-1)
            do j = 0, (N-1)
              parcial2d(i,j) = vizinho(rede2d,N,i,j, gerador)
            end do
          end do
          rede2d = parcial2d
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

    tmed = tsoma / ((N**2 - 1) * 20.0)

    write(arquivo1,*) tmed

  end do

  close(arquivo1)
end program main
