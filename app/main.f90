program main
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
  ! Chama os módulos que serão utilizados
  use rndgen_mod
  use m_emq
  use m_uni
  use m_vizinho
  use m_round
  implicit none

  ! Prepara o gerador de números aleatórios
  integer(kind=i4) :: seed = 1!213123
  type(rndgen) :: gerador

  ! Declara as variáveis
  integer(kind=i4) :: arquivo1, arquivo2
  integer(kind=i4) :: N, i, l, c, q, l2, c2, Nmax, m
  integer(kind=i4), allocatable :: rede1d(:), rede2d(:,:), rede3d(:,:,:), dados(:,:)
  integer(kind=i8) :: t, tsoma, soma1
  real(kind=sp) :: r0, r1, ang
  real(kind=dp) :: tmed, prob1, den
  
  ! Número máximo da ordem da matriz
  Nmax = 20

  ! Aloca a matriz
  allocate(rede2d(0:(Nmax-1),0:(Nmax-1)))

  ! Chama o gerador de números aleatórios
  call gerador%init(seed)

  ! Abre os arquivo de dados
  open(newunit=arquivo1, file='dados.dat')
  open(newunit=arquivo2, file='dados2.dat')

  ! Realiza um loop para cada ordem da matriz, de N = 2 até N = Nmáx
  do N = 2, Nmax
    ! Estabelece um contador para o tempo de consenso
    tsoma = 0

    ! Estabelece um loop para a densidade de opiniões +1, de 0 até N**2
    do den = 0, N ** 2
      ! Estabele um contador para o número de vezes que o consenso será +1
      soma1 = 0
      ! Realiza o código 100 vezes, para reduzir o erro
      do i = 1, 100
        ! Inicia a rede toda com elementos -1
        rede2d = -1

        ! estabele um contador de opiniões +1
        m = 0

        ! Loop que acontece até as opiniões +1 serem igual sua densidade
        do while (m < den)
          ! Escolhe aleatóriamente o elemento que será +1
          l = int(N * gerador%rnd())
          c = int(N * gerador%rnd())

          ! Se esse elemento não for +1, transforma-se em +1 e adiciona-se uma unidade ao contador
          if (rede2d(l,c) .ne. 1) then
            rede2d(l,c) = 1
            m = m + 1
          end if
        end do

        ! zera o contador do número de iterações
        t = 0

        ! Realiza o loop enquanto não for obtido o consenso
        do while (uni(rede2d,N) .eqv. .false.)
          ! Escolhe aleatoriamente um elemento da matriz
          l = int(N * gerador%rnd())
          c = int(N * gerador%rnd())

          ! Aquele elemento toma o valor de um de seus vizinhos
          rede2d(l,c) = vizinho(rede2d,N,l,c, gerador)
          ! Uma unidade é adicionada ao contador de iterações
          t = t + 1
        end do
        ! Se o consenso for +1
        if ( rede2d(0,0) .eq. 1 ) then
          soma1 = soma1 + 1  
        end if
      end do
      ! É somado o número de iterações para o consenso
      tsoma = tsoma + t
      ! É calculado a probabilidade do consenso
      prob1 = soma1 / 100.0
      write(arquivo2,*) N, den, prob1
    end do

    ! É calculado o tempo médio para aquela ordem
    ! Dividi-se pelo número de densidades utilizadas (N**2 + 1) multiplicado pelo número de iterações de cada densidade, 100.
    tmed = tsoma / ((N**2 + 1) * 100.0)

    !Escreve o tempo médio no arquivo
    write(arquivo1,*) tmed

  end do

  ! Fecha os arquivos
  close(arquivo1)
  close(arquivo2)
end program main
