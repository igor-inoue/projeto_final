module m_vizinho
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    use rndgen_mod
    use m_round
    implicit none
    
contains
    
    ! Declara função que recebe uma matriz quadrada, sua dimensão, dois parâmetros que definem o elemento da matriz e um
    ! gerador de números aleatórios
    function vizinho(matriz, N, l, c, gerador) result(resultado)
        ! Essa função recebe um elemento de uma matriz e retorna o valor de um dos vizinhos dele.

        ! Declara as variáveis
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4) :: l, c, l2, c2, N, resultado
        type(rndgen) :: gerador
        real(kind=i8) :: ang

        ! Define os parâmetros do vizinho fora da matriz, para entrar no loop a seguir

        l2 = -1
        c2 = -1

        ! caso o vizinho se encontre fora da matriz, o loop se mantém
        do while (l2 < 0 .or. l2 > (N - 1) .or. c2 < 0 .or. c2 > (N - 1))
            ! Escolhe aleatóriamente um de 4 ângulos, 0, pi/2, pi e 3pi/2
            ang = round(4 * gerador%rnd()) * asin(1.0)

            ! Cada ângulo define um vizinho unicamente, por meio da seguinte expressão
            l2 = l + round(cos(ang))
            c2 = c + round(sin(ang))
        end do

        ! O resultado é o valor do elemento vizinho
        resultado = matriz(l2,c2)
        
    end function vizinho
end module m_vizinho