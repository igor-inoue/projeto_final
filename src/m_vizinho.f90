module m_vizinho
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    use rndgen_mod
    use m_round
    implicit none
    
contains
    
    function vizinho(matriz, N, l, c, gerador) result(resultado)
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4) :: l, c, l2, c2, N, resultado
        type(rndgen) :: gerador
        real(kind=i8) :: ang

        l2 = -1
        c2 = -1

        do while (l2 < 0 .or. l2 > (N - 1) .or. c2 < 0 .or. c2 > (N - 1))
            ang = round(4 * gerador%rnd()) * asin(1.0)

            l2 = l + round(cos(ang))
            c2 = c + round(sin(ang))
        end do

        resultado = matriz(l2,c2)
        
    end function vizinho
end module m_vizinho