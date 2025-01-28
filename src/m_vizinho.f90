module m_vizinho
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    use rndgen_mod
    implicit none

    integer(kind=i4) :: seed_mv = 213123
    type(rndgen) :: gerador_mv
    
contains
    
    function vizinho(matriz, N, l, c) result(resultado)
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4) :: l, c, l2, c2, N, resultado
        real(kind=i8) :: ang

        l2 = -1
        c2 = -1

        do while (l2 < 0 .or. l2 > (N - 1) .or. c2 < 0 .or. c2 > (N - 1))
            ang = int(4 * gerador_mv%rnd()) * asin(1.0)

            l2 = l + int(cos(ang))
            c2 = c + int(sin(ang))
        end do

        resultado = matriz(l2,c2)

        if ( matriz(l2,c2) .ne. matriz(l,c) ) then
            print*, matriz(l,c), matriz(l2,c2)
        end if
        
    end function vizinho
end module m_vizinho