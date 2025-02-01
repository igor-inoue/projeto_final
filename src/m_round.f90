module m_round
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
    
contains
    
    ! Declara função round()
    function round(x) result(y)
        ! A função recebe um número real x e o arredonda
        
        ! Declaração de variáveis
        real(kind=dp) :: x
        integer(kind=i8) :: y

        real(kind=dp) :: resto


        ! Devido ao fato de int(x) aproximar para a direita {int(-0.9) = 0 ou int(-1.9 = -1)}, é necessário um construir um if para
        ! números negativos e positivos
        if (x > 0) then
            ! Se x é positivo, seu resto é definido como
            resto = x - int(x)

            if (resto < 0.5) then
                y = int(x)
            else
                y = int(x) + 1
            end if
        else
            ! Se x for negativo, o resto é definido como
            resto = int(x) - x
            if (resto < 0.5) then
                y = int(x) ! Exemplo: int(-0.4) = 0, resto = 0 - (-0.4) = 0.4, y = 0
            else
                y = int(x) - 1 ! Exemplo: int(-2.9) = -2, resto = -2 - (-2.9) = 0.9, y = -2 - 1 = -3
            end if
        end if 
    end function
end module m_round