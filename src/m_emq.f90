module m_emq
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
    
contains
    
    ! declara subrotina emq = escreve matriz quadrada
    subroutine emq(matriz,  D)
        ! Recebe uma matriz quadrada de dimensão D e a escreve no terminal
        
        ! Declara a matriz, suas dimensões e uma variável m
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4), intent(in) :: D
        integer(kind=i4) :: m

        ! Realiza um loop na variável m em todas as linhas da matriz
        do m = 0, D-1
            ! imprime a linha m da matriz
            print*, matriz(m,:)
        end do
    end subroutine emq
end module m_emq
