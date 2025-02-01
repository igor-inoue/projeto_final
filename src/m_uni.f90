module m_uni
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
    
contains
    
    ! Declara função uni() que recebe uma matriz quadrada e sua dimensão
    function uni(matriz,  D) result(resposta)
        ! O módulo retorna um valor lógico que indica se a matriz é uniforme ou não

        ! Declara as variáveis
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4) :: D, i, j, valor
        logical :: resposta

        ! Salva um valor da matriz
        valor = matriz(0,0)

        ! Inicialmente a matriz pode ser uniforme
        resposta = .true.

        ! Cria um laço que perocrre todos os elementos da matriz
        do i = 0, D-1
            do j = 0, D-1
                ! Se o primeiro valor for diferente de um dos valores da matriz, ela não será uniforme
                if (valor .ne. matriz(i,j)) then
                    resposta = .false. 
                    exit ! como já sabemos que ela não é uniforme, podemos sair do laço
                end if 
            end do
        end do
    end function uni
end module m_uni
