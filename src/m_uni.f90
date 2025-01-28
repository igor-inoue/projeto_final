module m_uni
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none

    integer(kind=i4) :: m
    
contains
    
    function uni(matriz,  D) result(resposta)
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4) :: D, i, j, valor
        logical :: resposta

        valor = matriz(0,0)

        resposta = .true.

        do i = 0, D-1
            do j = 0, D-1
                if (valor .ne. matriz(i,j)) then
                    resposta = .false.
                    exit
                end if 
            end do
        end do
    end function uni
end module m_uni
