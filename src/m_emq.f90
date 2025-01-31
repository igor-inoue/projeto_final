module m_emq
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
    
contains
    
    subroutine emq(matriz,  D)
        integer(kind=i4), allocatable, intent(in) :: matriz(:,:)
        integer(kind=i4), intent(in) :: D
        integer(kind=i4) :: m

        do m = 0, D-1
            print*, matriz(m,0:(D-1))
        end do
    end subroutine emq
end module m_emq
