module m_round
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
    
contains
    
    function round(x) result(y)
        real(kind=dp) :: x
        integer(kind=i8) :: y

        real(kind=dp) :: resto

        if (x > 0) then
            resto = x - int(x)

            if (resto < 0.5) then
            y = int(x)
            else
                y = int(x) + 1
            end if
        else
            resto = int(x) - x
            if (resto < 0.5) then
                y = int(x)
            else
                y = int(x) - 1
            end if
        end if 
    end function
end module m_round