program check
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none

    integer(kind=i8) :: N, i
    integer(kind=i4), allocatable :: rede(:,:) 
    real(kind=sp) :: teste

    teste = 2 * asin(1.0)

    print*, teste
end program check
