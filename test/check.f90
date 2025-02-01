program check
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    use m_uni
    use m_emq
    implicit none

    integer(kind=i4), allocatable :: rede2d(:,:)
    integer(kind=i4) :: N

    N = 10

    allocate(rede2d(0:(N-1),0:(N-1)))
  
    rede2d = -1

    call emq(rede2d,N)
    print*, uni(rede2d, N)
end program check
