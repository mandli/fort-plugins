! =============================================================================
module random_module

    implicit none
    
contains

    subroutine init_random_seed()

        implicit none
        integer :: i, n, clock
        integer, dimension(:), allocatable :: seed

        call random_seed(size = n)
        allocate(seed(n))

        call system_clock(count=clock)

        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        call random_seed(put = seed)

        deallocate(seed)
    end subroutine

end module random_module


! =============================================================================
module preprocessor_module

contains

    subroutine matrix_multiply_preprocessor(A, B, C)

        implicit none

        real(kind=8), intent(in) :: A(:, :), B(:, :)
        real(kind=8), intent(in out) :: C(:, :)
        
        ! BLAS routine
        external DDOT
        real(kind=8) :: DDOT

        ! Locals
        integer :: i, j, k, N

        N = size(A, 1)

        do i=1,N
            do j=1,N
#ifndef USE_BLAS
                do k=1,N
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                enddo
#else
                C(i, j) = DDOT(N, A(i, :), 1, B(:, j), 1)
#endif
            enddo
        enddo

    end subroutine matrix_multiply_preprocessor

end module preprocessor_module

! =============================================================================
module pointer_module

    implicit none

    abstract interface
        subroutine matrix_multiply(A, B, C)
            implicit none
            real(kind=8), intent(in) :: A(:, :), B(:, :)
            real(kind=8), intent(in out) :: C(:, :)
        end subroutine matrix_multiply
    end interface

    abstract interface
        real(kind=8) pure function multiply(a, b) result(c)
            implicit none
            real(kind=8), intent(in) :: a, b
        end function multiply
    end interface

    procedure (matrix_multiply), pointer :: func_matrix_multiply
    procedure (multiply), pointer :: func_multiply

contains

    subroutine set_pointer(method)

        implicit none

        integer, intent(in) :: method

        select case(method)
            case(1)
                func_matrix_multiply => matrix_multiply_non_blas
            case(2)
                func_matrix_multiply => matrix_multiply_blas
        end select 
        func_multiply => reg_multiply

    end subroutine set_pointer


    subroutine matrix_multiply_non_blas(A, B, C)
            
        implicit none
        real(kind=8), intent(in) :: A(:, :), B(:, :)
        real(kind=8), intent(in out) :: C(:, :)

        ! Locals
        integer :: i, j, k, N

        N = size(A, 1)

        do i=1,N
            do j=1,N
                do k=1,N
                    C(i, j) = C(i, j) + func_multiply(A(i, k), B(k, j))
                enddo
            enddo
        enddo

    end subroutine matrix_multiply_non_blas

    subroutine matrix_multiply_blas(A, B, C)
            
        implicit none

        real(kind=8), intent(in) :: A(:, :), B(:, :)
        real(kind=8), intent(in out) :: C(:, :)

        
        ! BLAS routine
        external DDOT
        real(kind=8) :: DDOT

        ! Locals
        integer :: i, j, N

        N = size(A, 1)

        do i=1,N
            do j=1,N
                C(i, j) = DDOT(N, A(i, :), 1, B(:, j), 1)
            enddo
        enddo

    end subroutine matrix_multiply_blas

    real(kind=8) pure function reg_multiply(a, b) result(c)
        implicit none
        real(kind=8), intent(in) :: a, b

        c = a * b
    end function reg_multiply


end module pointer_module


! =============================================================================
!  Test for if-statements to set methods
module branching_module

    implicit none

contains
    
    subroutine matrix_multiply_branching(A, B, C, method)

        implicit none

        real(kind=8), intent(in) :: A(:, :), B(:, :)
        real(kind=8), intent(in out) :: C(:, :)
        integer, intent(in) :: method
        
        ! BLAS routine
        external DDOT
        real(kind=8) :: DDOT

        ! Locals
        integer :: i, j, k, N

        N = size(A, 1)

        do i=1,N
            do j=1,N
                if (method == 1) then
                    do k=1,N
                        C(i, j) = C(i, j) + A(i, k) * B(k, j)
                    enddo
                else if (method == 2) then
                    C(i, j) = DDOT(N, A(i, :), 1, B(:, j), 1)
                end if
            enddo
        enddo

    end subroutine matrix_multiply_branching

end module branching_module


! =============================================================================
!  Main code
program plugin_test

    use random_module
    use preprocessor_module
    use pointer_module, only: func_matrix_multiply, set_pointer
    use branching_module

    implicit none

    integer :: N, M
    character(len=10) :: input

    integer :: i,j
    real(kind=8), allocatable :: A(:, :), B(:, :), C(:, :)
    integer(kind=8) :: start, finish, count_rate
    real(kind=8) :: time

    N = 100
    M = 100
    if (iargc() == 1) then
        call getarg(1, input)
        read(input, '(i10)') N
    else if (iargc() == 2) then
        call getarg(1, input)
        read(input, '(i10)') N
        call getarg(2, input)
        read(input, '(i10)') M
    end if

    ! Construct random matrices
    call init_random_seed()
    allocate(A(N, N), B(N, N), C(N, N))
    call random_number(A)
    call random_number(B)

    print '("Using",i5," x",i5," matrices")', N, N
    print '("Running each function",i8," times.")', M

    C = 0.0
    call system_clock(start, count_rate)
    do j=1, M
        call matrix_multiply_preprocessor(A, B, C)
    end do
    call system_clock(finish, count_rate)

    time = float(finish - start) / float(count_rate)
#ifndef USE_BLAS
    print '("Preprocessor 1: ",f16.10," s")', time
#else
    print '("Preprocessor 2: ",f16.10," s")', time
#endif

    do i=1, 2
        C = 0.0
        call system_clock(start, count_rate)
        call set_pointer(i)
        do j=1, M
            call func_matrix_multiply(A, B, C)
        end do
        call system_clock(finish, count_rate)

        time = float(finish - start) / float(count_rate)
        print '("Pointer ",i1,": ",f16.10," s")', i, time
    end do

    C = 0.0
    call system_clock(start, count_rate)
    do j=1, M
        call matrix_multiply_branching(A, B, C, 1)
    end do
    call system_clock(finish, count_rate)

    time = float(finish - start) / float(count_rate)
    print '("Branching 1: ",f16.10," s")', time

    C = 0.0
    call system_clock(start, count_rate)
    do j=1, M
       call matrix_multiply_branching(A, B, C, 2)
    end do
    call system_clock(finish, count_rate)

    time = float(finish - start) / float(count_rate)
    print '("Branching 2: ",f16.10," s")', time

end program plugin_test