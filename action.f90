!Functions
module functions
    implicit none
contains
        real function lagrangian_equation(lattice, arr_length, dim, sub_dim) result(return_value)
        integer :: arr_length, dim, sub_dim, j
        real, dimension(arr_length) :: lattice
        real :: tot_legragian, partial_legrangian, const_1 = 1, const_2 = 1

        write(*,*) "Calculating Lagrangian:"
        partial_legrangian = -1 * lattice(j) * (lattice(mod(j,sub_dim) - 1) + lattice(mod(j,sub_dim) + 1)) & 
        + const_1 * lattice(j) ** 2 + const_2 * lattice(j) ** 4
        tot_legragian = tot_legragian + partial_legrangian 
        return_value = tot_legragian
    end function lagrangian_equation      
end module functions



!Main Program
program legrangian
    use functions 
    use converter 
    use lattice_maker
    
    implicit none
    integer, parameter ::  dim = 3, sub_dim = 50, arr_length = sub_dim ** dim !dim (e.g. n x n for dim = 2, n x n x n...), sub_dim = n, length of array = dim ^ sub_dim
    real, dimension(arr_length) :: arr     ! Array to hold random values
    integer :: i
    real :: lagrangian_value
    integer, dimension(:), allocatable :: dimension_array
    real, dimension(:), allocatable :: flattened_lattice

    
  
    ! Seed the random number generator
    call random_seed()
  
    ! Generate random values for the array
    call random_number(arr)
  
    ! Print the array
    write(*,*) "Random Array:"
    do i = 1, arr_length
       write(*,"(I2, ': ', F200.4)") i, arr(i)

    end do
    call lattice_make(2, dimension_array, flattened_lattice)
    lagrangian_value = lagrangian_equation(arr, arr_length, dim, sub_dim)

    write(*, "('Total Legrangian: ' F90.6)") lagrangian_value

    

  
  end program legrangian