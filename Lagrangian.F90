!Functions 
module functions
    implicit none
contains
        real function lagrangian_equation(lattice, arr_length, dim, sub_dim) result(return_value)
        integer :: arr_length, dim, sub_dim, i, j, k, sub_dim_counter = 0, sub_dim_shift, dim_shift, dim_counter
        real, dimension(arr_length) :: lattice
        real :: tot_legragian, partial_legrangian, const_1 = 1, const_2 = 1

        write(*,*) "Calculating Lagrangian:"
        !k indexes the dimension you're summing over
    
        do k = 1, dim 
            if (dim_counter==0) then
                dim_shift = dim_counter
            else
                dim_shift = sub_dim ** k
            end if 
            !i is sub_dim index (this is a dimension (think of as a maxtrix))
            do i = 1, sub_dim
                sub_dim_shift = sub_dim_counter * sub_dim 
                !write(*,"(I2, ': ', I2)") i, sub_dim_shift
                !imgine this as the jth row of this matrix 
                do j = 1, sub_dim
                    
                    !write(*,"(I2000, ': 'I2000, ': ', I2000)") j + sub_dim_shift, mod(j,sub_dim) + 1  + sub_dim_shift, dim_shift
                    partial_legrangian = -1 * lattice(j + sub_dim_shift) * (lattice(mod(j,sub_dim) - 1 + sub_dim_shift) + lattice(mod(j,sub_dim) + 1 + sub_dim_shift)) + const_1 * lattice(j + sub_dim_shift) * 2 + const_2 * lattice(j + sub_dim_shift) ** 4
                    tot_legragian = tot_legragian + partial_legrangian
                    !write(*,"(I2, ': ', F8.6, ': ', F20.6)") j, partial_legrangian,tot_legragian
                    
                end do 
                
                sub_dim_counter = sub_dim_counter + 1 
            end do 
            sub_dim_counter = 0
            dim_counter = dim_counter + 1 
        end do 
        return_value = tot_legragian
    end function lagrangian_equation      
end module 



!Main Program
program legrangian
    use functions
    implicit none
    integer, parameter ::  dim = 3, sub_dim = 50, arr_length = sub_dim ** dim !dim (e.g. n x n for dim = 2, n x n x n...), sub_dim = n, length of array = dim ^ sub_dim
    real, dimension(arr_length) :: arr     ! Array to hold random values
    integer :: i
    real :: lagrangian_value
    
  
    ! Seed the random number generator
    call random_seed()
  
    ! Generate random values for the array
    call random_number(arr)
  
    ! Print the array
    write(*,*) "Random Array:"
    do i = 1, arr_length
       write(*,"(I2, ': ', F200.4)") i, arr(i)

    end do

    lagrangian_value = lagrangian_equation(arr, arr_length, dim, sub_dim)

    write(*, "('Total Legrangian: ' F90.6)") lagrangian_value

    

  
  end program legrangian



