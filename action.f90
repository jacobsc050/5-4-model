!Functions
module functions
    implicit none
contains
        real function action_equation(lattice, dim, size) result(return_value)
        integer:: dim, size, j = 1  
        real, dimension(size ** dim) :: lattice
        real :: tot_action, partial_action, const_1 = 1, const_2 = 1
    
        write(*,*) "Calculating action:"
        ! wrong
        partial_action = -1 * lattice(j) * (lattice(mod(j,size) - 1) & 
        + lattice(mod(j,size) + 1)) & 
        + const_1 * lattice(j) ** 2 + const_2 * lattice(j) ** 4
        tot_action = tot_action + partial_action 
        return_value = tot_action
    
    end function action_equation      
end module functions



!Main Program
program action
    use functions 
    use converter 
    use lattice_maker
    
    implicit none
    integer, parameter ::  dim = 2, size = 3
    real :: action_value
    real, dimension(:), allocatable :: flattened_lattice
    
    call lattice_make(dim, size, flattened_lattice)
    action_value = action_equation(flattened_lattice, dim, size)

    write(*, "('Total action: ' F100.9)") action_value
  end program action