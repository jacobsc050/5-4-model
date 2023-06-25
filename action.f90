!Functions
module functions
    use converter
    use iso_fortran_env
    implicit none
contains
        real function action_equation(lattice, dim, size, params) result(return_value)
        integer:: dim, size, j   
        real, dimension(size ** dim) :: lattice
        real, dimension(2) :: params 
        real :: tot_action, action_at_a_point !need to make consts something more realistic
        integer :: left, right, top, bottom
        !hello
        !write(*,*) "Calculating action:"
        if(dim==2) then 
            do j = 1,size**dim
                call nearestNeighbors(j, left, right, top, bottom)
                action_at_a_point = -1 * lattice(j) * (lattice(top) + lattice(bottom) + lattice(left) + lattice(right)) &
                + params(1)* lattice(j) ** 2 + params(2) * lattice(j) ** 4
                tot_action = tot_action + action_at_a_point 
                return_value = tot_action
            end do 
        end if
    end function action_equation   
    
    function metropolis_hastings(flattened_lattice, dim, sizes, params) result(new_lattice)
            real, dimension(:) :: flattened_lattice
            real, dimension(2) :: params
            real, allocatable :: new_lattice(:)
            integer :: i, dim, sizes
            double precision::  flattened_action, new_action
            real :: random_num, mini, accept_prob

            allocate(new_lattice(size(flattened_lattice)))
            new_lattice = flattened_lattice
            do i = 1, size(flattened_lattice)
                !write(*,*) i
                call random_number(random_num)
                new_lattice(i) = flattened_lattice(i) + 2.0*random_num - 1.0
                flattened_action = exp( -1 * action_equation(flattened_lattice, dim, sizes, params))
                new_action = exp( -1 * action_equation(new_lattice, dim, sizes, params))
                call random_number(accept_prob)
                !write(*, *)  new_action, flattened_action, new_action/flattened_action, accept_prob
                mini = min(1.0, new_action/flattened_action)
                
                if (accept_prob>mini) then
                    write(*,*) "No Update"
                    new_lattice(i) = flattened_lattice(i)
                
                end if 
                
            end do 

        


    end function metropolis_hastings
end module functions



!Main Program
program action
    use functions 
    use converter 
    use lattice_maker
    use helper
    

    implicit none
    integer, parameter ::  dim = 2, size = 100, array_length = size ** dim
    integer :: i,j
    real :: action_value, probability
    real, dimension(:), allocatable :: flattened_lattice
    real, dimension(:), allocatable :: new_lattice 
    real, dimension(:), allocatable :: x,y
    real, dimension(2) :: params
    character (len=:), allocatable :: filename
    params(1) = 1 ! mass
    params(2) = 1 ! lambda
    filename = "pre_lattice.txt"
   
    call lattice_make(dim, size, flattened_lattice)
    call write_lattice_to_file(dim, size, flattened_lattice, filename)
    ! action_value = action_equation(flattened_lattice,dim,size,params)
    ! probability = exp(-1 * action_value)
    ! write(*, "('Probability:' F10.27)") probability

    do j = 1,10
        write(*,'(A, I0)') "j: ", j 
        flattened_lattice = metropolis_hastings(flattened_lattice, dim, size, params) 
    end do 
    filename = "post_lattice.txt"
    call write_lattice_to_file(dim, size, flattened_lattice, filename)
    


    ! x = return_normal(2)
    ! y = return_normal(2)

    ! write(*,*) x+y

    deallocate(flattened_lattice)
    !deallocate(new_lattice)
    !deallocate(x)
    !deallocate(y)

  end program action