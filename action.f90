!Functions
module functions
    use converter
    implicit none
contains
        real function action_equation(lattice, dim, size, params) result(return_value)
        integer:: dim, size, j   
        real, dimension(size ** dim) :: lattice
        real, dimension(2) :: params 
        real :: tot_action, action_at_a_point !neeed to make consts something more realistic
        integer :: left, right, top, bottom

        write(*,*) "Calculating action:"
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
end module functions



!Main Program
program action
    use functions 
    use converter 
    use lattice_maker
    use helper

    implicit none
    integer, parameter ::  dim = 2, size = 100, array_length = size ** dim
    integer :: i 
    real :: action_value
    real, dimension(:), allocatable :: flattened_lattice
    real, dimension(:), allocatable :: x,y
    !hello, Margit
    real, dimension(2) :: params
    params(1) = 1 
    params(2) = 2 


    call lattice_make(dim, size, flattened_lattice)
    call write_lattice_to_file(dim, size, flattened_lattice)
    action_value = action_equation(flattened_lattice,dim,size,params)
   


    write(*, "('Total action:' F100.9)") action_value

    x = return_normal(2)
    y = return_normal(2)

    write(*,*) x+y

  end program action