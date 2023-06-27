
!Main Program
program action
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

    do j = 1,1000
        write(*,'(A, I0)') "j: ", j 
        flattened_lattice = metropolis_hastings(flattened_lattice, dim, size, params) 
    end do 
    filename = "post_lattice.txt"
    call write_lattice_to_file(dim, size, flattened_lattice, filename)

    do j = 1,1000
        write(*,'(A, I0)') "j: ", j 
        action_value = action_equation(flattened_lattice,dim, size, params)
        flattened_lattice = metropolis_hastings(flattened_lattice, dim, size, params) 
        call calculateAndOutputResults(flattened_lattice,size, "output_MH.txt", action_value)
    end do 
        
    


    !x = return_normal(2)
    !y = return_normal(2)

    !write(*,*) x+y

    !deallocate(flattened_lattice)
    !deallocate(new_lattice)
    !deallocate(x)
    !deallocate(y)

    !new_lattice = leapfrog_update(flattened_lattice, dim, size, params) 

  end program action