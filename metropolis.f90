module helper
    implicit none
    real :: PI = 4.0*atan(1.0)
    contains

<<<<<<< HEAD
    function sum_neighbor(data) result(res)  !this function returns an array the same dimension of data that has the sum of neighbors
        
        real, allocatable :: data(:)
        real, allocatable :: neighbor_list(:)
        real, allocatable :: result(:)   
        integer :: i, j 
        

        call find_neighbor(data, neighbor_list)  ! I need find_neighbor as a subroutine 

        array_loop:do i = 1, size(data)
            
            neighbor_loop:do j = 1, size(neighbor_list(2))
                result(i) += data(neighbor_list(i,j))
            end do neighbor_loop

        end do array_loop

    end function 
=======
    ! !this is an empty function 
    ! subroutine sum_neighbor(data,result)  !this function returns an array the same dimension of data that has the sum of neighbors
        
    !     real, intent(in) :: data(:)
    !     real, intent(out) :: result(:)    
    
    !     result(1:3)= 1 !fix this later 

    ! end subroutine sum_neighbor
>>>>>>> 3a6c9d99455dc0241087871a55c8e2fd8f216dc9


    !this works 
    function return_normal(N) result(res) !returns an array of N normally distributed number using Box-Muller
        integer :: N, i 
        real, dimension(:), allocatable :: res 
        real, dimension(:), allocatable :: x

        allocate(res(N))
        allocate(x(N))

        do i = 1,N
            call random_number(x)   !This puts random numbers in the declared array, x
            res(i) = sqrt(-2*log(x(1)))*cos(2*PI*x(2)) ! The box muller formula
        end do 
    end function return_normal

<<<<<<< HEAD
    function evaluate_x_deriv(x_data, params) result(res) !returns the derivative of the Hamiltonian w.r.t. position, array of the length N
        
        real, allocatable :: neighbor(:)   
        real, allocatable :: x_data(:)
        real, allocatable :: res(:)
=======
    ! function evaluate_x_deriv(x_data, params, result) result(result) !returns the derivative of the Hamiltonian w.r.t. position, array of the length N
        
    !     real, allocatable :: neighbor(:)   
    !     real, allocatable :: x_data(:)
    !     real, allocatable :: result(:)
>>>>>>> 3a6c9d99455dc0241087871a55c8e2fd8f216dc9

    !     real :: params(2)
    !     integer :: N,i 

<<<<<<< HEAD
        N = size(x_data)
        call sum_neighbor(x_data, neighbor)

        do i=1,N
            result(i) = -2*params(1)*neighbor(i)+2*x_data(i)+4*params(2)*(x_data(i)*x_data(i)-1)*x_data(i)
        end do 

    end function

    subroutine leapfrog_update(data, params, proposal) !can add some output for energy that shows whether we are on a phase space trajectory
=======

    !     N = size(x_data)
    !     call sum_neighbor(x_data, neighbor)

    !     do i =1,N
    !         result(i) = -2*params(1)*neighbor(i)+2*x_data(i)+4*params(2)*(x_data(i)*x_data(i)-1)*x_data(i)
    !     end do 
>>>>>>> 3a6c9d99455dc0241087871a55c8e2fd8f216dc9

    ! end function evaluate_x_deriv

    ! subroutine leapfrog_update(data, params, proposal)

    !     real, intent(in) :: data(:)
    !     real, intent(in) :: params(2)
    !     real, intent(out) :: proposal(:)

    !     integer :: N = size(data)
    !     real :: time_step = 0.005 !set the size of the timesteps. 
    !     real, dimension(:), allocatable :: neighbor
    !     real, dimension(:), allocatable :: momentum 
    !     real, dimension(:), allocatable :: update_step
    !     integer :: i 

    !     allocate(momentum_sample(N))
    !     momentum = return_normal(N)


    !     !this block runs over one leap frog update 
    !     proposal = data+time_step/2*momentum
    !     call evaluate_x_deriv(proposal, momentum)

    !     momentum = proposal -time_step*momentum
    !     call evaluate_p_deriv(momentum,update_step)

<<<<<<< HEAD
!just for me to test things g
program ex1
    use helper
    implicit none 
    real, dimension(2) :: x, y

    x,y = return_normal(2), return_normal(2)
    
    write(*,*) x+y
=======
    !     proposal = proposal +time_step/2*update_step
    ! end subroutine leapfrog_update
>>>>>>> 3a6c9d99455dc0241087871a55c8e2fd8f216dc9

end module helper