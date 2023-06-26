module helper
    use converter 
    implicit none
    real :: PI = 4.0*atan(1.0)
    contains
    !Seems like there is no error for now
    subroutine sum_neighbor(data, res)  !this function returns an array the same dimension of data that has the sum of neighbors
        
        real, intent(in) :: data(:)
        real, intent(out), allocatable :: res(:)
        integer :: N, i, left, right, top, bottom

        N = size(data)
        allocate(res(N))

        do i=1,N
            call nearestNeighbors(i, left, right, top, bottom)
            res(i) = data(left)+data(right)+data(top)+data(bottom)
        end do 

    end subroutine 


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

    function evaluate_x_deriv(x_data, params) result(result) !returns the derivative of the Hamiltonian w.r.t. position, array of the length N
        
        real, allocatable :: neighbor(:)   
        real:: x_data(:)
        real, allocatable :: result(:)

        real :: params(2)
        integer :: N,i 
        N = size(x_data)
        allocate(neighbor(N))
        allocate(result(N))


        call sum_neighbor(x_data, neighbor)

         do i =1,N
             result(i) = -2*params(1)*neighbor(i)+2*x_data(i)+4*params(2)*(x_data(i)*x_data(i)-1)*x_data(i)
         end do 

         deallocate(neighbor)

    end function evaluate_x_deriv
     
    function leapfrog_update(flattened_lattice, dim, sizes, params) result(new_lattice)

        real, dimension(:) :: flattened_lattice
        real, dimension(2) :: params
        real, allocatable :: new_lattice(:)
        integer :: i, dim, sizes
        real, dimension(:), allocatable :: momentum 

        integer :: N
        real :: time_step = 0.005 !set the size of the timesteps. 

        allocate(new_lattice(sizes**dim))
        allocate(momentum(sizes**dim))

        momentum = return_normal(sizes**dim)

        !this block runs over one leap frog update 
        new_lattice = flattened_lattice+(time_step/2)*momentum

        momentum = new_lattice -time_step*evaluate_x_deriv(new_lattice, params)
        
        new_lattice = new_lattice + (time_step/2)*momentum 

        deallocate(momentum)

    end function leapfrog_update

end module helper

! program ex1
!     use helper
!     implicit none 
!     real, dimension(2) :: x, y

!     x = return_normal(2)
!     y = return_normal(2)
    
!     write(*,*) x+y

! end program 