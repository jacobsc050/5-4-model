module helper
    use converter 
    use functions
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
            call random_number(x)
            !This puts random numbers in the declared array, x
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

    function find_hamiltonian(momentum, phi, dim, sizes, params) result(hamiltonian)
        real, dimension(:) :: momentum, phi
        integer :: i, dim, sizes
        real, dimension(2) ::params
        real ::hamiltonian
        hamiltonian = 0
        do i=1, size(momentum)
            hamiltonian = hamiltonian + momentum(i)*momentum(i)/2
        end do 
        write(*,*) "momentum sum is given", hamiltonian
        hamiltonian = hamiltonian+action_equation(phi, dim, sizes, params)
    end function find_hamiltonian
     
    subroutine leapfrog_update(flattened_lattice, dim, sizes, params, new_lattice, old_H, new_H)

        real, dimension(:), intent(in):: flattened_lattice
        real, dimension(2), intent(in):: params
        real, allocatable, intent(out):: new_lattice(:)
        real, intent(out):: old_H, new_H

        integer :: i, dim, sizes
        real, dimension(:), allocatable :: momentum 

        integer :: N
        real :: time_step = 0.005 !set the size of the timesteps. 

        allocate(new_lattice(sizes**dim))
        allocate(momentum(sizes**dim))

        momentum = return_normal(sizes**dim)
        old_H = find_hamiltonian(momentum, flattened_lattice, dim, sizes, params)
        write(*,*) "Hamiltonian in update is given", old_H

        !this block runs over one leap frog update 
        new_lattice = flattened_lattice+(time_step/2)*momentum

        momentum = new_lattice -time_step*evaluate_x_deriv(new_lattice, params)
        
        new_lattice = new_lattice + (time_step/2)*momentum 

        new_H =find_hamiltonian(momentum, new_lattice, dim, sizes, params)
        write(*,*) "Hamiltonian after update is given", new_H
        deallocate(momentum)

    end subroutine leapfrog_update     


    subroutine HMC(flattened_lattice, dim, sizes, params, new_lattice, H_val)

        real, dimension(:), intent(in):: flattened_lattice
        real, dimension(2), intent(in):: params
        real, allocatable, intent(out):: new_lattice(:)
        real, intent(out) :: H_val
        integer :: i, dim, sizes
        real ::  old_H, new_H
        real :: mini, accept_prob

        call leapfrog_update(flattened_lattice, dim,sizes, params, new_lattice, old_H, new_H) 
        ! metropolis step 
        mini = min(1.0, exp(-1*(new_H-old_H)))
        call random_number(accept_prob)
        if (accept_prob>mini) then 
            new_lattice = flattened_lattice
            write(*,*) "HMC update failed"
            H_val = old_H
        else
            H_val = new_H
        end if 

    end subroutine HMC 
end module helper

! program ex1
!     use helper
!     implicit none 
!     real, dimension(2) :: x, y

!     x = return_normal(2)
!     y = return_normal(2)
    
!     write(*,*) x+y

! end program 