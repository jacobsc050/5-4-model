module lattice_maker
    implicit none 
    contains
    subroutine lattice_make(N, myArray,lattice)
        !For an Nth dimensional lattice
        integer, intent(in) :: N 
    
        integer :: x,latticeCount,index,i
        integer, dimension(:), intent(out), allocatable :: myArray
        real, dimension(:),  intent(out),  allocatable :: lattice

        real :: y
        
        
        
        ! Allocate memory for the array
        allocate(myArray(N))
        
        ! Assign values to the integers
        do i = 1, N
        write(*,*) "Enter the N value for dimension ", i, ": "
        read(*,*) myArray(i)
        end do
        
        ! Display the values of the integers
        write(*,*) "The size (N value) of the dimensions are:"
        do i = 1, N
        write(*,*) "Dimension ", i, ": ", myArray(i)
        end do
        
        ! Calculate the required array index size for the 1 dimensional array indexing
        latticeCount = 1
        do i = 1, N
        latticeCount = latticeCount * myArray(i)
        end do
        Write(*,*) "Total latice index size is ", latticeCount,"."

        ! Allocate the memory for our lattice matrix
        latticeCount = latticeCount - 1
        allocate(lattice(latticeCount))

        !~~~ I know fortrans RNG is bad, this is placeholder for now ~~~
        ! (sorry paul :) ) 

        do i = 0, latticeCount
            call random_number(y) 
            lattice(i) = y * 10
        end do 

    
        ! Determine print format and then print out randomly generated numbers 
        if(N == 2) then
            ! print out formatted numbers (only works to 2d)
            index = 0
            do i = 1, myArray(1)
                do x = 1, myArray(2)
                    write(*, '(A, F10.5, A)',advance='no' ) " ", lattice(index), ", "
                    index = index + 1
                end do
                write(*,*)
            end do
        else 
            ! Print out the randomly generated numbers in 1D format
            write(*,*) "Randomly generated numbers in 1d format"
            do i = 0, latticeCount
            write(*,*)  i ,":", lattice(i)
            end do
        end if
    end subroutine lattice_make

end module lattice_maker 