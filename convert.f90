module converter
    implicit none
    
    integer, parameter :: Ndim = 2
    integer, parameter, dimension(1:Ndim) :: N = [100,100]
    integer, parameter :: Nsites = product(N)


    type lattice
        real, allocatable :: field(:)
    end type lattice
    
     contains

     function coordinatesToIndex(i,j) result(index)
            integer, intent(in) :: i,j
            integer :: index

             ! i,j = (1,1) --> index = 1
             ! i,j = (10,10) --> index = 100
            index = i + (j-1)*N(1)
     end function coordinatesToIndex

     subroutine indexToCoordinates(index,i,j)
            integer, intent(in) :: index
            integer, intent(out) :: i,j
            i = mod(index-1,N(1)) + 1
            j = (index - i)/N(1) + 1
     end subroutine indexToCoordinates

     subroutine nearestNeighbors(flattened_index, left, right, top, bottom)
       integer, intent(in) :: flattened_index 
       integer, intent(out) :: left, right, top, bottom 
       integer :: x_index, y_index

       call indexToCoordinates(flattened_index, x_index, y_index)

         ! Compute left neighbor
       if (x_index > 1) then
              left = coordinatesToIndex(x_index - 1, y_index)
       else
              left = coordinatesToIndex(N(1), y_index)
       end if

    ! Compute right neighbor
       if (x_index < N(1)) then
              right = coordinatesToIndex(x_index + 1, y_index)
       else
              right = coordinatesToIndex(1, y_index)
       end if

    ! Compute top neighbor
       if (y_index > 1) then
              top = coordinatesToIndex(x_index, y_index - 1)
       else
              top = coordinatesToIndex(x_index, N(2))
       end if

    ! Compute bottom neighbor
       if (y_index < N(2)) then
              bottom = coordinatesToIndex(x_index, y_index + 1)
       else
              bottom = coordinatesToIndex(x_index, 1)
       end if


       !write (*, '(A, I2, A, I2, A, I2, A, I2)') "Flattened index:", flattened_index, " Left:", left, " Right:", &
       !right, " Top:", top, " Bottom:", bottom 

     end subroutine nearestNeighbors
     
end module converter

