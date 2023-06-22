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
     
end module converter
