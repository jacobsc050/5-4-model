module my_module
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

     
end module my_module

program test
    use my_module
    implicit none
    integer :: i,j,index
    integer :: i_temp,j_temp,index_temp
    do i=1,N(1)
        do j=1,N(2)
            index = coordinatesToIndex(i,j)
            call indexToCoordinates(index,i_temp,j_temp)
            if (i_temp/=i .or. j_temp/=j) then
                print *, i,j, "    ",i_temp,j_temp
            end if

        end do
    end do
end program test
