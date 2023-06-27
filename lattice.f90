module lattice_maker
    use converter
    use mt19937
    implicit none 
    
    contains
    subroutine lattice_make(dimension, size, lattice)
        !For an Nth dimensional lattice
        integer, intent(in) :: dimension, size
    
        integer :: x,latticeCount,index, i
        real, dimension(:),  intent(out),  allocatable :: lattice

        real :: y
    
     
        ! Calculate the required array index size for the 1 dimensional array indexing
        
        latticeCount = size ** dimension
        
        Write(*,*) "Total lattice index size is ", latticeCount,"."

        
        allocate(lattice(latticeCount))

        !~~~ I know fortrans RNG is bad, this is placeholder for now ~~~
        ! (sorry paul :) ) 

        do i = 1, latticeCount
            call random_number(y)
            lattice(i) = y * 1 - 0.5 
        end do 

    
        ! Determine print format and then print out randomly generated numbers 
        if(dimension == 2) then
            ! print out formatted numbers (only works to 2d)
            index = 1
            do i = 1, size
                do x = 1, size
                    write(*, '(A, F10.5, A)',advance='no' ) " ", lattice(index), ", "
                    index = index + 1
                end do
                write(*,*)
            end do
        else 
            ! Print out the randomly generated numbers in 1D format
            write(*,*) "Randomly generated numbers in 1d format"
            do i = 1, latticeCount 
            write(*,*)  i ,":", lattice(i)
            end do
        end if
        
    end subroutine lattice_make

    
  

  subroutine write_lattice_to_file(dim, size, lattice, filename)
    real, intent(in) :: lattice(:)
    integer, intent(in) :: dim, size
    character (len=:), allocatable :: filename
    integer :: file_unit, i, j

    
    open(newunit=file_unit, file=trim(filename), status="replace")

    ! Write the lattice values to the file
    do j = 1, size
      do i = 1, size
        write(file_unit, *) lattice(coordinatesToIndex(i, j))
        
      end do
      ! Write a newline character to create a new column
      write(file_unit, *)
    end do

    ! Close the file
    close(file_unit)
  end subroutine write_lattice_to_file

  subroutine calculateAndOutputResults(myArray, size, fileName, action_value)
    implicit none
    real, intent(in) :: myArray(:)
    integer, intent(in) :: size
    character(len=*), intent(in) :: fileName
    real, intent(in) :: action_value
    real :: sumValues, sumOfSquares, sumOfFourthPower  ! Sum variables
    real :: meanValue, meanSquares, meanFourthPower   ! Mean variables
    integer :: i
    integer :: unitNumber
    logical :: fileExists

    ! Define column names
    character(len=21), dimension(4) :: columnNames
    columnNames = ["Action               ","Mean value           ", "Mean of squares      ", "Mean of fourth powers"]
  
    ! Check if the file already exists
    inquire(file=trim(fileName), exist=fileExists)
  
    if (.not. fileExists) then
      ! Open the file in write mode if it doesn't exist
      open(newunit=unitNumber, file=fileName, status='replace')
      write(unitNumber, '(A20,A1,A20,A1,A20,A1,A20)') trim(columnNames(1)), ' ', trim(columnNames(2)), ' ', &
      trim(columnNames(3)), ' ', trim(columnNames(4))
      
    else
      ! Open the file in append mode if it exists
      open(newunit=unitNumber, file=fileName, status='unknown', action='readwrite', position='append')
    end if
  
    ! Calculate the sum of the array values
    sumValues = sum(myArray)
  
    ! Calculate the sum of squares
    sumOfSquares = sum(myArray**2)
  
    ! Calculate the sum of fourth powers
    sumOfFourthPower = sum(myArray**4)
  
    ! Calculate the mean values
    meanValue = sumValues / real(size)
    meanSquares = sumOfSquares / real(size)
    meanFourthPower = sumOfFourthPower / real(size)
  
    ! Write the results to the file
    write(unitNumber, '(F21.6,A1,F20.6,A1,F20.6,A1,F20.6)') action_value, ' ', meanValue, ' ', meanSquares, ' ', meanFourthPower
  
    ! Close the file
    close(unitNumber)
  
  end subroutine calculateAndOutputResults
end module lattice_maker 