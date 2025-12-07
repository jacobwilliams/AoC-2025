program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip) :: i, j, m, p
integer :: n, iunit
integer(ip) :: icount, imax, imax_first, icurr
integer(ip),dimension(:),allocatable :: icount_rows
integer(ip),dimension(:),allocatable :: irow
integer,dimension(:,:),allocatable :: iarray

call clk%tic()

! iarray = read_file_to_int_array('inputs/day3-test.txt')
iarray = read_file_to_int_array('inputs/day3.txt')
n = size(iarray,1)   ! n rows
m = size(iarray,2)   ! n cols

allocate(icount_rows(n))
icount = 0_ip
do i = 1, n

    irow = iarray(i,:)
    imax_first = -1_ip  ! max of the first digit so far
    imax       = -1_ip  ! max of the two digits so far

    do j = 1, m-1  ! loop for first digit
        p = irow(j)
        if (p>imax_first) then ! this is now the max first digit
            imax_first = p
            imax = 0
        end if
        icurr = imax_first*10 + maxval(irow(j+1:))  ! now get the max 2nd digit:
        if (icurr > imax) imax = icurr  ! this is the new max value
    end do
    icount_rows(i) = imax
end do
close(iunit)
icount = sum(icount_rows)

write(*,*) '3a:', icount
! write(*,*) '3b:'  !, icount2

call clk%toc('3')

end program problem_3