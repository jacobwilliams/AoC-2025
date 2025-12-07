program problem_6

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip) :: i, j, m
integer :: n, iunit
integer(ip) :: icount, icount_row
integer(ip),dimension(:,:),allocatable :: iarray
type(string),dimension(:),allocatable :: vals, tmp
character(len=:),allocatable :: line
character(len=1),dimension(:),allocatable :: iops  !! operator array

call clk%tic()

! read the data into an array of integers
! open(newunit=iunit, file='inputs/day6-test.txt', status='OLD')
open(newunit=iunit, file='inputs/day6.txt', status='OLD')
n = number_of_lines_in_file(iunit)
m = 0
do i = 1, n
    line = read_line(iunit)
    vals = split(line, ' ')
    !--------------------------
    ! since the split function only splits on single characters, we have to do this workaround
    allocate(tmp(0))
    do j = 1, size(vals)
        if (trim(vals(j)%str) /= '') tmp = [tmp, vals(j)]  ! only keep the non-blank ones
    end do
    call move_alloc(tmp, vals)
    !--------------------------
    if (m==0) then
        m = size(vals) ! number of rows
        allocate(iarray(n-1,m))
    end if
    if (i==n) then ! it's the operator array
        allocate(iops(m))
        do j = 1, m
            iops(j) = trim(adjustl(vals(j)%str))
        end do
    else ! it's the values
        iarray(i,:) = vals(:)%to_int_64()
    end if
end do
close(iunit)

! now: do the calculations:
icount = 0_ip
do i = 1, m
    select case (iops(i))
    case('+'); icount_row = sum(iarray(:,i))
    case('*'); icount_row = product(iarray(:,i))
    end select
    icount = icount + icount_row
end do

write(*,*) '6a:', icount
! write(*,*) '6b:'  !, icount2

call clk%toc('6')

end program problem_6