program problem_1

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit, i, n, j
logical :: status_ok
character(len=:),allocatable :: line
character(len=1) :: rl
integer :: val, ival, icount, icount2, ival2

call clk%tic()

open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n = number_of_lines_in_file(iunit)

ival = 50 ! initial position
icount = 0
ival2 = 50 ! initial position
icount2 = 0
do i = 1, n
    line = read_line(iunit,status_ok)
    rl = line(1:1)
    val = int(line(2:))

    select case (rl)
    case('R'); ival = modulo(ival+val, 100)
    case('L'); ival = modulo(ival-val, 100)
    end select
    if (ival==0) icount = icount + 1

    do j = 1, val
        select case (rl)
        case('R'); ival2 = modulo(ival2+1, 100)
        case('L'); ival2 = modulo(ival2-1, 100)
        end select
        if (ival2==0) icount2 = icount2 + 1
    end do
end do
close(iunit)

write(*,*) '1a:', icount
write(*,*) '1b:', icount2

call clk%toc('1')

end program problem_1