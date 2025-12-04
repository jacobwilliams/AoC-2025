program problem_2

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip) :: i, j, n_values, k, m, p
integer :: n, iunit
logical :: status_ok
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals, initial_final
character(len=:),allocatable :: current
logical :: valid
integer(ip) :: icount

call clk%tic()

! open(newunit=iunit, file='inputs/day2-test.txt', status='OLD')
open(newunit=iunit, file='inputs/day2.txt', status='OLD')
n = number_of_lines_in_file(iunit)

icount = 0_ip
do i = 1, n
    line = read_line(iunit,status_ok)
    vals = split(line, ',')
    do j = 1, size(vals)
        initial_final = split(vals(j), '-')
        main: do k = str_to_int64(initial_final(1)), str_to_int64(initial_final(2))  ! loop over range
            current = int_to_str(k)  ! convert to string
            if (mod(len(current),2) == 0) then ! has to be an even number of digits
                valid = .true.
                p = len(current)/2
                do m = 1, p
                    if (current(m:m) /= current(m+p:m+p)) then
                        valid = .false.
                        exit
                    end if
                end do
                if (valid) icount = icount + k
            end if
        end do main
    end do
end do
close(iunit)

write(*,*) '2a:', icount
! write(*,*) '2b:'  !, icount2

call clk%toc('2')

end program problem_2