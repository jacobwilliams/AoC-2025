program problem_2

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip) :: i, j, k
integer :: n, iunit
logical :: status_ok
character(len=:),allocatable :: line
type(string),dimension(:),allocatable :: vals, initial_final
integer(ip) :: icount, icount2

call clk%tic()

! open(newunit=iunit, file='inputs/day2-test.txt', status='OLD')
open(newunit=iunit, file='inputs/day2.txt', status='OLD')
n = number_of_lines_in_file(iunit)

icount = 0_ip
icount2 = 0_ip
do i = 1, n
    line = read_line(iunit,status_ok)
    vals = split(line, ',')
    do j = 1, size(vals)
        initial_final = split(vals(j), '-')
        do k = str_to_int64(initial_final(1)), str_to_int64(initial_final(2))  ! loop over range
            if (part_a_invalid(k)) icount = icount + k
            if (part_b_invalid(k)) icount2 = icount2 + k
        end do
    end do
end do
close(iunit)

write(*,*) '2a:', icount
write(*,*) '2b:', icount2

call clk%toc('2')

contains

    pure function part_a_invalid(k) result(invalid)
    !! part a: test if value is invalid
    integer(ip), intent(in) :: k
    character(len=:), allocatable :: current
    logical :: invalid
    integer(ip) :: p, m
    current = int_to_str(k)  ! convert to string
    if (mod(len(current),2_ip) == 0) then ! has to be an even number of digits
        invalid = .true.
        p = len(current)/2_ip
        do m = 1, p
            if (current(m:m) /= current(m+p:m+p)) then
                invalid = .false.
                exit
            end if
        end do
    else
        invalid = .false.
    end if
    end function part_a_invalid

    function part_b_invalid(k) result(invalid)
    !! part b: test if value is invalid
    integer(ip), intent(in) :: k
    logical :: invalid

    integer(ip) :: n, i, j, m, chunk_size
    character(len=1), dimension(:), allocatable :: array
    integer(ip),dimension(:),allocatable :: idivisors
    logical :: all_same

    invalid = .false.
    array = str_to_array(int_to_str(k)) ! array of characters. eg. ['1','2','3','1','2','3']
    n = size(array) ! number of characters
    if (n<2) return ! must have at least 2 digits

    if (all(array(1)==array)) then ! first just check if they are all the same number
        invalid = .true.
    else
        idivisors = divisors(n) ! ways to divide this number into an equal number of digits
        if (allocated(idivisors)) then
            ! we divide the number k in this many parts and test each one
            main: do i = 1_ip, size(idivisors)
                m = idivisors(i)
                chunk_size = n / m  ! size of each chunk
                if (chunk_size*m /= n) error stop 'some problem here!'
                ! example for 123123:
                !   m=2: 123-123   (chunk_size = 3)
                !   m=3: 12-31-23  (chunk_size = 2)
                all_same = .false.
                do j = 2, m
                    if (all(array(1:chunk_size)==array(chunk_size*(j-1)+1:chunk_size*j))) then
                        all_same = .true.  ! they must all be true for it to be invalid
                    else
                        all_same = .false.
                        exit  ! stop checking this divisor, it can't be correct for this divisor
                    end if
                end do
                if (all_same) then
                    invalid = .true.
                    exit main
                end if
            end do main
        end if
    end if

    end function part_b_invalid

end program problem_2