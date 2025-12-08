program problem_7

use iso_fortran_env
use aoc_utilities

implicit none

character(len=1),dimension(:,:),allocatable :: array
integer(ip) :: icount
integer(ip) :: istart, nrows, ncols
character(len=:),allocatable :: filename

call clk%tic()

! filename = 'inputs/day7-test.txt'
filename = 'inputs/day7.txt'

array = read_file_to_char_array(filename)
nrows = size(array, 1)
ncols = size(array, 2)
istart = findloc(array(1,:), 'S', dim=1)  ! first find the column with 'S' on row one
icount = 0  ! split counter
call dfs(1_ip, istart)
write(*,*) '7a:', icount

array = read_file_to_char_array(filename)  ! read it again since it was changed
write(*,*) '7b:', bfs(istart)

call clk%toc('7')

contains
    recursive subroutine dfs(irow, icol)
    integer(ip),intent(in) :: irow, icol
    if (irow == nrows) return ! done
    if (irow<1 .or. icol>ncols) return ! off the board
    if (array(irow, icol)=='|') return ! already visited this one
    if (array(irow, icol)=='^' .or. array(irow, icol)=='*') then ! we have hit a splitter
        ! use a '*' to indicate this splitter was already visited so we don't double count it
        if (array(irow, icol)=='^') icount = icount + 1_ip
        array(irow, icol) = '*'
        call dfs(irow, icol+1)
        call dfs(irow, icol-1)
    else
        ! advance beam down
        call dfs(irow+1, icol)
    end if
    array(irow, icol)='|'
    end subroutine dfs

    function bfs(istart) result(itimelines)
        !! bfs row by row, accounting for multiple paths going through each point

    integer(ip),intent(in) :: istart
    integer(ip) :: itimelines

    integer(ip),dimension(:,:),allocatable :: ipath  !! this will be a count of each timeline that passes through each point
    integer(ip) :: irow, icol

    allocate(ipath(nrows, ncols))
    ipath = 0
    ipath(1,istart) = 1  ! starting timeline

    do irow = 2_ip, nrows
        ! advance all the beams to this row and update the counter
        do icol = 1, ncols
            if (ipath(irow-1, icol)>0) then
                ! advance this beam down. have to add to existing paths already there
                if (array(irow, icol)=='^' ) then
                    ! this is a splitter
                    ipath(irow, icol+1) = ipath(irow, icol+1) + ipath(irow-1, icol)
                    ipath(irow, icol-1) = ipath(irow, icol-1) + ipath(irow-1, icol)
                else
                    ! advance down normally
                    ipath(irow, icol) = ipath(irow, icol) + ipath(irow-1, icol)
                end if
            end if
        end do
    end do
    itimelines = sum(ipath(nrows,:))  ! last row, total timelines that got here.
    end function bfs

end program problem_7