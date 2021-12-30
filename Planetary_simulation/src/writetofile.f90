module writetofile
  !saving positional datapoints of the objects to an output file
  use readingfile
  implicit none



contains


  !saving the datapoints to the file filename
  subroutine writetofilesub(i, filename, kertoja, sulku )
    CHARACTER(MAXBUFF), INTENT(IN) ::  filename
    INTEGER(ik ), INTENT(IN) :: i               !what is the current iteration step
    INTEGER(ik ), INTENT(INOUT) :: kertoja                      !how many datapoints are already in the output file

    LOGICAL, optional, INTENT(IN) :: sulku          !closing the output file when the last datapoint is written to the file
    INTEGER :: ios, k


    !opening a new file
    if (i==1 ) then
      OPEN(unit=1, file=filename, iostat=ios, status='new')

      !file opening error check
      if (ios/=0) then
        print *, "Error in opening a new file!"
        stop
      end if
    end if



    !writing the coordinates of the objects to the file
    do k=1, lkm
      write(1, '( g0, x, g0, x, g0,  "/"     )', iostat=ios, advance='no')  objectarray(k)%r%x, &
      objectarray(k)%r%y, objectarray(k)%r%z
      !error check if datapoints were successfully written into the file
      if (ios/=0) then
        print *, "Alert! The current datapoint not saved!"
      end if
    end do
    !linefeed to the output file
    write(1, *)


    kertoja=kertoja+1

    !closing the file
    if (PRESENT(sulku)) then
      CLOSE(1)
    end if



end subroutine writetofilesub


end module writetofile
