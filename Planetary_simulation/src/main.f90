program main
  use readingfile
  use update
  use writetofile
  use printti

  implicit none

  !running the simulation
  call run()








contains

!for running the simulation
subroutine run()
  implicit none
  REAL(rk) :: t1, t2       !to calculate the duration of the simulation
  INTEGER(ik ) :: tofile=0        !datapoints saved to the file
  INTEGER(ik ):: i
  CHARACTER(MAXBUFF ):: filename    !filename where datapoints are stored


  CALL readfile()       !extracting the data of objects from an output file
                        !needed for simulation

  call convert()        !all the data are stored in the objectarray

  nos=totalen/deltaT      !calculating how many total steps in the simulation

  call GET_COMMAND_ARGUMENT(2, filename)          !get command line argument for the filename of the output file

  call CPU_TIME(t1)

  do i=1, nos
    !saving datapoints
    savefile: if ((i==1).or.(mod(i, savelen)==0)) then
      call writetofilesub(i, filename, tofile)
    end if savefile

    call CPU_TIME(t2)

    !printing datapoints to the terminal
    tulostus: if ((i==1).or.(i==nos).or.(mod(i, printlen)==0)) then
      call printing(t2-t1, i, tofile)
    end if tulostus

    !updating the data of the objects using the algorithm
    call updatesub()

  end do

  !closing the file
  if (i==nos) then
    call writetofilesub(i, filename, tofile, .True.)
  end if



end subroutine run


























end program main
