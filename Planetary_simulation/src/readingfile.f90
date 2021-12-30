module readingfile
  !for reading in the input file
  use parameters
  use functype
  implicit none

  !the following ones are global variables!
  INTEGER :: lkm             !number of objects to be simulated
  REAL(rk), ALLOCATABLE :: objectdata(:, :)  !the data of objects
  REAL(rk) :: deltaT, totalen       !the length of a time step and the total simulation time
  INTEGER(ik) :: printlen          !how often the objects' data to be printed to the screen
  INTEGER(ik) :: savelen           !how often the objects' data to be saved to the output file

  INTEGER(ik):: nos !number of steps
  TYPE(object), ALLOCATABLE :: objectarray(:) !saving all the objects





contains

  !reading file
  subroutine readfile()
    implicit none
    INTEGER:: i, j, ios
    CHARACTER(MAXBUFF) :: filename          !input filename


    call GET_COMMAND_ARGUMENT(1, filename)     !getting input filename from the command line
    open(unit=1, file=filename, iostat=ios, status='old')

    !error check
    if (ios/=0) then
      print '(a, a)', 'Error in opening the file ', trim(filename)
      stop
    end if


    !saving the data of the objects
    i=0
    looppi1: do
      !saving the number of objects
      if (i==0) then
        read(1, *, iostat=ios ) lkm

        !error check
        varmennus: if (ios/=0) then
          print '(a, i0, '' < lkm < '', i0)', 'The number must be between: ', HUGE(lkm), -HUGE(lkm)
          stop
        end if varmennus

        !defining an array where the data of objects are saved
        ALLOCATE(objectdata(lkm , 7))
        objectdata=0

      !saving the data of the objects
      else if ((i>0).and.(i<=lkm)) then
        !saving the data of an object to a row of the array objectdata
        read(1, *, iostat=ios ) objectdata(i, 1),objectdata(i, 2), objectdata(i, 3), objectdata(i, 4)  &
        , objectdata(i, 5), objectdata(i, 6), objectdata(i, 7)

        !error check
        varmennus2: if (ios/=0) then
          print *,  'Error in reading the data of objects! Check the formatting!'
          stop
        end if varmennus2

      !iteraatiotietojen lukeminen
      else
        read(1, *, iostat=ios ) deltaT, totalen, printlen, savelen

        !varmistetaan, että tiedoston luku onnistui.
        varmennus3: if (ios/=0) then
          print *,  'Error in reading time-related data! Check the formatting!'
          stop
        end if varmennus3

      end if

      if (i==lkm+1) exit
      i=i+1



    end do looppi1

    close(1)

  end subroutine readfile


!muunnetaan kaikki kappaleen datat object-type array:ksi
  subroutine convert()
    implicit none
    INTEGER :: i
    real(rk ) :: massa
    TYPE(position) :: paikka
    TYPE(velocity) :: nopeus
    TYPE(acceleration) :: kiihtyvyys=acceleration(0,0,0 )   !each object's acceleration is zero at the start

    ALLOCATE(objectarray(lkm ))

    !saving every objects's data to the objectarray
    do i=1, lkm
      massa=objectdata(i, 1)
      paikka=position( objectdata(i, 2), objectdata(i, 3), objectdata(i, 4)  )
      nopeus=velocity( objectdata(i, 5), objectdata(i, 6), objectdata(i, 7) )
      objectarray(i) =object(i, paikka, nopeus, kiihtyvyys, massa)
    end do


  end subroutine convert








end module readingfile
