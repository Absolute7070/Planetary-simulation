module printti
  !subroutines for printing the data to the terminal
  use functype
  use readingfile
  implicit none


contains

  !printing data
  subroutine printing(aikaalusta, iterationstep, tofile )
    implicit  none
    REAL(rk), INTENT(IN) :: aikaalusta                         !simulation duration from the beginning of the simulation
    INTEGER(ik), INTENT(IN) :: tofile, iterationstep           !how many datapoints were saved to the output file / how many steps have been gone since the beginning of the sim.
    INTEGER :: k

    print *
    write(6 , '(a, i0)')  'Number of objects: ', lkm
    write(6 , '(a, g0.3, '' s'')') 'Time since the beginning: ', aikaalusta
    write(6, '(a, i0)') 'Steps past: ', iterationstep
    write(6 , '(a, i0)') 'Datapoints in the output file: ', tofile
    print *
    print '(a)', 'The positional data of objects: '

    !printing the positional data of the objects
    do k=1, lkm
      call objectprinting(objectarray(k))
    end do

    print *
    print '(a)', '-------------------------------------------------'

  end subroutine printing



!printing objects' positional data
  subroutine objectprinting(kappale)
    implicit none
    type(object) , INTENT(IN) :: kappale
    character(200 ) :: fmt= '(i0,''.'',2x, ''('', g0.4, '' , '', g0.4, '' , '',g0.4, '')''  )'        ! (nimi. (x, y, z))

    print fmt , kappale%nimi, kappale%r%x,kappale%r%y, kappale%r%z


  end subroutine objectprinting











end module printti
