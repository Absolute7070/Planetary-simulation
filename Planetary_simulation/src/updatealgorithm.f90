module update
  use functype
  use readingfile
  implicit none





contains



!updating the objects' data using algorithm
subroutine updatesub()
  implicit none
  INTEGER :: i
  TYPE(acceleration) :: vanha_acc     !to save the acceleration of an object from the current step

  !updating all the objects' data
  do i=1, lkm
    call accsub(objectarray(i), vanha_acc)
    call velsub(objectarray(i), vanha_acc)
    call possub(objectarray(i))
  end do

end subroutine updatesub




!updating one object's acceleration
subroutine accsub(kappale, vanha_acc)
  implicit none
  TYPE(object) , intent(inout) :: kappale         !object to be updated
  type(acceleration), INTENT(OUT) :: vanha_acc   !object's current acceleration
  type(acceleration) :: uusi_acc     ! adding acceleration contributions from other objects
  INTEGER :: i

  vanha_acc=kappale%a
  uusi_acc=acceleration(0,0,0)


  !lasketaan kappaleen uusi kiihtyvyys käymällä objectarray-vektorin läpi
  !calculating the contributions to the object's acceleration from the other objects.
  do i= 1, lkm
    if (i==kappale%nimi) cycle    !skipping the object kappale itself
    uusi_acc=uusi_acc+accfunc(kappale, objectarray(i))
  end do

  kappale%a=uusi_acc         !acceleration update

end subroutine accsub




!laskee kappaleen kiihtyvyys ollessaan kohde-objectin gravitaatiopotentiaalissa
!to calculate the acceleration of the object kappale when in the other one's gravitational potential field.
type(acceleration) function accfunc(kappale, kohde)
  implicit none
  type(object), INTENT(IN) :: kappale, kohde
  type(position) :: r1, r2    !saving the positions of objects kappale and kohde
  real(rk) :: skalaariosa

  r1= kappale%r
  r2=kohde%r

  !Scalar part of Newton's law of gravitation
  skalaariosa = G * kohde%m/(r2.etaisyys.r1)**3

  !calculating acceleration components
  accfunc%ax = skalaariosa* (r2%x-r1%x  )
  accfunc%ay = skalaariosa* (r2%y-r1%y  )
  accfunc%az = skalaariosa* (r2%z-r1%z  )

end function accfunc




!updating the velocity of one object.
subroutine velsub(kappale, vanha_acc)
  implicit none
  type(object) , INTENT(INOUT) :: kappale
  type(acceleration) , INTENT(IN) :: vanha_acc

  kappale%v%vx =kappale%v%vx + 1.0/2.0* (vanha_acc%ax+ kappale%a%ax)*deltaT
  kappale%v%vy =kappale%v%vy + 1.0/2.0* (vanha_acc%ay+ kappale%a%ay)*deltaT
  kappale%v%vz =kappale%v%vz + 1.0/2.0* (vanha_acc%az+ kappale%a%az)*deltaT
end subroutine velsub



  !updating the position of one object
  subroutine possub(kappale )
    implicit none
    TYPE(object), INTENT(inout) :: kappale
    !calculating new position coordinates
    kappale%r%x= kappale%r%x+ kappale%v%vx*deltaT+ (1.0/2.0)*kappale%a%ax*deltaT**2
    kappale%r%y= kappale%r%y+ kappale%v%vy*deltaT+ (1.0/2.0)*kappale%a%ay*deltaT**2
    kappale%r%z= kappale%r%z+ kappale%v%vz*deltaT+ (1.0/2.0)*kappale%a%az*deltaT**2

  end subroutine possub







end module update
