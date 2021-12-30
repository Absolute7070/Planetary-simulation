module functype
  !defining derived-types and operators between derived-types 


  use parameters
  implicit none
  !position vector
  TYPE :: position
    REAL(rk)  :: x, y, z
  end type position
  !velocity vector
  TYPE :: velocity
    REAL(rk) :: vx, vy, vz
  end type velocity
 !acceleration vector
  TYPE :: acceleration
    REAL(rk):: ax, ay, az
  end type acceleration
  !all the data of an object is stored in this derived-type
  TYPE :: object
    INTEGER :: nimi          ! name of the object, an integer.
    TYPE(position) :: r
    TYPE(velocity) :: v
    TYPE(acceleration) :: a
    real(rk) :: m                       !mass
  end type object

 !the multiplication of two acceleration vectors
  interface operator(+)
    module procedure accadd
  end interface operator(+)

  !distance between two postion vectors
  interface operator(.etaisyys.)
    module procedure distance
  end interface operator(.etaisyys.)



contains


  !the multiplication of two acceleration vectors
  type(acceleration) function accadd(acc1, acc2)
    implicit none
    type(acceleration), INTENT(IN) :: acc1, acc2

    accadd%ax = acc1%ax+acc2%ax
    accadd%ay=  acc1%ay+acc2%ay
    accadd%az = acc1%az+acc2%az
  end function accadd


  !distance between two postion vectors
  real(rk) function distance(r1, r2)
    type(position), INTENT(IN) :: r1, r2

    distance= sqrt((r1%x- r2%x)**2+(r1%y- r2%y)**2+ (r1%z- r2%z)**2)

  end function distance






end module functype
