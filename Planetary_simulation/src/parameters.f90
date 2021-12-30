module parameters
  implicit none
  !parameters to be used in the project

  !kind-related parameters
  INTEGER, PARAMETER :: rk=selected_real_kind(20,100), ik=selected_int_kind(38)
  INTEGER, PARAMETER :: MAXBUFF=200

  !Gravitational constant
  REAL(rk), PARAMETER :: G= 6.67430*10.0**(-11) !m^3 kg^(-1) s^(-2)



end module parameters
