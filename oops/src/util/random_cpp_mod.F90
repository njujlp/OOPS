! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

!> Not so random number generator

module random_cpp_mod
use, intrinsic :: iso_c_binding
use kinds
implicit none
private
public random_cpp

! ------------------------------------------------------------------------------

interface
  subroutine random_c(nn,zz) bind(C,name="random_cpp")
  use iso_c_binding
  implicit none
  integer(c_int) :: nn
  real(c_double) :: zz
  end subroutine random_c
end interface

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

subroutine random_cpp(xx)
implicit none
real(kind=kind_real), intent(inout) :: xx(:)
real(kind=c_double) :: zz
integer(c_int) :: nn
integer :: jj

nn = size(xx)

do jj=1,nn
  call random_c(nn, zz)
  xx(jj) = zz
enddo

end subroutine random_cpp

! ------------------------------------------------------------------------------

end module random_cpp_mod
