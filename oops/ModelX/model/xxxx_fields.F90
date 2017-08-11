
!> Handle fields for an unknown model

module xxxx_fields

use config_mod
use xxxx_geom_mod
use xxxx_vars_mod
use kinds

implicit none
private

public :: xxxx_field, &
        & create, delete, zeros, random, copy, self_add, self_schur, self_sub, &
        & self_mul, axpy, dot_prod, add_incr, diff_incr, read_file, write_file, &
        & gpnorm, fldrms, change_resol
public :: xxxx_field_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold QG fields
type :: xxxx_field
  integer :: nxxxx   ! Don't keep this, put what is needed for a given model
end type xxxx_field

#define LISTED_TYPE xxxx_field

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: xxxx_field_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine create(self, geom, vars)
implicit none
type(xxxx_field), intent(inout) :: self
type(xxxx_geom),  intent(in)    :: geom
type(xxxx_vars),  intent(in)    :: vars

end subroutine create

! ------------------------------------------------------------------------------

subroutine delete(self)
implicit none
type(xxxx_field), intent(inout) :: self

end subroutine delete

! ------------------------------------------------------------------------------

subroutine zeros(self)
implicit none
type(xxxx_field), intent(inout) :: self

end subroutine zeros

! ------------------------------------------------------------------------------

subroutine random(self)
use random_vectors_mod
implicit none
type(xxxx_field), intent(inout) :: self

end subroutine random

! ------------------------------------------------------------------------------

subroutine copy(self,rhs)
implicit none
type(xxxx_field), intent(inout) :: self
type(xxxx_field), intent(in)    :: rhs

return
end subroutine copy

! ------------------------------------------------------------------------------

subroutine self_add(self,rhs)
implicit none
type(xxxx_field), intent(inout) :: self
type(xxxx_field), intent(in)    :: rhs

return
end subroutine self_add

! ------------------------------------------------------------------------------

subroutine self_schur(self,rhs)
implicit none
type(xxxx_field), intent(inout) :: self
type(xxxx_field), intent(in)    :: rhs

return
end subroutine self_schur

! ------------------------------------------------------------------------------

subroutine self_sub(self,rhs)
implicit none
type(xxxx_field), intent(inout) :: self
type(xxxx_field), intent(in)    :: rhs

return
end subroutine self_sub

! ------------------------------------------------------------------------------

subroutine self_mul(self,zz)
implicit none
type(xxxx_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz

return
end subroutine self_mul

! ------------------------------------------------------------------------------

subroutine axpy(self,zz,rhs)
implicit none
type(xxxx_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz
type(xxxx_field), intent(in)    :: rhs

return
end subroutine axpy

! ------------------------------------------------------------------------------

subroutine dot_prod(fld1,fld2,zprod)
implicit none
type(xxxx_field), intent(in) :: fld1, fld2
real(kind=kind_real), intent(inout) :: zprod

zprod = 0.0

return
end subroutine dot_prod

! ------------------------------------------------------------------------------

subroutine add_incr(self,rhs)
implicit none
type(xxxx_field), intent(inout) :: self
type(xxxx_field), intent(in)    :: rhs

return
end subroutine add_incr

! ------------------------------------------------------------------------------

subroutine diff_incr(lhs,x1,x2)
implicit none
type(xxxx_field), intent(inout) :: lhs
type(xxxx_field), intent(in)    :: x1
type(xxxx_field), intent(in)    :: x2

return
end subroutine diff_incr

! ------------------------------------------------------------------------------

subroutine change_resol(fld,rhs)
implicit none
type(xxxx_field), intent(inout) :: fld
type(xxxx_field), intent(in)    :: rhs

return
end subroutine change_resol

! ------------------------------------------------------------------------------

subroutine read_file(fld, c_conf, vdate)
use iso_c_binding
use datetime_mod

implicit none
type(xxxx_field), intent(inout) :: fld   !< Fields
type(c_ptr), intent(in)       :: c_conf  !< Configuration
type(datetime), intent(inout) :: vdate   !< DateTime
character(len=20) :: sdate

sdate = config_get_string(c_conf,len(sdate),"date")
call datetime_set(sdate, vdate)

return
end subroutine read_file

! ------------------------------------------------------------------------------

subroutine write_file(fld, c_conf, vdate)
use iso_c_binding
use datetime_mod

implicit none
type(xxxx_field), intent(in) :: fld   !< Fields
type(c_ptr), intent(in)    :: c_conf  !< Configuration
type(datetime), intent(in) :: vdate   !< DateTime

return
end subroutine write_file

! ------------------------------------------------------------------------------

subroutine gpnorm(fld, nf, pstat)
implicit none
type(xxxx_field), intent(in) :: fld
integer, intent(in) :: nf
real(kind=kind_real), intent(inout) :: pstat(3, nf)

pstat(:,:) = 1.0

return
end subroutine gpnorm

! ------------------------------------------------------------------------------

subroutine fldrms(fld, prms)
implicit none
type(xxxx_field), intent(in) :: fld
real(kind=kind_real), intent(out) :: prms

prms = 0.0

end subroutine fldrms

! ------------------------------------------------------------------------------

end module xxxx_fields
