
!> Handle fields for the QG model

module fv3gfs_fields

use config_mod
use fv3gfs_geom_mod
use fv3gfs_vars_mod
use kinds

implicit none
private

public :: fv3gfs_field, &
        & create, delete, zeros, random, copy, &
        & self_add, self_schur, self_sub, self_mul, axpy, &
        & dot_prod, add_incr, diff_incr, &
        & read_file, write_file, gpnorm, fldrms, &
        & change_resol
public :: fv3gfs_field_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold QG fields
type :: fv3gfs_field
  integer :: nl                     !< Number of levels
end type fv3gfs_field

#define LISTED_TYPE fv3gfs_field

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: fv3gfs_field_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine create(self, geom, vars)
implicit none
type(fv3gfs_field), intent(inout) :: self
type(fv3gfs_geom),  intent(in)    :: geom
type(fv3gfs_vars),  intent(in)    :: vars
integer :: ioff

self%nl = 2

end subroutine create

! ------------------------------------------------------------------------------

subroutine delete(self)
implicit none
type(fv3gfs_field), intent(inout) :: self

end subroutine delete

! ------------------------------------------------------------------------------

subroutine zeros(self)
implicit none
type(fv3gfs_field), intent(inout) :: self

end subroutine zeros

! ------------------------------------------------------------------------------

subroutine random(self)
implicit none
type(fv3gfs_field), intent(inout) :: self

end subroutine random

! ------------------------------------------------------------------------------

subroutine copy(self,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: self
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine copy

! ------------------------------------------------------------------------------

subroutine self_add(self,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: self
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine self_add

! ------------------------------------------------------------------------------

subroutine self_schur(self,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: self
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine self_schur

! ------------------------------------------------------------------------------

subroutine self_sub(self,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: self
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine self_sub

! ------------------------------------------------------------------------------

subroutine self_mul(self,zz)
implicit none
type(fv3gfs_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz

return
end subroutine self_mul

! ------------------------------------------------------------------------------

subroutine axpy(self,zz,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine axpy

! ------------------------------------------------------------------------------

subroutine dot_prod(fld1,fld2,zprod)
implicit none
type(fv3gfs_field), intent(in) :: fld1, fld2
real(kind=kind_real), intent(inout) :: zprod

zprod=0.0_kind_real

return
end subroutine dot_prod

! ------------------------------------------------------------------------------

subroutine add_incr(self,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: self
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine add_incr

! ------------------------------------------------------------------------------

subroutine diff_incr(lhs,x1,x2)
implicit none
type(fv3gfs_field), intent(inout) :: lhs
type(fv3gfs_field), intent(in)    :: x1
type(fv3gfs_field), intent(in)    :: x2

return
end subroutine diff_incr

! ------------------------------------------------------------------------------

subroutine change_resol(fld,rhs)
implicit none
type(fv3gfs_field), intent(inout) :: fld
type(fv3gfs_field), intent(in)    :: rhs

return
end subroutine change_resol

! ------------------------------------------------------------------------------

subroutine read_file(fld, c_conf, vdate)
! Needs more interface clean-up here...
use iso_c_binding
use datetime_mod
use fckit_log_module, only : log

implicit none
type(fv3gfs_field), intent(inout) :: fld      !< Fields
type(c_ptr), intent(in)       :: c_conf   !< Configuration
type(datetime), intent(inout) :: vdate    !< DateTime

return
end subroutine read_file

! ------------------------------------------------------------------------------

subroutine write_file(fld, c_conf, vdate)
use iso_c_binding
use datetime_mod
use fckit_log_module, only : log

implicit none
type(fv3gfs_field), intent(in) :: fld    !< Fields
type(c_ptr), intent(in)    :: c_conf !< Configuration
type(datetime), intent(in) :: vdate  !< DateTime

return
end subroutine write_file

! ------------------------------------------------------------------------------

subroutine gpnorm(fld, nf, pstat)
implicit none
type(fv3gfs_field), intent(in) :: fld
integer, intent(in) :: nf
real(kind=kind_real), intent(inout) :: pstat(3, nf)

pstat(:,:) = 0.0

return
end subroutine gpnorm

! ------------------------------------------------------------------------------

subroutine fldrms(fld, prms)
implicit none
type(fv3gfs_field), intent(in) :: fld
real(kind=kind_real), intent(out) :: prms

prms = 0.0

end subroutine fldrms

! ------------------------------------------------------------------------------

end module fv3gfs_fields
