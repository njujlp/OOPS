
!> Fortran module to handle variables for the QG model
module fv3_vars_mod

use iso_c_binding
use config_mod
use netcdf
USE fv3_misc_mod, ONLY : max_string_length, get_varnames,varseparator

implicit none
private
public :: fv3_vars, fv3_vars_clone
public :: fv3_vars_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to represent QG model variables
type :: fv3_vars
  INTEGER :: nv3d
  INTEGER :: nv2d
  CHARACTER(len=NF90_MAX_NAME), ALLOCATABLE, dimension(:) :: fldnames3d !< Variable identifiers
  CHARACTER(len=NF90_MAX_NAME), ALLOCATABLE, dimension(:) :: fldnames2d !< Variable identifiers

end type fv3_vars

#define LISTED_TYPE fv3_vars

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: fv3_vars_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------


subroutine c_fv3_vars_create(c_key_self, c_conf) bind(c,name='fv3_var_create_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(fv3_vars), pointer :: self
character(len=max_string_length) :: svar
CHARACTER(len=NF90_MAX_NAME), DIMENSION(NF90_MAX_VARS) :: varnames

call fv3_vars_registry%init()
call fv3_vars_registry%add(c_key_self)
call fv3_vars_registry%get(c_key_self, self)

svar = config_get_string(c_conf,len(svar),"variables3d")
CALL get_varnames(svar,varseparator,self%nv3d,varnames)
ALLOCATE(self%fldnames3d(self%nv3d))
self%fldnames3d(:) = varnames(1:self%nv3d)

svar = config_get_string(c_conf,len(svar),"variables2d")
IF (svar(1:1) /= ' ') THEN
   CALL get_varnames(svar,varseparator,self%nv2d,varnames)
   IF (self%nv2d > 0) THEN
      ALLOCATE(self%fldnames2d(self%nv2d))
      self%fldnames2d(:) = varnames(1:self%nv2d)
   ENDIF
ELSE
   self%nv2d =0
ENDIF

IF ((self%nv3d+self%nv2d) == 0) &
     &CALL abor1_ftn("fv3_vars_mod.1")


return
end subroutine c_fv3_vars_create

! ------------------------------------------------------------------------------

subroutine c_fv3_vars_clone(c_key_self, c_key_other) bind(c,name='fv3_var_clone_f90')
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(fv3_vars), pointer :: self, other

call fv3_vars_registry%get(c_key_self, self)
call fv3_vars_registry%add(c_key_other)
call fv3_vars_registry%get(c_key_other, other)

call fv3_vars_clone(self, other)

end subroutine c_fv3_vars_clone

! ------------------------------------------------------------------------------

subroutine fv3_vars_clone(self, other)
implicit none
type(fv3_vars), intent(in)    :: self
type(fv3_vars), intent(inout) :: other

other%nv3d = self%nv3d
other%nv2d = self%nv2d

allocate(other%fldnames3d(other%nv3d))
other%fldnames3d(:)=self%fldnames3d(:)

IF (other%nv2d > 0) THEN
   ALLOCATE(other%fldnames2d(other%nv2d))
   other%fldnames2d(:)=self%fldnames2d(:)
ENDIF

end subroutine fv3_vars_clone

! ------------------------------------------------------------------------------

subroutine c_fv3_vars_delete(c_key_self) bind(c,name='fv3_var_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self

type(fv3_vars), pointer :: self
call fv3_vars_registry%get(c_key_self, self)
IF (ALLOCATED(self%fldnames3d)) DEALLOCATE(self%fldnames3d)
IF (ALLOCATED(self%fldnames2d)) DEALLOCATE(self%fldnames2d)

call fv3_vars_registry%remove(c_key_self)

return
end subroutine c_fv3_vars_delete

! ------------------------------------------------------------------------------

SUBROUTINE c_fv3_vars_info(c_key_self, c_nv3d, c_nv2d) BIND(c,name='fv3_var_info_f90')
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: c_nv3d
integer(c_int), intent(inout) :: c_nv2d
type(fv3_vars), pointer :: self

call fv3_vars_registry%get(c_key_self, self)

c_nv3d = self%nv3d
c_nv2d = self%nv2d

return
end subroutine c_fv3_vars_info

! ------------------------------------------------------------------------------

end module fv3_vars_mod
