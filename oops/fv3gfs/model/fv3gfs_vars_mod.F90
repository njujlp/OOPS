
!> Fortran module to handle variables for the QG model
module fv3gfs_vars_mod

use iso_c_binding
use config_mod

implicit none
private
public :: fv3gfs_vars, fv3gfs_vars_setup, fv3gfs_vars_clone
public :: fv3gfs_vars_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to represent QG model variables
type :: fv3gfs_vars
  integer :: nv
  character(len=1), allocatable :: fldnames(:) !< Variable identifiers
  logical :: lbc
end type fv3gfs_vars

#define LISTED_TYPE fv3gfs_vars

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: fv3gfs_vars_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine fv3gfs_vars_setup(self, cvars)
implicit none
type(fv3gfs_vars), intent(inout) :: self
character(len=1), intent(in) :: cvars(:)
integer :: jj

self%nv = size(cvars)
self%lbc = .false.

do jj=1,self%nv
  if (cvars(jj)/="x" .and. cvars(jj)/="q" .and. cvars(jj)/="u" .and. cvars(jj)/="v") &
     & call abor1_ftn ("fv3gfs_vars_setup: unknown field")
enddo
allocate(self%fldnames(self%nv))
self%fldnames(:)=cvars(:)

end subroutine fv3gfs_vars_setup

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_vars_create(c_key_self, c_conf) bind(c,name='fv3gfs_var_create_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(fv3gfs_vars), pointer :: self
character(len=2) :: svar

call fv3gfs_vars_registry%init()
call fv3gfs_vars_registry%add(c_key_self)
call fv3gfs_vars_registry%get(c_key_self, self)

svar = config_get_string(c_conf,len(svar),"variables")
select case (svar)
case ("nl")
  self%nv = 4
  self%lbc = .true.
  allocate(self%fldnames(4))
  self%fldnames(:) = (/"x","q","u","v"/)
case ("tl")
  self%nv = 4
  self%lbc = .false.
  allocate(self%fldnames(4))
  self%fldnames(:) = (/"x","q","u","v"/)
case ("cv")
  self%nv = 1
  self%lbc = .true.
  allocate(self%fldnames(1))
  self%fldnames(1) = "x"
case ("ci")
  self%nv = 1
  self%lbc = .false.
  allocate(self%fldnames(1))
  self%fldnames(1) = "x"
case ("x")
  self%nv = 1
  self%lbc = .false.
  allocate(self%fldnames(1))
  self%fldnames(1) = "x"
case default
  call abor1_ftn("c_fv3gfs_vars_create: undefined variables")
end select

return
end subroutine c_fv3gfs_vars_create

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_vars_clone(c_key_self, c_key_other) bind(c,name='fv3gfs_var_clone_f90')
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(fv3gfs_vars), pointer :: self, other

call fv3gfs_vars_registry%get(c_key_self, self)
call fv3gfs_vars_registry%add(c_key_other)
call fv3gfs_vars_registry%get(c_key_other, other)

call fv3gfs_vars_clone(self, other)

end subroutine c_fv3gfs_vars_clone

! ------------------------------------------------------------------------------

subroutine fv3gfs_vars_clone(self, other)
implicit none
type(fv3gfs_vars), intent(in)    :: self
type(fv3gfs_vars), intent(inout) :: other

other%nv = self%nv
other%lbc = self%lbc

allocate(other%fldnames(other%nv))
other%fldnames(:)=self%fldnames(:)

end subroutine fv3gfs_vars_clone

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_vars_delete(c_key_self) bind(c,name='fv3gfs_var_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self

type(fv3gfs_vars), pointer :: self
call fv3gfs_vars_registry%get(c_key_self, self)
deallocate(self%fldnames)
call fv3gfs_vars_registry%remove(c_key_self)

return
end subroutine c_fv3gfs_vars_delete

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_vars_info(c_key_self, c_nv, c_nl) bind(c,name='fv3gfs_var_info_f90')
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: c_nv
integer(c_int), intent(inout) :: c_nl
type(fv3gfs_vars), pointer :: self

call fv3gfs_vars_registry%get(c_key_self, self)

c_nv = self%nv
c_nl = 0
if (self%lbc) c_nl = 1

!self%fldnames(:)

return
end subroutine c_fv3gfs_vars_info

! ------------------------------------------------------------------------------

end module fv3gfs_vars_mod
