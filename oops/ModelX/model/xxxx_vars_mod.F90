
!> Fortran module to handle variables for an unknown model
module xxxx_vars_mod

use iso_c_binding
use config_mod

implicit none
private
public :: xxxx_vars
public :: xxxx_vars_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to represent QG model variables
type :: xxxx_vars
  integer :: nv
end type xxxx_vars

#define LISTED_TYPE xxxx_vars

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: xxxx_vars_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine c_xxxx_vars_create(c_key_self, c_conf) bind(c,name='xxxx_var_create_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(xxxx_vars), pointer :: self
character(len=2) :: svar

call xxxx_vars_registry%init()
call xxxx_vars_registry%add(c_key_self)
call xxxx_vars_registry%get(c_key_self, self)

! Add code to allocate and initialize self here

return
end subroutine c_xxxx_vars_create

! ------------------------------------------------------------------------------

subroutine c_xxxx_vars_clone(c_key_self, c_key_other) bind(c,name='xxxx_var_clone_f90')
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(xxxx_vars), pointer :: self, other

call xxxx_vars_registry%get(c_key_self, self)
call xxxx_vars_registry%add(c_key_other)
call xxxx_vars_registry%get(c_key_other, other)

! Add code to copy other into self here

end subroutine c_xxxx_vars_clone

! ------------------------------------------------------------------------------

subroutine c_xxxx_vars_delete(c_key_self) bind(c,name='xxxx_var_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self
type(xxxx_vars), pointer :: self

call xxxx_vars_registry%get(c_key_self, self)
call xxxx_vars_registry%remove(c_key_self)

! Add code to deallocate self here

return
end subroutine c_xxxx_vars_delete

! ------------------------------------------------------------------------------

end module xxxx_vars_mod
