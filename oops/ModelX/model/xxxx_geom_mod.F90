
!> Fortran module handling geometry for an unknown model

module xxxx_geom_mod

use iso_c_binding
use config_mod

implicit none
private
public :: xxxx_geom
public :: xxxx_geom_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold geometry data for the QG model
type :: xxxx_geom
  integer :: nx  ! Do not keep, use what is releveant for the model
  integer :: ny  ! Do not keep, use what is releveant for the model
end type xxxx_geom

#define LISTED_TYPE xxxx_geom

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: xxxx_geom_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine c_xxxx_geo_setup(c_key_self, c_conf) bind(c,name='xxxx_geo_setup_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(xxxx_geom), pointer :: self

call xxxx_geom_registry%init()
call xxxx_geom_registry%add(c_key_self)
call xxxx_geom_registry%get(c_key_self,self)

! Add code to allocate and initilize geometry here

end subroutine c_xxxx_geo_setup

! ------------------------------------------------------------------------------

subroutine c_xxxx_geo_clone(c_key_self, c_key_other) bind(c,name='xxxx_geo_clone_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(xxxx_geom), pointer :: self, other

call xxxx_geom_registry%add(c_key_other)
call xxxx_geom_registry%get(c_key_other, other)
call xxxx_geom_registry%get(c_key_self , self )

! Add code to copy geometry here

end subroutine c_xxxx_geo_clone

! ------------------------------------------------------------------------------

subroutine c_xxxx_geo_delete(c_key_self) bind(c,name='xxxx_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self     

call xxxx_geom_registry%remove(c_key_self)

! Add code to deallocate geometry here

end subroutine c_xxxx_geo_delete

! ------------------------------------------------------------------------------

end module xxxx_geom_mod
