
!> Fortran module handling geometry for the QG model

module fv3gfs_geom_mod

use iso_c_binding
use config_mod

implicit none
private
public :: fv3gfs_geom
public :: fv3gfs_geom_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold geometry data for the QG model
type :: fv3gfs_geom
  integer :: nx
  integer :: ny
end type fv3gfs_geom

#define LISTED_TYPE fv3gfs_geom

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: fv3gfs_geom_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_setup(c_key_self, c_conf) bind(c,name='fv3gfs_geo_setup_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(fv3gfs_geom), pointer :: self

call fv3gfs_geom_registry%init()
call fv3gfs_geom_registry%add(c_key_self)
call fv3gfs_geom_registry%get(c_key_self,self)

self%nx = config_get_int(c_conf, "nx")
self%ny = config_get_int(c_conf, "ny")

end subroutine c_fv3gfs_geo_setup

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_clone(c_key_self, c_key_other) bind(c,name='fv3gfs_geo_clone_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(fv3gfs_geom), pointer :: self, other

call fv3gfs_geom_registry%add(c_key_other)
call fv3gfs_geom_registry%get(c_key_other, other)
call fv3gfs_geom_registry%get(c_key_self , self )
other%nx = self%nx
other%ny = self%ny

end subroutine c_fv3gfs_geo_clone

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_delete(c_key_self) bind(c,name='fv3gfs_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self     

call fv3gfs_geom_registry%remove(c_key_self)

end subroutine c_fv3gfs_geo_delete

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_info(c_key_self, c_nx, c_ny) bind(c,name='fv3gfs_geo_info_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_nx
integer(c_int), intent(inout) :: c_ny
type(fv3gfs_geom), pointer :: self

call fv3gfs_geom_registry%get(c_key_self , self )
c_nx = self%nx
c_ny = self%ny

end subroutine c_fv3gfs_geo_info

! ------------------------------------------------------------------------------

end module fv3gfs_geom_mod
