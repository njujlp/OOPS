
!> Fortran module handling geometry for the FV3-GFS model

module fv3gfs_geom_mod

use iso_c_binding
use config_mod
use mpp_domains_mod, only: domain2D

implicit none
private
public :: fv3gfs_geom_registry

! ------------------------------------------------------------------------------

#define LISTED_TYPE domain2D

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
use fv3gfs_mod, only: setup_geom
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

integer, parameter :: max_string_length=80 ! Yuk!
type(domain2D), pointer :: self
character(len=max_string_length) :: gtype
integer :: nx, ny, ntile, halo
integer :: layout(2) = (/4,2/)
integer :: io_layout(2) = (/2,2/)
integer :: halo = 1

call fv3gfs_geom_registry%init()
call fv3gfs_geom_registry%add(c_key_self)
call fv3gfs_geom_registry%get(c_key_self,self)

gtype = config_get_string(c_conf,len(gtype),"gridtype")
if (gtype == "cubic_grid") then
  nx = config_get_int(c_conf,"csize")
  ny = nx
  ntile = 6
elseif (gtype == "latlon_grid") then
  nx = config_get_int(c_conf,"xsize")
  ny = config_get_int(c_conf,"ysize")
  ntile = 1
else
  call abor1_ftn("fv3gfs_geo_setup: unkown grid type")
endif
call setup_geom(self, gtype, nx, ny, ntile, layout, io_layout, halo)

end subroutine c_fv3gfs_geo_setup

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_clone(c_key_self, c_key_other) bind(c,name='fv3gfs_geo_clone_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(domain2D), pointer :: self, other

call fv3gfs_geom_registry%add(c_key_other)
call fv3gfs_geom_registry%get(c_key_other, other)
call fv3gfs_geom_registry%get(c_key_self , self)

self = other
! or
call copy_geom(other, self)

end subroutine c_fv3gfs_geo_clone

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_delete(c_key_self) bind(c,name='fv3gfs_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self     

call fv3gfs_geom_registry%get(c_key_self, self)
! deallocate whatever is in self
call fv3gfs_geom_registry%remove(c_key_self)

end subroutine c_fv3gfs_geo_delete

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_info(c_key_self, c_n) bind(c,name='fv3gfs_geo_info_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_n
type(domain2D), pointer :: self

call fv3gfs_geom_registry%get(c_key_self, self)
! get a few numbers back to C++ to print so that one can have a quick idea
! about the definition of the domain
c_n = self%...

end subroutine c_fv3gfs_geo_info

! ------------------------------------------------------------------------------

end module fv3gfs_geom_mod
