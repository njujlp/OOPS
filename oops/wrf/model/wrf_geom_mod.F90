
!> Fortran module handling geometry for the QG model

module wrf_geom_mod

use iso_c_binding
use config_mod

implicit none
private
public :: wrf_geom
public :: wrf_geom_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold geometry data for the QG model
type :: wrf_geom
  integer :: west_east
  integer :: south_north
  integer :: bottom_top
end type wrf_geom

#define LISTED_TYPE wrf_geom

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrf_geom_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine c_wrf_geo_setup(c_key_self, c_conf) bind(c,name='wrf_geo_setup_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(wrf_geom), pointer :: self

call wrf_geom_registry%init()
call wrf_geom_registry%add(c_key_self)
call wrf_geom_registry%get(c_key_self,self)

!self%west_east   = config_get_int(c_conf, "west_east")
!self%south_north = config_get_int(c_conf, "south_north")
!self%bottom_top  = config_get_int(c_conf, "bottom_top")

end subroutine c_wrf_geo_setup

! ------------------------------------------------------------------------------

subroutine c_wrf_geo_clone(c_key_self, c_key_other) bind(c,name='wrf_geo_clone_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(wrf_geom), pointer :: self, other

call wrf_geom_registry%add(c_key_other)
call wrf_geom_registry%get(c_key_other, other)
call wrf_geom_registry%get(c_key_self , self )
other%west_east   = self%west_east
other%south_north = self%south_north
other%bottom_top  = self%bottom_top

end subroutine c_wrf_geo_clone

! ------------------------------------------------------------------------------

subroutine c_wrf_geo_delete(c_key_self) bind(c,name='wrf_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self     

call wrf_geom_registry%remove(c_key_self)

end subroutine c_wrf_geo_delete

! ------------------------------------------------------------------------------

subroutine c_wrf_geo_info(c_key_self, c_west_east, c_south_north, c_bottom_top) bind(c,name='wrf_geo_info_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_west_east
integer(c_int), intent(inout) :: c_south_north
integer(c_int), intent(inout) :: c_bottom_top
type(wrf_geom), pointer :: self

call wrf_geom_registry%get(c_key_self , self )
c_west_east   = self%west_east
c_south_north = self%south_north
c_bottom_top  = self%bottom_top

end subroutine c_wrf_geo_info

! ------------------------------------------------------------------------------

end module wrf_geom_mod
