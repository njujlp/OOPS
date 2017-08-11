
!> Fortran module handling geometry for the WRF model

module wrf_geom_mod

use iso_c_binding
use config_mod
use kinds
use tools_nc
use netcdf

implicit none
private
public :: wrf_geom
public :: wrf_geom_registry

! ------------------------------------------------------------------------------

integer, parameter :: max_string_length = 256

! ------------------------------------------------------------------------------

!> Fortran derived type to hold geometry data for the WRF model
type :: wrf_geom
  integer :: nlon
  integer :: nlat
  integer :: nlev
  character(len=max_string_length) :: gridfname
  real(kind=kind_real) :: dx
  real(kind=kind_real) :: dy
  real(kind=kind_real), DIMENSION (:,:), ALLOCATABLE :: lon
  real(kind=kind_real), DIMENSION (:,:), ALLOCATABLE :: lat
  real(kind=kind_real), DIMENSION (:,:), ALLOCATABLE :: mask
  real(kind=kind_real), DIMENSION (:),   ALLOCATABLE :: pres
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

!type(datetime), intent(inout) :: vdate    !< DateTime

integer :: ncid,nlon_id,nlat_id,nlev_id
integer :: lon_id,lat_id,pres_id,mask_id
integer :: iread
character (len=max_string_length) :: subr

call wrf_geom_registry%init()
call wrf_geom_registry%add(c_key_self)
call wrf_geom_registry%get(c_key_self,self)


!> Read the WRF file name to load in the namelist gridfname
!iread = 1
!if (config_element_exists(c_conf,"gridfname")) then
!  iread = config_get_int(c_conf,"gridfname")
!endif

!self%gridfname = config_get_string(c_conf, len(self%gridfname), "gridfname")
 self%gridfname = config_get_string(c_conf, max_string_length, "gridfname")
 subr = self%gridfname

!> Open file gridfname
call ncerr(subr,nf90_open(trim(self%gridfname),nf90_nowrite,ncid))

!> Get file dimensions
call ncerr(subr,nf90_inq_dimid(ncid,'west_east',nlon_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlon_id,len=self%nlon))

call ncerr(subr,nf90_inq_dimid(ncid,'south_north',nlat_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlat_id,len=self%nlat))

call ncerr(subr,nf90_inq_dimid(ncid,'bottom_top',nlev_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlev_id,len=self%nlev))

!> Read grid increments DX and DY
call ncerr(subr,nf90_get_att(ncid,nf90_global,'DX',self%dx))
call ncerr(subr,nf90_get_att(ncid,nf90_global,'DY',self%dy))

write (*,*) " west_east   = ",self%nlon
write (*,*) " south_north = ",self%nlat
write (*,*) " bottom_top  = ",self%nlev
write (*,*) " DX  = ",self%dx
write (*,*) " DY  = ",self%dy

!> Allocate memory
allocate(self%lon(self%nlon,self%nlat))
allocate(self%lat(self%nlon,self%nlat))
allocate(self%mask(self%nlon,self%nlat))
allocate(self%pres(self%nlev))

!> Read longitude XLONG
call ncerr(subr,nf90_inq_varid(ncid,'XLONG',lon_id))
call ncerr(subr,nf90_get_var(ncid,lon_id,self%lon,(/1,1,1/),(/self%nlon,self%nlat,1/)))
write (*,*) "XLON = ",self%lon(1,1)

!> Read latitude XLAT
call ncerr(subr,nf90_inq_varid(ncid,'XLAT',lat_id))
call ncerr(subr,nf90_get_var(ncid,lat_id,self%lat,(/1,1,1/),(/self%nlon,self%nlat,1/)))
write (*,*) "XLAT = ",self%lat(1,1)

!> Read mask MASK
call ncerr(subr,nf90_inq_varid(ncid,'LANDMASK',mask_id))
call ncerr(subr,nf90_get_var(ncid,mask_id,self%mask,(/1,1,1/),(/self%nlon,self%nlat,1/)))
write (*,*) "MASK = ",self%mask(1,1)

!> Read pressure PB
call ncerr(subr,nf90_inq_varid(ncid,'PB',pres_id))
call ncerr(subr,nf90_get_var(ncid,pres_id,self%pres))
write (*,*) "PB = ",self%pres(1)

!> close file
call ncerr(subr,nf90_close(ncid))


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
other%nlon = self%nlon
other%nlat = self%nlat
other%nlev = self%nlev

end subroutine c_wrf_geo_clone

! ------------------------------------------------------------------------------

subroutine c_wrf_geo_delete(c_key_self) bind(c,name='wrf_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self     

call wrf_geom_registry%remove(c_key_self)

end subroutine c_wrf_geo_delete

! ------------------------------------------------------------------------------

subroutine c_wrf_geo_info(c_key_self, c_nlon, c_nlat, c_nlev) bind(c,name='wrf_geo_info_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_nlon
integer(c_int), intent(inout) :: c_nlat
integer(c_int), intent(inout) :: c_nlev
type(wrf_geom), pointer :: self

call wrf_geom_registry%get(c_key_self , self )
c_nlon = self%nlon
c_nlat = self%nlat
c_nlev = self%nlev

end subroutine c_wrf_geo_info

! ------------------------------------------------------------------------------

end module wrf_geom_mod
