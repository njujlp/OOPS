!----------------------------------------------------------------------
! Module: module_ifs.f90
!> Purpose: IFS model routines
!> <br>
!> Author: Benjamin Menetrier
!> <br>
!> Licensing: this code is distributed under the CeCILL-C license
!> <br>
!> Copyright © 2017 METEO-FRANCE
!----------------------------------------------------------------------
module model_ifs

use module_namelist, only: nam
use netcdf
use tools_const, only: pi,deg2rad,ps
use tools_kinds,only: kind_real
use tools_missing, only: msvalr,msr,isanynotmsr,isallnotmsr
use tools_nc, only: ncerr,ncfloat
use type_ndata, only: ndatatype,ndata_alloc

implicit none

private
public :: model_ifs_coord,model_ifs_read,model_ifs_write

contains

!----------------------------------------------------------------------
! Subroutine: model_ifs_coord
!> Purpose: get IFS coordinates
!----------------------------------------------------------------------
subroutine model_ifs_coord(ndata)

implicit none

! Passed variables
type(ndatatype),intent(inout) :: ndata !< Sampling data

! Local variables
integer :: ilon,ilat
integer :: ncid,nlon_id,nlat_id,nlev_id,lon_id,lat_id,pres_id
real(kind=4),allocatable :: lon(:,:),lat(:,:),pres(:)
character(len=1024) :: subr = 'model_ifs_coord'

! Open file and get dimensions
call ncerr(subr,nf90_open(trim(nam%datadir)//'/grid.nc',nf90_nowrite,ncid))
call ncerr(subr,nf90_inq_dimid(ncid,'longitude',nlon_id))
call ncerr(subr,nf90_inq_dimid(ncid,'latitude',nlat_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlon_id,len=ndata%nlon))
call ncerr(subr,nf90_inquire_dimension(ncid,nlat_id,len=ndata%nlat))
ndata%nc0 = ndata%nlon*ndata%nlat
call ncerr(subr,nf90_inq_dimid(ncid,'level',nlev_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlev_id,len=ndata%nlev))

! Allocation
allocate(lon(ndata%nlon,ndata%nlat))
allocate(lat(ndata%nlon,ndata%nlat))
allocate(pres(ndata%nlev))

! Read data and close file
call ncerr(subr,nf90_inq_varid(ncid,'longitude',lon_id))
call ncerr(subr,nf90_inq_varid(ncid,'latitude',lat_id))
call ncerr(subr,nf90_inq_varid(ncid,'pf',pres_id))
call ncerr(subr,nf90_get_var(ncid,lon_id,lon(:,1)))
call ncerr(subr,nf90_get_var(ncid,lat_id,lat(1,:)))
call ncerr(subr,nf90_get_var(ncid,pres_id,pres))
call ncerr(subr,nf90_close(ncid))

! Compute normalized area
allocate(ndata%area(ndata%nl0))
ndata%area = 4.0*pi

! Convert to radian
lon(:,1) = lon(:,1)*real(deg2rad,kind=4)
lat(1,:) = lat(1,:)*real(deg2rad,kind=4)

! Fill arrays
do ilat=1,ndata%nlat
   lon(:,ilat) = lon(:,1)
end do
do ilon=1,ndata%nlon
   lat(ilon,:) = lat(1,:)
end do

! Pack
call ndata_alloc(ndata)
ndata%lon = pack(real(lon,kind_real),mask=.true.)
ndata%lat = pack(real(lat,kind_real),mask=.true.)
ndata%mask = .true.

! Vertical unit
if (nam%logpres) then
   ndata%vunit = log(pres(nam%levs(1:ndata%nl0)))
else
   ndata%vunit = float(nam%levs(1:ndata%nl0))
end if

! Release memory
deallocate(lon)
deallocate(lat)

end subroutine model_ifs_coord

!----------------------------------------------------------------------
! Subroutine: model_ifs_read
!> Purpose: read IFS field
!----------------------------------------------------------------------
subroutine model_ifs_read(ncid,varname,ndata,fld)

implicit none

! Passed variables
integer,intent(in) :: ncid                              !< NetCDF file ID
character(len=*),intent(in) :: varname                  !< Variable name
type(ndatatype),intent(in) :: ndata                     !< Sampling data
real(kind_real),intent(out) :: fld(ndata%nc0,ndata%nl0) !< Read field

! Local variables
integer :: il0
integer :: fld_id
real(kind=4) :: fld_loc(ndata%nlon,ndata%nlat)
character(len=1024) :: subr = 'model_ifs_read'

! Initialize field
call msr(fld)

! Get variable id
call ncerr(subr,nf90_inq_varid(ncid,trim(varname),fld_id))

! Read variable
do il0=1,ndata%nl0
   call ncerr(subr,nf90_get_var(ncid,fld_id,fld_loc,(/1,1,nam%levs(il0),1/),(/ndata%nlon,ndata%nlat,1,1/)))
   fld(:,il0) = pack(real(fld_loc,kind_real),mask=.true.)
end do

end subroutine model_ifs_read

!----------------------------------------------------------------------
! Subroutine: model_ifs_write
!> Purpose: write IFS field
!----------------------------------------------------------------------
subroutine model_ifs_write(ncid,varname,ndata,fld)

implicit none

! Passed variables
integer,intent(in) :: ncid                             !< NetCDF file ID
character(len=*),intent(in) :: varname                 !< Variable name
type(ndatatype),intent(in) :: ndata                    !< Sampling data
real(kind_real),intent(in) :: fld(ndata%nc0,ndata%nl0) !< Written field

! Local variables
integer :: il0,ierr
integer :: nlon_id,nlat_id,nlev_id,nt_id,fld_id
real(kind_real) :: fld_loc(ndata%nlon,ndata%nlat)
logical :: mask_unpack(ndata%nlon,ndata%nlat)
character(len=1024) :: subr = 'model_ifs_write'

! Initialization
mask_unpack = .true.

! Get variable id
ierr = nf90_inq_varid(ncid,trim(varname),fld_id)

! Define dimensions and variable if necessary
if (ierr/=nf90_noerr) then
   call ncerr(subr,nf90_redef(ncid))
   ierr = nf90_inq_dimid(ncid,'longitude',nlon_id)
   if (ierr/=nf90_noerr) call ncerr(subr,nf90_def_dim(ncid,'longitude',ndata%nlon,nlon_id))
   ierr = nf90_inq_dimid(ncid,'latitude',nlat_id)
   if (ierr/=nf90_noerr) call ncerr(subr,nf90_def_dim(ncid,'latitude',ndata%nlat,nlat_id))
   ierr = nf90_inq_dimid(ncid,'level',nlev_id)
   if (ierr/=nf90_noerr) call ncerr(subr,nf90_def_dim(ncid,'level',ndata%nl0,nlev_id))
   ierr = nf90_inq_dimid(ncid,'time',nt_id)
   if (ierr/=nf90_noerr) call ncerr(subr,nf90_def_dim(ncid,'time',1,nt_id))
   call ncerr(subr,nf90_def_var(ncid,trim(varname),ncfloat,(/nlon_id,nlat_id,nlev_id,nt_id/),fld_id))
   call ncerr(subr,nf90_put_att(ncid,fld_id,'_FillValue',msvalr))
   call ncerr(subr,nf90_enddef(ncid))
end if

! Write data
do il0=1,ndata%nl0
   if (isanynotmsr(fld(:,il0))) then
      call msr(fld_loc)
      fld_loc = unpack(fld(:,il0),mask=mask_unpack,field=fld_loc)
      call ncerr(subr,nf90_put_var(ncid,fld_id,fld_loc,(/1,1,il0,1/),(/ndata%nlon,ndata%nlat,1,1/)))
   end if
end do

end subroutine model_ifs_write

end module model_ifs
