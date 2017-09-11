
!> Handle fields for the WRF model

module wrf_fields

use iso_c_binding
use wrf_geom_mod
use wrf_vars_mod
use config_mod
use kinds
use tools_nc
use string_f_c_mod
use datetime_mod
use netcdf

implicit none
private

public :: wrf_field, &
        & create, delete, zeros, random, copy, &
        & self_add, self_schur, self_sub, self_mul, axpy, &
        & dot_prod, add_incr, diff_incr, &
        & read_file, write_file, gpnorm, fldrms, &
        & change_resol, &
        & convert_from_ug, convert_to_ug, dirac
public :: wrf_field_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold QG fields
type :: wrf_field
  type(wrf_geom), pointer :: geom                    !< Geometry
  integer :: nx                     !< Zonal grid dimension
  integer :: ny                     !< Meridional grid dimension
  integer :: nz                     !< Number of levels
  integer :: nf                     !< Number of fields
  logical :: lbc                    !< North-South boundary is present
  real(kind=kind_real), pointer :: gfld3d(:,:,:)    !< 3D fields
  real(kind=kind_real), allocatable :: U(:,:,:)         !< x-wind component
  real(kind=kind_real), allocatable :: V(:,:,:)         !< y-wind component
  real(kind=kind_real), allocatable :: T(:,:,:)         !< perturbation potential temperature (theta-t0)
  real(kind=kind_real), allocatable :: Q(:,:,:)    !< water vapor mixing ratio
  real(kind=kind_real), allocatable :: P(:,:,:)         !< perturbation pressure
  real(kind=kind_real), allocatable :: H(:,:,:)         !< perturbation pressure
  character(len=max_string_length), allocatable :: fldnames(:) !< Variable identifiers
  character(len=max_string_length) :: filename !< File name
  real(kind=kind_real), pointer :: qbound(:,:)         !< perturbation pressure
  real(kind=kind_real), pointer :: xbound(:)         !< perturbation pressure
  real(kind=kind_real), pointer :: x(:)         !< perturbation pressure
end type wrf_field

#define LISTED_TYPE wrf_field

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrf_field_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine create(self, geom, vars)
implicit none
type(wrf_field), intent(inout) :: self
type(wrf_geom),  pointer, intent(in)    :: geom
type(wrf_vars),  intent(in)    :: vars

  self%geom => geom
  self%nx = geom%nlon
  self%ny = geom%nlat
  self%nz = geom%nlev
  self%nf = vars%nv

! write(*,*) 'nlon = ',geom%nlon,' nlat = ',geom%nlat,' nlev = ',geom%nlev,'nvar = ',vars%nv

  allocate(self%U(self%nx,self%ny,self%nz))
  allocate(self%V(self%nx,self%ny,self%nz))
  allocate(self%T(self%nx,self%ny,self%nz))
  allocate(self%Q(self%nx,self%ny,self%nz))
  allocate(self%P(self%nx,self%ny,self%nz))
  allocate(self%H(self%nx,self%ny,self%nz))

  allocate (self%fldnames(self%nf))
  self%fldnames = vars%fldnames

  self%U = 0.0_kind_real
  self%V = 0.0_kind_real
  self%T = 0.0_kind_real
  self%Q = 0.0_kind_real
  self%P = 0.0_kind_real
  self%H = 0.0_kind_real

  call check(self)

return
end subroutine create

! ------------------------------------------------------------------------------

subroutine delete(self)
implicit none
type(wrf_field), intent(inout) :: self

 call check(self)

  if (allocated(self%U)) deallocate(self%U)
  if (allocated(self%V)) deallocate(self%V)
  if (allocated(self%T)) deallocate(self%T)
  if (allocated(self%Q)) deallocate(self%Q)
  if (allocated(self%P)) deallocate(self%P)
  if (allocated(self%H)) deallocate(self%H)
  if (allocated(self%fldnames)) deallocate(self%fldnames)

return
end subroutine delete

! ------------------------------------------------------------------------------

subroutine zeros(self)
implicit none
type(wrf_field), intent(inout) :: self

  call check(self)

  self%U = 0.0_kind_real
  self%V = 0.0_kind_real
  self%T = 0.0_kind_real
  self%Q = 0.0_kind_real
  self%P = 0.0_kind_real
  self%H = 0.0_kind_real

return
end subroutine zeros

! ------------------------------------------------------------------------------

subroutine random(self)
use random_vectors_mod
implicit none
type(wrf_field), intent(inout) :: self

  call check(self)

  call random_vector(self%U)
  call random_vector(self%V)
  call random_vector(self%T)
  call random_vector(self%Q)
  call random_vector(self%P)
  call random_vector(self%H)

return
end subroutine random

! ------------------------------------------------------------------------------

subroutine copy(self,rhs)
implicit none
type(wrf_field), intent(inout) :: self
type(wrf_field), intent(in)    :: rhs
integer :: nf

  call check_resolution(self, rhs)

  self%U = rhs%U 
  self%V = rhs%V
  self%T = rhs%T
  self%Q = rhs%Q
  self%P = rhs%P
  self%H = rhs%H

return
end subroutine copy

! ------------------------------------------------------------------------------

subroutine self_add(self,rhs)
implicit none
type(wrf_field), intent(inout) :: self
type(wrf_field), intent(in)    :: rhs
integer :: nf

  call check_resolution(self, rhs)

  self%U = self%U + rhs%U
  self%V = self%V + rhs%V
  self%T = self%T + rhs%T
  self%Q = self%Q + rhs%Q
  self%P = self%P + rhs%P
  self%H = self%H + rhs%H

return
end subroutine self_add

! ------------------------------------------------------------------------------

subroutine self_schur(self,rhs)
implicit none
type(wrf_field), intent(inout) :: self
type(wrf_field), intent(in)    :: rhs
integer :: nf

  call check_resolution(self, rhs)

  self%U=self%U*rhs%U
  self%V=self%V*rhs%V
  self%T=self%T*rhs%T
  self%P=self%P*rhs%P
  self%Q=self%Q*rhs%Q
  self%P=self%P*rhs%P
  self%H=self%H*rhs%H

return
end subroutine self_schur

! ------------------------------------------------------------------------------

subroutine self_sub(self,rhs)
implicit none
type(wrf_field), intent(inout) :: self
type(wrf_field), intent(in)    :: rhs
integer :: nf

  call check_resolution(self, rhs)

  self%U = self%U - rhs%U
  self%V = self%V - rhs%V
  self%T = self%T - rhs%T
  self%Q = self%Q - rhs%Q
  self%P = self%P - rhs%P
  self%H = self%H - rhs%H

return
end subroutine self_sub

! ------------------------------------------------------------------------------

subroutine self_mul(self,zz)
implicit none
type(wrf_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz

  call check(self)

  self%U = zz * self%U
  self%V = zz * self%V
  self%T = zz * self%T
  self%Q = zz * self%Q
  self%P = zz * self%P
  self%H = zz * self%H

return
end subroutine self_mul

! ------------------------------------------------------------------------------

subroutine axpy(self,zz,rhs)
implicit none
type(wrf_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz
type(wrf_field), intent(in)    :: rhs
integer :: nf

  call check_resolution(self, rhs)

  self%U = self%U + zz * rhs%U
  self%V = self%V + zz * rhs%V
  self%T = self%T + zz * rhs%T
  self%Q = self%Q + zz * rhs%Q
  self%P = self%P + zz * rhs%P
  self%H = self%H + zz * rhs%H

return
end subroutine axpy

! ------------------------------------------------------------------------------

subroutine dot_prod(fld1,fld2,zprod)
implicit none
type(wrf_field), intent(in) :: fld1, fld2
real(kind=kind_real), intent(inout) :: zprod
real(kind=kind_real) :: zz
integer :: jx, jy, jz

  call check_resolution(fld1, fld2)

  if (fld1%nf /= fld2%nf .or. fld1%nz /= fld2%nz) then
     call abor1_ftn("wrf_fields:field_prod error number of fields")
  endif

! if (fld1%lbc .or. fld2%lbc) then
!    call abor1_ftn("wrf_fields:field_prod should never dot_product full state")
! endif

  zprod = 0.0_kind_real
  zz = 0.0_kind_real

  do jy=1,fld1%ny
     do jx=1,fld1%nx
        do jz=1,fld1%nz
           zz = zz + fld1%H(jx,jy,jz) * fld2%H(jx,jy,jz)
           zz = zz + fld1%P(jx,jy,jz) * fld2%P(jx,jy,jz)
           zz = zz + fld1%U(jx,jy,jz) * fld2%U(jx,jy,jz)
           zz = zz + fld1%V(jx,jy,jz) * fld2%V(jx,jy,jz)
           zz = zz + fld1%T(jx,jy,jz) * fld2%T(jx,jy,jz)
           zz = zz + fld1%Q(jx,jy,jz) * fld2%Q(jx,jy,jz)
        enddo
     enddo
  enddo

! zprod = &
!       + DOT_PRODUCT (fld1%U,fld2%U) &
!       + DOT_PRODUCT (fld1%V,fld2%V) &
!       + DOT_PRODUCT (fld1%T,fld2%T) &
!       + DOT_PRODUCT (fld1%Q,fld2%Q) &
!       + DOT_PRODUCT (fld1%P,fld2%P) &
!       + DOT_PRODUCT (fld1%H,fld2%H)

return
end subroutine dot_prod

! ------------------------------------------------------------------------------

subroutine add_incr(self,rhs)
implicit none
type(wrf_field), intent(inout) :: self
type(wrf_field), intent(in)    :: rhs

  call check(self)
  call check(rhs)

  if (self%nx==rhs%nx .and. self%ny==rhs%ny) then
      self%U = self%U + rhs%U
      self%V = self%V + rhs%V
      self%T = self%T + rhs%T
      self%Q = self%Q + rhs%Q
      self%P = self%P + rhs%P
      self%H = self%H + rhs%H
  else
      call abor1_ftn("wrf_fields:add_incr: not coded for low res increment yet")
  endif

return
end subroutine add_incr

! ------------------------------------------------------------------------------

subroutine diff_incr(lhs,x1,x2)
implicit none
type(wrf_field), intent(inout) :: lhs
type(wrf_field), intent(in)    :: x1
type(wrf_field), intent(in)    :: x2

  call check(lhs)
  call check(x1)
  call check(x2)

  call zeros(lhs)

  if (x1%nx==x2%nx .and. x1%ny==x2%ny) then

      if (lhs%nx==x1%nx .and. lhs%ny==x1%ny) then
         lhs%U = x1%U - x2%U
         lhs%V = x1%V - x2%V
         lhs%T = x1%T - x2%T
         lhs%Q = x1%Q - x2%Q
         lhs%P = x1%P - x2%H
         lhs%H = x1%H - x2%H
      else
         call abor1_ftn("wrf_fields:diff_incr: not coded for low res increment yet")
      endif

  else
      call abor1_ftn("wrf_fields:diff_incr: states not at same resolution")
  endif

return
end subroutine diff_incr

! ------------------------------------------------------------------------------

subroutine change_resol(fld,rhs)
implicit none
type(wrf_field), intent(inout) :: fld
type(wrf_field), intent(in)    :: rhs
real(kind=kind_real), allocatable :: ztmp(:,:)
real(kind=kind_real) :: dy1, dy2, ya, yb, dx1, dx2, xa, xb
integer :: jx, jy, jf, iy, ia, ib

  call check(fld)
  call check(rhs)

  if (fld%nx==rhs%nx .and. fld%ny==rhs%ny) then
      call copy(fld, rhs)
  else
      call abor1_ftn("wrf_fields:field_resol: untested code")
  endif

return
end subroutine change_resol

! ------------------------------------------------------------------------------
subroutine read_file(fld, c_conf, vdate)

implicit none

type(wrf_field), intent(inout) :: fld    !< Fields
type(c_ptr), intent(in)       :: c_conf   !< Configuration
type(datetime), intent(inout) :: vdate    !< DateTime

!type(wrf_geom), pointer :: self

character(len=20) :: sdate, fmtn

integer :: ncid,nlon_id,nlat_id,nlev_id
integer :: P_id,H_id,U_id,V_id,T_id,QVAPOR_id,PB_id,PH_id,PHB_id
integer :: west_east, south_north,bottom_top
character (len=max_string_length) :: subr
character (len=max_string_length), DIMENSION (:,:,:), ALLOCATABLE :: fldnames
real(kind=kind_real), DIMENSION (:,:,:), ALLOCATABLE :: P,H,U,V,T,QVAPOR,PB,PH,PHB

integer :: nf 
nf = 0

!> Read the WRF file name to load in the namelist gridfname
if (.NOT. config_element_exists(c_conf,"filename")) then
    write (*,'(/,A,/)') "ERROR: Cannot find entry 'filename' in 'Fields' record"
 call abort
endif

 subr = config_get_string(c_conf, len(fld%filename), "filename")

!> Open file filename
call ncerr(subr,nf90_open(trim(subr),nf90_nowrite,ncid))

!> Get file dimensions
call ncerr(subr,nf90_inq_dimid(ncid,'west_east',nlon_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlon_id,len=west_east))

call ncerr(subr,nf90_inq_dimid(ncid,'south_north',nlat_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlat_id,len=south_north))

call ncerr(subr,nf90_inq_dimid(ncid,'bottom_top',nlev_id))
call ncerr(subr,nf90_inquire_dimension(ncid,nlev_id,len=bottom_top))

write (*,*) " west_east   = ",west_east
write (*,*) " south_north = ",south_north
write (*,*) " bottom_top  = ",bottom_top

!> Define dimensions
   fld%nx = west_east
   fld%ny = south_north
   fld%nz = bottom_top

!> Find the valid date and time
  sdate = config_get_string(c_conf,len(sdate),"date")
  WRITE(*,*) 'validity date is: '//sdate
  call datetime_set(sdate, vdate)

!> Read temperature

allocate (T  (west_east,south_north,bottom_top))
T = 0_kind_real

call ncerr(subr,nf90_inq_varid(ncid,'T',T_id))
call ncerr(subr,nf90_get_var(ncid,T_id,T,(/1,1,1/),(/west_east,south_north,bottom_top/)))

!write (*,*) " T    = ",T  (west_east/2,south_north/2,bottom_top/2)

!> Read pressure and pressure perturbation

allocate (PB (west_east,south_north,bottom_top))
PB = 0_kind_real
call ncerr(subr,nf90_inq_varid(ncid,'PB',PB_id))
call ncerr(subr,nf90_get_var(ncid,PB_id,PB,(/1,1,1/),(/west_east,south_north,bottom_top/)))

!write (*,*) " PB   = ",PB  (west_east/2,south_north/2,bottom_top/2)

allocate (P  (west_east,south_north,bottom_top))
P = 0_kind_real
call ncerr(subr,nf90_inq_varid(ncid,'P',P_id))
call ncerr(subr,nf90_get_var(ncid,P_id,P,(/1,1,1/),(/west_east,south_north,bottom_top/)))

!write (*,*) " P    = ",P   (west_east/2,south_north/2,bottom_top/2)

!> Read height and heigh perturbation

allocate (PHB (west_east,south_north,bottom_top))
PHB = 0_kind_real

call ncerr(subr,nf90_inq_varid(ncid,'PHB',PHB_id))
call ncerr(subr,nf90_get_var(ncid,PHB_id,PHB,(/1,1,1/),(/west_east,south_north,bottom_top/)))

!write (*,*) " PHB  = ",PHB   (west_east/2,south_north/2,bottom_top/2)

allocate (PH (west_east,south_north,bottom_top))
PH = 0_kind_real

call ncerr(subr,nf90_inq_varid(ncid,'PH',PH_id))
call ncerr(subr,nf90_get_var(ncid,PH_id,PH,(/1,1,1/),(/west_east,south_north,bottom_top/)))

!write (*,*) " PH   = ",PH    (west_east/2,south_north/2,bottom_top/2)

!> Read water vapor mixing ratio

allocate (QVAPOR (west_east,south_north,bottom_top))
QVAPOR = 0_kind_real

call ncerr(subr,nf90_inq_varid(ncid,'QVAPOR',QVAPOR_id))
call ncerr(subr,nf90_get_var(ncid,QVAPOR_id,QVAPOR,(/1,1,1/),(/west_east,south_north,bottom_top/)))

!write (*,*) " QVAP = ",QVAPOR    (west_east/2,south_north/2,bottom_top/2)

!> Read wind x component (beware of the staggering)

allocate (U (west_east+1,south_north,bottom_top))
U = 0_kind_real

call ncerr(subr,nf90_inq_varid(ncid,'U',U_id))
call ncerr(subr,nf90_get_var(ncid,U_id,U,(/1,1,1/),(/west_east+1,south_north,bottom_top/)))

!write (*,*) " U = ",U    (west_east/2,south_north/2,bottom_top/2)

!> Read wind y component (beware of the staggering)

allocate (V (west_east,south_north+1,bottom_top))
V = 0_kind_real

call ncerr(subr,nf90_inq_varid(ncid,'V',V_id))
call ncerr(subr,nf90_get_var(ncid,V_id,V,(/1,1,1/),(/west_east,south_north+1,bottom_top/)))

!write (*,*) " V = ",V    (west_east/2,south_north/2,bottom_top/2)

!> Initialize fields counter
   nf = 0

!> Load pressure in hPa
   P = (P + PB) / 100_kind_real
   fld%P = P

   nf = nf + 1
   fld%fldnames(nf) = "P" 

!> Load geopotential height in m
   H = (PH+PHB) / 9.81_kind_real
   fld%H = H
   nf = nf + 1
   fld%fldnames(nf) = "H" 

!> Load temperature in kelvin
   T = (T+300_kind_real) * ( P/ 1000_kind_real)**(287.04_kind_real/1004_kind_real)
   fld%T = T

   nf = nf + 1
   fld%fldnames(nf) = "T" 

!> Load water vapor mixing ratio in kg/kg
   fld%Q = QVAPOR

   nf = nf + 1
   fld%fldnames(nf) = "Q" 

!> Load wind x component in m/s
   fld%U = 0_kind_real
   fld%U = 0.5*(U (1:west_east,  :,:) + &
                U (2:west_east+1,:,:))

   nf = nf + 1
   fld%fldnames(nf) = "U" 

!> Load wind y component in m/s
   fld%V = 0_kind_real
   fld%V = 0.5*(V (:,1:south_north,  :) + &
                V (:,2:south_north+1,:))

   nf = nf + 1
   fld%fldnames(nf) = "V" 

!> Free memory
  deallocate (U)
  deallocate (V)
  deallocate (T)
  deallocate (P)
  deallocate (PB)
  deallocate (PH)
  deallocate (PHB)
  deallocate (QVAPOR)

  write (*,*) " P = ",fld%P(west_east/2,south_north/2,bottom_top/2)
  write (*,*) " H = ",fld%H(west_east/2,south_north/2,bottom_top/2)
  write (*,*) " T = ",fld%T(west_east/2,south_north/2,bottom_top/2)
  write (*,*) " Q = ",fld%Q(west_east/2,south_north/2,bottom_top/2)
  write (*,*) " U = ",fld%U(west_east/2,south_north/2,bottom_top/2)
  write (*,*) " V = ",fld%V(west_east/2,south_north/2,bottom_top/2)

  return
end subroutine read_file

! ------------------------------------------------------------------------------
subroutine write_file(fld, c_conf, vdate)

implicit none

type(wrf_field), intent(inout) :: fld    !< Fields
type(c_ptr), intent(in)       :: c_conf   !< Configuration
type(datetime), intent(inout) :: vdate    !< DateTime

!type(wrf_geom), pointer :: self

character(len=20) :: sdate, fmtn

integer :: ncid,nlon_id,nlat_id,nlev_id,ntime_id
integer :: P_id,H_id,U_id,V_id,T_id,Q_id
integer :: QVAPOR_id,PB_id,PH_id,PHB_id
integer :: west_east, south_north,bottom_top
integer :: iret
character (len=max_string_length) :: subr = "write_fields"
character (len=max_string_length) :: filename

   call check(fld)

   filename = config_get_string(c_conf, len(filename), "fileout")

!> open file
   WRITE (*, '(2A)') "Open NETCDF file: ",TRIM (filename)
   iret = nf90_create(TRIM(filename), nf90_CLOBBER, ncid)
   CALL check_err (iret, subr)

!> define dimesions

   WRITE (*, '(A)') 'Define NETCDF dimensions:'

!  iret = nf90_def_dim(ncid, 'time', nf90_UNLIMITED, ntime_id)
!  CALL check_err (iret, subr)

   iret = nf90_def_dim(ncid, 'south_east',fld%nx, nlon_id)
   CALL check_err (iret, subr)

   iret = nf90_def_dim(ncid, 'south_north',fld%ny, nlat_id)
   CALL check_err (iret, subr)

   iret = nf90_def_dim(ncid, 'bottom_top', fld%nz, nlev_id)
   CALL check_err (iret, subr)

!> Assign global attributes

!  WRITE (*,'(A)') 'Define global attributes:'
!  iret = nf90_put_att (ncid, nf90_GLOBAL, 'DX', geom%DX)
!  CALL check_err (iret, subr)

!  iret = nf90_put_att (ncid, nf90_GLOBAL, 'DY', geo%DY)
!  CALL check_err (iret, subr)

!> Assign variables ids

   WRITE (*, '(A)') 'Assign variable ids:'

   iret = nf90_def_var (ncid, "P", nf90_double, &
         (/nlon_id,nlat_id,nlev_id/),P_id)

   CALL check_err (iret, subr)
!  CALL std_put_att(ncid, P_id, "Full pressure", "hPa")
   
   iret = nf90_def_var (ncid, "H", nf90_double, &
         (/nlon_id,nlat_id,nlev_id/),H_id)
   CALL check_err (iret, subr)
!  CALL std_put_att(ncid, P_id, "Geopotential height", "m")

   iret = nf90_def_var (ncid, "T", nf90_double, &
         (/nlon_id,nlat_id,nlev_id/),T_id)
   CALL check_err (iret, subr)
!  CALL std_put_att(ncid, P_id, "Temperature", "K")

   iret = nf90_def_var (ncid, "Q", nf90_double, &
         (/nlon_id,nlat_id,nlev_id/),Q_id)
   CALL check_err (iret, subr)
!  CALL std_put_att(ncid, P_id, "Water vapor mixing ratio", "kg/kg")

   iret = nf90_def_var (ncid, "U", nf90_double, &
         (/nlon_id,nlat_id,nlev_id/),U_id)
   CALL check_err (iret, subr)
!  CALL std_put_att(ncid, P_id, "X-wind components", "m/s")

   iret = nf90_def_var (ncid, "V", nf90_double, &
         (/nlon_id,nlat_id,nlev_id/),V_id)
   CALL check_err (iret, subr)
!  CALL std_put_att(ncid, P_id, "Y-wind components", "m/s")
   
   iret = nf90_enddef(ncid)
   CALL check_err (iret, subr)

!> Put the variables in the file

   WRITE (*, '(A)') 'Put variables:'

   WRITE (*, '(A)') 'Put variable: P'
   iret = nf90_put_var (ncid, P_id,fld%P)

   CALL check_err (iret, subr)

   WRITE (*, '(A)') 'Put variable: H'
   iret = nf90_put_var (ncid, H_id,fld%H)
   CALL check_err (iret, subr)

   iret = nf90_put_var (ncid, T_id, fld%T)
   CALL check_err (iret, subr)

   iret = nf90_put_var (ncid, Q_id, fld%Q)
   CALL check_err (iret, subr)

   iret = nf90_put_var (ncid, U_id, fld%U)
   CALL check_err (iret, subr)

   iret = nf90_put_var (ncid, V_id, fld%V)
   CALL check_err (iret, subr)

!> Close file

   iret = nf90_close(ncid)
   CALL check_err (iret, subr)
 
   return
end subroutine write_file

! ------------------------------------------------------------------------------
subroutine check_err (iret,subr)

  integer :: iret
  character (len=max_string_length) :: subr

  IF (iret .NE. nf90_NOERR) then
     WRITE (*,'(2A)') "ERROR in routine ",TRIM(subr)
     WRITE (*,'(2A)') "ERROR code: ",nf90_strerror(iret)
     CALL ABORT
  ENDIF

end subroutine check_err
! ------------------------------------------------------------------------------

subroutine gpnorm(fld, nf, pstat)
implicit none
type(wrf_field), intent(in) :: fld
integer, intent(in) :: nf
real(kind=kind_real), intent(inout) :: pstat(3, nf)
integer :: jj,joff

call check(fld)

!do jj=1,fld%nf
!  joff=(jj-1)*fld%nz
!  pstat(1,jj)=minval(fld%gfld3d(:,:,joff+1:joff+fld%nz))
!  pstat(2,jj)=maxval(fld%gfld3d(:,:,joff+1:joff+fld%nz))
!  pstat(3,jj)=sqrt(sum(fld%gfld3d(:,:,joff+1:joff+fld%nz)**2) &
!               & /real(fld%nz*fld%nx*fld%ny,kind_real))
!enddo
!jj=jj-1

!if (fld%lbc) then
!  jj=jj+1
!  pstat(1,jj)=minval(fld%xbound(:))
!  pstat(2,jj)=maxval(fld%xbound(:))
!  pstat(3,jj)=sqrt(sum(fld%xbound(:)**2)/real(4,kind_real))

!  jj=jj+1
!  pstat(1,jj)=minval(fld%qbound(:,:))
!  pstat(2,jj)=maxval(fld%qbound(:,:))
!  pstat(3,jj)=sqrt(sum(fld%qbound(:,:)**2)/real(4*fld%nx,kind_real))
!endif

!if (jj /= nf) call abor1_ftn("wrf_fields_gpnorm: error number of fields")

 pstat = 0

return
end subroutine gpnorm

! ------------------------------------------------------------------------------

subroutine fldrms (fld1,prms)
implicit none
type(wrf_field), intent(in) :: fld1
real(kind=kind_real), intent(inout) :: prms
real(kind=kind_real) :: zz
integer :: jx, jy, jz, ii

  prms = 0.0_kind_real

  call check(fld1) 

  zz = 0.0_kind_real
  ii = 0

  do jy=1,fld1%ny
     do jx=1,fld1%nx
        do jz=1,fld1%nz
!          write(*,*) jx,jy,jz
!          write(*,*) ' H = ',fld1%H(jx,jy,jz)
!          write(*,*) ' P = ',fld1%P(jx,jy,jz)
!          write(*,*) ' U = ',fld1%U(jx,jy,jz)
!          write(*,*) ' V = ',fld1%V(jx,jy,jz)
!          write(*,*) ' T = ',fld1%T(jx,jy,jz)
!          write(*,*) ' Q = ',fld1%Q(jx,jy,jz)
           zz = zz + fld1%H(jx,jy,jz) * fld1%H(jx,jy,jz)
           ii = ii + 1
           zz = zz + fld1%P(jx,jy,jz) * fld1%P(jx,jy,jz)
           ii = ii + 1
           zz = zz + fld1%U(jx,jy,jz) * fld1%U(jx,jy,jz)
           ii = ii + 1
           zz = zz + fld1%V(jx,jy,jz) * fld1%V(jx,jy,jz)
           ii = ii + 1
           zz = zz + fld1%T(jx,jy,jz) * fld1%T(jx,jy,jz)
           ii = ii + 1
           zz = zz + fld1%Q(jx,jy,jz) * fld1%Q(jx,jy,jz)
           ii = ii + 1
        enddo
     enddo
  enddo

! write(*,*) ' zz = ',zz,' ii = ',ii

  prms = sqrt(zz/real(ii,kind_real))

! write(*,*) ' prms = ',prms

  return
end subroutine fldrms
! ------------------------------------------------------------------------------

function common_vars(x1, x2)

implicit none
type(wrf_field), intent(in) :: x1, x2
integer :: common_vars
integer :: jf

! We assume here that one set of fields is a subset of the other,
! that fields are always in the same order starting with x,
! and that the common fields are the first ones.

common_vars = min(x1%nf, x2%nf)
do jf = 1, common_vars
  if (x1%fldnames(jf)/=x2%fldnames(jf)) &
    & call abor1_ftn("common_vars: fields do not match")
enddo
if (x1%nz /= x2%nz) call abor1_ftn("common_vars: error number of levels")
common_vars = x1%nz * common_vars

end function common_vars

! ------------------------------------------------------------------------------

subroutine check_resolution(x1, x2)

implicit none
type(wrf_field), intent(in) :: x1, x2

if (x1%nx /= x2%nx .or.  x1%ny /= x2%ny .or.  x1%nz /= x2%nz) then
  call abor1_ftn ("wrf_fields: resolution error")
endif
call check(x1)
call check(x2)

end subroutine check_resolution

! ------------------------------------------------------------------------------

subroutine check(self)
implicit none
type(wrf_field), intent(in) :: self
logical :: bad

bad = .FALSE.

!bad = .not.associated(self%gfld3d)

!bad = bad .or. (size(self%gfld3d, 1) /= self%nx)
!bad = bad .or. (size(self%gfld3d, 2) /= self%ny)
!bad = bad .or. (size(self%gfld3d, 3) /= self%nz*self%nf)

!bad = bad .or. .not.associated(self%x)

if (self%nf>1) then
  bad = bad .or. .not.allocated(self%p)
  bad = bad .or. .not.allocated(self%h)
  bad = bad .or. .not.allocated(self%t)
  bad = bad .or. .not.allocated(self%q)
  bad = bad .or. .not.allocated(self%u)
  bad = bad .or. .not.allocated(self%v)
else
  bad = bad .or. allocated(self%p)
  bad = bad .or. allocated(self%h)
  bad = bad .or. allocated(self%t)
  bad = bad .or. allocated(self%q)
  bad = bad .or. allocated(self%u)
  bad = bad .or. allocated(self%v)
endif

!allocate(self%fldnames(self%nf))

!if (self%lbc) then
!  bad = bad .or. .not.associated(self%xbound)
!  bad = bad .or. (size(self%xbound) /= 4)

!  bad = bad .or. .not.associated(self%qbound)
!  bad = bad .or. (size(self%qbound, 1) /= self%nx)
!  bad = bad .or. (size(self%qbound, 2) /= 4)
!else
!  bad = bad .or. associated(self%xbound)
!  bad = bad .or. associated(self%qbound)
!endif

 if (bad) then
!  write(0,*)'nx, ny, nf, nl, lbc = ',self%nx,self%ny,self%nf,self%nz,self%lbc
!  if (associated(self%gfld3d)) write(0,*)'shape(gfld3d) = ',shape(self%gfld3d)
!  if (associated(self%xbound)) write(0,*)'shape(xbound) = ',shape(self%xbound)
!  if (associated(self%qbound)) write(0,*)'shape(qbound) = ',shape(self%qbound)
   call abor1_ftn ("wrf_fields: field not consistent")
 endif

end subroutine check
! ------------------------------------------------------------------------------

subroutine dirac(self, c_conf)

use iso_c_binding
implicit none
type(wrf_field), intent(inout) :: self
type(c_ptr), intent(in)       :: c_conf   !< Configuration
integer :: ndir,idir,ildir,ifdir,ioff
integer,allocatable :: ixdir(:),iydir(:)
character(len=3) :: idirchar

call check(self)

! Get Diracs positions
ndir = config_get_int(c_conf,"ndir")
allocate(ixdir(ndir))
allocate(iydir(ndir))

do idir=1,ndir
   write(idirchar,'(i3)') idir
   ixdir(idir) = config_get_int(c_conf,"ixdir("//trim(adjustl(idirchar))//")")
   iydir(idir) = config_get_int(c_conf,"iydir("//trim(adjustl(idirchar))//")")
end do
ildir = config_get_int(c_conf,"ildir")
ifdir = config_get_int(c_conf,"ifdir")

! Check
if (ndir<1) call abor1_ftn("wrf_fields:dirac non-positive ndir")
if (any(ixdir<1).or.any(ixdir>self%nx)) call abor1_ftn("wrf_fields:dirac invalid ixdir")
if (any(iydir<1).or.any(iydir>self%ny)) call abor1_ftn("wrf_fields:dirac invalid iydir")
if ((ildir<1).or.(ildir>self%nz)) call abor1_ftn("wrf_fields:dirac invalid ildir")
if ((ifdir<1).or.(ifdir>self%nf)) call abor1_ftn("wrf_fields:dirac invalid ifdir")

! Setup Diracs
call zeros(self)

ioff = (ifdir-1)*self%nz

do idir=1,ndir
!  self%P(ixdir(idir),iydir(idir),ioff+ildir) = 1.0
!  self%H(ixdir(idir),iydir(idir),ioff+ildir) = 1.0
   self%T(ixdir(idir),iydir(idir),ioff+ildir) = 1.0
   self%Q(ixdir(idir),iydir(idir),ioff+ildir) = 1.0
   self%U(ixdir(idir),iydir(idir),ioff+ildir) = 1.0
   self%V(ixdir(idir),iydir(idir),ioff+ildir) = 1.0
 end do

end subroutine dirac

! ------------------------------------------------------------------------------

  subroutine convert_to_ug(self, ug)

    use unstructured_grid_mod

    implicit none
    type(wrf_field), intent(in) :: self
    type(unstructured_grid), intent(inout) :: ug
    real(kind=kind_real), allocatable :: zz(:)
    real(kind=kind_real), allocatable :: vv(:)
    integer, allocatable :: cmask(:)
    real(kind=kind_real)  :: area
    real(kind=kind_real)  :: zero = 0.
    integer :: jx,jy,jz,jk,glbind
    integer :: nz_total     ! Total number of levels in the 3D fields
    integer :: n_vars       ! Number of 3D variables 
    integer :: n_surf_vars  ! Number of surf vars (sould be 0 for ocean/ice)

!> code convert_to_ug

    print *,'in convert to ug ............'

!> Need the grid box area in m2
   area = self%geom%DX * self%geom%DY

!> Start with only one var
    n_vars = 1      
!> No surface var
    n_surf_vars = 0 

    nz_total = self%nz

!> Allocate memory for 3d fields
    allocate(zz(nz_total))
    allocate(vv(nz_total))
    allocate(cmask(nz_total))

!> Load vertical level distribution
    do jz = 1,nz_total
       zz(jz) = real(self%geom%levs(jz))
    end do

!> Create an ustructured grid 
    call create_unstructured_grid(ug, nz_total, zz)

!> Loop on horizontal dimensions
    do jy=1,self%ny

!      write(*,*) 'Processing latitude ',self%geom%lat(1,jy)

       do jx=1,self%nx

!> Define a global index
          glbind = (jy-1)*self%nx+jx

!> Initialise counter
          jk = 1

!> Load first variable T
          do jz = 1,self%nz                            ! T
!            cmask(jk) = int(self%geom%mask(jx,jy))    ! Do not use land mask
             cmask(jk) = 1
             vv(jk) = self%T(jx,jy,jz)
             jk = jk + 1
          end do

!      write(*,'(3I4,2(A,F10.5,A))') jx,jy,jz,' lon = ',self%geom%lon(jx,jy),'E, ',' lat = ',self%geom%lat(1,jy),'N'

!> Add the point on the unstructured grid
          call add_column(ug, self%geom%lat(jx,jy), self%geom%lon(jx,jy), &
               area, &
               nz_total, &
               n_vars, &
               n_surf_vars, &
               cmask, &
               1, glbind)
!              0)
          ug%last%column%fld3d(:) = vv(:)
!         ug%last%column%cols(:) = vv(:)

       enddo
    enddo

    deallocate(zz)
    deallocate(vv)
    deallocate(cmask)

  return
  end subroutine convert_to_ug

! -----------------------------------------------------------------------------

  subroutine convert_from_ug(self, ug)

    use unstructured_grid_mod

    implicit none
    type(wrf_field), intent(inout) :: self
    type(unstructured_grid), intent(in) :: ug
    type(column_element), pointer :: current
    real(kind=kind_real) :: dx, dy
    integer :: jx,jy,jz,jk
    integer :: n_vars       ! Number of 3D variables 
    integer :: n_surf_vars  ! Number of surf vars (sould be 0 for ocean/ice)

!> convert_from_ug: code inverse of convert_to_ug

    print *,'in convert from ug ............'

!> Start with only one var
    n_vars = 1      
!> No surface var
    n_surf_vars = 0

    current => ug%head

!> Loop over horizontal grid
    do jy=1,self%ny
       do jx=1,self%nx

!> initialize counter
          jk = 1

          do jz = 1,self%nz                                          ! T
!            self%T(jx,jy,jz) = current%column%cols(jk)
             self%T(jx,jy,jz) = current%column%fld3d(jk)
             jk = jk + 1
          end do

          current => current%next
       enddo
    enddo

  return
  end subroutine convert_from_ug

! ------------------------------------------------------------------------------

end module wrf_fields
