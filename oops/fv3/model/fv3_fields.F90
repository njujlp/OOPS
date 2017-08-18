!> Handle fields for the QG model

module fv3_fields

use config_mod
use netcdf
use fv3_geom_mod
use fv3_vars_mod
use fv3_misc_mod
use kinds

implicit none
private

public :: fv3_field, &
        & create, delete, zeros, random, copy, &
        & self_add, self_schur, self_sub, self_mul, axpy, &
        & dot_prod, add_incr, diff_incr, &
        & read_file, write_file, gpnorm, fldrms, &
        & change_resol, convert_to_ug, convert_from_ug
public :: fv3_field_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold QG fields
type :: fv3_field
  integer :: nxg                     !< Zonal grid dimension
  integer :: nyg                     !< Meridional grid dimension
  integer :: nl                     !< Number of levels
  integer :: ntiles
  integer :: layout_x
  integer :: layout_y
  INTEGER :: nf2d,nf3d
  INTEGER :: nftot                     !< Number of fields
  CHARACTER(len=NF90_MAX_NAME), ALLOCATABLE, DIMENSION(:) :: fldnames3d !< Variable identifiers
  CHARACTER(len=NF90_MAX_NAME), ALLOCATABLE, DIMENSION(:) :: fldnames2d !< Variable identifiers
  CHARACTER(len=max_string_length) :: bckgfile
  integer :: pe
  integer :: itile
  integer :: nx
  integer :: ny
  INTEGER :: ibeg
  INTEGER :: jbeg
  TYPE(fv3_geom), POINTER :: geom

  REAL(kind=kind_real), POINTER :: gfld(:,:,:)    !< 3D+2D fields

end type fv3_field

#define LISTED_TYPE fv3_field

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: fv3_field_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------

SUBROUTINE create(self, geom, vars)
implicit none
type(fv3_field), intent(inout) :: self
TYPE(fv3_geom), POINTER,  INTENT(in)    :: geom
type(fv3_vars),  intent(in)    :: vars

self%nxg = geom%nxg
self%nyg = geom%nyg
self%nl = geom%nz
self%ntiles = geom%ntiles
self%layout_x = geom%layout_x
self%layout_y = geom%layout_y
self%nf3d = vars%nv3d
self%nf2d = vars%nv2d
self%nftot = self%nf3d*self%nl + self%nf2d

self%pe = geom%pe
self%itile = geom%itile
self%nx = geom%nx
self%ny = geom%ny
self%ibeg = geom%ibeg
self%jbeg = geom%jbeg

ALLOCATE(self%gfld(self%nx,self%ny,self%nftot))
self%gfld(:,:,:)=0.0_kind_real

allocate(self%fldnames3d(self%nf3d))
self%fldnames3d(:)=vars%fldnames3d(:)

IF (self%nf2d > 0) THEN
   ALLOCATE(self%fldnames2d(self%nf2d))
   self%fldnames2d(:)=vars%fldnames2d(:)
ENDIF

self%geom => geom

call check(self)

END SUBROUTINE create

! ------------------------------------------------------------------------------

SUBROUTINE delete(self)
implicit none
type(fv3_field), intent(inout) :: self

CALL check(self)

IF (ASSOCIATED(self%gfld)) DEALLOCATE(self%gfld)
IF (ALLOCATED(self%fldnames3d)) DEALLOCATE(self%fldnames3d)
IF (ALLOCATED(self%fldnames2d)) DEALLOCATE(self%fldnames2d)

END SUBROUTINE delete

! ------------------------------------------------------------------------------

subroutine zeros(self)
implicit none
type(fv3_field), intent(inout) :: self

call check(self)

self%gfld(:,:,:) = 0.0_kind_real

end subroutine zeros

! ------------------------------------------------------------------------------

subroutine random(self)
use random_vectors_mod
implicit none
type(fv3_field), intent(inout) :: self

call check(self)

call random_vector(self%gfld(:,:,:))

end subroutine random

! ------------------------------------------------------------------------------

subroutine copy(self,rhs)
implicit none
type(fv3_field), intent(inout) :: self
type(fv3_field), intent(in)    :: rhs
integer :: nf

call check_resolution(self, rhs)

self%gfld(:,:,:) = rhs%gfld(:,:,:)

return
end subroutine copy

! ------------------------------------------------------------------------------

subroutine self_add(self,rhs)
implicit none
type(fv3_field), intent(inout) :: self
type(fv3_field), intent(in)    :: rhs
integer :: nf

call check_resolution(self, rhs)

self%gfld(:,:,:) = self%gfld(:,:,:) + rhs%gfld(:,:,:)

return
end subroutine self_add

! ------------------------------------------------------------------------------

subroutine self_schur(self,rhs)
implicit none
type(fv3_field), intent(inout) :: self
type(fv3_field), intent(in)    :: rhs
integer :: nf

call check_resolution(self, rhs)

self%gfld(:,:,:) = self%gfld(:,:,:) * rhs%gfld(:,:,:)

return
end subroutine self_schur

! ------------------------------------------------------------------------------

subroutine self_sub(self,rhs)
implicit none
type(fv3_field), intent(inout) :: self
type(fv3_field), intent(in)    :: rhs
integer :: nf

call check_resolution(self, rhs)

self%gfld(:,:,:) = self%gfld(:,:,:) - rhs%gfld(:,:,:)

return
end subroutine self_sub

! ------------------------------------------------------------------------------

subroutine self_mul(self,zz)
implicit none
type(fv3_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz

call check(self)

self%gfld(:,:,:) = zz * self%gfld(:,:,:)

return
end subroutine self_mul

! ------------------------------------------------------------------------------

subroutine axpy(self,zz,rhs)
implicit none
type(fv3_field), intent(inout) :: self
real(kind=kind_real), intent(in) :: zz
type(fv3_field), intent(in)    :: rhs

call check_resolution(self, rhs)

self%gfld(:,:,:) = self%gfld(:,:,:) + zz * rhs%gfld(:,:,:)

return
end subroutine axpy

! ------------------------------------------------------------------------------

subroutine dot_prod(fld1,fld2,zprod)
implicit none
type(fv3_field), intent(in) :: fld1, fld2
real(kind=kind_real), intent(inout) :: zprod

integer :: jx,jy,jz,jj
real(kind=kind_real), allocatable :: zz(:)

call check_resolution(fld1, fld2)
if (fld1%nftot /= fld2%nftot .or. fld1%nl /= fld2%nl) then
  call abor1_ftn("fv3_fields.dot_prod:stop.1")
endif

ALLOCATE(zz(fld1%nftot))
zz(:)=0.0_kind_real
do jy=1,fld1%ny
do jx=1,fld1%nx
DO jz=1,fld1%nftot
  zz(jz) = zz(jz) + fld1%gfld(jx,jy,jz) * fld2%gfld(jx,jy,jz)
enddo
enddo
enddo

zprod=0.0_kind_real
DO jj=1,fld1%nftot
  zprod=zprod+zz(jj)
enddo
deallocate(zz)

return
end subroutine dot_prod

! ------------------------------------------------------------------------------

subroutine add_incr(self,rhs)
implicit none
type(fv3_field), intent(inout) :: self
type(fv3_field), intent(in)    :: rhs

call check(self)
call check(rhs)

CALL self_add(self,rhs)

return
end subroutine add_incr

! ------------------------------------------------------------------------------

subroutine diff_incr(lhs,x1,x2)
implicit none
type(fv3_field), intent(inout) :: lhs
type(fv3_field), intent(in)    :: x1
type(fv3_field), intent(in)    :: x2

call check(lhs)
call check(x1)
call check(x2)

call zeros(lhs)
IF (x1%nx==x2%nx .AND. x1%ny==x2%ny) THEN
   IF (lhs%nx==x1%nx .AND. lhs%ny==x1%ny) THEN
      lhs%gfld(:,:,:) = x1%gfld(:,:,:) - x2%gfld(:,:,:)
   ELSE
      CALL abor1_ftn("fv3_fields.diff_incr:stop.1")
   ENDIF
ELSE
   CALL abor1_ftn("fv3_fields.diff_incr:stop.2")
ENDIF

return
end subroutine diff_incr

! ------------------------------------------------------------------------------

subroutine change_resol(fld,rhs)
implicit none
type(fv3_field), intent(inout) :: fld
type(fv3_field), intent(in)    :: rhs

call check(fld)
call check(rhs)

IF (fld%nx==rhs%nx .AND. fld%ny==rhs%ny) THEN
  call copy(fld, rhs)
else
  call abor1_ftn("fv3_fields.change_resol:stop.1")
ENDIF

return
end subroutine change_resol

! ------------------------------------------------------------------------------

SUBROUTINE read_file(fld, c_conf, vdate)

  USE iso_c_binding
  USE datetime_mod
  USE fckit_log_module, ONLY : log

  IMPLICIT NONE
  TYPE(fv3_field), INTENT(inout) :: fld      !< Fields
  TYPE(c_ptr), INTENT(in)       :: c_conf   !< Configuration
  TYPE(datetime), INTENT(inout) :: vdate    !< DateTime

  INTEGER :: ihisttime,itindex,ifield,ix,iy,iz,it,itile,&
       &ioff,ioffg,joffg
  CHARACTER(len=max_string_length) :: fname
  INTEGER :: ncfileid,ncvarid,ncstatus,nprocs
  INTEGER, DIMENSION(nc_maxdims) :: dimids
  CHARACTER(len=1) :: ctile
  CHARACTER(len=NF90_MAX_NAME) :: varname
  REAL(kind=kind_real), ALLOCATABLE, DIMENSION(:,:,:) :: gfldg

! mpi definitions.
  INCLUDE 'mpif.h'
  INTEGER :: MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr
  INTEGER :: color,key,newcomm,newnumprocs,newnproc,numprocs_per_tile,&
       &numprocself,numprocself_per_tile,pe_root
  LOGICAL :: mpi_started

  CHARACTER(len=20) :: sdate
  sdate = config_get_string(c_conf,LEN(sdate),"date")
  CALL datetime_set(sdate, vdate)

  fld%bckgfile = config_get_string(c_conf,max_string_length,"bckgfile")

  ihisttime = config_get_int(c_conf, "ihisttime")
  itindex=INDEX(fld%bckgfile,'tile')

  CALL MPI_Initialized(mpi_started,iret)

  IF (.NOT. mpi_started) CALL MPI_Init(iret)
  CALL determine_mpi_type_real

  CALL MPI_Comm_size(MPI_COMM_WORLD,numprocs,iret)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
  
! nproc is process number, numprocs is total number of processes.
  
  numprocs_per_tile=numprocs/fld%ntiles
  numprocself_per_tile=fld%layout_x*fld%layout_y
  numprocself=fld%ntiles*numprocs_per_tile
  
  IF (numprocself /= numprocs) CALL MPI_Abort(MPI_COMM_WORLD,100,iret)
  IF (MOD(numprocs,fld%ntiles) /= 0) &
       &CALL MPI_Abort(MPI_COMM_WORLD,101,iret)
  
  IF (numprocs_per_tile /= numprocself_per_tile) &
       &CALL MPI_Abort(MPI_COMM_WORLD,102,iret)
  
  IF (MOD(fld%nxg,fld%layout_x) /= 0) &
       &CALL MPI_Abort(MPI_COMM_WORLD,103,iret)
  
  IF (MOD(fld%nyg,fld%layout_y) /= 0) &
     &CALL MPI_Abort(MPI_COMM_WORLD,104,iret)
  
  ALLOCATE(gfldg(fld%nxg,fld%nyg,fld%nl))

!  fld%gfld is already allocated

  CALL zeros(fld)

  color=nproc/numprocs_per_tile
  key=MOD(nproc,numprocs_per_tile)
  
  CALL MPI_Comm_split(MPI_COMM_WORLD,color,key,newcomm,iret)

  ioffg=fld%ibeg
  joffg=fld%jbeg
  
  DO ifield=1,fld%nf3d
     
     IF (key == 0) THEN
        
        WRITE(ctile,'(i1)')(color+1)
        fname=fld%bckgfile(1:itindex+3)//ctile//fld%bckgfile(itindex+5:)
        
        IF (nf90_noerr /= nf90_open(path=TRIM(ADJUSTL(fname)),&
             &mode=nf90_nowrite,ncid=ncfileid)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.1")
        
        varname=fld%fldnames3d(ifield)

        IF (nf90_noerr /= nf90_inq_varid(ncfileid,TRIM(varname),&
             &varid=ncvarid)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.2"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,dimids=dimids)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.3"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(1),&
             &len = ix)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.4"//&
             &TRIM(varname))

        IF (ix /= fld%nxg ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.4.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(2),&
             &len = iy)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.5"//&
             &TRIM(varname))

        IF (iy /= fld%nyg ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.5.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(3),&
             &len = iz)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.6"//&
             &TRIM(varname))

        IF (iz /= fld%nl ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.6.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(4),&
             &len = it)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.7"//&
             &TRIM(varname))

        IF (it < ihisttime ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.7.1"//&
             &TRIM(varname))

!        ncstatus=nf90_get_var(ncfileid, ncvarid, gfldg,&
!             &start=(/1,1,1,ihisttime/),&
!             &count=(/fld%nxg,fld%nyg,fld%nl,1/))

!        PRINT *,'@@@1',ncstatus,nf90_strerror(ncstatus)


        IF (nf90_noerr /= nf90_get_var(ncfileid, ncvarid, gfldg,&
             &start=(/1,1,1,ihisttime/),&
             &count=(/fld%nxg,fld%nyg,fld%nl,1/)))&
             &CALL abor1_ftn("fv3_fields.read_file:stop.8")
     ENDIF

     CALL MPI_Bcast(gfldg,fld%nxg*fld%nyg*fld%nl,mpi_type_real,&
          &0,newcomm,iret)

     ioff=(ifield-1)*fld%nl

!     PRINT *,'@@@0',SIZE(fld%gfld,3)
!     PRINT *,'@@@1',nproc,ioff+1,ioff+fld%nl,joffg,joffg+fld%ny-1
!
!     PRINT *,SIZE(fld%gfld,1),SIZE(fld%gfld,2),&
!          &SIZE(gfldg(ioffg:ioffg+fld%nx-1,:,:),1),&
!          &SIZE(gfldg(:,joffg:joffg+fld%ny-1,:),1),&
!          &SIZE(gfldg,1)


     fld%gfld(:,:,ioff+1:ioff+fld%nl)=gfldg(&
          &ioffg:ioffg+fld%nx-1,&
          &joffg:joffg+fld%ny-1,&
          &:)

  ENDDO

  DO ifield=1,fld%nf2d

     IF (key == 0) THEN

        varname=fld%fldnames2d(ifield)

        IF (nf90_noerr /= nf90_inq_varid(ncfileid,TRIM(varname),&
             &varid=ncvarid)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.2"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,dimids=dimids)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.3"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(1),&
             &len = ix)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.4"//&
             &TRIM(varname))

        IF (ix /= fld%nxg ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.4.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(2),&
             &len = iy)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.5"//&
             &TRIM(varname))

        IF (iy /= fld%nyg ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.5.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(3),&
             &len = it)) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.6"//&
             &TRIM(varname))

        IF (it < ihisttime ) &
             &CALL abor1_ftn("fv3_fields.read_file:stop.6.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_get_var(ncfileid, ncvarid, &
             &gfldg(:,:,1),&
             &start=(/1,1,ihisttime/),&
             &count=(/fld%nxg,fld%nyg,1/)))&
             &CALL abor1_ftn("fv3_fields.read_file:stop.7")

     ENDIF

     CALL MPI_Bcast(gfldg,fld%nxg*fld%nyg*fld%nl,mpi_type_real,&
          &0,newcomm,iret)

     fld%gfld(:,:,ioff+1:ioff+1)=gfldg(&
          &ioffg:ioffg+fld%nx-1,&
          &joffg:joffg+fld%ny-1,&
          &1:1)
     
     ioff=ioff+1

  ENDDO

  DEALLOCATE(gfldg)

  IF (key == 0) THEN  
     IF (nf90_noerr /= nf90_close(ncfileid))&
          &CALL abor1_ftn("fv3_fields.read_file:stop.9")
  ENDIF

  CALL check(fld)
  
  RETURN

END SUBROUTINE read_file


! ------------------------------------------------------------------------------

SUBROUTINE write_file(fld, c_conf, vdate)
  USE iso_c_binding
  USE datetime_mod
  USE fckit_log_module, ONLY : log

  IMPLICIT NONE
  TYPE(fv3_field), INTENT(in) :: fld    !< Fields
  TYPE(c_ptr), INTENT(in)    :: c_conf !< Configuration
  TYPE(datetime), INTENT(in) :: vdate  !< DateTime

  INTEGER :: ihisttime,itindex,ifield,ix,iy,iz,it,itile,&
       &ioff,ioffg,joffg
  CHARACTER(len=max_string_length) :: fname,analfile
  INTEGER :: ncfileid,ncvarid,ncstatus,nprocs
  INTEGER, DIMENSION(nc_maxdims) :: dimids
  CHARACTER(len=1) :: ctile
  CHARACTER(len=NF90_MAX_NAME) :: varname
  REAL(kind=kind_real), ALLOCATABLE, DIMENSION(:,:,:) :: gfldg

! mpi definitions.
  INCLUDE 'mpif.h'
  INTEGER :: MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr
  INTEGER :: color,key,newcomm,newnumprocs,newnproc,numprocs_per_tile,&
       &numprocself,numprocself_per_tile,pe_root
  LOGICAL :: mpi_started

  CHARACTER(len=20) :: sdate
  CALL datetime_to_string(vdate, sdate)

  analfile = config_get_string(c_conf,max_string_length,"analfile")
  itindex=INDEX(analfile,'tile')

  CALL check(fld)

  CALL MPI_Initialized(mpi_started,iret)

  IF (.NOT. mpi_started) CALL MPI_Init(iret)
  CALL determine_mpi_type_real

  CALL MPI_Comm_size(MPI_COMM_WORLD,numprocs,iret)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)

! nproc is process number, numprocs is total number of processes.

  numprocs_per_tile=numprocs/fld%ntiles
  numprocself_per_tile=fld%layout_x*fld%layout_y
  numprocself=fld%ntiles*numprocs_per_tile

  ALLOCATE(gfldg(fld%nxg,fld%nyg,fld%nl))

  gfldg=0.

  color=nproc/numprocs_per_tile
  key=MOD(nproc,numprocs_per_tile)

  CALL MPI_Comm_split(MPI_COMM_WORLD,color,key,newcomm,iret)

  ioffg=fld%ibeg
  joffg=fld%jbeg

  DO ifield=1,fld%nf3d
     
     IF (key == 0) THEN
        
        WRITE(ctile,'(i1)')(color+1)
        fname=analfile(1:itindex+3)//ctile//analfile(itindex+5:)

        IF (nf90_noerr /= nf90_open(path=TRIM(ADJUSTL(fname)),&
             &mode=nf90_write,ncid=ncfileid)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.1")

        varname=fld%fldnames3d(ifield)

        IF (nf90_noerr /= nf90_inq_varid(ncfileid,TRIM(varname),&
             &varid=ncvarid)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.2"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,dimids=dimids)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.3"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(1),&
             &len = ix)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.4"//&
             &TRIM(varname))

        IF (ix /= fld%nxg ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.4.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(2),&
             &len = iy)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.5"//&
             &TRIM(varname))

        IF (iy /= fld%nyg ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.5.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(3),&
             &len = iz)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.6"//&
             &TRIM(varname))

        IF (iz /= fld%nl ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.6.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(4),&
             &len = it)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.7"//&
             &TRIM(varname))

        IF (it /= 1 ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.7.1"//&
             &TRIM(varname))

     ENDIF

     ioff=(ifield-1)*fld%nl

     CALL MPI_Gather(fld%gfld(:,:,ioff+1:ioff+fld%nl),&
          &fld%nx*fld%ny*fld%nl,mpi_type_real,&
          &gfldg(ioffg:ioffg+fld%nx-1,joffg:joffg+fld%ny-1,:),&
          &fld%nx*fld%ny*fld%nl,mpi_type_real,&
          &0,newcomm,iret)
     
     IF (key == 0) THEN
        IF (nf90_noerr /= nf90_put_var(ncfileid, ncvarid, gfldg))&
             &CALL abor1_ftn("fv3_fields.write_file:stop.8")
     ENDIF
     
  ENDDO

  DO ifield=1,fld%nf2d
     
     IF (key == 0) THEN
        
        varname=fld%fldnames2d(ifield)

        IF (nf90_noerr /= nf90_inq_varid(ncfileid,TRIM(varname),&
             &varid=ncvarid)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.2"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,dimids=dimids)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.3"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(1),&
             &len = ix)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.4"//&
             &TRIM(varname))

        IF (ix /= fld%nxg ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.4.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(2),&
             &len = iy)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.5"//&
             &TRIM(varname))

        IF (iy /= fld%nyg ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.5.1"//&
             &TRIM(varname))

        IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(3),&
             &len = it)) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.6"//&
             &TRIM(varname))

        IF (it /= 1 ) &
             &CALL abor1_ftn("fv3_fields.write_file:stop.6.1"//&
             &TRIM(varname))

     ENDIF

     ioff=ioff+1

     CALL MPI_Gather(fld%gfld(:,:,ioff+1:ioff+1),&
          &fld%nx*fld%ny,mpi_type_real,&
          &gfldg(ioffg:ioffg+fld%nx-1,joffg:joffg+fld%ny-1,1),&
          &fld%nx*fld%ny,mpi_type_real,&
          &0,newcomm,iret)

     IF (key == 0) THEN
        IF (nf90_noerr /= nf90_put_var(ncfileid, ncvarid, gfldg(:,:,1)))&
             &CALL abor1_ftn("fv3_fields.write_file:stop.8")
     ENDIF

  ENDDO

  IF (key == 0) THEN
     IF (nf90_noerr /= nf90_close(ncfileid))&
          &CALL abor1_ftn("fv3_fields.write_file:stop.9")
  ENDIF

  DEALLOCATE(gfldg)

!  filename = genfilename(c_conf,max_string_length,vdate)
!  WRITE(buf,*) 'fv3_field:write_file: writing '//filename
!  CALL log%info(buf)
!  OPEN(unit=iunit, file=TRIM(fname), form='formatted', action='write')
!
!  is=0
!
!  WRITE(iunit,*) fld%nx, fld%ny, fld%nl, fld%nftot, is
!
!  CALL datetime_to_string(vdate, sdate)
!  WRITE(iunit,*) sdate
!
!  IF (fld%nx>9999)  CALL abor1_ftn("Format too small")
!  WRITE(cnx,'(I4)')fld%nx
!  fmtn='('//TRIM(cnx)//fmt1//')'
!
!  DO jf=1,fld%nftot
!     DO jy=1,fld%ny
!        WRITE(iunit,fmtn) (fld%gfld(jx,jy,jf), jx=1,fld%nx)
!     ENDDO
!  ENDDO
!
!  CLOSE(iunit)
!
  RETURN
END SUBROUTINE write_file

! ------------------------------------------------------------------------------

SUBROUTINE gpnorm(fld, nf3d, nf2d, pstat)
implicit none
type(fv3_field), intent(in) :: fld
INTEGER, INTENT(in) :: nf3d, nf2d
REAL(kind=kind_real), INTENT(inout) :: pstat(3, nf3d+nf2d)
integer :: jj,joff

call check(fld)

do jj=1,fld%nf3d
  joff=(jj-1)*fld%nl
  pstat(1,jj)=minval(fld%gfld(:,:,joff+1:joff+fld%nl))
  pstat(2,jj)=maxval(fld%gfld(:,:,joff+1:joff+fld%nl))
  pstat(3,jj)=sqrt(sum(fld%gfld(:,:,joff+1:joff+fld%nl)**2) &
               & /real(fld%nl*fld%nx*fld%ny,kind_real))
enddo

do jj=1,fld%nf2d
  joff=(jj-1)
  pstat(1,jj)=minval(fld%gfld(:,:,joff+1:joff+1))
  pstat(2,jj)=maxval(fld%gfld(:,:,joff+1:joff+1))
  pstat(3,jj)=sqrt(sum(fld%gfld(:,:,joff+1:joff+1)**2) &
               & /real(fld%nl*fld%nx*fld%ny,kind_real))
enddo

return
end subroutine gpnorm

! ------------------------------------------------------------------------------

subroutine fldrms(fld, prms)
implicit none
type(fv3_field), intent(in) :: fld
real(kind=kind_real), intent(out) :: prms
integer :: jf,jy,jx,ii
real(kind=kind_real) :: zz

call check(fld)

CALL dot_prod(fld,fld,prms)

!ii = fld%nftot*fld%ny*fld%nx

!prms = sqrt(zz/real(ii,kind_real))

end subroutine fldrms

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

function genfilename (c_conf,length,vdate)
use iso_c_binding
use datetime_mod
use duration_mod
type(c_ptr), intent(in)    :: c_conf  !< Configuration
integer, intent(in) :: length
character(len=length) :: genfilename
type(datetime), intent(in) :: vdate

character(len=length) :: fdbdir, expver, typ, validitydate, referencedate, sstep, &
                       & prefix, mmb
type(datetime) :: rdate
type(duration) :: step
integer lenfn

! here we should query the length and then allocate "string".
! But Fortran 90 does not allow variable-length allocatable strings.
! config_get_string checks the string length and aborts if too short.
fdbdir = config_get_string(c_conf,len(fdbdir),"datadir")
expver = config_get_string(c_conf,len(expver),"exp")
typ    = config_get_string(c_conf,len(typ)   ,"type")

if (typ=="ens") then
  mmb = config_get_string(c_conf, len(mmb), "member")
  lenfn = LEN_TRIM(fdbdir) + 1 + LEN_TRIM(expver) + 1 + LEN_TRIM(typ) + 1 + LEN_TRIM(mmb)
  prefix = TRIM(fdbdir) // "/" // TRIM(expver) // "." // TRIM(typ) // "." // TRIM(mmb)
else
  lenfn = LEN_TRIM(fdbdir) + 1 + LEN_TRIM(expver) + 1 + LEN_TRIM(typ)
  prefix = TRIM(fdbdir) // "/" // TRIM(expver) // "." // TRIM(typ)
endif

if (typ=="fc" .or. typ=="ens") then
  referencedate = config_get_string(c_conf,len(referencedate),"date")
  call datetime_to_string(vdate, validitydate)
  call datetime_create(TRIM(referencedate), rdate)
  call datetime_diff(vdate, rdate, step)
  call duration_to_string(step, sstep)
  lenfn = lenfn + 1 + LEN_TRIM(referencedate) + 1 + LEN_TRIM(sstep)
  genfilename = TRIM(prefix) // "." // TRIM(referencedate) // "." // TRIM(sstep)
endif

if (typ=="an") then
  call datetime_to_string(vdate, validitydate)
  lenfn = lenfn + 1 + LEN_TRIM(validitydate)
  genfilename = TRIM(prefix) // "." // TRIM(validitydate)
endif

if (lenfn>length) &
  & call abor1_ftn("fv3_fields:genfilename: filename too long")

end function genfilename

! ------------------------------------------------------------------------------

subroutine check_resolution(x1, x2)

implicit none
type(fv3_field), intent(in) :: x1, x2

if (x1%nx /= x2%nx .or.  x1%ny /= x2%ny .or.  x1%nl /= x2%nl) then
  call abor1_ftn ("fv3_fields: resolution error")
endif
call check(x1)
call check(x2)

end subroutine check_resolution

! ------------------------------------------------------------------------------

SUBROUTINE check(self)
implicit none
type(fv3_field), intent(in) :: self
logical :: bad

bad = .not.associated(self%gfld)

IF (bad) CALL abor1_ftn("fv3_fields.bad.1")

bad = bad .or. (size(self%gfld, 1) /= self%nx)
IF (bad) CALL abor1_ftn("fv3_fields.bad.2")
bad = bad .or. (size(self%gfld, 2) /= self%ny)
IF (bad) CALL abor1_ftn("fv3_fields.bad.3")
bad = bad .OR. (SIZE(self%gfld, 3) /= self%nftot)
IF (bad) CALL abor1_ftn("fv3_fields.bad.4")

END SUBROUTINE check

! ------------------------------------------------------------------------------

subroutine convert_to_ug(self, ug)
use unstructured_grid_mod
implicit none
type(fv3_field), intent(in) :: self
type(unstructured_grid), intent(inout) :: ug
real(kind=kind_real) :: zz(self%nl)
integer :: jx,jy,jl,jf,joff,j
integer :: cmask(self%nl)

do jl=1,self%nl
  zz(jl) = real(jl,kind=kind_real)
enddo
call create_unstructured_grid(ug, self%nl, zz)

cmask = 1
do jy=1,self%geom%ny
  do jx=1,self%geom%nx
    CALL add_column(ug, self%geom%lat(jx,jy), self%geom%lon(jx,jy), &
         &self%geom%area(jx,jy), self%nl, self%nf3d, 0, cmask, 1)
    j = 0
    do jf=1,self%nf3d
      joff = (jf-1)*self%nl
      do jl=1,self%nl
         j = j+1
         ug%last%column%cols(j) = self%gfld(jx,jy,joff+jl)
      enddo
    enddo
  enddo
enddo

end subroutine convert_to_ug

! ------------------------------------------------------------------------------

subroutine convert_from_ug(self, ug)
use unstructured_grid_mod
implicit none
type(fv3_field), intent(inout) :: self
type(unstructured_grid), intent(in) :: ug
type(column_element), pointer :: current
integer :: jx,jy,jl,jf,joff,j

current => ug%head
do jy=1,self%geom%ny
  do jx=1,self%geom%nx
    j = 0
    do jf=1,self%nf3d
      joff = (jf-1)*self%nl
      do jl=1,self%nl
         j = j+1
         self%gfld(jx,jy,joff+jl) = current%column%cols(j)
      enddo
    enddo
    current => current%next
  enddo
enddo

end subroutine convert_from_ug

! ------------------------------------------------------------------------


! ------------------------------------------------------------------------------

END MODULE fv3_fields
