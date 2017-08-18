
!> Fortran module handling geometry for the QG model

module fv3_geom_mod

use iso_c_binding
use config_mod
use netcdf
use kinds
USE fv3_misc_mod

implicit none
private
public :: fv3_geom
public :: fv3_geom_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold geometry data for the QG model
type :: fv3_geom
  integer :: nxg
  integer :: nyg
  integer :: nz
  integer :: ntiles
  integer :: layout_x
  integer :: layout_y
  CHARACTER(len=max_string_length) :: gridfile
  integer :: pe
  integer :: itile
  integer :: nx
  integer :: ny
  INTEGER :: ibeg
  INTEGER :: jbeg 
  REAL(kind=kind_real), ALLOCATABLE, DIMENSION(:,:) :: lon
  REAL(kind=kind_real), ALLOCATABLE, DIMENSION(:,:) :: lat

end type fv3_geom

#define LISTED_TYPE fv3_geom

!> Linked list interface - defines registry_t type
#include "util/linkedList_i.f"

!> Global registry
type(registry_t) :: fv3_geom_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "util/linkedList_c.f"

! ------------------------------------------------------------------------------

SUBROUTINE c_fv3_geo_setup(c_key_self, c_conf) BIND(c,name='fv3_geo_setup_f90')
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in)    :: c_conf

type(fv3_geom), pointer :: self

INTEGER :: itile,iproc,itindex,ix,iy,ndivs,i,j,k
CHARACTER(len=max_string_length) :: fname,namelistfile
REAL(kind=kind_real), ALLOCATABLE, DIMENSION(:,:) :: long, latg
INTEGER, ALLOCATABLE, DIMENSION(:) :: ibegg,iendg,jbegg,jendg
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: begindex

INTEGER :: ncfileid,ncvarid,ncstatus,nprocs
INTEGER, DIMENSION(nc_maxdims) :: dimids
CHARACTER(len=1) :: ctile
INTEGER :: nxblocks,nyblocks

! mpi definitions.
INCLUDE 'mpif.h'
INTEGER :: MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr
INTEGER :: color,key,newcomm,newnumprocs,newnproc,numprocs_per_tile,&
     &numprocself,numprocself_per_tile
LOGICAL :: mpi_started

call fv3_geom_registry%init()
call fv3_geom_registry%add(c_key_self)
call fv3_geom_registry%get(c_key_self,self)

namelistfile = config_get_string(c_conf,max_string_length,&
     &"namelistfile")

CALL namelist_fv3(namelistfile,self%nxg,self%nyg,self%nz,&
     &self%ntiles,self%layout_x,self%layout_y,nxblocks,nyblocks)

self%gridfile = config_get_string(c_conf,max_string_length,&
     &"gridfile")

CALL MPI_Initialized(mpi_started,iret)

IF (.NOT. mpi_started) CALL MPI_Init(iret)
CALL determine_mpi_type_real

CALL MPI_Comm_size(MPI_COMM_WORLD,numprocs,iret)
CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)

! nproc is process number, numprocs is total number of processes.

numprocs_per_tile=numprocs/self%ntiles
numprocself_per_tile=self%layout_x*self%layout_y
numprocself=self%ntiles*numprocs_per_tile

IF (numprocself /= numprocs) CALL MPI_Abort(MPI_COMM_WORLD,100,iret)
IF (MOD(numprocs,self%ntiles) /= 0) &
     &CALL MPI_Abort(MPI_COMM_WORLD,101,iret)

IF (numprocs_per_tile /= numprocself_per_tile) &
     &CALL MPI_Abort(MPI_COMM_WORLD,102,iret)

IF (MOD(self%nxg,self%layout_x) /= 0) &
     &CALL MPI_Abort(MPI_COMM_WORLD,103,iret)

IF (MOD(self%nyg,self%layout_y) /= 0) &
     &CALL MPI_Abort(MPI_COMM_WORLD,104,iret)

ndivs=self%layout_x
ALLOCATE(ibegg(ndivs),iendg(ndivs))
CALL mpp_compute_extent(1,self%nxg,ndivs,ibegg,iendg)

ndivs=self%layout_y
ALLOCATE(jbegg(ndivs),jendg(ndivs))
CALL mpp_compute_extent(1,self%nyg,ndivs,jbegg,jendg)

ALLOCATE(begindex(numprocs_per_tile,2))

!lazy 
k=0
DO j=1,self%layout_y
   DO i=1,self%layout_x
      k=k+1
      begindex(k,1)=ibegg(i)
      begindex(k,2)=jbegg(j)
   ENDDO
ENDDO

ALLOCATE(long(self%nxg,self%nyg),latg(self%nxg,self%nyg))

self%nx=iendg(1)-ibegg(1)+1
self%ny=jendg(1)-jbegg(1)+1

ALLOCATE(self%lon(self%nx,self%ny),self%lat(self%nx,self%ny))

color=nproc/numprocs_per_tile
key=MOD(nproc,numprocs_per_tile)

IF (key == 0) THEN

   itindex=INDEX(self%gridfile,'tile')
   WRITE(ctile,'(i1)')(color+1)
   fname=self%gridfile(1:itindex+3)//ctile//self%gridfile(itindex+5:)

   IF (nf90_noerr /= nf90_open(path=TRIM(ADJUSTL(fname)),&
        &mode=nf90_nowrite,ncid=ncfileid)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.1")
   
   IF (nf90_noerr /= nf90_inq_varid(ncfileid,TRIM(lonname),&
        &varid=ncvarid)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.2")

   IF (nf90_noerr /= nf90_inquire_variable(ncid=ncfileid,&
        &varid=ncvarid,dimids=dimids)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.3")

   IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(1),&
        &len = ix)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.4")

   IF (nf90_noerr /= nf90_inquire_dimension(ncfileid,dimids(2),&
        &len = iy)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.5")

   IF (ix /= self%nxg .OR. iy /= self%nyg) &
        &CALL abor1_ftn("fv3_geom_mod:stop.6")

   IF (nf90_noerr /= nf90_get_var(ncfileid, ncvarid, long)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.7")

   IF (nf90_noerr /= nf90_inq_varid(ncfileid,TRIM(latname),&
        &varid=ncvarid)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.8")

   IF (nf90_noerr /= nf90_get_var(ncfileid, ncvarid, latg)) &
        &CALL abor1_ftn("fv3_geom_mod:stop.9")

ENDIF

CALL MPI_Comm_split(MPI_COMM_WORLD,color,key,newcomm,iret)

CALL MPI_Bcast(long,self%nxg*self%nyg,mpi_type_real,0,newcomm,iret)
CALL MPI_Bcast(latg,self%nxg*self%nyg,mpi_type_real,0,newcomm,iret)

!PRINT *,nproc,color,MINVAL(long),MAXVAL(long),MINVAL(latg),MAXVAL(latg)

self%ibeg=begindex(key+1,1)
self%jbeg=begindex(key+1,2)

self%lon=long(self%ibeg:self%ibeg+self%nx-1,self%jbeg:self%jbeg+self%ny-1)
self%lat=latg(self%ibeg:self%ibeg+self%nx-1,self%jbeg:self%jbeg+self%ny-1)
self%itile=color+1
self%pe=nproc

DEALLOCATE(begindex)
DEALLOCATE(long,latg)   

CALL MPI_Barrier(MPI_COMM_WORLD,iret)
!CALL MPI_Finalize(iret)

end subroutine c_fv3_geo_setup

! ------------------------------------------------------------------------------

subroutine c_fv3_geo_clone(c_key_self, c_key_other) bind(c,name='fv3_geo_clone_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(fv3_geom), pointer :: self, other

call fv3_geom_registry%add(c_key_other)
call fv3_geom_registry%get(c_key_other, other)
call fv3_geom_registry%get(c_key_self , self )
other%nxg = self%nxg
other%nyg = self%nyg
other%nz = self%nz
other%ntiles = self%ntiles
other%layout_x = self%layout_x
other%layout_y = self%layout_y
other%gridfile = self%gridfile
other%itile = self%itile
other%pe = self%pe
other%nx = self%nx
other%ny = self%ny
other%ibeg = self%ibeg
other%jbeg = self%jbeg
other%lon = self%lon
other%lat = self%lat

end subroutine c_fv3_geo_clone

! ------------------------------------------------------------------------------

subroutine c_fv3_geo_delete(c_key_self) bind(c,name='fv3_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self     
TYPE(fv3_geom), pointer :: self

CALL fv3_geom_registry%get(c_key_self , self )
if (allocated(self%lon)) deallocate(self%lon)
if (allocated(self%lat)) deallocate(self%lat)
call fv3_geom_registry%remove(c_key_self)

end subroutine c_fv3_geo_delete

! ------------------------------------------------------------------------------

SUBROUTINE c_fv3_geo_info(c_key_self, c_pe, c_nx, c_ny, c_ibeg, c_jbeg) BIND(c,name='fv3_geo_info_f90')
implicit none
integer(c_int), intent(in   ) :: c_key_self
integer(c_int), intent(inout) :: c_pe
integer(c_int), intent(inout) :: c_nx
integer(c_int), intent(inout) :: c_ny
integer(c_int), intent(inout) :: c_ibeg
integer(c_int), intent(inout) :: c_jbeg

type(fv3_geom), pointer :: self

call fv3_geom_registry%get(c_key_self , self )
c_pe = self%pe
c_nx = self%nx
c_ny = self%ny
c_ibeg = self%ibeg
c_jbeg = self%jbeg


end subroutine c_fv3_geo_info

! ------------------------------------------------------------------------------

end module fv3_geom_mod
