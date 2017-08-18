
!> Interfaces to be called from C++ for Fortran handling of QG model fields

! ------------------------------------------------------------------------------

SUBROUTINE fv3_field_create_c(c_key_self, c_key_geom, c_key_vars) BIND(c,name='fv3_field_create_f90')
use iso_c_binding
use fv3_fields
use fv3_geom_mod
use fv3_vars_mod
implicit none
integer(c_int), intent(inout) :: c_key_self
integer(c_int), intent(in) :: c_key_geom !< Geometry
integer(c_int), intent(in) :: c_key_vars !< List of variables

type(fv3_field), pointer :: self
type(fv3_geom),  pointer :: geom
type(fv3_vars),  pointer :: vars

call fv3_geom_registry%get(c_key_geom, geom)
call fv3_vars_registry%get(c_key_vars, vars)
call fv3_field_registry%init()
call fv3_field_registry%add(c_key_self)
call fv3_field_registry%get(c_key_self,self)

call create(self, geom, vars)

end subroutine fv3_field_create_c

! ------------------------------------------------------------------------------

subroutine fv3_field_delete_c(c_key_self) bind(c,name='fv3_field_delete_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(inout) :: c_key_self
type(fv3_field), pointer :: self

call fv3_field_registry%get(c_key_self,self)

call delete(self)

call fv3_field_registry%remove(c_key_self)

end subroutine fv3_field_delete_c

! ------------------------------------------------------------------------------

subroutine fv3_field_zero_c(c_key_self) bind(c,name='fv3_field_zero_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
type(fv3_field), pointer :: self

call fv3_field_registry%get(c_key_self,self)
call zeros(self)

end subroutine fv3_field_zero_c

! ------------------------------------------------------------------------------

subroutine fv3_field_random_c(c_key_self) bind(c,name='fv3_field_random_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
type(fv3_field), pointer :: self

call fv3_field_registry%get(c_key_self,self)
call random(self)

end subroutine fv3_field_random_c

! ------------------------------------------------------------------------------

subroutine fv3_field_copy_c(c_key_self,c_key_rhs) bind(c,name='fv3_field_copy_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
integer(c_int), intent(in) :: c_key_rhs

type(fv3_field), pointer :: self
type(fv3_field), pointer :: rhs
call fv3_field_registry%get(c_key_self,self)
call fv3_field_registry%get(c_key_rhs,rhs)

call copy(self, rhs)

end subroutine fv3_field_copy_c

! ------------------------------------------------------------------------------

subroutine fv3_field_self_add_c(c_key_self,c_key_rhs) bind(c,name='fv3_field_self_add_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
integer(c_int), intent(in) :: c_key_rhs

type(fv3_field), pointer :: self
type(fv3_field), pointer :: rhs
call fv3_field_registry%get(c_key_self,self)
call fv3_field_registry%get(c_key_rhs,rhs)

call self_add(self,rhs)

end subroutine fv3_field_self_add_c

! ------------------------------------------------------------------------------

subroutine fv3_field_self_schur_c(c_key_self,c_key_rhs) bind(c,name='fv3_field_self_schur_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
integer(c_int), intent(in) :: c_key_rhs

type(fv3_field), pointer :: self
type(fv3_field), pointer :: rhs
call fv3_field_registry%get(c_key_self,self)
call fv3_field_registry%get(c_key_rhs,rhs)

call self_schur(self,rhs)

end subroutine fv3_field_self_schur_c

! ------------------------------------------------------------------------------

subroutine fv3_field_self_sub_c(c_key_self,c_key_rhs) bind(c,name='fv3_field_self_sub_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
integer(c_int), intent(in) :: c_key_rhs

type(fv3_field), pointer :: self
type(fv3_field), pointer :: rhs
call fv3_field_registry%get(c_key_self,self)
call fv3_field_registry%get(c_key_rhs,rhs)

call self_sub(self,rhs)

end subroutine fv3_field_self_sub_c

! ------------------------------------------------------------------------------

subroutine fv3_field_self_mul_c(c_key_self,c_zz) bind(c,name='fv3_field_self_mul_f90')
use iso_c_binding
use fv3_fields
use kinds
implicit none
integer(c_int), intent(in) :: c_key_self
real(c_double), intent(in) :: c_zz
type(fv3_field), pointer :: self
real(kind=kind_real) :: zz

call fv3_field_registry%get(c_key_self,self)
zz = c_zz

call self_mul(self,zz)

end subroutine fv3_field_self_mul_c

! ------------------------------------------------------------------------------

subroutine fv3_field_axpy_c(c_key_self,c_zz,c_key_rhs) bind(c,name='fv3_field_axpy_f90')
use iso_c_binding
use fv3_fields
use kinds
implicit none
integer(c_int), intent(in) :: c_key_self
real(c_double), intent(in) :: c_zz
integer(c_int), intent(in) :: c_key_rhs

type(fv3_field), pointer :: self
type(fv3_field), pointer :: rhs
real(kind=kind_real) :: zz

call fv3_field_registry%get(c_key_self,self)
call fv3_field_registry%get(c_key_rhs,rhs)
zz = c_zz

call axpy(self,zz,rhs)

end subroutine fv3_field_axpy_c

! ------------------------------------------------------------------------------

subroutine fv3_field_dot_prod_c(c_key_fld1,c_key_fld2,c_prod) bind(c,name='fv3_field_dot_prod_f90')
use iso_c_binding
use fv3_fields
use fv3_misc_mod
use kinds
implicit none
integer(c_int), intent(in)    :: c_key_fld1, c_key_fld2
real(c_double), intent(inout) :: c_prod
real(kind=kind_real) :: zz
type(fv3_field), pointer :: fld1, fld2

! mpi definitions.
INCLUDE 'mpif.h'
INTEGER :: MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr
INTEGER :: color,key,newcomm,newnumprocs,newnproc,numprocs_per_tile,&
     &numprocself,numprocself_per_tile
LOGICAL :: mpi_started

CALL MPI_Initialized(mpi_started,iret)

IF (.NOT. mpi_started) CALL MPI_Init(iret)
CALL determine_mpi_type_real

call fv3_field_registry%get(c_key_fld1,fld1)
call fv3_field_registry%get(c_key_fld2,fld2)

call dot_prod(fld1,fld2,zz)

CALL MPI_allreduce(zz,c_prod,1,mpi_type_real,mpi_sum,mpi_comm_world,iret)

end subroutine fv3_field_dot_prod_c

! ------------------------------------------------------------------------------

subroutine fv3_field_add_incr_c(c_key_self,c_key_rhs) bind(c,name='fv3_field_add_incr_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_self
integer(c_int), intent(in) :: c_key_rhs
type(fv3_field), pointer :: self
type(fv3_field), pointer :: rhs

call fv3_field_registry%get(c_key_self,self)
call fv3_field_registry%get(c_key_rhs,rhs)

call add_incr(self,rhs)

end subroutine fv3_field_add_incr_c

! ------------------------------------------------------------------------------

subroutine fv3_field_diff_incr_c(c_key_lhs,c_key_x1,c_key_x2) bind(c,name='fv3_field_diff_incr_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_lhs
integer(c_int), intent(in) :: c_key_x1
integer(c_int), intent(in) :: c_key_x2
type(fv3_field), pointer :: lhs
type(fv3_field), pointer :: x1
type(fv3_field), pointer :: x2

call fv3_field_registry%get(c_key_lhs,lhs)
call fv3_field_registry%get(c_key_x1,x1)
call fv3_field_registry%get(c_key_x2,x2)

call diff_incr(lhs,x1,x2)

end subroutine fv3_field_diff_incr_c

! ------------------------------------------------------------------------------

subroutine fv3_field_change_resol_c(c_key_fld,c_key_rhs) bind(c,name='fv3_field_change_resol_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_fld
integer(c_int), intent(in) :: c_key_rhs
type(fv3_field), pointer :: fld, rhs

call fv3_field_registry%get(c_key_fld,fld)
call fv3_field_registry%get(c_key_rhs,rhs)

call change_resol(fld,rhs)

end subroutine fv3_field_change_resol_c

! ------------------------------------------------------------------------------

subroutine fv3_field_read_file_c(c_key_fld, c_conf, c_dt) bind(c,name='fv3_field_read_file_f90')
use iso_c_binding
use fv3_fields
use datetime_mod

implicit none
integer(c_int), intent(in) :: c_key_fld  !< Fields
type(c_ptr), intent(in)    :: c_conf !< Configuration
type(c_ptr), intent(inout) :: c_dt   !< DateTime

type(fv3_field), pointer :: fld
type(datetime) :: fdate

call fv3_field_registry%get(c_key_fld,fld)
call c_f_datetime(c_dt, fdate)
call read_file(fld, c_conf, fdate)

end subroutine fv3_field_read_file_c

! ------------------------------------------------------------------------------

subroutine fv3_field_write_file_c(c_key_fld, c_conf, c_dt) bind(c,name='fv3_field_write_file_f90')
use iso_c_binding
use fv3_fields
use datetime_mod

implicit none
integer(c_int), intent(in) :: c_key_fld  !< Fields
type(c_ptr), intent(in) :: c_conf !< Configuration
type(c_ptr), intent(in) :: c_dt   !< DateTime

type(fv3_field), pointer :: fld
type(datetime) :: fdate

call fv3_field_registry%get(c_key_fld,fld)
call c_f_datetime(c_dt, fdate)
call write_file(fld, c_conf, fdate)

end subroutine fv3_field_write_file_c

! ------------------------------------------------------------------------------

SUBROUTINE fv3_field_gpnorm_c(c_key_fld, kf3d, kf2d, pstat) BIND(c,name='fv3_field_gpnorm_f90')
use iso_c_binding
use fv3_fields
use kinds
implicit none
integer(c_int), intent(in) :: c_key_fld
INTEGER(c_int), INTENT(in) :: kf3d, kf2d
REAL(c_double), INTENT(inout) :: pstat(3*(kf3d+kf2d))

type(fv3_field), pointer :: fld
REAL(kind=kind_real) :: zstat(3, kf3d+kf2d)
integer :: jj, js, jf
integer :: kf

kf=kf3d+kf2d

call fv3_field_registry%get(c_key_fld,fld)

CALL gpnorm(fld, kf3d, kf2d, zstat)
jj=0
do jf = 1, kf
  do js = 1, 3
    jj=jj+1
    pstat(jj) = zstat(js,jf)
  enddo
enddo

end subroutine fv3_field_gpnorm_c

! ------------------------------------------------------------------------------

subroutine fv3_field_rms_c(c_key_fld, prms) bind(c,name='fv3_field_rms_f90')
use iso_c_binding
use fv3_fields
use fv3_misc_mod
use kinds
implicit none
integer(c_int), intent(in) :: c_key_fld
real(c_double), intent(inout) :: prms

type(fv3_field), pointer :: fld
real(kind=kind_real) :: zz

! mpi definitions.
INCLUDE 'mpif.h'
INTEGER :: MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr
INTEGER :: color,key,newcomm,newnumprocs,newnproc,numprocs_per_tile,&
     &numprocself,numprocself_per_tile
     LOGICAL :: mpi_started

CALL MPI_Initialized(mpi_started,iret)

IF (.NOT. mpi_started) CALL MPI_Init(iret)
CALL determine_mpi_type_real

call fv3_field_registry%get(c_key_fld,fld)

call fldrms(fld, zz)

CALL MPI_allreduce(zz,prms,1,mpi_type_real,mpi_sum,mpi_comm_world,iret)

prms=SQRT(prms/(REAL(fld%nftot*fld%nyg*fld%nxg*fld%ntiles,kind_real)))

end subroutine fv3_field_rms_c

! ------------------------------------------------------------------------------

SUBROUTINE fv3_fieldnum_c(c_key_fld, nx, ny, nf2d, nf3d) BIND(c,name='fv3_field_sizes_f90')
use iso_c_binding
use fv3_fields
implicit none
integer(c_int), intent(in) :: c_key_fld
INTEGER(kind=c_int), INTENT(inout) :: nx, ny, nf3d, nf2d
type(fv3_field), pointer :: fld

call fv3_field_registry%get(c_key_fld,fld)

nx = fld%nx
ny = fld%ny
nf3d = fld%nf3d
nf2d = fld%nf2d

end subroutine fv3_fieldnum_c

! ------------------------------------------------------------------------------
