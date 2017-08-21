
!> Fortran module handling geometry for the FV3-GFS model

module fv3gfs_geom_mod

use iso_c_binding
use config_mod

use mpp_mod,         only: mpp_pe, mpp_npes
use mpp_mod,         only: mpp_error, FATAL
use mpp_domains_mod, only: mpp_domains_set_stack_size
use mpp_domains_mod, only: domain2D, mpp_define_layout, mpp_define_mosaic
use mpp_domains_mod, only: mpp_define_io_domain
use mpp_domains_mod, only: mpp_get_compute_domain, mpp_get_data_domain
use mpp_domains_mod, only: mpp_copy_domain, mpp_deallocate_domain

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

type(domain2D), pointer :: self
character(len=32) :: gtype = 'unknown'
integer :: nx, ny, ntile, halo
integer :: layout_in(2) = (/4,2/)
integer :: io_layout(2) = (/1,1/)
integer :: halo = 1
integer                              :: pe, npes, npes_per_tile, tile
integer                              :: num_contact
integer                              :: n, layout(2)
integer, allocatable, dimension(:,:) :: global_indices, layout2D
integer, allocatable, dimension(:)   :: pe_start, pe_end
integer, dimension(1)                :: tile1, tile2
integer, dimension(1)                :: istart1, iend1, jstart1, jend1
integer, dimension(1)                :: istart2, iend2, jstart2, jend2
integer                              :: isc, iec, jsc, jec
integer                              :: isd, ied, jsd, jed

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!subroutine setup_geom(self, gtype, nx, ny, ntiles, layout_in, io_layout, halo)

  pe = mpp_pe()
  npes = mpp_npes()

  if (mod(npes,ntiles) /= 0) call mpp_error(FATAL, &
     "setup_geom: npes can not be divided by ntiles, no test will be done for "//trim(gtype))

  npes_per_tile = npes/ntiles
  tile = pe/npes_per_tile + 1

  if (layout_in(1)*layout_in(2) == npes_per_tile) then
     layout = layout_in
  else
     call mpp_define_layout( (/1,nx,1,ny/), npes_per_tile, layout )
  endif

  if (io_layout(1) <1 .or. io_layout(2) <1) call mpp_error(FATAL, &
          "setup_geom: both elements of variable io_layout must be positive integer")
  if (mod(layout(1), io_layout(1)) /= 0 ) call mpp_error(FATAL, &
       "setup_geom: layout(1) must be divided by io_layout(1)")
  if (mod(layout(2), io_layout(2)) /= 0 ) call mpp_error(FATAL, &
       "setup_geom: layout(2) must be divided by io_layout(2)")

  allocate(global_indices(4,ntiles), layout2D(2,ntiles), pe_start(ntiles), pe_end(ntiles) )
  do n = 1, ntiles
     global_indices(:,n) = (/1,nx,1,ny/)
     layout2D(:,n)       = layout
     pe_start(n)         = (n-1)*npes_per_tile
     pe_end(n)           = n*npes_per_tile-1
  enddo

  num_contact = 0

  call mpp_define_mosaic(global_indices, layout2D, self, ntiles, num_contact, tile1, tile2, &
                         istart1, iend1, jstart1, jend1, istart2, iend2, jstart2, jend2,    &
                         pe_start, pe_end, whalo=halo, ehalo=halo, shalo=halo, nhalo=halo,  &
                         name=gtype)

  if (io_layout(1) /= 1 .or. io_layout(2) /= 1) &
      call mpp_define_io_domain(self, io_layout)

  call mpp_get_compute_domain(self, isc, iec, jsc, jec)
  call mpp_get_data_domain(self, isd, ied, jsd, jed)

!end subroutine setup_geom
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

! copy self to other
call mpp_copy_domain(self, other)

end subroutine c_fv3gfs_geo_clone

! ------------------------------------------------------------------------------

subroutine c_fv3gfs_geo_delete(c_key_self) bind(c,name='fv3gfs_geo_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self

call fv3gfs_geom_registry%get(c_key_self, self)

! deallocate whatever is in self
call mpp_deallocate_domain(self)

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
