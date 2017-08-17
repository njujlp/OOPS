module fv3gfs_mod

    #include <fms_platform.h>

    use mpp_mod,         only: mpp_pe, mpp_npes, mpp_init, mpp_exit
    use mpp_mod,         only: stdout, mpp_error, FATAL, NOTE
    use mpp_mod,         only: input_nml_file
    use mpp_domains_mod, only: domain2D, mpp_define_layout, mpp_define_mosaic
    use mpp_domains_mod, only: mpp_get_compute_domain, mpp_get_data_domain
    use mpp_domains_mod, only: mpp_domains_init, mpp_domains_exit
    use mpp_domains_mod, only: mpp_domains_set_stack_size, mpp_define_io_domain
    use mpp_io_mod,      only: mpp_open, mpp_close, MPP_ASCII, MPP_RDONLY
    use fms_io_mod,      only: fms_io_init, fms_io_exit
    use fms_io_mod,      only: file_exist

    implicit none

    private

    public :: setup_geom

contains

subroutine setup_geom(self, gtype, nx, ny, ntiles, layout_in, io_layout_in, halo)

    implicit none

    type(domain2D),   intent(inout) :: self
    character(len=*), intent(in)    :: gtype
    integer,          intent(in)    :: nx, ny, ntiles
    integer,          intent(in)    :: layout_in(:), io_layout_in(:)
    integer,          intent(in)    :: halo

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

    pe = mpp_pe()
    npes = mpp_npes()

    if (mod(npes,ntiles) /= 0) then
       call mpp_error(NOTE, "setup_geom: npes can not be divided by ntiles, no test will be done for "//trim(gtype))
       return
    endif
    npes_per_tile = npes/ntiles
    tile = pe/npes_per_tile + 1

    if (layout_in(1)*layout_in(2) == npes_per_tile) then
       layout = layout_in
    else
       call mpp_define_layout( (/1,nx,1,ny/), npes_per_tile, layout )
    endif

    if (io_layout_in(1) <1 .or. io_layout_in(2) <1) call mpp_error(FATAL, &
            "setup_geom: both elements of variable io_layout_in must be positive integer")
    if (mod(layout(1), io_layout_in(1)) /= 0 ) call mpp_error(FATAL, &
         "setup_geom: layout(1) must be divided by io_layout_in(1)")
    if (mod(layout(2), io_layout_in(2)) /= 0 ) call mpp_error(FATAL, &
         "setup_geom: layout(2) must be divided by io_layout_in(2)")

    allocate(global_indices(4,ntiles), layout2D(2,ntiles), pe_start(ntiles), pe_end(ntiles) )
    do n = 1, ntiles
       global_indices(:,n) = (/1,nx,1,ny/)
       layout2D(:,n)       = layout
       pe_start(n)         = (n-1)*npes_per_tile
       pe_end(n)           = n*npes_per_tile-1
    enddo

    num_contact = 0

    call mpp_define_mosaic(global_indices, layout2D, self, ntiles, num_contact, tile1, tile2, &
                           istart1, iend1, jstart1, jend1, istart2, iend2, jstart2, jend2,      &
                           pe_start, pe_end, whalo=halo, ehalo=halo, shalo=halo, nhalo=halo,    &
                           name=gtype)

    if (io_layout_in(1) /= 1 .or. io_layout_in(2) /= 1) &
        call mpp_define_io_domain(self, io_layout_in)

    call mpp_get_compute_domain(self, isc, iec, jsc, jec)
    call mpp_get_data_domain(self, isd, ied, jsd, jed)

end subroutine setup_geom

end module fv3gfs_mod
