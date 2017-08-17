program fv3gfs_geom

    #include <fms_platform.h>

    use mpp_mod,         only: mpp_pe, mpp_npes, mpp_init, mpp_exit
    use mpp_mod,         only: stdout, mpp_error, FATAL, NOTE
    use mpp_mod,         only: input_nml_file
    use mpp_domains_mod, only: domain2D
    use mpp_domains_mod, only: mpp_domains_init, mpp_domains_exit
    use mpp_domains_mod, only: mpp_domains_set_stack_size
    use mpp_io_mod,      only: mpp_open, mpp_close, MPP_ASCII, MPP_RDONLY
    use fms_io_mod,      only: fms_io_init, fms_io_exit
    use fms_io_mod,      only: file_exist

    use fv3gfs_mod, only: setup_geom

    implicit none

    integer :: sizex_latlon_grid = 144
    integer :: sizey_latlon_grid = 90
    integer :: size_cubic_grid = 48
    integer :: nz = 10
    integer :: halo = 1
    integer :: stackmax = 4000000
    integer :: layout_cubic(2)  = (/4,2/)
    integer :: layout_latlon(2) = (/4,2/)
    integer :: io_layout(2) = (/1,1/) ! set ndivs_x and ndivs_y to divide each tile into io_layout(1)*io_layout(2)
                                      ! group and write out data from the root pe of each group.
    namelist /fv3gfs_geom_nml/ sizex_latlon_grid, sizey_latlon_grid, size_cubic_grid, &
                               nz, halo, stackmax, layout_cubic, layout_latlon, io_layout

    type(domain2D), save :: domain_latlon
    type(domain2D), save :: domain_cubic
    integer, parameter :: ntile_latlon = 1
    integer, parameter :: ntile_cubic = 6
    integer :: pe, npes
    integer :: nmlunit, outunit, io_status

    call mpp_init
    call mpp_domains_init
    call fms_io_init

    pe = mpp_pe()
    npes = mpp_npes()

    if (file_exist('input.nml') )then
       call mpp_open(nmlunit, 'input.nml', form=MPP_ASCII, action=MPP_RDONLY)
       read(nmlunit,fv3gfs_geom_nml,iostat=io_status)
       call mpp_close(nmlunit)
    endif
    if (io_status > 0) then
       call mpp_error(FATAL, '=>fv3gfs_geom: Error reading fv3gfs_geom_nml')
    endif

    outunit = stdout()
    write(outunit, fv3gfs_geom_nml)
    call mpp_domains_set_stack_size(stackmax)

    call setup_geom("latlon_grid", sizex_latlon_grid, sizey_latlon_grid, ntile_latlon, layout_latlon, io_layout, halo, domain_latlon)
    if (mod(npes,ntile_latlon) == 0) &
        call mpp_error(NOTE, "fv3gfs_geom: setup_geom is done for latlon_grid")

    call setup_geom("cubic_grid", size_cubic_grid, size_cubic_grid, ntile_cubic, layout_cubic, io_layout, halo, domain_cubic)
    if (mod(npes,ntile_cubic) == 0) &
        call mpp_error(NOTE, "fv3gfs_geom: setup_geom is done for cubic_grid")

    call fms_io_exit
    call mpp_domains_exit
    call mpp_exit

end program fv3gfs_geom
