MODULE fv3_misc_mod

  USE kinds 
  USE netcdf

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: mpp_compute_extent,namelist_fv3,get_varnames,&
       &determine_mpi_type_real
  PUBLIC :: max_string_length,nc_maxdims,&
       &lonname,latname,areaname,varseparator
  PUBLIC :: mpi_type_real

  INTEGER, PARAMETER :: max_string_length=500, nc_maxdims=4
  CHARACTER(len=1),PARAMETER :: varseparator=','
  CHARACTER(len=NF90_MAX_NAME), PARAMETER :: lonname="grid_lont"
  CHARACTER(len=NF90_MAX_NAME), PARAMETER :: latname="grid_latt"
  CHARACTER(len=NF90_MAX_NAME), PARAMETER :: areaname="area"

  INTEGER :: mpi_type_real

CONTAINS

  SUBROUTINE determine_mpi_type_real
    
    INCLUDE 'mpif.h'
    
    REAL(kind=kind_real) :: trash
    INTEGER :: rprecision, rexponent, iret
    LOGICAL :: mpi_started

    trash=1_kind_real
    rprecision=PRECISION(trash)
    rexponent=RANGE(trash)
    
    CALL MPI_Initialized(mpi_started,iret)
    
    IF (.NOT. mpi_started) CALL MPI_Init(iret)
    
    CALL MPI_TYPE_CREATE_F90_REAL(rprecision, rexponent, &
         &mpi_type_real, iret)

  END SUBROUTINE determine_mpi_type_real

  SUBROUTINE mpp_compute_extent(isg,ieg,ndivs,ibegin,iend)

    INTEGER,                 INTENT(in)          :: isg, ieg, ndivs
    INTEGER, DIMENSION(0:ndivs-1), INTENT(out)          :: ibegin, iend

    INTEGER :: ndiv, imax, ndmax, ndmirror
    INTEGER :: is, ie, n
    LOGICAL :: symmetrize
!statement functions
    LOGICAL :: even, odd
    even(n) = (MOD(n,2).EQ.0)
    odd (n) = (MOD(n,2).EQ.1)

    is = isg

    DO ndiv=0,ndivs-1
       symmetrize = ( even(ndivs) .AND. even(ieg-isg+1) ) .OR. &
            (  odd(ndivs) .AND.  odd(ieg-isg+1) ) .OR. &
            (  odd(ndivs) .AND. even(ieg-isg+1) .AND. ndivs.LT.(ieg-isg+1)/2 )

       IF( ndiv.EQ.0 )THEN
          imax = ieg
          ndmax = ndivs
       END IF
!do bottom half of decomposition, going over the midpoint for odd ndivs

       IF( ndiv.LT.(ndivs-1)/2+1 )THEN
!domain is sized by dividing remaining points by remaining domains
          ie = is + CEILING( REAL(imax-is+1)/(ndmax-ndiv) ) - 1
          ndmirror = (ndivs-1) - ndiv !mirror domain
          IF( ndmirror.GT.ndiv .AND. symmetrize )THEN !only for domains over the midpoint
!mirror extents, the max(,) is to eliminate overlaps
             ibegin(ndmirror) = MAX( isg+ieg-ie, ie+1 )
             iend(ndmirror)   = MAX( isg+ieg-is, ie+1 )
             imax = ibegin(ndmirror) - 1
             ndmax = ndmax - 1
          END IF
       ELSE
          IF( symmetrize )THEN
!do top half of decomposition by retrieving saved values
             is = ibegin(ndiv)
             ie = iend(ndiv)
          ELSE
             ie = is + CEILING( REAL(imax-is+1)/(ndmax-ndiv) ) - 1
          END IF
       END IF
       ibegin(ndiv) = is
       iend(ndiv) = ie

       IF( ie.LT.is ) THEN
          PRINT *,'domain extents must bepositive definite - Stopping'
          STOP
       ENDIF

       IF( ndiv.EQ.ndivs-1 .AND. iend(ndiv).NE.ieg ) THEN
          PRINT *,'domain extents do not span space completely Stopping'
          STOP
       ENDIF

       is = ie + 1

    END DO

  END SUBROUTINE mpp_compute_extent

  SUBROUTINE namelist_fv3(input_nml_file,nxg,nyg,npz,ntiles,&
       &layout_x,layout_y,nxblocks,nyblocks)
    
    CHARACTER(len=max_string_length) :: input_nml_file
    INTEGER :: layout_x,layout_y,nxg,nyg
    INTEGER :: nml_unit=101,ios

    LOGICAL :: use_logp, do_schmidt, fv_debug, fv_land, nudge,do_sat_adj,&
         &do_f3d,external_ic,nggps_ic,use_new_ncep,read_increment,&
         &ncep_ic,use_ncep_phy,fv_diag_ic,adjust_dry_mass,&
         &non_ortho,warm_start,mountain,convert_ke,use_old_omega,&
         &do_Held_Suarez,do_reed_physics,reed_cond_only,fill,&
         &filter_phys,fill_wz,fill_dp,range_warn,z_tracer,&
         &reproduce_sum,adiabatic,inline_q,consv_am,dwind_2d,&
         &do_vort_damp,&
         &hydrostatic,no_dycore,breed_vortex_inline,hybrid_z,Make_NH,&
         &reset_eta,remap_t,phys_hydrostatic,use_hydro_pressure,&
         &make_hybrid_z,nested,twowaynest,old_divg_damp,check_negative,&
         &nudge_ic,agrid_vel_rst,gocart_esrl,gfs_phil

    INTEGER, PARAMETER :: nlayout=2

    INTEGER :: npx,npy,ntiles, npz, npz_rst, ncnst, nwat, &
         &k_split, n_split, m_split,q_split,&
         &hord_mt,hord_vt, hord_tm, hord_dp, hord_tr,&
         &kord_mt, kord_wz, kord_tm, kord_tr,&
         &print_freq,n_sponge,nord,nord_tr,&
         &grid_type,nf_omega,fv_sg_adj,na_init,n_zs_filter,&
         &dnats,a2b_ord,c2l_ord,nord_zs_filter,pnats,&
         &parent_grid_num,parent_tile,nestbctype,nsponge,&
         &nestupdate,refinement,ioffset,joffset,halo_update_type

    INTEGER, DIMENSION(nlayout) :: layout,io_layout

    REAL :: a_imp,p_fac,shift_fac,stretch_fac,target_lat,target_lon,&
         &scale_z,w_max,z_min,beta,dddmp,d2_bg, d4_bg,d_ext,&
         &vtdm4,trdm2,ke_bg,d_con,dry_mass,consv_te,tau,tau_h2o,&
         &rf_cutoff,dx_const,dy_const,p_ref,d2_bg_k1,d2_bg_k2,deglat,&
         &deglon_start, deglon_stop, deglat_start, deglat_stop,&
         &add_noise,umax,s_weight

    CHARACTER(len=128) :: res_latlon_dynamics
    CHARACTER(len=128) :: res_latlon_tracers

    LOGICAL :: surface_debug, dycore_only, debug, sync

    INTEGER :: nxblocks, nyblocks

    NAMELIST /atmos_model_nml/ nxblocks, nyblocks, &
         &surface_debug, dycore_only,debug, sync

    NAMELIST /fv_core_nml/npx, npy, ntiles, npz, npz_rst, &
         &layout, io_layout, ncnst, nwat,  &
         &use_logp, p_fac, a_imp, k_split, n_split, m_split, q_split, &
         &print_freq, do_schmidt,      &
         &hord_mt, hord_vt, hord_tm, hord_dp, hord_tr, &
         &shift_fac, stretch_fac, target_lat, target_lon, &
         &kord_mt, kord_wz, kord_tm, kord_tr, &
         &fv_debug, fv_land, nudge, do_sat_adj, do_f3d, &
         &external_ic, read_increment, ncep_ic, nggps_ic, use_new_ncep, &
         &use_ncep_phy, fv_diag_ic, &
         &res_latlon_dynamics, res_latlon_tracers, &
         &scale_z, w_max, z_min, &
         &dddmp, d2_bg, d4_bg, vtdm4, trdm2, d_ext, &
         &beta, non_ortho, n_sponge, &
         &warm_start, adjust_dry_mass, mountain, d_con, ke_bg, &
         &nord, nord_tr, convert_ke, use_old_omega, &
         &dry_mass, grid_type, do_Held_Suarez, do_reed_physics, &
         &reed_cond_only, &
         &consv_te, fill, filter_phys, fill_dp, fill_wz, &
         &consv_am, range_warn, dwind_2d, inline_q, z_tracer, &
         &reproduce_sum, adiabatic, do_vort_damp, no_dycore,   &
         &tau, tau_h2o, rf_cutoff, nf_omega, hydrostatic, fv_sg_adj, &
         &breed_vortex_inline,  &
         &na_init, hybrid_z, Make_NH, n_zs_filter, nord_zs_filter, &
         &reset_eta,         &
         &pnats, dnats, a2b_ord, remap_t, p_ref, &
         &d2_bg_k1, d2_bg_k2,  &
         &c2l_ord, dx_const, dy_const, umax, deglat,      &
         &deglon_start, deglon_stop, deglat_start, deglat_stop, &
         &phys_hydrostatic, use_hydro_pressure, make_hybrid_z, &
         &old_divg_damp, add_noise, &
         &nested, twowaynest, parent_grid_num, parent_tile, &
         &refinement, nestbctype, nestupdate, nsponge, s_weight, &
         &ioffset, joffset, check_negative, nudge_ic, halo_update_type, &
         &gfs_phil, agrid_vel_rst,gocart_esrl


    OPEN(unit=nml_unit,file=TRIM(input_nml_file))
    READ (nml_unit,atmos_model_nml,iostat=ios)
    READ (nml_unit,fv_core_nml,iostat=ios)
    CLOSE(nml_unit)

    layout_x=layout(1)
    layout_y=layout(2)
    
    nxg=npx-1
    nyg=npy-1

  END SUBROUTINE namelist_fv3

  SUBROUTINE get_varnames(string,separator,nvars,varnames)

    CHARACTER(len=max_string_length), INTENT(in) :: string
    CHARACTER(len=1), INTENT(in) :: separator
    INTEGER, INTENT(inout) :: nvars
    CHARACTER(len=NF90_MAX_NAME), DIMENSION(NF90_MAX_VARS), &
         &INTENT(inout) :: varnames

    INTEGER, DIMENSION(NF90_MAX_VARS) :: indices


    INTEGER :: i,j,strlength
    
    nvars=0
    varnames=''
    indices=0

    strlength=LEN(TRIM(string))
    indices(1)=1
    
    j=1
    DO i=1,strlength
       IF (string(i:i)==separator) THEN
          j=j+1
          indices(j)=i+1
       ENDIF
    ENDDO
    
    DO i=1,j-1
       varnames(i)=string(indices(i):indices(i+1)-2)
    ENDDO
    
    varnames(j)=string(indices(j):strlength)
    
    nvars=j
    
  END SUBROUTINE get_varnames
          
END MODULE fv3_misc_mod

