! (C) Copyright 2009-2016 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> Perform a timestep of the QG model - Tangent Linear

!> This routine is called from C++ to propagate the increment

subroutine c_qg_propagate_tl(c_key_conf, c_key_incr, c_key_traj) &
           bind(c,name='qg_propagate_tl_f90')

use iso_c_binding
use qg_fields
use qg_trajectories
use qg_configs
use qg_constants, only: bet
use kinds

implicit none
integer(c_int), intent(in) :: c_key_conf !< Config structure
integer(c_int), intent(in) :: c_key_incr !< Model fields
integer(c_int), intent(in) :: c_key_traj !< Trajectory structure

type(qg_config),     pointer :: conf
type(qg_field),      pointer :: flds
type(qg_trajectory), pointer :: traj

real(kind_real), allocatable :: qnew(:,:,:), x_traj(:,:,:)
real(kind_real), allocatable :: q_traj(:,:,:), u_traj(:,:,:), v_traj(:,:,:)
real(kind_real), allocatable :: qn_traj(:,:), qs_traj(:,:)
real(kind_real) :: xn_traj(2), xs_traj(2)

! ------------------------------------------------------------------------------

call qg_config_registry%get(c_key_conf, conf)
call qg_field_registry%get(c_key_incr,flds)
call qg_traj_registry%get(c_key_traj,traj)

!--- workspace and trajectory
allocate(qnew(flds%geom%nx,flds%geom%ny,2))
allocate(x_traj(flds%geom%nx,flds%geom%ny,2))
allocate(qn_traj(flds%geom%nx,2))
allocate(qs_traj(flds%geom%nx,2))
allocate(q_traj(flds%geom%nx,flds%geom%ny,2))
allocate(u_traj(flds%geom%nx,flds%geom%ny,2))
allocate(v_traj(flds%geom%nx,flds%geom%ny,2))
call get_traj(traj,flds%geom%nx,flds%geom%ny,x_traj,xn_traj,xs_traj,qn_traj,qs_traj)

!--- generate trajectory values for potential vorticity and wind

call calc_pv(flds%geom%nx,flds%geom%ny,q_traj,x_traj,xn_traj,xs_traj, &
    &        conf%f1,conf%f2,conf%deltax,conf%deltay,bet,conf%rs)
call zonal_wind (u_traj,x_traj,xn_traj,xs_traj,flds%geom%nx,flds%geom%ny, conf%deltay)
call meridional_wind (v_traj,x_traj,flds%geom%nx,flds%geom%ny,conf%deltax)

!--- advect the potential vorticity

qnew(:,:,:)=0.0_kind_real
call advect_pv_tl(qnew,flds%q,q_traj,qn_traj,qs_traj, &
    &             flds%u,u_traj,flds%v,v_traj,flds%geom%nx,flds%geom%ny,&
    &             conf%deltax,conf%deltay,conf%dt)

!--- invert the potential vorticity to determine streamfunction

call invert_pv_tl(flds%x,qnew,flds%geom%nx,flds%geom%ny, &
                & conf%deltax,conf%deltay,conf%f1,conf%f2)

! -- calculate potential vorticity and wind components

flds%q(:,:,:) = qnew(:,:,:)
call zonal_wind_tl(flds%u,flds%x,flds%geom%nx,flds%geom%ny,conf%deltay)
call meridional_wind_tl(flds%v,flds%x,flds%geom%nx,flds%geom%ny,conf%deltax)

!--- clean-up
deallocate(qnew)
deallocate(x_traj)
deallocate(qn_traj)
deallocate(qs_traj)
deallocate(q_traj)
deallocate(u_traj)
deallocate(v_traj)
! ------------------------------------------------------------------------------
return
end subroutine c_qg_propagate_tl
