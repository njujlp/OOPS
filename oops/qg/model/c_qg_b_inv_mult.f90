! (C) Copyright 2009-2016 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> Multiply streamfunction by inverse of covariance

subroutine c_qg_b_inv_mult(c_key_conf, c_key_in, c_key_out) bind(c,name='qg_b_invmult_f90')

use iso_c_binding
use qg_covariance_mod
use qg_fields
use kinds

implicit none
integer(c_int), intent(in) :: c_key_conf  !< covar config structure
integer(c_int), intent(in) :: c_key_in    !< Streamfunction: psi
integer(c_int), intent(in) :: c_key_out   !< Streamfunction: psi

type(qg_3d_covar_config), pointer :: conf
type(qg_field), pointer :: xin
type(qg_field), pointer :: xout

real(kind=kind_real), allocatable :: xctl(:,:,:) ! Control vector

! ------------------------------------------------------------------------------

call qg_3d_cov_registry%get(c_key_conf,conf)
call qg_field_registry%get(c_key_in,xin)
call qg_field_registry%get(c_key_out,xout)

allocate(xctl(conf%nx, conf%ny, 2))
xctl(:,:,:)=0.0_kind_real

call qg_3d_covar_sqrt_inv_mult(conf%nx,conf%ny,xctl,xin,conf)
call zeros(xout)
call qg_3d_covar_sqrt_inv_mult_ad(conf%nx,conf%ny,xctl,xout,conf)

deallocate(xctl)

end subroutine c_qg_b_inv_mult
