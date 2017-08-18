/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef WRF_MODEL_WRFFORTRAN_H_
#define WRF_MODEL_WRFFORTRAN_H_

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
  class Duration;
}

namespace wrf {

/// Interface to Fortran WRF model
/*!
 * The core of the WRF model is coded in Fortran.
 * Here we define the interfaces to the Fortran code.
 */

extern "C" {
// -----------------------------------------------------------------------------
//  Geometry
// -----------------------------------------------------------------------------
  void wrf_geo_setup_f90(int & keyGeom, const eckit::Configuration * const *);
  void wrf_geo_clone_f90(const int & keyGeom, int & keyGeom_other);
  void wrf_geo_info_f90(const int & keyGeom, int &, int &, int &, double &, double &);
  void wrf_geo_delete_f90(int & keyGeom);

// -----------------------------------------------------------------------------
//  Fields
// -----------------------------------------------------------------------------
  void wrf_field_create_f90(int & keyFlds, const int &, const int & keyVars);
  void wrf_field_delete_f90(int & keyFlds);

  void wrf_field_copy_f90(const int & keyFlds, const int & keyFldsOther);
  void wrf_field_zero_f90(const int & keyFlds);
  void wrf_field_dirac_f90(const int & keyFlds, const eckit::Configuration * const *);
  void wrf_field_self_add_f90(const int & keyFlds, const int & keyFldsOther);
  void wrf_field_self_sub_f90(const int & keyFlds, const int & keyFldsOther);
  void wrf_field_self_mul_f90(const int & keyFlds, const double &);
  void wrf_field_axpy_f90(const int & keyFlds, const double &, const int & keyFldsOther);
  void wrf_field_dot_prod_f90(const int & keyFlds, const int & keyFldsOther, double &);
  void wrf_field_self_schur_f90(const int & keyFlds, const int & keyFldsOther);
  void wrf_field_random_f90(const int & keyFlds);

  void wrf_field_add_incr_f90(const int & keyFlds, const int & keyFldsOther);
  void wrf_field_diff_incr_f90(const int & keyFlds, const int & keyFldsOther,
                              const int & keyFldsOther2);

  void wrf_field_change_resol_f90(const int & keyFlds, const int & keyFldsOther);

  void wrf_field_read_file_f90(const int & keyFlds, const eckit::Configuration * const *,
                              util::DateTime * const *);
  void wrf_field_write_file_f90(const int & keyFlds, const eckit::Configuration * const *,
                               const util::DateTime * const *);
  void wrf_field_convert_to_f90(const int &, const int &);
  void wrf_field_convert_from_f90(const int &, const int &);

  void wrf_field_gpnorm_f90(const int & keyFlds, const int &, double &);
  void wrf_field_sizes_f90(const int & keyFlds, int &, int &, int &, int &);
  void wrf_field_rms_f90(const int & keyFlds, double &);

// -----------------------------------------------------------------------------
//  Variables
// -----------------------------------------------------------------------------
  void wrf_var_create_f90(int & keyVars, const eckit::Configuration * const *);
  void wrf_var_clone_f90(const int & keyVars, int & keyVars_other);
  void wrf_var_info_f90(const int & keyVars, int &, int &);
  void wrf_var_delete_f90(int & keyVars);
}

// -----------------------------------------------------------------------------

}  // namespace wrf
#endif  // WRF_MODEL_WRFFORTRAN_H_
