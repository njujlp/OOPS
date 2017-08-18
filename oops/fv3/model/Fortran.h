/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3_MODEL_FV3FORTRAN_H_
#define FV3_MODEL_FV3FORTRAN_H_

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
  class Duration;
}

namespace fv3 {

/// Interface to Fortran FV3 model
/*!
 * The core of the FV3 model is coded in Fortran.
 * Here we define the interfaces to the Fortran code.
 */

extern "C" {
// -----------------------------------------------------------------------------
//  Geometry
// -----------------------------------------------------------------------------
  void fv3_geo_setup_f90(int & keyGeom, const eckit::Configuration * const *);
  void fv3_geo_clone_f90(const int & keyGeom, int & keyGeom_other);
  void fv3_geo_info_f90(const int & keyGeom, int &, int &, int &, int &, int &);
  void fv3_geo_delete_f90(int & keyGeom);

// -----------------------------------------------------------------------------
//  Fields
// -----------------------------------------------------------------------------
  void fv3_field_create_f90(int & keyFlds, const int &, const int & keyVars);
  void fv3_field_delete_f90(int & keyFlds);

  void fv3_field_copy_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3_field_zero_f90(const int & keyFlds);
  void fv3_field_dirac_f90(const int & keyFlds, const eckit::Configuration * const *);

  void fv3_field_self_add_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3_field_self_sub_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3_field_self_mul_f90(const int & keyFlds, const double &);
  void fv3_field_axpy_f90(const int & keyFlds, const double &, const int & keyFldsOther);
  void fv3_field_dot_prod_f90(const int & keyFlds, const int & keyFldsOther, double &);
  void fv3_field_self_schur_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3_field_random_f90(const int & keyFlds);

  void fv3_field_add_incr_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3_field_diff_incr_f90(const int & keyFlds, const int & keyFldsOther,
                              const int & keyFldsOther2);

  void fv3_field_change_resol_f90(const int & keyFlds, const int & keyFldsOther);

  void fv3_field_read_file_f90(const int & keyFlds, const eckit::Configuration * const *,
                              util::DateTime * const *);
  void fv3_field_write_file_f90(const int & keyFlds, const eckit::Configuration * const *,
                               const util::DateTime * const *);

  void fv3_field_gpnorm_f90(const int & keyFlds, const int &, double &);
  void fv3_field_sizes_f90(const int & keyFlds, int &, int &, int &, int &);
  void fv3_field_rms_f90(const int & keyFlds, double &);

// -----------------------------------------------------------------------------
//  Variables
// -----------------------------------------------------------------------------
  void fv3_var_create_f90(int & keyVars, const eckit::Configuration * const *);
  void fv3_var_clone_f90(const int & keyVars, int & keyVars_other);
  void fv3_var_info_f90(const int & keyVars, int &, int &, int &);
  void fv3_var_delete_f90(int & keyVars);
}

// -----------------------------------------------------------------------------

}  // namespace fv3
#endif  // FV3_MODEL_FV3FORTRAN_H_
