/*
 * (C) Copyright 2017 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef FV3GFS_MODEL_FV3GFSFORTRAN_H_
#define FV3GFS_MODEL_FV3GFSFORTRAN_H_

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
  class Duration;
}

namespace fv3gfs {

/// Interface to Fortran FV3GFS model
/*!
 * The core of the FV3GFS model is coded in Fortran.
 * Here we define the interfaces to the Fortran code.
 */

extern "C" {
// -----------------------------------------------------------------------------
//  Geometry
// -----------------------------------------------------------------------------
  void fv3gfs_geo_setup_f90(int & keyGeom, const eckit::Configuration * const *);
  void fv3gfs_geo_clone_f90(const int & keyGeom, int & keyGeom_other);
  void fv3gfs_geo_info_f90(const int & keyGeom, int &);
  void fv3gfs_geo_delete_f90(int & keyGeom);

// -----------------------------------------------------------------------------
//  Fields
// -----------------------------------------------------------------------------
  void fv3gfs_field_create_f90(int & keyFlds, const int &, const int & keyVars);
  void fv3gfs_field_delete_f90(int & keyFlds);

  void fv3gfs_field_copy_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3gfs_field_zero_f90(const int & keyFlds);
  void fv3gfs_field_self_add_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3gfs_field_self_sub_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3gfs_field_self_mul_f90(const int & keyFlds, const double &);
  void fv3gfs_field_axpy_f90(const int & keyFlds, const double &, const int & keyFldsOther);
  void fv3gfs_field_dot_prod_f90(const int & keyFlds, const int & keyFldsOther, double &);
  void fv3gfs_field_self_schur_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3gfs_field_random_f90(const int & keyFlds);

  void fv3gfs_field_add_incr_f90(const int & keyFlds, const int & keyFldsOther);
  void fv3gfs_field_diff_incr_f90(const int & keyFlds, const int & keyFldsOther,
                              const int & keyFldsOther2);

  void fv3gfs_field_change_resol_f90(const int & keyFlds, const int & keyFldsOther);

  void fv3gfs_field_read_file_f90(const int & keyFlds, const eckit::Configuration * const *,
                              util::DateTime * const *);
  void fv3gfs_field_write_file_f90(const int & keyFlds, const eckit::Configuration * const *,
                               const util::DateTime * const *);

  void fv3gfs_field_gpnorm_f90(const int & keyFlds, const int &, double &);
  void fv3gfs_field_sizes_f90(const int & keyFlds, int &, int &, int &, int &);
  void fv3gfs_field_rms_f90(const int & keyFlds, double &);

// -----------------------------------------------------------------------------
//  Variables
// -----------------------------------------------------------------------------
  void fv3gfs_var_create_f90(int & keyVars, const eckit::Configuration * const *);
  void fv3gfs_var_clone_f90(const int & keyVars, int & keyVars_other);
  void fv3gfs_var_info_f90(const int & keyVars, int &, int &);
  void fv3gfs_var_delete_f90(int & keyVars);
}

// -----------------------------------------------------------------------------

}  // namespace fv3gfs
#endif  // FV3GFS_MODEL_FV3GFSFORTRAN_H_
