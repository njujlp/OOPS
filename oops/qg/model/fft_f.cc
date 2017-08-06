/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "model/fft_f.h"

#include <cmath>
#include <unsupported/Eigen/FFT>

// -----------------------------------------------------------------------------
namespace qg {
// -----------------------------------------------------------------------------

void fft_fwd_f(const int & kk, const double * xx, double * ff) {
  std::vector<double> grid(kk);
  const unsigned int size = kk/2+1;
  std::vector<std::complex<double> > coefs(size);

  for (unsigned int jj = 0; jj < kk; ++jj) grid[jj] = xx[jj];

  Eigen::FFT<double> fft;
  fft.fwd(coefs, grid);

  for (unsigned int jj = 0; jj < size; ++jj) {
    ff[2*jj]   = coefs[jj].real();
    ff[2*jj+1] = coefs[jj].imag();
  }
}

// -----------------------------------------------------------------------------

void fft_inv_f(const int & kk, const double * ff, double * xx) {
  std::vector<double> grid(kk);
  const unsigned int size = kk/2+1;
  std::vector<std::complex<double> > coefs(size);

// Why is this needed???
// It seems fft doesn't work if fwd is not run first???
  std::vector<double> zz(kk);
  for (unsigned int jj = 0; jj < kk; ++jj) zz[jj] = 0.0;
  Eigen::FFT<double> fft;
  fft.fwd(coefs, zz);
// Why is this needed???

  for (unsigned int jj = 0; jj < size; ++jj) {
    std::complex<double> zz(ff[2*jj], ff[2*jj+1]);
    coefs[jj] = zz;
  }

  fft.inv(grid, coefs);

  for (unsigned int jj = 0; jj < kk; ++jj) xx[jj] = grid[jj];
}

// -----------------------------------------------------------------------------

}  // namespace qg
