
#include "RunFV3.h"

#include "util/Logger.h"
#include "oops/runs/Run.h"
#include "eckit/config/Configuration.h"

#include "model/Fortran.h"

namespace fv3gfs {

// -----------------------------------------------------------------------------

RunFV3::RunFV3(int argc, char ** argv): oops::Run(argc, argv) {
  oops::Log::trace() << "Creating RunFV3" << std::endl;
  const eckit::Configuration * conf = &config();
  fv3gfs_setup_f(&conf);
  oops::Log::trace() << "RunFV3 created" << std::endl;
}

// -----------------------------------------------------------------------------

RunFV3::~RunFV3() {
  oops::Log::trace() << "Destructing RunFV3" << std::endl;
  fv3gfs_finalize_f();
  oops::Log::trace() << "MPI finalized, RunFV3 destructed" << std::endl;
}

// -----------------------------------------------------------------------------

}  // namespace fv3gfs
