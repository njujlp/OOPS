
#ifndef FV3_RUNFV3_H
#define FV3_RUNFV3_H

#include "oops/runs/Run.h"

namespace fv3gfs {

// -----------------------------------------------------------------------------

class RunFV3 : public oops::Run {
public:
  RunFV3(int, char **);
  ~RunFV3();
};

// -----------------------------------------------------------------------------

}  // namespace fv3gfs

#endif // FV3_RUNFV3_H
