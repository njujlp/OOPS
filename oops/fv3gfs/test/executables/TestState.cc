
#include "model/RunFV3.h"
#include "model/Traits.h"
#include "test/interface/State.h"

int main(int argc,  char ** argv) {
  fv3gfs::RunFV3 run(argc, argv);
  test::State<fv3gfs::Traits> tests;
  run.execute(tests);
  return 0;
};

