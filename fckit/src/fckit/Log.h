#pragma once

#include "eckit/log/Log.h"

namespace fckit {

class Log : public eckit::Log {
public:
  enum Style {
    SIMPLE=0,PREFIX=1,TIMESTAMP=2
  };
  
  static void addFortranUnit(int unit, Style=PREFIX, const char* prefix="");
  static void setFortranUnit(int unit, Style=PREFIX, const char* prefix="");

  static void addFile(const char* path, Style=PREFIX, const char* prefix="");
  static void setFile(const char* path, Style=PREFIX, const char* prefix="");

  static void addStdOut(Style=PREFIX, const char* prefix="");
  static void setStdOut(Style=PREFIX, const char* prefix="");

  static void reset();

  static void flush();

  // Fortran unit numbers
  static int output_unit();
  static int error_unit();
};

} // namespace fckit
