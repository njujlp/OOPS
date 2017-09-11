#include <cstring>
#include "eckit/log/Channel.h"
#include "fckit/Log.h"
using fckit::Log;
using eckit::Channel;

extern "C" {

void fckit__log(Channel* channel, char *msg, int newl, int flush)
{
  if( ::strlen(msg) )
    *channel << msg;
  else
    *channel << " ";
  if( newl )
    *channel << eckit::newl;
  if( flush )
    *channel << std::flush;
}

void fckit__log_debug(char *msg, int newl, int flush)
{
  fckit__log( &Log::debug(), msg, newl, flush );
}

void fckit__log_info(char *msg, int newl, int flush)
{
  fckit__log( &Log::info(), msg, newl, flush );
}

void fckit__log_warning(char *msg, int newl, int flush)
{
  fckit__log( &Log::warning(), msg, newl, flush );
}

void fckit__log_error(char *msg, int newl, int flush)
{
  fckit__log( &Log::error(), msg, newl, flush );
}

void fckit__log_add_fortran_unit(int unit, int style)
{
  Log::addFortranUnit(unit,Log::Style(style));
}

void fckit__log_set_fortran_unit(int unit, int style)
{
  Log::setFortranUnit(unit,Log::Style(style));
}

void fckit__log_add_file(const char* path, int style)
{
  Log::addFile(path,Log::Style(style));
}

void fckit__log_set_file(const char* path, int style)
{
  Log::setFile(path,Log::Style(style));
}

void fckit__log_add_stdout(int style)
{
  Log::addStdOut(Log::Style(style));
}

void fckit__log_set_stdout(int style)
{
  Log::setStdOut(Log::Style(style));
}

void fckit__log_reset()
{
    Log::reset();
}

void fckit__log_flush()
{
    Log::flush();
}

Channel* fckit__log_info_channel()
{
    return &Log::info();
}

Channel* fckit__log_warning_channel()
{
    return &Log::warning();
}

Channel* fckit__log_error_channel()
{
    return &Log::error();
}

Channel* fckit__log_debug_channel()
{
    return &Log::debug();
}

} // extern "C"
