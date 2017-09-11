#include <csignal>
#include <exception>
#include <iomanip>
#include "fckit/Main.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/thread/Once.h"
#include "fckit/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/config/LibEcKit.h"
#include "eckit/os/BackTrace.h"

static eckit::Once<eckit::Mutex> local_mutex;

namespace fckit{

static std::string         exception_what;
static eckit::CodeLocation exception_location;
static std::string         exception_callstack;

//------------------------------------------------------------------------------------------------------------------

void fckit_terminate() {

  // This routine is called for uncaught exceptions.
  // It can be set with std::set_terminate( &fckit_terminate )

    Log::flush();

    if( std::exception_ptr eptr = std::current_exception() ) {
        std::ostream& out = Log::error();
        try {
             std::rethrow_exception( eptr ); // throw to recognise the type
        }
        catch( const eckit::Abort& exception ) {
            out << "\n"
                << "=========================================\n"
                << "Aborting " << Main::instance().displayName() << "\n"
                << "-----------------------------------------\n"
                << exception.what() << "\n";
            if( exception.location() )
                out << "-----------------------------------------\n"
                    << "LOCATION: " << exception.location() << "\n";
            out << "-----------------------------------------\n"
                << "BACKTRACE\n"
                << "-----------------------------------------\n"
                << eckit::BackTrace::dump() << "\n"
                << "=========================================\n"
                << std::endl;
            exception_what      = exception.what();
            exception_location  = exception.location();
            exception_callstack = exception.callStack();
        }
        catch( const eckit::Exception& exception ) {
            out << "\n"
                << "=========================================\n"
                << "TERMINATING " << Main::instance().displayName() << "\n"
                << "-----------------------------------------\n"
                << exception.what() << "\n"
                << "-----------------------------------------\n";

            if( exception.location() )
                out << "LOCATION: " << exception.location() << "\n"
                    << "-----------------------------------------\n";

            out << "BACKTRACE\n"
                << "-----------------------------------------\n"
                << eckit::BackTrace::dump() << "\n"
                << "=========================================\n"
                << std::endl;
            exception_what      = exception.what();
            exception_location  = exception.location();
            exception_callstack = eckit::BackTrace::dump();
        }
        catch( const std::exception& exception ) {
            out << "\n"
                << "=========================================\n"
                << "TERMINATING " << Main::instance().displayName() << "\n"
                << "-----------------------------------------\n"
                << exception.what() << "\n"
                << "-----------------------------------------\n"
                << "BACKTRACE\n"
                << "-----------------------------------------\n"
                << eckit::BackTrace::dump() << "\n"
                << "=========================================\n"
                << std::endl;
            exception_what      = exception.what();
            exception_location  = eckit::CodeLocation();
            exception_callstack = eckit::BackTrace::dump();
        }
        catch( ... ) {
            out << "\n"
                << "=========================================\n"
                << "TERMINATING " << Main::instance().displayName() << "\n"
                << "-----------------------------------------\n"
                << "BACKTRACE\n"
                << "-----------------------------------------\n"
                << eckit::BackTrace::dump() << "\n"
                << "========================================="
                << std::endl;
            exception_what      = "Uncaught exception";
            exception_location  = eckit::CodeLocation();
            exception_callstack = eckit::BackTrace::dump();
        }
    }
    
    eckit::LibEcKit::instance().abort();

    // Just in case we end up here, as last resort, exit immediately without cleanup.
    std::_Exit(EXIT_FAILURE);
}

//------------------------------------------------------------------------------------------------------------------



void fckit_signal_handler(int signum) {

    Signal signal = Signals::instance().signal(signum);

    // Restore default signal handlers in case another signal is raised by accident
    fckit::Signals::instance().restoreAllSignalHandlers();

    std::ostream& out = fckit::Log::error();
    out << "\n"
        << "=========================================\n"
        << signal << " (signal intercepted by fckit)\n";
    out << "-----------------------------------------\n"
        << "BACKTRACE\n"
        << "-----------------------------------------\n"
        << eckit::BackTrace::dump() << "\n"
        << "=========================================\n"
        << std::endl;

    exception_what = "Signal "+signal.str();
    exception_location = eckit::CodeLocation();
    exception_callstack = eckit::BackTrace::dump();
    eckit::LibEcKit::instance().abort();
  
    // Just in case we end up here, which normally we shouldn't.
    std::_Exit(EXIT_FAILURE );
}

Signals& Signals::instance() {
    static Signals signals;
    return signals;
}

void Signals::restoreSignalHandler( int signum ) {
    if( registered_signals_.find(signum) != registered_signals_.end() ) {
        eckit::Log::debug() << "\n";
        eckit::Log::debug() << "Restoring default signal handler for signal "  << registered_signals_[signum] << "\n";
        std::signal( signum, SIG_DFL );
        eckit::Log::debug() << std::endl;
        registered_signals_.erase(signum);
    }
}

void Signals::restoreAllSignalHandlers() {
    eckit::Log::debug() << "\n";
    for( registered_signals_t::const_iterator it = registered_signals_.begin(); it!=registered_signals_.end(); ++it ) {
      eckit::Log::debug() << "Restoring default signal handler for signal "  << it->second.str() << "\n";
      std::signal(it->first,SIG_DFL);
    }
    eckit::Log::debug() << std::endl;
    registered_signals_.clear();
}

const Signal& Signals::signal(int signum) const {
    return registered_signals_.at(signum);
}

std::ostream& operator<< ( std::ostream& out , const Signal& signal ) {
  out << signal.str();
  return out;
}

void Signals::setSignalHandlers() {
  setSignalHandler(SIGABRT);
  setSignalHandler(SIGFPE );
  setSignalHandler(SIGILL );
  setSignalHandler(SIGINT );
  setSignalHandler(SIGSEGV);
  setSignalHandler(SIGTERM);
  setSignalHandler(SIGKILL);
}

void Signals::setSignalHandler( const Signal& signal ) {
  if( Main::instance().taskID() == 0 ) 
    eckit::Log::debug() << "Registering signal handler for signal " << std::setw(2) << int(signal) << " [" << signal << "]" << std::endl;
  registered_signals_[signal] = signal;
  std::signal( signal, signal.handler() );
}

Signal::Signal(int signum) :
  signum_(signum),
  str_( strsignal(signum) ),
  signal_handler_( fckit_signal_handler ) {
}

Signal::Signal() :
  signum_(0),
  str_(),
  signal_handler_(SIG_DFL) {
}


Signal::Signal(int signum, signal_handler_t signal_handler ) :
  signum_(signum),
  str_( strsignal(signum) ),
  signal_handler_(signal_handler) {
}

Main::Main(
    int argc, char **argv,
    const char* homeenv)
    : eckit::Main(argc,argv,homeenv)
{
  std::set_terminate( &fckit_terminate );

  for( int j=0; j<argc; ++j )
  {
    std::string arg(argv[j]);
    if( arg.find("--displayname=") == 0 )
    {
       size_t pos = arg.find("--displayname=") + 14;
       displayName_ = arg.substr(pos);
    }
    if( arg == "--displayname" )
    {
      if( j+1 < argc ) displayName_ = argv[j+1];
    }
  }

  taskID(eckit::mpi::comm("world").rank());
}

void Main::initialise(
    int argc, char** argv,
    const char* homeenv)
{
    eckit::AutoLock<eckit::Mutex> lock(local_mutex);
    if( not ready() ) {
        new Main(argc,argv,homeenv);
    }
}

void Main::finalise()
{
    // Temporary until ECKIT-166 is fixed, only included for MacOSX
#ifdef BUG_ECKIT_166
    eckit::mpi::finaliseAllComms();
#endif

    eckit::Log::flush();
}

#define SUCCESS 0
extern "C" {
  int fckit__exception_what (char* &what, int &what_size) {
    what_size = exception_what.size();
    what = new char[what_size+1];
    ::strcpy(what,exception_what.c_str());
    return SUCCESS;
  }
  
  int fckit__exception_location () {
    return bool(exception_location);
  }
  
  int fckit__exception_file (char* &file, int &file_size) {
    std::string f = exception_location ? exception_location.file() : "";
    file_size = f.size();
    file = new char[file_size+1];
    ::strcpy(file,f.c_str());
    return SUCCESS;
  }
  int fckit__exception_function (char* &function, int &function_size) {
    std::string f = exception_location ? exception_location.func() : "";
    function_size = f.size();
    function = new char[function_size+1];
    ::strcpy(function,f.c_str());
    return SUCCESS;
  }
  int fckit__exception_line () {
    return exception_location ? exception_location.line() : 0;
  }
  int fckit__exception_callstack (char* &callstack, int &callstack_size) {
    std::string f = exception_callstack;
    callstack_size = f.size();
    callstack = new char[callstack_size+1];
    ::strcpy(callstack,f.c_str());
    return SUCCESS;
  }
}


} // namespace fckit

