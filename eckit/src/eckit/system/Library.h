/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   August 2016

#ifndef eckit_system_Library_H
#define eckit_system_Library_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/memory/ScopedPtr.h"
#include "eckit/thread/Mutex.h"
#include "eckit/config/Configuration.h"

namespace eckit {

class Channel;

namespace system {

//----------------------------------------------------------------------------------------------------------------------

class Library : private eckit::NonCopyable {

public: // methods


    Library(const std::string& name);

    virtual ~Library();

    const std::string& name() const;

    virtual std::string prefixDirectory() const;

    virtual std::string libraryHome() const;

    virtual void libraryHome(const std::string&);

    virtual std::string expandPath(const std::string& path) const;

    std::string libraryPath() const;

    virtual std::string version() const = 0;

    virtual std::string gitsha1(unsigned int count = 40) const = 0;

    virtual bool debug() const { return debug_; }

    virtual Channel& debugChannel() const;

    virtual const Configuration& configuration() const;

public: // class methods

    static std::vector<std::string> list();
    static void list(std::ostream&);

    static bool exists(const std::string& name);
    static const Library& lookup(const std::string& name);

    void lock()   { mutex_.lock(); }
    void unlock() { mutex_.unlock(); }

protected: // methods

    virtual std::string home() const;

    virtual const void* addr() const = 0;

    void print(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& s, const Library& p) { p.print(s); return s; }

private: // methods

    std::string location() const;

private: // members

    std::string name_;
    std::string prefix_;
    std::string home_;    // if not set explicitly, will be empty

    bool debug_;

    mutable eckit::Mutex mutex_;

    mutable std::string libraryPath_;
    mutable std::string prefixDirectory_;

    mutable eckit::ScopedPtr<eckit::Channel> debugChannel_;

    mutable eckit::ScopedPtr<eckit::Configuration> configuration_;

};

//----------------------------------------------------------------------------------------------------------------------

template <class T>
struct LibraryRegistration {
    LibraryRegistration() {
        T::instance();
    }
};

#define REGISTER_LIBRARY(X)                             \
static eckit::system::LibraryRegistration<X> libregist; \
void force_link_library_register_##X(void*) {           \
    force_link_library_register_##X(&libregist);        \
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace system
} // namespace eckit

#endif

