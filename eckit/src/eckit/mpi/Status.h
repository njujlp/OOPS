/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef eckit_mpi_Status_h
#define eckit_mpi_Status_h

#include <iosfwd>

#include "eckit/memory/Counted.h"

namespace eckit {
namespace mpi {

//----------------------------------------------------------------------------------------------------------------------

class StatusContent : public Counted {
public:

    virtual ~StatusContent();

    virtual int source() const = 0;
    virtual int tag() const = 0;
    virtual int error() const = 0;

    virtual void print(std::ostream&) const = 0;

};

//----------------------------------------------------------------------------------------------------------------------

class NullStatus : public StatusContent {
public:

    virtual ~NullStatus() {}

    virtual int source() const { return -1; };
    virtual int tag() const { return -1; };
    virtual int error() const { return 1; };

    virtual void print(std::ostream&) const;

};

//----------------------------------------------------------------------------------------------------------------------


/// Status by construction has always a valid content_
/// @invariant content_ is not null

class Status {

public: // methods

    /// Null Status constructor
    Status();

    /// @pre content is not null
    Status(StatusContent* content);

    ~Status();

    Status(const Status&);

    Status& operator=(const Status&);

    int source() const { return content_->source(); }
    int tag() const    { return content_->tag(); }
    int error() const  { return content_->error(); }

    template <class T>
    T& as() {
        return dynamic_cast<T&>(*content_);
    }

private: // methods

    void print(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& s, const Status& o) {
        o.print(s); return s;
    }

private: // members

    StatusContent* content_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace mpi
}  // namespace eckit

#endif
