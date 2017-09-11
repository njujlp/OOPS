/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef eckit_utils_SHA1_H
#define eckit_utils_SHA1_H

#include "eckit/eckit_config.h"

#ifdef ECKIT_HAVE_SSL
#include <openssl/sha.h>
#else
#error "eckit was not configured with OpenSSL, SHA1 is disabled. Use conditional ECKIT_HAVE_SSL from eckit/eckit_config.h"
#endif

#ifndef SHA_DIGEST_LENGTH
#define SHA_DIGEST_LENGTH 20
#endif

#include "eckit/utils/Hash.h"

namespace eckit {

class SHA1 : public Hash {

public:  // types

  SHA1();

  explicit SHA1(const char*);
  explicit SHA1(const std::string&);

  SHA1(const void* data, size_t len);

  virtual ~SHA1();

  virtual void reset() const;

  virtual digest_t compute(const void*, long);

  virtual void update(const void*, long);

  virtual digest_t digest() const;

  template<class T>
  SHA1& operator<<(const T& x) { add(x); return *this; }

private: // members

  mutable SHA_CTX ctx_;

};

}  // end namespace eckit

#endif
