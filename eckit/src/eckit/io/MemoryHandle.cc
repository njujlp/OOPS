/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/io/MemoryHandle.h"
#include "eckit/io/Buffer.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

#if 0
ClassSpec MemoryHandle::classSpec_ = {&DataHandle::classSpec(),"MemoryHandle",};
Reanimator<MemoryHandle> MemoryHandle::reanimator_;
#endif


MemoryHandle::MemoryHandle(const Buffer& buffer):
	address_(const_cast<Buffer&>(buffer)),
    size_(buffer.size()),
    opened_(false),
    readOnly_(true),
    read_(false),
    position_(0)
{
}


MemoryHandle::MemoryHandle(Buffer& buffer):
    address_(buffer),
    size_(buffer.size()),
    opened_(false),
    readOnly_(false),
    read_(false),
    position_(0)
{
}

MemoryHandle::MemoryHandle(const void* address,size_t size):
	address_(const_cast<char*>(reinterpret_cast<const char*>(address))),
    size_(size),
    opened_(false),
    readOnly_(true),
    read_(false),
    position_(0)
{
}


MemoryHandle::MemoryHandle(void* address,size_t size):
    address_(reinterpret_cast<char*>(address)),
    size_(size),
    opened_(false),
    readOnly_(false),
    read_(false),
    position_(0)
{
}

MemoryHandle::~MemoryHandle()
{
}

Length MemoryHandle::openForRead()
{
    read_ = true;
    position_ = 0;
    opened_ = true;
    return size_;
}

void MemoryHandle::openForWrite(const Length& length)
{
    ASSERT(!readOnly_);
    read_ = false;
    position_ = 0;
    opened_ = true;
}

void MemoryHandle::openForAppend(const Length& )
{
	NOTIMP;
}

void MemoryHandle::skip(const Length& len)
{
    ASSERT(read_);
    seek(position() + len);
}

long MemoryHandle::read(void* buffer,long length)
{
    ASSERT(opened_);
    ASSERT(read_);

    size_t left = size_ - position_;
    size_t size = std::min(left, size_t(length));
    ::memcpy(buffer, address_ + position_, size);
    position_ += size;

	return size;
}

long MemoryHandle::write(const void* buffer,long length)
{
    ASSERT(opened_);
    ASSERT(!read_);

    size_t left = size_ - position_;
    size_t size = std::min(left, size_t(length));
    ::memcpy(address_ + position_, buffer, size);
    position_ += size;

    return size;
}

void MemoryHandle::close()
{
    opened_ = false;
}

void MemoryHandle::flush()
{

}

void MemoryHandle::rewind()
{
    ASSERT(opened_);
	position_ = 0;
}

Offset MemoryHandle::seek(const Offset& off)
{
    ASSERT(opened_);
    ASSERT(size_t(off) < size_);
    position_ = off;
    return position_;
}


void MemoryHandle::print(std::ostream& s) const
{
	s << "MemoryHandle[size=" << size_ << ']';
}

Length MemoryHandle::estimate()
{
    return size_;
}

Offset MemoryHandle::position()
{
    ASSERT(opened_);
    return position_;
}

std::string MemoryHandle::title() const {
    return "<mem>";
}

DataHandle* MemoryHandle::clone() const
{
    return new MemoryHandle(address_, size_);
}
//-----------------------------------------------------------------------------

} // namespace eckit

