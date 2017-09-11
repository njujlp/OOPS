/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/value/DateTimeContent.h"
#include "eckit/value/NumberContent.h"
#include "eckit/parser/JSON.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

ClassSpec DateTimeContent::classSpec_ = {&Content::classSpec(),"DateTimeContent",};
Reanimator<DateTimeContent> DateTimeContent::reanimator_;

DateTimeContent::DateTimeContent(const DateTime& d):
    value_(d)
{
}

DateTimeContent::DateTimeContent(Stream& s):
	Content(s)
{
	std::string dd;
	s >> dd;
    value_ = DateTime(dd);
}

void DateTimeContent::encode(Stream& s) const
{
	Content::encode(s);
    std::string dd = value_;
	s << dd;
}

DateTimeContent::~DateTimeContent()
{
}

Content* DateTimeContent::clone() const {
    return new DateTimeContent(value_);
}

void DateTimeContent::print(std::ostream& s) const
{
    s << value_;
}

void DateTimeContent::json(JSON& s) const
{
    s << std::string(value_);
}

int DateTimeContent::compare(const Content& other) const
{
	return -other.compareDateTime(*this);
}

int DateTimeContent::compareDateTime(const DateTimeContent& other) const
{
    if(value_ < other.value_)
		return -1;
    else if(value_ == other.value_)
		return 1;

	return 0;
}

void DateTimeContent::value(DateTime& d) const
{
    d = value_;
}

Content* DateTimeContent::add(const Content& other) const
{
	return other.addDateTime(*this);
}

Content* DateTimeContent::sub(const Content& other) const
{
	return other.subDateTime(*this);
}

Content* DateTimeContent::mul(const Content& other) const
{
    return other.mulDateTime(*this);
}

Content* DateTimeContent::div(const Content& other) const
{
    return other.divDateTime(*this);
}

Content* DateTimeContent::mod(const Content& other) const
{
    return other.modDateTime(*this);
}

//-----------------------------------------------------------------------------

} // namespace eckit
