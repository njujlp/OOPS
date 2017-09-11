/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/value/DateContent.h"
#include "eckit/value/TimeContent.h"
#include "eckit/value/DateTimeContent.h"
#include "eckit/value/ListContent.h"
#include "eckit/value/NilContent.h"
#include "eckit/value/NumberContent.h"
#include "eckit/value/StringContent.h"
#include "eckit/value/BoolContent.h"
#include "eckit/value/DoubleContent.h"
#include "eckit/value/MapContent.h"
#include "eckit/value/Value.h"
#include "eckit/io/Length.h"
#include "eckit/filesystem/PathName.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

Value::Value():
	content_(new NilContent())
{
	content_->attach();
}

Value::Value(int l):
    content_(new NumberContent(l))
{
    content_->attach();
}

Value::Value(long long l):
	content_(new NumberContent(l))
{
	content_->attach();
}

Value::Value(unsigned long long l):
    content_(new NumberContent(l))
{
    content_->attach();
}

Value::Value(unsigned long l):
    content_(new NumberContent(l))
{
    content_->attach();
}


Value::Value(unsigned int l):
    content_(new NumberContent(l))
{
    content_->attach();
}

Value::Value(long l):
    content_(new NumberContent(l))
{
    content_->attach();
}


Value::Value(bool l):
    content_(new BoolContent(l))
{
    content_->attach();
}

Value::Value(double l):
    content_(new DoubleContent(l))
{
    content_->attach();
}

Value::Value(const std::string& s):
	content_(new StringContent(s))
{
	content_->attach();
}

Value::Value(const char* s):
	content_(new StringContent(s))
{
	content_->attach();
}

Value::Value(const Length& l):
	content_(new NumberContent(l))
{
	content_->attach();
}

Value::Value(const PathName& p):
	content_(new StringContent(p.asString()))
{
	content_->attach();
}

Value::Value(const Date& d):
	content_(new DateContent(d))
{
	content_->attach();
}

Value::Value(const Time& d):
    content_(new TimeContent(d))
{
    content_->attach();
}

Value::Value(const DateTime& d):
    content_(new DateTimeContent(d))
{
    content_->attach();
}

Value::Value(Stream& s):
    content_(Reanimator<Content>::reanimate(s))
{
    ASSERT(content_);
    content_->attach();
}

Value::~Value()
{
	content_->detach();
}

Value::Value(const Value& other):
	content_(other.content_)
{
	content_->attach();
}

Value Value::clone() const {
    return Value(content_->clone());
}

bool Value::shared() const {
    return content_->count() > 1;
}

Value& Value::operator=(const Value& other)
{
	other.content_->attach();
	content_->detach();
	content_ = other.content_;

	return *this;
}

Value Value::operator+(const Value& v) const
{
	return Value(content_->add(*(v.content_)));
}

Value& Value::operator+=(const Value& v)
{
	*this = *this + v;
	return *this;
}

Value Value::operator-(const Value& v) const
{
	return Value(content_->sub(*(v.content_)));
}

Value& Value::operator-=(const Value& v)
{
	*this = *this - v;
	return *this;
}

Value Value::operator*(const Value& v) const
{
	return Value(content_->mul(*(v.content_)));
}

Value& Value::operator*=(const Value& v)
{
	*this = *this * v;
	return *this;
}

Value Value::operator/(const Value& v) const
{
	return Value(content_->div(*(v.content_)));
}

Value& Value::operator/=(const Value& v)
{
	*this = *this / v;
	return *this;
}

Value Value::operator%(const Value& v) const
{
    return Value(content_->mod(*(v.content_)));
}

Value& Value::operator%=(const Value& v)
{
    *this = *this % v;
    return *this;
}

Value Value::makeList()
{
	return Value(new ListContent());
}

Value Value::makeMap()
{
    return Value(new MapContent());
}

Value Value::makeMap(const ValueMap & m)
{
    return Value(new MapContent(m));
}

Value Value::makeList(const Value& v)
{
	return Value(new ListContent(v));
}

Value Value::makeList(const ValueList& v)
{
    return Value(new ListContent(v));
}

Value::Value(const ValueList& v):
	content_(new ListContent(v))
{
	content_->attach();
}

Value::Value(const ValueMap& m):
	content_(new MapContent(m))
{
	content_->attach();
}

Value::Value(Content* c):
	content_(c)
{
	content_->attach();
}

Value Value::head() const
{
	ValueList v;
	content_->value(v);

//	std::cout << __FUNCTION__ << " " << *this << " " << v.size() << std::endl;

	return v.size() > 0 ? v[0] : Value();
}

Value Value::tail() const
{
	ValueList v;
	content_->value(v);

//	std::cout << __FUNCTION__ << " " << *this << " " << v.size() << std::endl;

	if (v.size() > 1)
	{
		v.erase(v.begin());
		return v;
	}
	else
		return Value();
}

Value::operator ValueList() const
{
	ValueList v;
	content_->value(v);
	return v;
}

Value::operator ValueMap() const
{
	ValueMap v;
	content_->value(v);
	return v;
}

//=========================================================
const Value& Value::operator[](const Value& key) const
{
    return content_->element(key);
}

const Value& Value::operator[](const char* key) const
{
    return content_->element(Value(std::string(key)));
}

const Value& Value::operator[](const std::string& key) const
{
    return content_->element(Value(key));
}

//=========================================================

Value& Value::operator[](const Value& key)
{
    return content_->element(key);
}

Value& Value::operator[](const char* key)
{
    return content_->element(Value(std::string(key)));
}

Value& Value::operator[](const std::string& key)
{
    return content_->element(Value(key));
}

Value& Value::operator[](int key)
{
    return content_->element(Value(key));
}
//=========================================================

Value& Value::element(const std::string& key) {
    return content_->element(Value(key));
}

const Value& Value::operator[](int key) const
{
    return content_->element(Value(key));
}

bool Value::contains(const Value& key) const
{
    return content_->contains(key);
}

bool Value::contains(const char* key) const
{
    return content_->contains(Value(std::string(key)));
}

bool Value::contains(const std::string& key) const
{
    return content_->contains(Value(key));
}

bool Value::contains(int key) const
{
    return content_->contains(Value(key));
}

Value Value::operator-() const
{
    return content_->negate();
}


Value Value::keys() const
{
    return content_->keys();
}

size_t Value::size() const
{
    return content_->size();
}
//-----------------------------------------------------------------------------

} // namespace eckit

