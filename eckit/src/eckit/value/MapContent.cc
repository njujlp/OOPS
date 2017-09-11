/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/value/MapContent.h"
#include "eckit/parser/JSON.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

ClassSpec MapContent::classSpec_ = {&Content::classSpec(),"MapContent",};
Reanimator<MapContent>  MapContent::reanimator_;


MapContent::MapContent()
{
}

MapContent::MapContent(const ValueMap& v)
{
    value_ = v;
}


MapContent::MapContent(Stream& s):
    Content(s)
{
    bool more;
    s >> more;
    while(more)
    {
        Value k(s);
        Value v(s);
        value_[k] = v;
        s >> more;
    }

}

void MapContent::encode(Stream& s) const
{
    Content::encode(s);
    for(ValueMap::const_iterator j = value_.begin(); j != value_.end(); ++j)
    {
        s << true;
        s << (*j).first;
        s << (*j).second;
    }
    s << false;
}

MapContent::~MapContent()
{
}

Value& MapContent::element(const Value& key)
{
    return value_[key];
}

bool MapContent::contains(const Value& key) const
{
    return value_.find(key) != value_.end();
}

void MapContent::value(ValueMap& v) const
{
    v = value_;
}

int MapContent::compare(const Content& other)const
{
    return -other.compareMap(*this);
}

int MapContent::compareMap(const MapContent& other) const
{
    if(value_ == other.value_)
        return 0;
    if(value_ < other.value_)
        return -1;
    return 1;

}

Value MapContent::keys() const {
    ValueList list;
    for(ValueMap::const_iterator j = value_.begin(); j != value_.end(); ++j) {
        list.push_back((*j).first);
    }
    return Value::makeList(list);
}

void MapContent::print(std::ostream& s) const
{
    s << '{';
    for(ValueMap::const_iterator j = value_.begin(); j != value_.end(); ++j)
    {
        if(j != value_.begin()) s << " , ";
        s << (*j).first;
        s << " => ";
        s << (*j).second;
    }
    s << '}';
}

void MapContent::json(JSON& s) const
{
    s.startObject();
    for(ValueMap::const_iterator j = value_.begin(); j != value_.end(); ++j)
    {
        s << (*j).first;
        s << (*j).second;
    }
    s.endObject();
}


Content* MapContent::clone() const {
    ValueMap v;
    for(ValueMap::const_iterator j = value_.begin(); j != value_.end(); ++j) {
        v[(*j).first.clone()] = (*j).second.clone();
    }
    return new MapContent(v);
}


Content* MapContent::add(const Content& other) const
{
    return other.addMap(*this);
}

Content* MapContent::sub(const Content& other) const
{
    return other.subMap(*this);
}

Content* MapContent::mul(const Content& other) const
{
    return other.mulMap(*this);
}

Content* MapContent::div(const Content& other) const
{
    return other.divMap(*this);
}

Content* MapContent::mod(const Content& other) const
{
    return other.modMap(*this);
}

//-----------------------------------------------------------------------------

} // namespace eckit

