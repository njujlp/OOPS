/*
 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/log/Seconds.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

Seconds::Seconds(double seconds):
	seconds_(seconds)
{
}

Seconds::Seconds(const ::timeval& time):
	seconds_(time.tv_sec + time.tv_usec / 1000000.0)
{
}

static struct  {
    int length_;
    const char *name_;
} periods[] = {
    {7 * 24 * 60 * 60, "week",},    
    {24 * 60 * 60, "day",}, 
    {60 * 60, "hour",}, 
    {60, "minute",},    
    {1, "second",}, 
    {0,0,},
};

std::ostream& operator<<(std::ostream& s,const Seconds&  sec)
{
	double t = sec.seconds_;
    long n  = t;
    int flg = 0;

    for(int i=0;periods[i].length_;i++)
    {
        long m = n / periods[i].length_;
        if(m) {
            if(flg) s << ' ';
            s << m << ' ' << periods[i].name_;
            if(m>1) s << 's';
            n %= periods[i].length_;
            flg++;
        }
    }   

    if(!flg) s << t << " second";

	return s;
}

Seconds::operator std::string() const
{
    std::ostringstream s;
    s << *this;
    return s.str();
}

//-----------------------------------------------------------------------------

} // namespace eckit

