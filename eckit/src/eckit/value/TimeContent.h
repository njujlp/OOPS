/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File TimeContent.h
// Manuel Fuentes - ECMWF Jun 97

#ifndef eckit_TimeContent_h
#define eckit_TimeContent_h

#include "eckit/value/Content.h"
#include "eckit/value/Value.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class TimeContent : public Content {

protected:

// -- Constructor

	TimeContent(const Time&);
	TimeContent(Stream&);

// -- Destructor

	virtual ~TimeContent();

// -- Overridden methods

	// -- From Content

    virtual int compare(const Content& other) const;

    virtual void value(bool& n)        const { Content::value(n); }
    virtual void value(long long& n)   const { Content::value(n); }
    virtual void value(double& n)      const { Content::value(n); }
    virtual void value(std::string& n) const { Content::value(n); }
    virtual void value(Date& n)        const { Content::value(n); }
    virtual void value(Time& n)        const;
    virtual void value(DateTime& n)    const { Content::value(n); }
    virtual void value(ValueList& n)   const { Content::value(n); }
    virtual void value(ValueMap& n)    const { Content::value(n); }

    virtual int  compareBool(const BoolContent&)            const {return -1; }
    virtual int  compareNumber(const NumberContent&)        const {return -1; }
    virtual int  compareDouble(const DoubleContent&)        const {return -1; }
    virtual int  compareString(const StringContent&)        const {return -1; }
    virtual int  compareNil(const NilContent&)              const {return -1; }
    virtual int  compareList(const ListContent&)            const {return -1; }
    virtual int  compareMap(const MapContent&)              const {return -1; }
    virtual int  compareDate(const DateContent&)            const {return -1; }
    virtual int  compareTime(const TimeContent&)            const;
    virtual int  compareDateTime(const DateTimeContent&)    const {return 1; }

	virtual Content* add(const Content&) const;
	virtual Content* sub(const Content&) const;
	virtual Content* mul(const Content&) const;
	virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

	virtual void    print(std::ostream&) const;
    virtual void   json(JSON&)     const;
	virtual std::string  typeName() const       { return "Time"; }
	virtual bool    isTime() const         { return true; }
    virtual Content* clone() const;

    // -- From Streamable

    virtual void encode(Stream&) const;
    virtual const ReanimatorBase& reanimator() const { return reanimator_; }

// -- Class methods

    static  const ClassSpec&  classSpec()            { return classSpec_;}

private:

	TimeContent(const TimeContent&);
	TimeContent& operator=(const TimeContent&);

// -- Members

    Time value_;

// -- Class Members

	static  ClassSpec                classSpec_;
    static  Reanimator<TimeContent>  reanimator_;

// -- Friends

	friend class Reanimator<TimeContent>;
	friend class Value;
};


//-----------------------------------------------------------------------------

} // namespace eckit

#endif
