/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File NumberContent.h
// Manuel Fuentes - ECMWF Jun 97

#ifndef eckit_NumberContent_h
#define eckit_NumberContent_h

#include "eckit/value/Content.h"
#include "eckit/value/Value.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

class NumberContent : public Content {

protected:

// -- Constructor

	NumberContent(long long);
	NumberContent(Stream&);

// -- Destructor

	virtual ~NumberContent();

// -- Overridden methods

	// -- From Content

    virtual int compare(const Content& other) const;

    virtual void value(bool& n)        const;
    virtual void value(long long& n)   const;
    virtual void value(double& n)      const;
    virtual void value(std::string& n) const;
    virtual void value(Date& n)        const { Content::value(n); }
    virtual void value(Time& n)        const { Content::value(n); }
    virtual void value(DateTime& n)    const { Content::value(n); }
    virtual void value(ValueList& n)   const { Content::value(n); }
    virtual void value(ValueMap& n)    const { Content::value(n); }

    virtual int  compareBool(const BoolContent&)            const {return -1; }
    virtual int  compareNumber(const NumberContent&)        const;
    virtual int  compareDouble(const DoubleContent&)        const;
    virtual int  compareString(const StringContent&)        const {return 1; }
    virtual int  compareNil(const NilContent&)              const {return 1; }
    virtual int  compareList(const ListContent&)            const {return 1; }
    virtual int  compareMap(const MapContent&)              const {return 1; }
    virtual int  compareDate(const DateContent&)            const {return 1; }
    virtual int  compareTime(const TimeContent&)            const {return 1; }
    virtual int  compareDateTime(const DateTimeContent&)    const {return 1; }

    virtual Content* add(const Content&) const;
	virtual Content* sub(const Content&) const;
	virtual Content* mul(const Content&) const;
	virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

    virtual Content* addNumber(const NumberContent&) const;
    virtual Content* subNumber(const NumberContent&) const;
    virtual Content* mulNumber(const NumberContent&) const;
    virtual Content* divNumber(const NumberContent&) const;

    virtual Value negate() const;

//    virtual Content* addDouble(const DoubleContent&) const;
//    virtual Content* subDouble(const DoubleContent&) const;
//    virtual Content* mulDouble(const DoubleContent&) const;
//    virtual Content* divDouble(const DoubleContent&) const;

    virtual void    print(std::ostream&) const;
    virtual void   json(JSON&)     const;
	virtual std::string  typeName()      const { return "Number"; }
	virtual bool    isNumber()      const { return true; }
    virtual Content* clone() const;

    // -- From Streamable

    virtual void encode(Stream&) const;
    virtual const ReanimatorBase& reanimator() const { return reanimator_; }

// -- Class methods

    static  const ClassSpec&  classSpec()            { return classSpec_;}

private:

	NumberContent(const NumberContent&);
	NumberContent& operator=(const NumberContent&);

// -- Members

    long long value_;

// -- Class Members

	static  ClassSpec                  classSpec_;
    static  Reanimator<NumberContent>  reanimator_;

// -- Friends

	friend class Reanimator<NumberContent>;
	friend class Value;
    friend class DateContent;
    friend class DoubleContent;
};


//-----------------------------------------------------------------------------

} // namespace eckit

#endif
