/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Fraction.h
// Baudouin Raoult - ECMWF Mar 16

#ifndef eckit_Fraction_h
#define eckit_Fraction_h

#include <string>
#include "eckit/exception/Exceptions.h"
#include <limits>
#include "eckit/types/FloatCompare.h"


//-----------------------------------------------------------------------------

namespace eckit {

class MD5;
class Stream;

//-----------------------------------------------------------------------------


class Fraction {
public:
    typedef long long value_type;

    // typedef __int128 value_type;

public: // methods


// -- Contructors

    Fraction(): top_(0), bottom_(1) {}

    // template<class T>
    // explicit Fraction(T top): top_(top), bottom_(1) {}

    Fraction(value_type top, value_type bottom);

    explicit Fraction(double n);
    explicit Fraction(int n): top_(n), bottom_(1) {}
    explicit Fraction(short n): top_(n), bottom_(1) {}
    explicit Fraction(long n): top_(n), bottom_(1) {}
    explicit Fraction(long long n): top_(n), bottom_(1) {}

    explicit Fraction(unsigned int n): top_(n), bottom_(1) {}
    explicit Fraction(unsigned short n): top_(n), bottom_(1) {}
    explicit Fraction(unsigned long n): top_(n), bottom_(1) {}
    explicit Fraction(unsigned long long n): top_(n), bottom_(1) {}

    // Fraction(const Fraction& other):
    //     top_(other.top_), bottom_(other.bottom_) {}

    explicit Fraction(const std::string&);
    explicit Fraction(const char*);

    bool integer() const {
        return bottom_ == 1;
    }

public: // operators

    operator double() const {
        return double(top_) / double(bottom_);
    }

    operator value_type() const;

    Fraction operator-()  const {
        return Fraction(-top_, bottom_);
    }

    value_type integralPart() const {
        return top_ / bottom_;
    }

    Fraction decimalPart() const {
        return *this - integralPart();
    }

    Fraction operator+(const Fraction& other) const;

    Fraction operator-(const Fraction& other) const;

    Fraction operator/(const Fraction& other) const;

    Fraction operator*(const Fraction& other) const;

    bool operator==(const Fraction& other) const;

    bool operator<(const Fraction& other) const;

    bool operator<=(const Fraction& other) const;

    bool operator!=(const Fraction& other) const;

    bool operator>(const Fraction& other) const;

    bool operator>=(const Fraction& other) const;

    Fraction& operator+=(const Fraction& other) {
        *this = (*this) + other;
        return *this;
    }

    Fraction& operator-=(const Fraction& other) {
        *this = (*this) - other;
        return *this;
    }

    Fraction& operator/=(const Fraction& other) {
        *this = (*this) / other;
        return *this;
    }

    Fraction& operator*=(const Fraction& other) {
        *this = (*this) * other;
        return *this;
    }

    template<class T>
    Fraction operator+(T other) const {
        return *this + Fraction(other);
    }

    template<class T>
    Fraction operator-(T other) const {
        return *this - Fraction(other);
    }

    template<class T>
    Fraction operator/(T other) const {
        return *this / Fraction(other);
    }

    template<class T>
    Fraction operator*(T other) const {
        return *this * Fraction(other);
    }

    //====================================

    template<class T>
    bool operator==(T other) const {
        return *this == Fraction(other);
    }

    template<class T>
    bool operator<(T other) const {
        return *this < Fraction(other);
    }

    template<class T>
    bool operator<=(T other) const {
        return *this <= Fraction(other);
    }

    template<class T>
    bool operator!=(T other) const {
        return *this != Fraction(other);
    }

    template<class T>
    bool operator>(T other) const {
        return *this > Fraction(other);
    }

    template<class T>
    bool operator>=(T other) const {
        return *this >= Fraction(other);
    }

    template<class T>
    Fraction& operator+=(T other) {
        return (*this) += Fraction(other);
    }

    template<class T>
    Fraction& operator-=(T other) {
        return (*this) -= Fraction(other);
    }

    template<class T>
    Fraction& operator/=(T other) {
        return (*this) /= Fraction(other);
    }

    template<class T>
    Fraction& operator*=(T other) {
        return (*this) *= Fraction(other);
    }

    void hash(eckit::MD5&) const;

private: // members

    value_type top_;
    value_type bottom_;

    void print(std::ostream& out) const;
    void encode(Stream& out) const;
    void decode(Stream& out);


    friend std::ostream& operator<<(std::ostream& s, const Fraction& x) {
        x.print(s);
        return s;
    }

    friend Stream& operator<<(Stream& s, const Fraction& x) {
        x.encode(s);
        return s;
    }

    friend Stream& operator>>(Stream& s, Fraction& x) {
        x.decode(s);
        return s;
    }

};

template<class T>
Fraction operator+(T n, const Fraction& f)
{
    // static_assert(std::is_same<T, long long>::value,"some meaningful error message");
    return Fraction(n) + f;
}

template<class T>
Fraction operator-(T n, const Fraction& f)
{
    return Fraction(n) - f;
}

template<class T>
Fraction operator/(T n, const Fraction& f)
{
    return Fraction(n) / f;
}

template<class T>
Fraction operator*(T n, const Fraction& f)
{
    return Fraction(n) * f;
}

template<class T>
bool operator==(T n, const Fraction& f)
{
    return Fraction(n) == f;
}

template<class T>
bool operator<(T n, const Fraction& f)
{
    return Fraction(n) < f;
}

template<class T>
bool operator<=(T n, const Fraction& f)
{
    return Fraction(n) <= f;
}

template<class T>
bool operator!=(T n, const Fraction& f)
{
    return Fraction(n) != f;
}

template<class T>
bool operator>(T n, const Fraction& f)
{
    return Fraction(n) > f;
}

template<class T>
bool operator>=(T n, const Fraction& f)
{
    return Fraction(n) >= f;
}

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
