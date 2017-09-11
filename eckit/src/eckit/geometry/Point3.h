#ifndef eckit_Point3_h
#define eckit_Point3_h

#include "eckit/geometry/KPoint.h"

//-----------------------------------------------------------------------------

namespace eckit {
namespace geometry {

//------------------------------------------------------------------------------------------------------

void lonlat_to_3d( const double lonlat[], double xyz[], const double r, const double h );

void lonlat_to_3d( const double lonlat[], double xyz[] );

void lonlat_to_3d( const double lon, const double lat, double xyz[], const double r, const double h );

void lonlat_to_3d( const double lon, const double lat, double xyz[] );

void latlon_to_3d( const double lat, const double lon, double xyz[], const double r, const double h );

void latlon_to_3d( const double lat, const double lon, double xyz[] );

//-----------------------------------------------------------------------------

class Point3 : public eckit::geometry::KPoint<3> {

    typedef KPoint<3> BasePoint;

public:

    Point3(): BasePoint() {}

    Point3( const BasePoint& p ): BasePoint(p) {}

    Point3( double lat, double lon ): KPoint<3>()
    {
        eckit::geometry::latlon_to_3d( lat, lon, x_ );
    }

    Point3( double x, double y, double z ): KPoint<3>( NoInit() )
    {
        x_[XX] = x;
        x_[YY] = y;
        x_[ZZ] = z;
    }

    double  operator[] (const size_t& i) const { assert(i<3); return x_[i]; }
    double& operator[] (const size_t& i)       { assert(i<3); return x_[i]; }

    template < typename T >
    void assign( const T& p )
    {
        x_[XX] = p[XX];
        x_[YY] = p[YY];
        x_[ZZ] = p[ZZ];
    }

    void print(std::ostream& s) const;

    friend std::ostream& operator<<(std::ostream& s,const Point3& p);

};

//------------------------------------------------------------------------------------------------------

class LLPoint3 : public KPoint<3> {

    double lat_;
    double lon_;

public:

    LLPoint3(): KPoint<3>(), lat_(0), lon_(0) {}

    double lat() const { return lat_; }
    double lon() const { return lon_; }

    LLPoint3(double lat, double lon):
        KPoint<3>(),
        lat_(lat), lon_(lon)
    {
        latlon_to_3d(lat,lon,this->data());
    }

    void print(std::ostream& s) const;

    friend std::ostream& operator<<(std::ostream& s,const LLPoint3& p);

};

//------------------------------------------------------------------------------------------------------

bool points_equal( const Point3& a, const Point3& b );

//---------------------------------------------------------------------------------------------------------

} // namespace geometry
} // namespace eckit

#endif

