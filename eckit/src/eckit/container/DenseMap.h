/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef eckit_DenseMap_h
#define eckit_DenseMap_h

/// @author Tiago Quintino

#include <vector>
#include <algorithm>
#include <utility>

#include "eckit/exception/Exceptions.h"

//-----------------------------------------------------------------------------

namespace eckit {

//-----------------------------------------------------------------------------

template < typename K, typename V >
class DenseMap {
private: // types
  typedef size_t index_t;   ///< index type

public: // types

	typedef K key_type;     ///< key type
	typedef V value_type;   ///< value type

	class kidx_t {
	public:

		/// \brief Key that is used for lookup
		key_type key() const { return key_; };

		/// \brief Index in values storage
		index_t idx() const { return idx_; };
		friend class DenseMap;

	private:
		kidx_t(key_type k, index_t i) : idx_(i), key_(k)  {}
		index_t idx_;
		key_type key_;
	};

private: // types

	typedef std::vector< kidx_t >      key_store_t;
	typedef std::vector< value_type >  value_store_t;

public: // methods

	typedef typename key_store_t::iterator iterator;
	typedef typename key_store_t::const_iterator const_iterator;

	DenseMap( size_t s = 0 ) : sorted_(true)
	{
		if(s > 0) reserve(s);
	}

	~DenseMap() {}

	void reserve( size_t s )
	{
		keys_.reserve(s);
		values_.reserve(s);
	}

	void insert( const K& k, const V& v )
	{
		keys_.push_back( kidx_t(k,values_.size()) );
		values_.push_back(v);
		sorted_ = false;
	}

    /// @TODO shoudl we implement this?
    //	size_t erase( const key_type& k );

	void replace( const K& k, const V& v )
	{
		iterator it = find(k);
		if( it != end() )
		{
			values_[ it->idx() ] = v;
		}
		else
		{
			insert(k,v);
		}
	}

	void clear()
	{
		keys_.clear();
		values_.clear();
		sorted_ = true;
	}

	bool sorted() const { return sorted_; }

	size_t size() const { return keys_.size(); }
	bool empty() const { return keys_.size() == 0; }

	void sort()
	{
		if(!sorted_)
		{
			std::sort( begin(), end(), LessThan() );
			sorted_ = true;
		}
	}

	iterator begin() { return keys_.begin(); }
	const_iterator cbegin() const { return keys_.begin(); }

	iterator end() { return keys_.end(); }
	const_iterator cend() const { return keys_.end(); }

	bool has( const K& k ) const { return find(k) != cend(); }

	const V& get( iterator it ) const { return values_[ it->idx() ]; }
	V& get( iterator it ) { return values_[ it->idx() ]; }

	const V& get( const_iterator it ) const { return values_[ it->idx() ]; }
	V& get( const_iterator it ) { return values_[ it->idx() ]; }

	const V& get( const K& k ) const { return values_[ find(k)->idx() ]; }
	V& get( const K& k ) { return values_[ find(k)->idx() ]; }

	const V& at( const size_t i ) const { ASSERT(i < keys_.size()); return values_[ keys_[i].idx() ]; }
	V& at( const size_t i ) { ASSERT(i < keys_.size()); return values_[ i ]; }

	const V& operator[] (const K& k ) const { return values_[ find(k)->idx() ]; }
	V& operator[] (const K& k ) { return values_[ find(k)->idx() ]; }

	const V& operator[] (const size_t& i ) const { ASSERT(i < values_.size()); return values_[ i ]; }
	V& operator[] (const size_t& i ) { ASSERT(i < keys_.size()); return values_[ i ]; }

	iterator find( const K& k )
	{
		if( !empty() )
		{
			ASSERT(sorted());
			iterator it = std::lower_bound( begin(), end(), k, Compare());
			if( it != end() && it->key() == k )
				return it;
		}
		return end();
	}

	const_iterator find( const K& k ) const
	{
		if( !empty() )
		{
			ASSERT(sorted());
			const_iterator it = std::lower_bound( cbegin(), cend(), k, Compare());
			if( it != cend() && it->key() == k )
				return it;
		}
		return cend();
	}

	void print(std::ostream& s) const
	{
		const_iterator it = cbegin();
		for( ; it != cend(); ++it )
			s << it->key() << " " << values_[ it->idx() ] << std::endl;
	}

	friend std::ostream& operator<<(std::ostream& s, const DenseMap& m) { m.print(s);  return s; }

private: // types

	class LessThan {
	public:
		bool operator() (const kidx_t& e1, const kidx_t& e2) const
		{
			return (e1.key() < e2.key()) ? true : false;
		}
	};

	class Compare {
	public:
		bool operator() (const kidx_t& e, const K& k) const
		{
			return (e.key() < k) ? true : false;
		}
		bool operator() (const K& k, const kidx_t& e) const
		{
			return (e.key() > k) ? true : false;
		}
	};

private: // members

	key_store_t   keys_;   ///< storage of the keys
	value_store_t values_; ///< storage of the values

	bool sorted_;

};

//-----------------------------------------------------------------------------

} // namespace eckit

#endif
