#ifndef HAVE_HS_MCL_IMPL
#define HAVE_HS_MCL_IMPL

#include "HsFFI.h"
#include "misc.hpp"

namespace hs_mcl {
namespace field_impl {

template <typename FpT>
inline void hash_to(const char *s, const size_t s_length, FpT *result)
{
	mem_util::init(result);
	result->setMsg(s, s_length);
}

template <typename FpT>
inline void add(const FpT *a, const FpT *b, FpT *result)
{
	mem_util::init(result);
	FpT::add(*result, *a, *b);
}

template <typename FpT>
inline void subtract(const FpT *a, const FpT *b, FpT *result)
{
	mem_util::init(result);
	FpT::sub(*result, *a, *b);
}

template <typename FpT>
inline void multiply(const FpT *a, const FpT *b, FpT *result)
{
	mem_util::init(result);
	FpT::mul(*result, *a, *b);
}

template <typename FpT>
inline void negate(const FpT *a, FpT *result)
{
	mem_util::init(result);
	FpT::neg(*result, *a);
}

template <typename FpT>
inline void invert(const FpT *a, FpT *result)
{
	mem_util::init(result);
	FpT::inv(*result, *a);
}

template <typename FpT>
inline int eq(const FpT *a, const FpT *b)
{
	return *a == *b;
}

template <typename FpT>
inline int is_zero(const FpT *a)
{
	return a->isZero();
}

template <typename FpT>
inline int sqrt(const FpT *a, FpT *result)
{
	mem_util::init(result);
	return FpT::squareRoot(*result, *a);
}

template <typename FpT>
inline void to_gmp_integer(const FpT *a, mp_limb_t *result,
                           const size_t result_length_bytes)
{
	mcl::fp::Block bl;
	a->getBlock(bl);

	static_assert(sizeof(bl.v_[0]) == sizeof(mp_limb_t),
	              "mcl::fp::Unit and mp_limb_t have different size");

	// Check that we have the exact amount of bytes needed.
	assert(result_length_bytes == sizeof(bl.v_[0]) * bl.n);

	std::copy(bl.v_, bl.v_ + bl.n, result);
}

template <typename FpT>
inline void from_gmp_integer(const mp_limb_t *scalar,
                             const mp_size_t scalar_limbs,
                             FpT *result)
{
	mem_util::init(result);
	// Throws if passed value bigger than modulus, we divide modulo in Haskell.
	result->setArray(scalar, scalar_limbs);
}

template <typename FpT>
inline void from_hsint(const HsInt scalar, FpT *result)
{
	mem_util::copy(result, scalar);
}

template <typename FpT, typename... Args>
inline void from_base(FpT *result, Args&&... args)
{
	mem_util::init(result);
	result->set(std::forward<Args>(args)...);
}

}

namespace group_impl {

template <typename GrT>
inline void zero(GrT *result)
{
	mem_util::init(result);
	result->clear();
}

template <typename FpT, typename GrT>
inline int construct(const FpT *x, const FpT *y, GrT *result)
{
	mem_util::init(result);
	result->set(*x, *y, false);
	return result->isValid();
}

template <typename MapT, typename FpT, typename GrT>
inline void map_to(MapT map, const FpT *ca, GrT *result)
{
	FpT a = *ca;
	mem_util::init(result);
	while (true)
	{
		try
		{
			map(*result, a);
			break;
		}
		catch (cybozu::Exception &)
		{
			*a.getFp0() += FpT::BaseFp::one();
		}
	}
}

template <typename GrT>
inline void add(const GrT *p, const GrT *q, GrT *result)
{
	mem_util::init(result);
	GrT::add(*result, *p, *q);
}

template <typename GrT>
inline void invert(const GrT *p, GrT *result)
{
	mem_util::init(result);
	GrT::neg(*result, *p);
}

template <typename FrT, typename GrT>
inline void scalar_mul_native(const int const_time,
                              const FrT *scalar,
                              const GrT *p,
                              GrT *result)
{
	mem_util::init(result);
	if (const_time)
		GrT::mulCT(*result, *p, *scalar);
	else
		GrT::mul(*result, *p, *scalar);
}

template <typename GrT>
inline void scalar_mul(const int const_time,
                       const mp_limb_t *scalar,
                       const mp_size_t scalar_limbs,
                       const int is_negative,
                       const GrT *p,
                       GrT *result)
{
	mem_util::init(result);
	GrT::mulArray(*result, *p, limb_unit_compat(scalar).units,
	              scalar_limbs, is_negative, const_time);
}

template <typename FrT, typename GrT>
inline void scalar_mul_small(const int const_time,
                             const HsInt scalar,
                             const GrT *p,
                             GrT *result)
{
	mem_util::init(result);
	if (const_time)
		GrT::mulCT(*result, *p, FrT{scalar});
	else
		GrT::mul(*result, *p, FrT{scalar});
}

template <typename GrT>
inline int eq(const GrT *p, const GrT *q)
{
	return *p == *q;
}

template <typename GrT>
inline int is_zero(const GrT *p)
{
	return p->isZero();
}

template <typename GrT, typename FpT>
inline void affine_coords(const GrT *p, FpT *result_x, FpT *result_y)
{
	assert(!p->isZero());

	if (p->isNormalized())
	{
		mem_util::copy(result_x, p->x);
		mem_util::copy(result_y, p->y);
	}
	else
	{
		GrT r{*p};
		r.normalize();
		mem_util::copy(result_x, r.x);
		mem_util::copy(result_y, r.y);
	}
}

template <typename GrT, typename FpT>
inline int y_from_x(const int y_lsb, const FpT *x, FpT *result)
{
	int success = 1;
	mem_util::init(result);
	try {
		GrT::getYfromX(*result, *x, y_lsb);
	} catch (cybozu::Exception &) {
		success = 0;
	}
	return success;
}

}
}

#endif // HAVE_HS_MCL_IMPL
