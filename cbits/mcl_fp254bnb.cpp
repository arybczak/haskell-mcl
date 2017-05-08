#include <mcl/bn256.hpp>
#include "impl.hpp"

using namespace hs_mcl;
using namespace mcl::bn256;

namespace {

void initialize_parameters()
{
	// C++11 guarantees static variable initializers to be run at most once and in
	// a thread-safe manner.
	static void *initializer = [] {
		bn256init(mcl::bn::CurveFp254BNb);
		return nullptr;
	}();
	static_cast<void>(initializer);
}

}

extern "C" {

////////////////////////////////////////////////////////////
// Fr

int hs_mcl_fp254bnb_fr_size()
{
	return sizeof(Fr);
}

int hs_mcl_fp254bnb_fr_limbs()
{
	initialize_parameters();

	return Fr::getByteSize() / sizeof(mp_limb_t);
}

void hs_mcl_fp254bnb_fr_modulus(mp_limb_t *result,
                                const size_t result_length_bytes)
{
	initialize_parameters();

	mpz_class_to_gmp_integer(BN::param.r, result, result_length_bytes);
}

void hs_mcl_fp254bnb_fr_hash_to(const char *s, const size_t s_length, Fr *result)
{
	initialize_parameters();

	return field_impl::hash_to(s, s_length, result);
}

void hs_mcl_fp254bnb_fr_add(const Fr *a, const Fr *b, Fr *result)
{
	return field_impl::add(a, b, result);
}

void hs_mcl_fp254bnb_fr_subtract(const Fr *a, const Fr *b, Fr *result)
{
	return field_impl::subtract(a, b, result);
}

void hs_mcl_fp254bnb_fr_multiply(const Fr *a, const Fr *b, Fr *result)
{
	return field_impl::multiply(a, b, result);
}

void hs_mcl_fp254bnb_fr_negate(const Fr *a, Fr *result)
{
	return field_impl::negate(a, result);
}

void hs_mcl_fp254bnb_fr_from_integer(const mp_limb_t *scalar,
                                     const mp_size_t scalar_limbs,
                                     Fr *result)
{
	initialize_parameters();

	return field_impl::from_gmp_integer(scalar, scalar_limbs, result);
}

void hs_mcl_fp254bnb_fr_from_hsint(const HsInt scalar, Fr *result)
{
	initialize_parameters();

	return field_impl::from_hsint(scalar, result);
}

void hs_mcl_fp254bnb_fr_invert(const Fr *a, Fr *result)
{
	return field_impl::invert(a, result);
}

int hs_mcl_fp254bnb_fr_eq(const Fr *a, const Fr *b)
{
	return field_impl::eq(a, b);
}

void hs_mcl_fp254bnb_fr_to_gmp_integer(const Fr *a, mp_limb_t *result,
                                       const int result_length_bytes)
{
	return field_impl::to_gmp_integer(a, result, result_length_bytes);
}

int hs_mcl_fp254bnb_fr_is_zero(const Fr *a)
{
	return field_impl::is_zero(a);
}

int hs_mcl_fp254bnb_fr_sqrt(const Fr *a, Fr *result)
{
	return field_impl::sqrt(a, result);
}

////////////////////////////////////////////////////////////
// Fp

int hs_mcl_fp254bnb_fp_size()
{
	return sizeof(Fp);
}

int hs_mcl_fp254bnb_fp_limbs()
{
	initialize_parameters();

	return Fp::getByteSize() / sizeof(mp_limb_t);
}

void hs_mcl_fp254bnb_fp_modulus(mp_limb_t *result,
                                const size_t result_length_bytes)
{
	initialize_parameters();

	mpz_class_to_gmp_integer(BN::param.p, result, result_length_bytes);
}

void hs_mcl_fp254bnb_fp_hash_to(const char *s, const size_t s_length, Fp *result)
{
	initialize_parameters();

	return field_impl::hash_to(s, s_length, result);
}

void hs_mcl_fp254bnb_fp_add(const Fp *a, const Fp *b, Fp *result)
{
	return field_impl::add(a, b, result);
}

void hs_mcl_fp254bnb_fp_subtract(const Fp *a, const Fp *b, Fp *result)
{
	return field_impl::subtract(a, b, result);
}

void hs_mcl_fp254bnb_fp_multiply(const Fp *a, const Fp *b, Fp *result)
{
	return field_impl::multiply(a, b, result);
}

void hs_mcl_fp254bnb_fp_negate(const Fp *a, Fp *result)
{
	return field_impl::negate(a, result);
}

void hs_mcl_fp254bnb_fp_from_integer(const mp_limb_t *scalar,
                                     const mp_size_t scalar_limbs,
                                     Fp *result)
{
	initialize_parameters();

	return field_impl::from_gmp_integer(scalar, scalar_limbs, result);
}

void hs_mcl_fp254bnb_fp_from_hsint(const HsInt scalar, Fp *result)
{
	initialize_parameters();

	return field_impl::from_hsint(scalar, result);
}

void hs_mcl_fp254bnb_fp_invert(const Fp *a, Fp *result)
{
	return field_impl::invert(a, result);
}

int hs_mcl_fp254bnb_fp_eq(const Fp *a, const Fp *b)
{
	return field_impl::eq(a, b);
}

void hs_mcl_fp254bnb_fp_to_gmp_integer(const Fp *a, mp_limb_t *result,
                                       const int result_length_bytes)
{
	return field_impl::to_gmp_integer(a, result, result_length_bytes);
}

int hs_mcl_fp254bnb_fp_is_zero(const Fp *a)
{
	return field_impl::is_zero(a);
}

int hs_mcl_fp254bnb_fp_sqrt(const Fp *a, Fp *result)
{
	return field_impl::sqrt(a, result);
}

////////////////////////////////////////////////////////////
// Fp2

int hs_mcl_fp254bnb_fp2_size()
{
	return sizeof(Fp2);
}

void hs_mcl_fp254bnb_fp2_add(const Fp2 *a, const Fp2 *b, Fp2 *result)
{
	return field_impl::add(a, b, result);
}

void hs_mcl_fp254bnb_fp2_subtract(const Fp2 *a, const Fp2 *b, Fp2 *result)
{
	return field_impl::subtract(a, b, result);
}

void hs_mcl_fp254bnb_fp2_multiply(const Fp2 *a, const Fp2 *b, Fp2 *result)
{
	return field_impl::multiply(a, b, result);
}

void hs_mcl_fp254bnb_fp2_negate(const Fp2 *a, Fp2 *result)
{
	return field_impl::negate(a, result);
}

void hs_mcl_fp254bnb_fp2_from_base(const Fp *c0, const Fp *c1, Fp2 *result)
{
	return field_impl::from_base(result, *c0, *c1);
}

void hs_mcl_fp254bnb_fp2_invert(const Fp2 *a, Fp2 *result)
{
	return field_impl::invert(a, result);
}

int hs_mcl_fp254bnb_fp2_eq(const Fp2 *a, const Fp2 *b)
{
	return field_impl::eq(a, b);
}

void hs_mcl_fp254bnb_fp2_c0(const Fp2 *a, Fp *result)
{
	mem_util::copy(result, a->a);
}

void hs_mcl_fp254bnb_fp2_c1(const Fp2 *a, Fp *result)
{
	mem_util::copy(result, a->b);
}

int hs_mcl_fp254bnb_fp2_is_zero(const Fp2 *a)
{
	return field_impl::is_zero(a);
}

int hs_mcl_fp254bnb_fp2_sqrt(const Fp2 *a, Fp2 *result)
{
	return field_impl::sqrt(a, result);
}

////////////////////////////////////////////////////////////
// Fp12

int hs_mcl_fp254bnb_fp12_size()
{
	return sizeof(Fp12);
}

void hs_mcl_fp254bnb_fp12_add(const Fp12 *a, const Fp12 *b, Fp12 *result)
{
	return field_impl::add(a, b, result);
}

void hs_mcl_fp254bnb_fp12_subtract(const Fp12 *a, const Fp12 *b, Fp12 *result)
{
	return field_impl::subtract(a, b, result);
}

void hs_mcl_fp254bnb_fp12_multiply(const Fp12 *a, const Fp12 *b, Fp12 *result)
{
	return field_impl::multiply(a, b, result);
}

void hs_mcl_fp254bnb_fp12_negate(const Fp12 *a, Fp12 *result)
{
	return field_impl::negate(a, result);
}

void hs_mcl_fp254bnb_fp12_from_base(const Fp2 *c0, const Fp2 *c1, const Fp2 *c2,
                                    const Fp2 *c3, const Fp2 *c4, const Fp2 *c5,
                                    Fp12 *result)
{
	return field_impl::from_base(result, *c0, *c1, *c2, *c3, *c4, *c5);
}

void hs_mcl_fp254bnb_fp12_invert(const Fp12 *a, Fp12 *result)
{
	return field_impl::invert(a, result);
}

int hs_mcl_fp254bnb_fp12_eq(const Fp12 *a, const Fp12 *b)
{
	return field_impl::eq(a, b);
}

void hs_mcl_fp254bnb_fp12_c0(const Fp12 *a, Fp2 *result)
{
	mem_util::copy(result, a->a.a);
}

void hs_mcl_fp254bnb_fp12_c1(const Fp12 *a, Fp2 *result)
{
	mem_util::copy(result, a->a.b);
}

void hs_mcl_fp254bnb_fp12_c2(const Fp12 *a, Fp2 *result)
{
	mem_util::copy(result, a->a.c);
}

void hs_mcl_fp254bnb_fp12_c3(const Fp12 *a, Fp2 *result)
{
	mem_util::copy(result, a->b.a);
}

void hs_mcl_fp254bnb_fp12_c4(const Fp12 *a, Fp2 *result)
{
	mem_util::copy(result, a->b.b);
}

void hs_mcl_fp254bnb_fp12_c5(const Fp12 *a, Fp2 *result)
{
	mem_util::copy(result, a->b.c);
}

int hs_mcl_fp254bnb_fp12_is_zero(const Fp12 *a)
{
	return field_impl::is_zero(a);
}

////////////////////////////////////////////////////////////
// G1

int hs_mcl_fp254bnb_g1_size()
{
	return sizeof(G1);
}

void hs_mcl_fp254bnb_g1_zero(G1 *result)
{
	initialize_parameters();

	return group_impl::zero(result);
}

int hs_mcl_fp254bnb_g1_construct(const Fp *x, const Fp *y, G1 *result)
{
	return group_impl::construct(x, y, result);
}

void hs_mcl_fp254bnb_g1_map_to(const Fp *a, G1 *result)
{
	return group_impl::map_to(BN::mapToG1, a, result);
}

void hs_mcl_fp254bnb_g1_add(const G1 *p, const G1 *q, G1 *result)
{
	return group_impl::add(p, q, result);
}

void hs_mcl_fp254bnb_g1_invert(const G1 *p, G1 *result)
{
	return group_impl::invert(p, result);
}

void hs_mcl_fp254bnb_g1_scalar_mul_native(const int const_time,
                                          const Fr *scalar,
                                          const G1 *p,
                                          G1 *result)
{
	return group_impl::scalar_mul_native(const_time, scalar, p, result);
}

void hs_mcl_fp254bnb_g1_scalar_mul(const int const_time,
                                   const mp_limb_t *scalar,
                                   const mp_size_t scalar_limbs,
                                   const int is_negative,
                                   const G1 *p,
                                   G1 *result)
{
	return group_impl::scalar_mul(const_time, scalar, scalar_limbs,
	                              is_negative, p, result);
}

void hs_mcl_fp254bnb_g1_scalar_mul_small(const int const_time,
                                         const HsInt scalar,
                                         const G1 *p,
                                         G1 *result)
{
	return group_impl::scalar_mul_small<Fr>(const_time, scalar, p, result);
}

int hs_mcl_fp254bnb_g1_eq(const G1 *p, const G1 *q)
{
	return group_impl::eq(p, q);
}

int hs_mcl_fp254bnb_g1_is_zero(const G1 *p)
{
	return group_impl::is_zero(p);
}

void hs_mcl_fp254bnb_g1_affine_coords(const G1 *p, Fp *result_x, Fp *result_y)
{
	return group_impl::affine_coords(p, result_x, result_y);
}

int hs_mcl_fp254bnb_g1_y_from_x(const int y_lsb, const Fp *x, Fp *result)
{
	return group_impl::y_from_x<G1>(y_lsb, x, result);
}

////////////////////////////////////////////////////////////
// G2

int hs_mcl_fp254bnb_g2_size()
{
	return sizeof(G2);
}

void hs_mcl_fp254bnb_g2_zero(G2 *result)
{
	initialize_parameters();

	return group_impl::zero(result);
}

int hs_mcl_fp254bnb_g2_construct(const Fp2 *x, const Fp2 *y, G2 *result)
{
	return group_impl::construct(x, y, result);
}

void hs_mcl_fp254bnb_g2_map_to(const Fp2 *a, G2 *result)
{
	return group_impl::map_to(BN::mapToG2, a, result);
}

void hs_mcl_fp254bnb_g2_add(const G2 *p, const G2 *q, G2 *result)
{
	return group_impl::add(p, q, result);
}

void hs_mcl_fp254bnb_g2_invert(const G2 *p, G2 *result)
{
	return group_impl::invert(p, result);
}

void hs_mcl_fp254bnb_g2_scalar_mul_native(const int const_time,
                                          const Fr *scalar,
                                          const G2 *p,
                                          G2 *result)
{
	return group_impl::scalar_mul_native(const_time, scalar, p, result);
}

void hs_mcl_fp254bnb_g2_scalar_mul(const int const_time,
                                   const mp_limb_t *scalar,
                                   const mp_size_t scalar_limbs,
                                   const int is_negative,
                                   const G2 *p,
                                   G2 *result)
{
	return group_impl::scalar_mul(const_time, scalar, scalar_limbs,
	                              is_negative, p, result);
}

void hs_mcl_fp254bnb_g2_scalar_mul_small(const int const_time,
                                         const HsInt scalar,
                                         const G2 *p,
                                         G2 *result)
{
	return group_impl::scalar_mul_small<Fr>(const_time, scalar, p, result);
}

int hs_mcl_fp254bnb_g2_eq(const G2 *p, const G2 *q)
{
	return group_impl::eq(p, q);
}

int hs_mcl_fp254bnb_g2_is_zero(const G2 *p)
{
	return group_impl::is_zero(p);
}

void hs_mcl_fp254bnb_g2_affine_coords(const G2 *p, Fp2 *result_x, Fp2 *result_y)
{
	return group_impl::affine_coords(p, result_x, result_y);
}

int hs_mcl_fp254bnb_g2_y_from_x(const int y_lsb, const Fp2 *x, Fp2 *result)
{
	return group_impl::y_from_x<G2>(y_lsb, x, result);
}

////////////////////////////////////////////////////////////
// GT

void hs_mcl_fp254bnb_gt_pow_native(const int const_time, const Fp12 *a,
                                   const Fr *b, Fp12 *result)
{
	return field_impl::pow_native(const_time, a, b, result);
}

////////////////////////////////////////////////////////////
// Pairing

void hs_mcl_fp254bnb_pairing(const G1 *p, const G2 *q, Fp12 *result)
{
	mem_util::init(result);
	BN::pairing(*result, *p, *q);
}

}
