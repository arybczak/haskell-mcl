#ifndef HAVE_HS_MCL_MISC
#define HAVE_HS_MCL_MISC

#include <mcl/fp.hpp>

namespace hs_mcl {

// On x86_32 mp_limb_t is a typedef for unsigned long int, whereas mcl::fp::Unit
// is typedef for unsigned int. These, even though of the same size, are
// distinct types, hence compiler complains when we pass a pointer to the former
// when a pointer to the latter is expected. Below is used to safely bridge that
// gap.
union limb_unit_compat
{
	limb_unit_compat(const mp_limb_t *limbs_)
		: limbs(limbs_)
	{ }

	const mp_limb_t *limbs;
	const mcl::fp::Unit *units;

	static_assert(sizeof(*limbs) == sizeof(*units),
	              "mp_limb_t and mcl::fp::Unit have different size");
};

// In order to avoid using foreign pointers to manage memory for MCL numeric
// classes inside Haskell we take advantage of the fact that all of them are
// trivially destructible. As there is no need to run the destructor, we can
// allocate memory for them inside Haskell and construct them at C++ level
// within that memory block using placement new operator.
//
// This class statically guarantees that objects we construct and copy are
// trivially destructible classes. If at any point in time some of them cease to
// be so (which is highly doubtful, but nevertheless), it can be easily adjusted
// to use ordinary operator new (unfortunately then we can't escape ForeignPtr
// management in Haskell).
struct mem_util
{
	template <typename TargetT>
	inline static void init(TargetT *target)
	{
		static_assert(std::is_class<TargetT>::value,
		              "Target is not a class");

		mem_util_impl<TargetT, TargetT,
		              std::is_trivially_destructible<TargetT>::value
		              >::init(target);
	}

	template <typename TargetT, typename SourceT>
	inline static void copy(TargetT *target, SourceT &&source)
	{
		static_assert(std::is_class<TargetT>::value,
		              "Target is not a class");

		mem_util_impl<TargetT, SourceT,
		              std::is_trivially_destructible<TargetT>::value
		              >::copy(target,
		                      std::forward<SourceT>(source));
	}

private:
	template <typename TargetT, typename SourceT,
	          bool is_trivially_destructible>
	struct mem_util_impl
	{
		static_assert(is_trivially_destructible,
		              "Target is not trivially destructible");
	};

	template <typename TargetT, typename SourceT>
	struct mem_util_impl<TargetT, SourceT, true>
	{
		inline static void init(TargetT *target)
		{
			new (target) TargetT{};
		}

		inline static void copy(TargetT *target, SourceT &&source)
		{
			new (target) TargetT{std::forward<SourceT>(source)};
		}
	};

};

////////////////////////////////////////////////////////////

inline void mpz_class_to_gmp_integer(const mpz_class &z, mp_limb_t *result,
                                     const size_t result_length_bytes)
{
	mpz_srcptr mp = z.get_mpz_t();
	size_t n = mpz_size(mp);

	assert(n * sizeof(mp->_mp_d[0]) == result_length_bytes);

	std::copy(mp->_mp_d, mp->_mp_d + n, result);
}

}

#endif // HAVE_HS_MCL_MISC
