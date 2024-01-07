#pragma once
#include <random>
#include <ranges>
#include <vector>
#include <string>
#include <concepts>
#include <limits>
#include <type_traits>
#include <algorithm>

/// <summary>
/// Implementation for RandomGen.
///	A better random generation utility, as a class object,
///	with min and max distribution value range and templated for string type.
///	(using declaration for standard config).
/// </summary>
/// <typeparam name="StrType"> By default, a single byte element string that uses the implementation defined signed-ness of the type 'char' </typeparam>
/// <typeparam name="WStrType">By default, a wide character or otherwise unicode/larger character type that uses the implementation defined signed-ness of the type 'wchar_t'</typeparam>
template<class StrType = std::string, class WStrType = std::wstring>
struct RandomGenImpl
{
	using CountType = size_t;
	using StringType = StrType;
	using WStringType = WStrType;
public:
	std::random_device rd;
	std::mt19937 randomElementGenerator{ rd() }; // seed mersenne engine
private:
	/// <typeparam name="T">Typename of values you want in the container.</typeparam>
	///	<typeparam name="X">Distribution template param to use, not less than sizeof an int</typeparam>
	/// <summary>Helper function used to fill a container</summary>
	///	<param name="containerType">some range capable container type</param>
	///	<param name="minLength">the minimum count of the type T in the filled range.</param>
	///	<param name="maxLength">the maximum count of the type T in the filled range.</param>
	///	<param name="minValue">minimum integer value used in the distribution.</param>
	///	<param name="maxValue">maximum integer value used in the distribution.</param>
	///	<exception cref="bad_alloc">Throws <c>std::bad_alloc</c> on failure to resize container type. </exception>
	template <typename T, typename X>
		requires std::integral<T>&& std::integral<X>
	void DoGenerate(std::ranges::range auto& containerType,
		const CountType minLength,
		const CountType maxLength,
		const T minValue = std::numeric_limits<T>::min(),
		const T maxValue = std::numeric_limits<T>::max())
	{
		std::uniform_int_distribution<X> distElementPossibility(minValue, maxValue);
		std::uniform_int_distribution distLengthPossibility(minLength, maxLength);
		//the distribution uses the generator engine to get the value
		const auto tLength = static_cast<std::size_t>(distLengthPossibility(randomElementGenerator));
		containerType.resize(tLength); // <-- can fail to allocate the memory.
		const auto GenLambda = [&]() constexpr { return static_cast<T>(distElementPossibility(randomElementGenerator)); };
		std::ranges::generate(containerType, GenLambda);
	}

	/// <typeparam name="T">Typename of value you want in the return value.</typeparam>
	///	<typeparam name="X">Distribution template param to use, not less than sizeof an int</typeparam>
	/// <summary>Helper function used to get a range-bound value</summary>
	///	<param name="minValue">the minimum value of the type T in the returned value.</param>
	///	<param name="maxValue">the maximum value of the type T in the returned value.</param>
	///	<returns>T with random value</returns>
	template <typename T, typename X> requires std::integral<T>&& std::integral<X>
	[[nodiscard]] T DoGenerateSingle(const T minValue, const T maxValue) noexcept
	{
		std::uniform_int_distribution<X> distElementPossibility(minValue, maxValue);
		//the distribution uses the generator engine to get the value
		return static_cast<T>(distElementPossibility(randomElementGenerator));
	}
public:
	/// <summary>Returns a vector of a random number of type <c>T</c> with randomized content using a uniform distribution. T must be default constructable.</summary>
	///	<param name="maxLength">the maximum count of the type T in the returned vector.</param>
	///	<param name="minLength">the minimum count of the type T in the returned vector.</param>
	/// <returns> a vector of type T with randomized content. Empty vector on error. </returns>
	template<typename T>
		requires std::integral<T> && (!std::same_as<T, bool>)
	[[nodiscard]] auto BuildRandomVector(const CountType minLength, const CountType maxLength) -> std::vector<T>
	{
		//arg error checking, returns empty vector as per description
		if (minLength > maxLength || (maxLength <= 0) || (minLength <= 0))
		{
			return std::vector<T>();
		}
		std::vector<T> currentBuiltVector;
		if constexpr (sizeof(T) <= sizeof(int) && std::unsigned_integral<T>)
		{
			DoGenerate<T, unsigned int>(currentBuiltVector, minLength, maxLength);
		}
		else if constexpr (sizeof(T) <= sizeof(int) && std::signed_integral<T>)
		{
			DoGenerate<T, int>(currentBuiltVector, minLength, maxLength);
		}
		else
		{
			DoGenerate<T, T>(currentBuiltVector, minLength, maxLength);
		}
		return currentBuiltVector;
	}

	/// <summary>Fills a container with type <c>T</c> storing randomized values using a uniform distribution. Primitive type T must be default constructable.</summary>
	///	<param name="containerType">some range capable container type</param>
	///	<param name="minLength">the minimum count of the type T in the returned vector.</param>
	///	<param name="maxLength">the maximum count of the type T in the returned vector.</param>
	/// <returns> true on success, false on error.</returns>
	template<typename T> requires std::integral<T> && (!std::same_as<T, bool>)
	[[nodiscard]] bool FillContainerRandom(std::ranges::range auto& containerType, const CountType minLength, const CountType maxLength)
	{
		//arg error checking, returns false as per description
		if (minLength > maxLength || (maxLength <= 0) || (minLength <= 0))
		{
			return false;
		}
		if constexpr (sizeof(T) <= sizeof(int) && std::unsigned_integral<T>)
		{
			DoGenerate<T, unsigned int>(containerType, minLength, maxLength);
		}
		else if constexpr (sizeof(T) <= sizeof(int) && std::signed_integral<T>)
		{
			DoGenerate<T, int>(containerType, minLength, maxLength);
		}
		else
		{
			DoGenerate<T, T>(containerType, minLength, maxLength);
		}
		return true;
	}

	/// <summary>Returns a vector of <c>StringType</c> with randomized content using a uniform distribution.<para>NOTE:
	///	The char type is a character representation type that efficiently encodes members of the basic execution character set.
	///	The C++ compiler treats variables of type char, signed char, and unsigned char as having different types.
	///	<para>***Microsoft - specific: Variables of type char are promoted to int as if from type signed char by default,
	///	unless the / J compilation option is used. In this case, they're treated as type unsigned char and are promoted to int without sign extension.***</para>
	///	</para><para><a href="https://docs.microsoft.com/en-us/cpp/cpp/fundamental-types-cpp/">MSDOCS</a></para></summary>
	///	<param name="numberOfStrings">the number of strings in the returned vector.</param>
	///	<param name="minLength">the minimum length of the strings in the returned vector.</param>
	///	<param name="maxLength">the maximum length of the strings in the returned vector.</param>
	/// <returns> a <c>std::vector</c> of <c>StringType</c> with randomized content. <b>Empty vector on error.</b> </returns>
	[[nodiscard]] auto BuildRandomStringVector(const CountType numberOfStrings, const CountType minLength, const CountType maxLength) -> std::vector<StringType>
	{
		//arg error checking, returns empty vector as per description
		if (minLength > maxLength || (maxLength <= 0) || (numberOfStrings <= 0) || (minLength <= 0))
		{
			return {};
		}
		std::vector<StringType> ret{};
		ret.reserve(numberOfStrings);
		for (CountType i = 0; i < numberOfStrings; i++)
		{
			const auto tempCharVector = RandomGenImpl::BuildRandomVector<StringType::value_type>(minLength, maxLength);
			ret.emplace_back(StringType{ tempCharVector.begin(), tempCharVector.end() });
		}
		return ret;
	}

	/// <summary>Returns a vector of <c>WStringType</c> with randomized content using a uniform distribution.</summary>
	///	<param name="numberOfStrings">the number of strings in the returned vector.</param>
	///	<param name="minLength">the minimum length of the strings in the returned vector.</param>
	///	<param name="maxLength">the maximum length of the strings in the returned vector.</param>
	/// <returns> a vector of <c>WStringType</c> with randomized content. Empty vector on error. </returns>
	[[nodiscard]] auto BuildRandomWStringVector(const CountType numberOfStrings, const CountType minLength, const CountType maxLength)
	{
		//arg error checking, returns empty vector as per description
		if (minLength > maxLength || (maxLength <= 0) || (numberOfStrings <= 0) || (minLength <= 0))
		{
			return std::vector<WStringType>();
		}
		std::vector<WStringType> ret;
		ret.reserve(numberOfStrings);
		for (CountType i = 0; i < numberOfStrings; i++)
		{
			const auto tempString = RandomGenImpl::BuildRandomVector<WStringType::value_type>(minLength, maxLength);
			ret.emplace_back(WStringType{ tempString.begin(), tempString.end() });
		}
		return ret;
	}

	/// <summary>Returns a vector of a random number of type <c>T</c> with randomized content using a uniform distribution. T must be default constructable.</summary>
	///	<param name="minValue">the minimum value of the type T in the returned value.</param>
	///	<param name="maxValue">the maximum value of the type T in the returned value.</param>
	/// <returns> A type T with random value. Default constructed T on error. </returns>
	template<typename T> requires std::integral<T> && (!std::same_as<T, bool>)
	[[nodiscard]] T BuildRandomSingleValue(const T minValue = std::numeric_limits<T>::min(), const T maxValue = std::numeric_limits<T>::max()) noexcept
	{
		//arg error checking, returns default constructed T as per description
		if (minValue > maxValue)
		{
			return T{};
		}
		if constexpr (sizeof(T) <= sizeof(int) && std::unsigned_integral<T>)
		{
			return DoGenerateSingle<T, unsigned int>(minValue, maxValue);
		}
		else if constexpr (sizeof(T) <= sizeof(int) && std::signed_integral<T>)
		{
			return DoGenerateSingle<T, int>(minValue, maxValue);
		}
		else
		{
			return DoGenerateSingle<T, T>(minValue, maxValue);
		}
	}
};

/// <summary>
///	Using declaration for standard config.
///	</summary>
using RandomGen = RandomGenImpl<>;