#include <iostream>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <stack>
#include <list>
#include <bitset>
#include <utility>
#include <type_traits>
#include <typeinfo>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cmath>
#include <climits>
#include <cfloat>
#include <cstdarg>
#include <cstddef>
#include <cwchar>

#include "StreamToActionTranslator.h"
#include "rand_gen.h"

auto GetPrintDurationsString(const auto dur) -> std::string
{
	using namespace std::chrono_literals;
	using namespace std::chrono;
	std::stringstream ss;
	ss << "Nanoseconds: " << duration_cast<nanoseconds>(dur) << '\n'
		<< "Microseconds: " << duration_cast<microseconds>(dur) << '\n'
		<< "Milliseconds: " << duration_cast<milliseconds>(dur) << '\n'
		<< "Seconds: " << duration_cast<seconds>(dur) << '\n';
	return ss.str();
}

auto GetPrintBehaviorsForKey(const std::string keyName) -> sds::KeyStateBehaviors
{
	using std::cout;
	sds::KeyStateBehaviors behaviors
	{
		.OnDown = [=]() { cout << "[" << keyName << "]-[OnDown]\n"; },
		.OnUp = [=]() { cout << "[" << keyName << "]-[OnUp]\n"; },
		.OnRepeat = [=]() { cout << "[" << keyName << "]-[OnRepeat]\n"; },
		.OnReset = [=]() { cout << "[" << keyName << "]-[OnReset]\n"; }
	};
	return behaviors;
}

// Mappings buffer for the test driver, with a single group.
auto GetTestDriverMappings(size_t count = 100) -> std::vector<sds::MappingContainer>
{
	using namespace sds;
	using std::cout, std::println;
	using namespace std::chrono_literals;
	constexpr int Grouping{ 101 };
	
	std::vector<int> maps(count);
	std::ranges::iota(maps, 1);
	const auto mappings = maps | std::views::transform([=](const int i)
	{
		return MappingContainer
		{
			ButtonDescription{i, {}, Grouping},
			KeyStateBehaviors{GetPrintBehaviorsForKey(std::to_string(i))}
		};
	});

	return std::ranges::to<std::vector<MappingContainer>>(mappings);
}

// Range of mapping IDs, from the mappings buffer.
auto GetMappingIdRange(const std::vector<sds::MappingContainer>& mappings) -> std::vector<int>
{
	auto idRange = mappings | std::views::transform([](const sds::MappingContainer& m) { return m.Button.ButtonVirtualKeycode; });
	return std::ranges::to<std::vector<int>>(idRange);
}

// Test data sets for our test, built from the range of mapping IDs.
auto GetInputSequence(const std::vector<sds::MappingContainer>& mappings, size_t count)
{
	const auto mappingIdsView = GetMappingIdRange(mappings);

	RandomGen rander;
	std::vector<std::vector<int>> dataSet;
	dataSet.reserve(count);

	for (int i{}; i < count; ++i)
	{
		assert(mappingIdsView.size() > 10);
		const auto rangeBegin = rander.BuildRandomSingleValue(1, (int)mappingIdsView.size() / 2);
		auto mappingIds = std::vector(mappingIdsView.cbegin(), mappingIdsView.cbegin() + rangeBegin);
		std::ranges::shuffle(mappingIds, rander.randomElementGenerator);
		dataSet.emplace_back(std::move(mappingIds));
	}
	return dataSet;
}

auto GetBuiltTranslator(std::vector<sds::MappingContainer>&& mappings) -> sds::Translator
{
	if (mappings.empty())
	{
		std::cerr << "Test mappings buffer was created empty!";
		std::exit(EXIT_FAILURE);
	}
	sds::Translator translator{ std::move(mappings) };
	return translator;
}

// Timed run
int main()
{
	using namespace std::chrono_literals;
	using namespace std::chrono;
	constexpr int DataSetSize = 15;

	auto mappings = GetTestDriverMappings();
	const auto dataSet = GetInputSequence(mappings, DataSetSize);
	auto translator = GetBuiltTranslator(std::move(mappings));

	// Begin clock start
	const auto startTime = steady_clock::now();

	// Run test data set
	for (const auto& data : dataSet)
	{
		translator(data)();
		std::cout << "-------------------------\n";
	}

	// Compute times
	const auto totalTime = steady_clock::now() - startTime;
	const auto printed = GetPrintDurationsString(totalTime);
	std::cout << std::vformat("[Total Duration]\n{}\n", std::make_format_args(printed));
	const auto averageMessage = GetPrintDurationsString(totalTime / DataSetSize);
	std::cout << std::vformat("[Time Per Iteration]\n{}\n", std::make_format_args(averageMessage));
	return 0;
}