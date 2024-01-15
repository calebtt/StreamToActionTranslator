#include <iostream>
#include <vector>
#include <ranges>
#include <chrono>
#include <source_location>
#include <cassert>

#include "rand_gen.h"
#include "StreamToActionTranslator.h"

const int buttonA = 1;
const int buttonB = 2;
const int buttonX = 3;
const int buttonY = 4;

void pretty_log(const std::string_view message,
	const std::source_location location =
	std::source_location::current())
{
	std::cout << "file: "
		<< location.file_name() << '('
		<< location.line() << ':'
		<< location.column() << ") `"
		<< location.function_name() << "`: "
		<< message << '\n';
}

void AssertEqual(const auto& lhs, const auto& rhs, const std::source_location location = std::source_location::current())
{
	if (lhs != rhs)
	{
		std::stringstream msg;  
		msg << "Assertion of {" << lhs << "} == {" << rhs << "} is false. Terminating.";
		pretty_log(msg.str(), location);
		std::exit(1);
	}
}

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
		//.OnDown = [=]() { cout << "[" << keyName << "]-[OnDown]\n"; },
		//.OnUp = [=]() { cout << "[" << keyName << "]-[OnUp]\n"; },
		//.OnRepeat = [=]() { cout << "[" << keyName << "]-[OnRepeat]\n"; },
		//.OnReset = [=]() { cout << "[" << keyName << "]-[OnReset]\n"; }
	};
	return behaviors;
}

// Mappings buffer for the test driver, with a single group.
auto GetTestDriverMappings(size_t count = 100, const int beginId = 1) -> std::vector<sds::MappingContainer>
{
	using namespace sds;
	using std::cout, std::println;
	using namespace std::chrono_literals;
	constexpr int Grouping{ 101 };

	std::vector<int> maps(count);
	std::ranges::iota(maps, beginId);
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

	for (size_t i{}; i < count; ++i)
	{
		assert(mappingIdsView.size() > 10);
		//const auto rangeBegin = rander.BuildRandomSingleValue(1, (int)mappingIdsView.size() / 2);
		//auto mappingIds = std::vector(mappingIdsView.cbegin(), mappingIdsView.cbegin() + rangeBegin);
		auto mappingIds = std::vector(mappingIdsView.cbegin(), mappingIdsView.cend());
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

auto GetBuiltTranslator() -> sds::Translator
{
	sds::Translator translator{ GetTestDriverMappings() };
	return translator;
}

auto GetBuiltFilter(const auto& translator)
{
	return sds::OvertakingFilter{ translator };
}

auto getMappingWithId(int id, int group)
{
	using namespace std::chrono_literals;
	return 	sds::MappingContainer
	{
		sds::ButtonDescription{id, {}, group},
		sds::KeyStateBehaviors{},
		0s,
		0s
	};
}