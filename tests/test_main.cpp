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

#include "test_utils.h"
#include "rand_gen.h"
#include "StreamToActionTranslator.h"


auto RunTestLoop(auto& translator, auto& filter, auto dataSet, const size_t DataSetSize)
{
	using namespace std::chrono_literals;
	using namespace std::chrono;

	// Begin clock start
	const auto startTime = steady_clock::now();

	// Run test data set
	for (auto data : dataSet)
	{
		translator(filter(data))();
	}

	// Compute times
	const auto totalTime = steady_clock::now() - startTime;
	const auto averageMessage = GetPrintDurationsString(totalTime / DataSetSize);
	return std::vformat("[Time Per Iteration]\n{}\n", std::make_format_args(averageMessage));
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
	auto filter = GetBuiltFilter(translator);

	std::vector<std::string> timeStrings;
	timeStrings.reserve(DataSetSize);
	for(size_t i{}; i < DataSetSize; ++i)
	{
		timeStrings.push_back(RunTestLoop(translator, filter, dataSet, DataSetSize));
	}
	
	for(const auto timeMessage : timeStrings)
	{
		std::cout << timeMessage << '\n';
	}
	return 0;
}