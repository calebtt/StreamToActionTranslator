#include <iostream>
#include <cassert>
#include "test_utils.h"
#include "StreamToActionTranslator.h"

struct FilterFixture
{
	std::vector<sds::MappingContainer> Mappings{ GetTestDriverMappings() };

	auto GetMappingsRange() const -> const std::vector<sds::MappingContainer>&
	{
		return Mappings;
	}

	auto GetUpdatedState(std::vector<int32_t> stateUpdate) -> sds::TranslationPack
	{
		return sds::TranslationPack{};
	}
};

int TestFreeFuncs()
{
	const auto mappings = GetTestDriverMappings(10, 1);
	const auto indexA = sds::GetMappingIndexForVk(1, mappings);
	const auto indexB = sds::GetMappingIndexForVk(2, mappings);
	const bool isZeroGood = indexA == 0;
	const bool isOneGood = indexB == 1;
	return isZeroGood && isOneGood ? 0 : 1;
}

int TestFilter()
{
	using namespace std::chrono_literals;
	using namespace std::chrono;

	FilterFixture fixt;
	sds::OvertakingFilter filt{ fixt };

	// Begin clock start
	const auto startTime = steady_clock::now();

	// A and B are in the same ex. group, it should filter it so only ButtonA will be sent a down.
	auto filteredState = filt.GetFilteredButtonState({ buttonA, buttonB });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonA, filteredState.front());

	// X and B are in the same ex. group, it should filter it so only ButtonX will be sent a down.
	filteredState = filt.GetFilteredButtonState({ buttonX, buttonB });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonX, filteredState.front());

	// now we will remove ButtonX and see that ButtonB has replaced it and needs a key-down.
	filteredState = filt.GetFilteredButtonState({ buttonB });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonB, filteredState.front());

	// for this case, buttonB is activated, buttonX overtakes it, and buttonY is just a duplicate (with a matching group) that gets filtered.
	filteredState = filt.GetFilteredButtonState({ buttonB, buttonX, buttonY });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonX, filteredState.front());

	// Same as last state, different ordering, and this time it will process the next overtaking.
	filteredState = filt.GetFilteredButtonState({ buttonB, buttonX, buttonY });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonY, filteredState.front());
	// Post: ButtonY activated, X and B overtaken.

	filteredState = filt.GetFilteredButtonState({ buttonX, buttonY, buttonB });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonY, filteredState.front());

	filteredState = filt.GetFilteredButtonState({ buttonB, buttonX, buttonY, buttonA });
	AssertEqual(1ull, filteredState.size());
	AssertEqual(buttonA, filteredState.front());


	const auto totalTime = steady_clock::now() - startTime;
	std::cout << std::vformat("Total time: {}\n", std::make_format_args(duration_cast<microseconds>(totalTime))).c_str();
	return 0;
}

// Here we build a large queue of activated/overtaken keys and then key-up them all at once.
int TestLargeQueueToAllUp()
{
	using namespace std::chrono_literals;
	using namespace std::chrono;

	auto translator = GetBuiltTranslator({ 
		getMappingWithId(buttonA, 101), 
		getMappingWithId(buttonB, 101), 
		getMappingWithId(buttonX, 101), 
		getMappingWithId(buttonY, 101) });
	auto filter = GetBuiltFilter(translator);

	// A and B are in the same ex. group, it should filter it so only buttonA will be sent a down.
	std::cout << "A and B are in the same ex. group, it should filter it so that only ButtonA will be sent a down.\n";
	auto translationPack = translator(filter({ buttonA, buttonB }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	// Post: B overtaken, A down.

	std::cout << "A and B again, it should filter it so that only ButtonB will be sent a down after A goes up.\n";
	translationPack = translator(filter({ buttonA, buttonB }));
	translationPack();
	AssertEqual(translationPack.UpRequests.size(), 1);
	AssertEqual(translationPack.DownRequests.size(), 1);
	// Post: B overtook A, so B is next-state and A is overtaken (key-up)

	// X and B are in the same ex. group, it should filter it so only ButtonX will be sent a down.
	std::cout << "X and B are in the same ex. group, it should filter it so that only ButtonX will be sent a down after B goes up.\n";
	translationPack = translator(filter({ buttonX, buttonB }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	AssertEqual(translationPack.UpRequests.size(), 1);

	std::cout << "X, B, Y, A are in the same ex. group, it should filter it so that only ButtonY will be sent a down after X goes up.\n";
	translationPack = translator(filter({ buttonX, buttonB, buttonY, buttonA }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	AssertEqual(translationPack.UpRequests.size(), 1);

	std::cout << "X, B, Y, A are in the same ex. group, it should filter it so that only ButtonA will be sent a down after Y goes up.\n";
	translationPack = translator(filter({ buttonX, buttonB, buttonY, buttonA }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	AssertEqual(translationPack.UpRequests.size(), 1);

	return 0;
}

int TestFilterWithTranslator()
{
	using namespace std::chrono_literals;
	using namespace std::chrono;

	auto translator = GetBuiltTranslator({
		getMappingWithId(buttonA, 101),
		getMappingWithId(buttonB, 101),
		getMappingWithId(buttonX, 101),
		getMappingWithId(buttonY, 101) });
	auto filter = GetBuiltFilter(translator);

	std::cout << "A and B are in the same ex. group, it should filter it so that only ButtonA will be sent a down.\n";
	auto translationPack = translator(filter({ buttonA, buttonB }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);

	std::cout << "B overtakes A, it should filter it so that only ButtonB will be sent a down, A is overtaken.\n";
	translationPack = translator(filter({ buttonA, buttonB }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	AssertEqual(translationPack.UpRequests.size(), 1);

	std::cout << "Y overtakes B, it should filter it so that only ButtonY will be sent a down, B is overtaken.\n";
	translationPack = translator(filter({ buttonA, buttonB, buttonY }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	AssertEqual(translationPack.UpRequests.size(), 1);

	// Note that multiple keys in the overtaken queue can be removed from the overtaken queue in one iteration, plus the single modification for their group.
	std::cout << "A,B removed from overtaken queue, Y still activated (no change to activated key).\n";
	translationPack = translator(filter({ buttonY }));
	translationPack();
	AssertEqual(translationPack.DownRequests.empty(), true);
	AssertEqual(translationPack.UpRequests.empty(), true);

	std::cout << "A few iterations to set the state for next test...\n";
	// Add buttons A,B back to the overtaken queue with Y activated.
	translator(filter({  }))();
	translator(filter({ buttonA }))();
	translator(filter({ buttonA, buttonB }))();
	translator(filter({ buttonA, buttonB, buttonY }))();

	// Note that multiple keys in the overtaken queue can be removed from the overtaken queue in one iteration, plus the single modification for their group.
	std::cout << "With Y activated, A,B overtaken\n";
	std::cout << "X overtakes Y, it should filter it so that only X will be sent a down, Y is overtaken. A,B are removed from overtaken queue.\n";
	translationPack = translator(filter({ buttonY, buttonX }));
	translationPack();
	AssertEqual(translationPack.DownRequests.size(), 1);
	AssertEqual(translationPack.UpRequests.size(), 1);
	return 0;
}

int main()
{
	TestFreeFuncs();

	TestFilter();

	TestLargeQueueToAllUp();

	TestFilterWithTranslator();
	return 0;
}