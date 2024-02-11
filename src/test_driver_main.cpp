#include <iostream>
#include <print>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cassert>
#include <chrono>

#include "StreamToActionTranslator.h"
#include "../tests/test_utils.h"

// Crude mechanism to keep the loop running until [enter] is pressed.
struct GetterExitCallable final
{
    std::atomic<bool> IsDone{ false };
    void GetExitSignal()
    {
        std::string buf;
        std::getline(std::cin, buf);
        IsDone.store(true, std::memory_order_relaxed);
    }
};

auto GetEpochTimestamp()
{
    const auto currentTime = std::chrono::steady_clock::now();
    return std::chrono::duration_cast<std::chrono::seconds>(currentTime.time_since_epoch());
}

auto GetDriverButtonMappings()
{
    using std::vector, sds::MappingContainer, std::cout;
    using namespace std::chrono_literals;
    using namespace sds;

    constexpr int PadButtonsGroup = 111; // Buttons exclusivity grouping.
    constexpr int LeftThumbGroup = 101; // Left thumbstick exclusivity grouping.
    const auto PrintMessageAndTime = [](std::string_view msg)
        {
            cout << msg << " @" << GetEpochTimestamp() << '\n';
        };
    const auto GetDownLambdaForKeyNamed = [=](const std::string& keyName)
        {
            return [=]() { PrintMessageAndTime(keyName + "=[DOWN]"); };
        };
    const auto GetUpLambdaForKeyNamed = [=](const std::string& keyName)
        {
            return [=]() { PrintMessageAndTime(keyName + "=[UP]"); };
        };
    const auto GetRepeatLambdaForKeyNamed = [=](const std::string& keyName)
        {
            return [=]() { PrintMessageAndTime(keyName + "=[REPEAT]"); };
        };
    const auto GetResetLambdaForKeyNamed = [=](const std::string& keyName)
        {
            return [=]() { PrintMessageAndTime(keyName + "=[RESET]"); };
        };

    const auto GetBuiltMapForKeyNamed = [&](const std::string& keyName, const auto virtualKey, const int exGroup, const auto firstDelay)
        {
            return MappingContainer
            {
                .ButtonVirtualKeycode = virtualKey,
                .UsesInfiniteRepeat = true,
                .ExclusivityGrouping = exGroup,
                .OnDown = GetDownLambdaForKeyNamed(keyName),
                .OnUp = GetUpLambdaForKeyNamed(keyName),
                .OnRepeat = GetRepeatLambdaForKeyNamed(keyName),
                .OnReset = GetResetLambdaForKeyNamed(keyName),
                .DelayBeforeFirstRepeat = firstDelay
            };
        };

    KeyboardSettings ksp;

    vector mapBuffer
    {
        // Pad buttons
        GetBuiltMapForKeyNamed("[PAD_A]", ksp.ButtonA, PadButtonsGroup, 500ms),
        GetBuiltMapForKeyNamed("[PAD_B]", ksp.ButtonB, PadButtonsGroup, 500ms),
        GetBuiltMapForKeyNamed("[PAD_X]", ksp.ButtonX, PadButtonsGroup, 500ms),
        GetBuiltMapForKeyNamed("[PAD_Y]", ksp.ButtonY, PadButtonsGroup, 500ms),
        // Left thumbstick directional stuff
        GetBuiltMapForKeyNamed("[LTHUMB_UP]", ksp.LeftThumbstickUp, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_DOWN]", ksp.LeftThumbstickDown, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_RIGHT]", ksp.LeftThumbstickRight, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_LEFT]", ksp.LeftThumbstickLeft, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_DOWN_RIGHT]", ksp.LeftThumbstickDownRight, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_DOWN_LEFT]", ksp.LeftThumbstickDownLeft, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_UP_RIGHT]", ksp.LeftThumbstickUpRight, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTHUMB_UP_LEFT]", ksp.LeftThumbstickUpLeft, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[LTRIGGER]", ksp.LeftTrigger, LeftThumbGroup, 500ms),
        GetBuiltMapForKeyNamed("[RTRIGGER]", ksp.RightTrigger, LeftThumbGroup, 500ms),
        // Shoulder buttons
        MappingContainer{
            .ButtonVirtualKeycode = ksp.ButtonShoulderRight,
            .UsesInfiniteRepeat = false,
            .OnDown = []() { system("cls"); std::cout << "Cleared.\n"; }
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.ButtonShoulderLeft,
            .UsesInfiniteRepeat = false,
            .OnDown = []()
            {
            // Add impl for something to do here
        }
    },
    };

    return mapBuffer;
}

auto GetDriverMouseMappings()
{
    using std::vector, std::cout;
    using namespace std::chrono_literals;
    using namespace sds;
    static constexpr KeyboardSettings ksp{};
    constexpr auto FirstDelay = 0ns; // mouse move delays
    constexpr auto RepeatDelay = 1200us;
    constexpr int MouseExGroup = 102;

    vector mapBuffer
    {
        // Mouse move stuff
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickUp,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(0, 1);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(0, 1);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickUpRight,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(1, 1);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(1, 1);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickUpLeft,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(-1, 1);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(-1, 1);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickDown,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(0, -1);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(0, -1);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickLeft,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(-1, 0);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(-1, 0);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickRight,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(1, 0);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(1, 0);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickDownRight,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(1, -1);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(1, -1);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
        MappingContainer{
            .ButtonVirtualKeycode = ksp.RightThumbstickDownLeft,
            .UsesInfiniteRepeat = true,
            .ExclusivityGrouping = MouseExGroup,
            .OnDown = []()
            {
                Utilities::SendMouseMove(-1, -1);
            },
            .OnRepeat = []()
            {
                Utilities::SendMouseMove(-1, -1);
            },
            .DelayBeforeFirstRepeat = FirstDelay,
            .DelayForRepeats = RepeatDelay
        },
    };

    return mapBuffer;
}

inline
void TranslationLoop(const sds::KeyboardSettingsPack& settingsPack, sds::KeyboardTranslator<>& translator, const std::chrono::nanoseconds sleepDelay)
{
    using namespace std::chrono_literals;
    const auto translation = translator.GetUpdatedState(GetWrappedLegacyApiStateUpdate(settingsPack));
    translation();
    nanotime_sleep(sleepDelay.count());
}

auto RunTestDriverLoop()
{
    using namespace std::chrono_literals;

    // Building mappings buffer
    auto mapBuffer = GetDriverButtonMappings();
    mapBuffer.append_range(GetDriverMouseMappings());

    std::cout << std::vformat("Created mappings buffer with {} mappings. Total size: {} bytes.\n", std::make_format_args(mapBuffer.size(), sizeof(mapBuffer.front()) * mapBuffer.size()));

    // Creating a few polling/translation related types
    sds::KeyboardSettingsPack settingsPack{};
    // The filter is constructed here, to support custom filters with their own construction needs.
    sds::KeyboardOvertakingFilter filter{};
    // Filter is then moved into the translator at construction.
    sds::KeyboardTranslator translator{ std::move(mapBuffer), std::move(filter) };

    constexpr auto SleepDelay = std::chrono::nanoseconds{ 500us };
    static constexpr std::size_t IterationsForTiming{ 10'000 };
    std::size_t iterationCount{};
    auto startTime{ std::chrono::steady_clock::now() };
    const auto updateLoopTimer = [&startTime, &iterationCount](const std::chrono::nanoseconds sleepDelay)
        {
            ++iterationCount;
            if (iterationCount >= IterationsForTiming)
            {
                const auto endTime = std::chrono::steady_clock::now();
                const auto diffTime = endTime - startTime;
                const auto timePerIter = std::chrono::duration_cast<std::chrono::microseconds>(diffTime / IterationsForTiming - sleepDelay);
                std::cout << "Average time over " << IterationsForTiming << " iterations, per iteration: " << timePerIter << '\n';

                iterationCount = {};
                startTime = std::chrono::steady_clock::now();
            }
        };

    GetterExitCallable gec;
    const auto exitFuture = std::async(std::launch::async, [&]() { gec.GetExitSignal(); });
    while (!gec.IsDone)
    {
        TranslationLoop(settingsPack, translator, SleepDelay);
        updateLoopTimer(SleepDelay);
    }
    std::cout << "Performing cleanup actions...\n";
    const auto cleanupTranslations = translator.GetCleanupActions();
    for (auto& cleanupAction : cleanupTranslations)
        cleanupAction();

    exitFuture.wait();
}


// Test driver program for keyboard mapping
int main()
{
    RunTestDriverLoop();
}