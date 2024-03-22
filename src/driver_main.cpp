#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cassert>
#include <iostream>
#include <print>
#include <chrono>
#include <thread>
#include <future>
#include <atomic>
#include <string_view>
#include <format>
#include <concepts>
#include <type_traits>
#include <numbers>
#include <numeric>

#include "StreamToActionTranslator.h"
#define NOMINMAX
#include <Windows.h>
#include <Xinput.h>
#pragma comment(lib, "Xinput.lib")

enum class ThumbstickDirection
{
    Up,
    UpRight,
    Right,
    RightDown,
    Down,
    DownLeft,
    Left,
    LeftUp,
    Invalid
};

enum class ControllerStick
{
    LeftStick,
    RightStick
};

/**
 * \brief Some constants that are not configurable.
 */
struct KeyboardSettings
{
    /**
     * \brief Delay each iteration of a polling loop, short enough to not miss information, long enough to not waste CPU cycles.
     */
    static constexpr sds::Nanos_t PollingLoopDelay{ std::chrono::milliseconds{1} };
    /**
     * \brief Key Repeat Delay is the time delay a button has in-between activations.
     */
    static constexpr sds::Nanos_t KeyRepeatDelay{ std::chrono::microseconds{100'000} };

    // Controller buttons
    static constexpr int32_t ButtonA{ 4'096 };
    static constexpr int32_t ButtonB{ 8'192 };
    static constexpr int32_t ButtonX{ 16'384 };
    static constexpr int32_t ButtonY{ 32'768 };

    // Dpad buttons
    static constexpr int32_t DpadUp{ 1 };
    static constexpr int32_t DpadDown{ 2 };
    static constexpr int32_t DpadLeft{ 4 };
    static constexpr int32_t DpadRight{ 8 };

    // Left thumbstick directions
    static constexpr int32_t LeftThumbstickUp{ 211 }; // UP
    static constexpr int32_t LeftThumbstickDown{ 212 }; // DOWN
    static constexpr int32_t LeftThumbstickRight{ 213 }; // RIGHT
    static constexpr int32_t LeftThumbstickLeft{ 214 }; // LEFT

    static constexpr int32_t LeftThumbstickUpLeft{ 22'564 }; // UP-LEFT
    static constexpr int32_t LeftThumbstickUpRight{ 22'565 }; // UP-RIGHT
    static constexpr int32_t LeftThumbstickDownRight{ 22'566 }; // RIGHT-DOWN
    static constexpr int32_t LeftThumbstickDownLeft{ 22'567 }; // DOWN-LEFT

    // Right thumbstick directions
    static constexpr int32_t RightThumbstickUp{ 215 }; // UP
    static constexpr int32_t RightThumbstickDown{ 216 }; // DOWN
    static constexpr int32_t RightThumbstickRight{ 217 }; // RIGHT
    static constexpr int32_t RightThumbstickLeft{ 218 }; // LEFT

    static constexpr int32_t RightThumbstickUpLeft{ 22'580 }; // UP-LEFT
    static constexpr int32_t RightThumbstickUpRight{ 22'581 }; //UP-RIGHT
    static constexpr int32_t RightThumbstickDownRight{ 22'582 }; // RIGHT-DOWN
    static constexpr int32_t RightThumbstickDownLeft{ 22'583 }; // DOWN-LEFT

    // Other buttons
    static constexpr int32_t ButtonStart{ 16 };
    static constexpr int32_t ButtonBack{ 32 };
    static constexpr int32_t ButtonShoulderLeft{ 256 };
    static constexpr int32_t ButtonShoulderRight{ 512 };
    static constexpr int32_t ThumbLeftClick{ 64 };
    static constexpr int32_t ThumbRightClick{ 128 };

    // used internally to denote left or right triggers, similar to the button VKs though they may
    // not be used by the OS API state updates in the same way--we virtualize them.
    static constexpr int32_t LeftTrigger{ 201 };
    static constexpr int32_t RightTrigger{ 202 };

    /**
     * \brief The button virtual keycodes as a flat array.
     */
    static constexpr std::array<int32_t, 14> ButtonCodeArray
    {
        DpadUp,
        DpadDown,
        DpadLeft,
        DpadRight,
        ButtonStart,
        ButtonBack,
        ThumbLeftClick,
        ThumbRightClick,
        ButtonShoulderLeft,
        ButtonShoulderRight,
        ButtonA,
        ButtonB,
        ButtonX,
        ButtonY
    };

    static constexpr int LeftStickDeadzone{ 7849 };
    static constexpr int RightStickDeadzone{ 8689 };

    static constexpr int LeftTriggerThreshold{ 30 };
    static constexpr int RightTriggerThreshold{ 30 };

    // The type of the button buffer without const/volatile/reference.
    using ButtonBuffer_t = std::remove_reference_t< std::remove_cv_t<decltype(ButtonCodeArray)> >;

    // TODO update this if the values become non-const
    friend auto hash_value([[maybe_unused]] const KeyboardSettings& obj) -> std::size_t { return 0x0ED35098; }
};

static constexpr float MY_PI{ std::numbers::pi_v<float> };
static constexpr float MY_PI2{ std::numbers::pi_v<float> / float{2} };
static constexpr float MY_PI8{ std::numbers::pi_v<float> / float{8} };

// Primary function template for a directional bounds checker.
template<float Low, float High, ThumbstickDirection Dir>
constexpr auto GetDirection(const float theta) noexcept -> std::optional<ThumbstickDirection>
{
    return (theta >= Low && theta <= High) ? Dir : std::optional<ThumbstickDirection>{};
}

// This specialization requires custom logic in the bounds check to work.
static constexpr auto specialLow = 7 * MY_PI8;
static constexpr auto specialHigh = 7 * -MY_PI8;

template<>
constexpr auto GetDirection<specialLow, specialHigh, ThumbstickDirection::Left>(const float theta) noexcept -> std::optional<ThumbstickDirection>
{
    const bool isThetaPositive = theta >= decltype(theta){};
    const bool isWithinBounds = isThetaPositive ? theta >= specialLow : theta <= specialHigh;
    return isWithinBounds ? ThumbstickDirection::Left : std::optional<ThumbstickDirection>{};
}

[[nodiscard]] constexpr auto GetDirectionForPolarTheta(const float theta) noexcept -> ThumbstickDirection
{
    const auto dir = GetDirection<-MY_PI8, MY_PI8, ThumbstickDirection::Right>(theta)
        .or_else([theta]() { return GetDirection<MY_PI8, 3 * MY_PI8, ThumbstickDirection::UpRight>(theta); })
        .or_else([theta]() { return GetDirection<3 * MY_PI8, 5 * MY_PI8, ThumbstickDirection::Up>(theta); })
        .or_else([theta]() { return GetDirection<5 * MY_PI8, 7 * MY_PI8, ThumbstickDirection::LeftUp>(theta); })
        .or_else([theta]() { return GetDirection<7 * MY_PI8, 7 * -MY_PI8, ThumbstickDirection::Left>(theta); })
        .or_else([theta]() { return GetDirection<7 * -MY_PI, 5 * -MY_PI8, ThumbstickDirection::DownLeft>(theta); })
        .or_else([theta]() { return GetDirection<5 * -MY_PI8, 3 * -MY_PI8, ThumbstickDirection::Down>(theta); })
        .or_else([theta]() { return GetDirection<3 * -MY_PI8, -MY_PI8, ThumbstickDirection::RightDown>(theta); });
    return dir.value_or(ThumbstickDirection::Invalid);
}

[[nodiscard]] constexpr auto GetVirtualKeyFromDirection(const ThumbstickDirection direction, const ControllerStick whichStick) -> std::optional<int32_t>
{
    const bool isLeftStick = whichStick == ControllerStick::LeftStick;

    switch (direction)
    {
    case ThumbstickDirection::Up: return isLeftStick ? KeyboardSettings::LeftThumbstickUp : KeyboardSettings::RightThumbstickUp;
    case ThumbstickDirection::UpRight: return isLeftStick ? KeyboardSettings::LeftThumbstickUpRight : KeyboardSettings::RightThumbstickUpRight;
    case ThumbstickDirection::Right: return isLeftStick ? KeyboardSettings::LeftThumbstickRight : KeyboardSettings::RightThumbstickRight;
    case ThumbstickDirection::RightDown:  return isLeftStick ? KeyboardSettings::LeftThumbstickDownRight : KeyboardSettings::RightThumbstickDownRight;
    case ThumbstickDirection::Down: return isLeftStick ? KeyboardSettings::LeftThumbstickDown : KeyboardSettings::RightThumbstickDown;
    case ThumbstickDirection::DownLeft: return isLeftStick ? KeyboardSettings::LeftThumbstickDownLeft : KeyboardSettings::RightThumbstickDownLeft;
    case ThumbstickDirection::Left: return isLeftStick ? KeyboardSettings::LeftThumbstickLeft : KeyboardSettings::RightThumbstickLeft;
    case ThumbstickDirection::LeftUp: return isLeftStick ? KeyboardSettings::LeftThumbstickUpLeft : KeyboardSettings::RightThumbstickUpLeft;
    case ThumbstickDirection::Invalid: return {};
    default:
    {
        throw std::runtime_error("Bad mapping of ThumbstickDirection to virtual key.");
    }
    }
    return {};
}

[[nodiscard]] constexpr bool IsFloatZero(const auto testFloat) noexcept
{
    constexpr auto eps = std::numeric_limits<decltype(testFloat)>::epsilon();
    constexpr auto eps2 = eps * 2;
    return std::abs(testFloat) <= eps2;
}

// [FloatingType, FloatingType] wherein the first member is the polar radius, and the second is the polar theta angle.
using PolarInfoPair = std::pair<float, float>;
[[nodiscard]] inline auto ComputePolarPair(const float xStickValue, const float yStickValue) noexcept -> PolarInfoPair
{
    constexpr auto nonZeroValue{ std::numeric_limits<float>::min() }; // cannot compute with both values at 0, this is used instead
    const bool areBothZero = IsFloatZero(xStickValue) && IsFloatZero(yStickValue);

    const float xValue = areBothZero ? nonZeroValue : xStickValue;
    const float yValue = areBothZero ? nonZeroValue : yStickValue;
    const auto rad = std::hypot(xValue, yValue);
    const auto angle = std::atan2(yValue, xValue);
    return { rad, angle };
}

/**
* \brief	Important helper function to build a small vector of button VKs that are 'down'. Essential function
*	is to decompose bit masked state updates into an array.
* \param settingsPack	Settings pertaining to deadzone info and virtual keycodes.
* \param controllerState	The OS API state update.
* \return	small vector of down buttons.
*/
[[nodiscard]] inline auto GetDownVirtualKeycodesRange(
    const std::ranges::range auto& buttonCodeArray, 
    const auto leftTriggerThresh, 
    const auto rightTriggerThresh, 
    const XINPUT_STATE& controllerState) -> sds::SmallVector_t<int32_t>
{
    const auto IsTriggerBeyondThreshold = [](const uint32_t triggerValue, const uint32_t triggerThreshold) noexcept -> bool
        {
            return triggerValue > triggerThreshold;
        };

    // Keys
    sds::SmallVector_t<int32_t> allKeys{};
    for (const auto elem : buttonCodeArray)
    {
        if (controllerState.Gamepad.wButtons & elem)
            allKeys.emplace_back(elem);
    }

    // Triggers
    if (IsTriggerBeyondThreshold(controllerState.Gamepad.bLeftTrigger, leftTriggerThresh))
        allKeys.emplace_back(KeyboardSettings::LeftTrigger);
    if (IsTriggerBeyondThreshold(controllerState.Gamepad.bRightTrigger, rightTriggerThresh))
        allKeys.emplace_back(KeyboardSettings::RightTrigger);

    // Stick axes
    constexpr auto LeftStickDz{ KeyboardSettings::LeftStickDeadzone };
    constexpr auto RightStickDz{ KeyboardSettings::RightStickDeadzone };

    const auto leftThumbstickX{ controllerState.Gamepad.sThumbLX };
    const auto rightThumbstickX{ controllerState.Gamepad.sThumbRX };

    const auto leftThumbstickY{ controllerState.Gamepad.sThumbLY };
    const auto rightThumbstickY{ controllerState.Gamepad.sThumbRY };

    const auto leftStickPolarInfo{ ComputePolarPair(leftThumbstickX, leftThumbstickY) };
    const auto rightStickPolarInfo{ ComputePolarPair(rightThumbstickX, rightThumbstickY) };

    const auto leftDirection{ GetDirectionForPolarTheta(leftStickPolarInfo.second) };
    const auto rightDirection{ GetDirectionForPolarTheta(rightStickPolarInfo.second) };

    const auto leftThumbstickVk{ GetVirtualKeyFromDirection(leftDirection, ControllerStick::LeftStick) };
    const auto rightThumbstickVk{ GetVirtualKeyFromDirection(rightDirection, ControllerStick::RightStick) };

    const bool leftIsDown = leftStickPolarInfo.first > LeftStickDz && leftThumbstickVk.has_value();
    const bool rightIsDown = rightStickPolarInfo.first > RightStickDz && rightThumbstickVk.has_value();

    if (leftIsDown)
        allKeys.emplace_back(leftThumbstickVk.value());
    if (rightIsDown)
        allKeys.emplace_back(rightThumbstickVk.value());

    return allKeys;
}

/**
* \brief Calls the OS API function(s).
* \param playerId Most commonly 0 for a single device connected.
* \return Platform/API specific state structure.
*/
[[nodiscard]] inline auto GetLegacyApiStateUpdate(const int playerId = 0) noexcept -> XINPUT_STATE
{
    XINPUT_STATE controllerState{};
    XInputGetState(playerId, &controllerState);
    return controllerState;
}

[[nodiscard]] inline auto GetWrappedLegacyApiStateUpdate(const std::ranges::range auto& buttonCodeArray, const int playerId) noexcept -> sds::SmallVector_t<int32_t>
{
    return GetDownVirtualKeycodesRange(buttonCodeArray, KeyboardSettings::LeftTriggerThreshold, KeyboardSettings::RightTriggerThreshold, GetLegacyApiStateUpdate(playerId));
}


inline auto CallSendInput(INPUT* inp, std::uint32_t numSent) noexcept -> UINT
{
    return SendInput(static_cast<UINT>(numSent), inp, sizeof(INPUT));
}

inline void SendMouseMove(const int x, const int y) noexcept
{
    INPUT m_mouseMoveInput{};
    m_mouseMoveInput.type = INPUT_MOUSE;
    m_mouseMoveInput.mi.dwFlags = MOUSEEVENTF_MOVE;

    using dx_t = decltype(m_mouseMoveInput.mi.dx);
    using dy_t = decltype(m_mouseMoveInput.mi.dy);
    m_mouseMoveInput.mi.dx = static_cast<dx_t>(x);
    m_mouseMoveInput.mi.dy = -static_cast<dy_t>(y);
    m_mouseMoveInput.mi.dwExtraInfo = GetMessageExtraInfo();
    //Finally, send the input
    CallSendInput(&m_mouseMoveInput, 1);
}

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
            std::println("{} @{}", msg, GetEpochTimestamp());
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
                .OnDown = GetDownLambdaForKeyNamed(keyName),
                .OnUp = GetUpLambdaForKeyNamed(keyName),
                .OnRepeat = GetRepeatLambdaForKeyNamed(keyName),
                .OnReset = GetResetLambdaForKeyNamed(keyName),
                .ButtonVirtualKeycode = virtualKey,
                .RepeatingKeyBehavior = sds::RepeatType::Infinite,
                .ExclusivityGrouping = exGroup,
                .DelayBeforeFirstRepeat = firstDelay,
                //.BetweenRepeatDelay = std::chrono::milliseconds(100)
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
        MappingContainer
        {
            .OnDown = []() { system("cls"); std::cout << "Cleared.\n"; },
            .ButtonVirtualKeycode = ksp.ButtonShoulderRight,
            .RepeatingKeyBehavior = sds::RepeatType::None,
        },
        MappingContainer
        {
            .OnDown = []() { /* Add impl for something to do here */ },
            .ButtonVirtualKeycode = ksp.ButtonShoulderLeft,
            .RepeatingKeyBehavior = sds::RepeatType::None,
        },
    };

    return mapBuffer;
}

auto GetDriverMouseMappings()
{
    using std::vector, std::cout;
    using namespace std::chrono_literals;
    using namespace sds;
    constexpr auto FirstDelay = 0ns; // mouse move delays
    constexpr auto RepeatDelay = 1200us;
    constexpr int MouseExGroup = 102;

    vector mapBuffer
    {
        // Mouse move stuff
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(0, 1);
            },
            .OnRepeat = []()
            {
                SendMouseMove(0, 1);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickUp,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(1, 1);
            },
            .OnRepeat = []()
            {
                SendMouseMove(1, 1);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickUpRight,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(-1, 1);
            },
            .OnRepeat = []()
            {
                SendMouseMove(-1, 1);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickUpLeft,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(0, -1);
            },
            .OnRepeat = []()
            {
                SendMouseMove(0, -1);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickDown,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(-1, 0);
            },
            .OnRepeat = []()
            {
                SendMouseMove(-1, 0);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickLeft,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(1, 0);
            },
            .OnRepeat = []()
            {
                SendMouseMove(1, 0);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickRight,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(1, -1);
            },
            .OnRepeat = []()
            {
                SendMouseMove(1, -1);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickDownRight,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
        MappingContainer
        {
            .OnDown = []()
            {
                SendMouseMove(-1, -1);
            },
            .OnRepeat = []()
            {
                SendMouseMove(-1, -1);
            },
            .ButtonVirtualKeycode = KeyboardSettings::RightThumbstickDownLeft,
            .RepeatingKeyBehavior = sds::RepeatType::Infinite,
            .ExclusivityGrouping = MouseExGroup,
            .DelayBeforeFirstRepeat = FirstDelay,
            .BetweenRepeatDelay = RepeatDelay
        },
    };

    return mapBuffer;
}

inline void TranslationLoop(sds::Translator& translator, sds::OvertakingFilter<>& filter, const std::chrono::nanoseconds sleepDelay)
{
    using namespace std::chrono_literals;
    const auto translation = translator(filter(GetWrappedLegacyApiStateUpdate(KeyboardSettings::ButtonCodeArray, 0)));
    translation();
    //std::this_thread::sleep_for(sleepDelay);
}

auto RunTestDriverLoop()
{
    using namespace std::chrono_literals;

    // Building mappings buffer
    auto mapBuffer = GetDriverButtonMappings();
    mapBuffer.append_range(GetDriverMouseMappings());

    std::cout << "Test driver program for XBOX 360 controller (or another XINPUT device.)\n";
    std::cout << std::vformat("Created mappings buffer with {} mappings. Total size: {} bytes.\n", std::make_format_args(mapBuffer.size(), sizeof(mapBuffer.front()) * mapBuffer.size()));
    std::cout << "Starting poll loop for player 0\n";
    
    // Mappings are then moved into the translator at construction.
    sds::Translator translator{ std::move(mapBuffer) };

    // The filter is constructed here, to support custom filters with their own construction needs.
    sds::OvertakingFilter filter{translator};

    constexpr auto SleepDelay = std::chrono::nanoseconds{ 1 };

    GetterExitCallable gec;
    const auto exitFuture = std::async(std::launch::async, [&]() { gec.GetExitSignal(); });
    while (!gec.IsDone)
    {
        TranslationLoop(translator, filter, SleepDelay);
    }
    std::cout << "Performing cleanup actions...\n";
    const auto cleanupTranslations = translator.GetCleanupActions();
    for (auto& cleanupAction : cleanupTranslations)
        cleanupAction();

    exitFuture.wait();
}


int main()
{
    RunTestDriverLoop();
}