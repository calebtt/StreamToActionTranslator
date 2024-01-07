#pragma once

/*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.

In jurisdictions that recognize copyright laws, the author or authors of this software dedicate any and all copyright interest in the software to the public domain. We make this dedication for the benefit of the public at large and to the detriment of our heirs and successors. We intend this dedication to be an overt act of relinquishment in perpetuity of all present and future rights to this software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to https://unlicense.org
*/

#include <iostream>
#include <span>
#include <ranges>
#include <algorithm>
#include <functional>
#include <optional>
#include <numeric>
#include <vector>
#include <chrono>
#include <type_traits>
#include <concepts>
#include <map>
#include <stacktrace>
#include <source_location>

namespace sds
{
	namespace chron = std::chrono;
	using Index_t = uint32_t;
	using Nanos_t = chron::nanoseconds;
	using Clock_t = chron::steady_clock;
	using TimePoint_t = chron::time_point <Clock_t, Nanos_t>;
	using Fn_t = std::function<void()>;
	using GrpVal_t = uint8_t;
	using OptGrp_t = std::optional<GrpVal_t>;
	template<typename T>
	using SmallVector_t = std::vector<T>;
	template<typename Key_t, typename Val_t>
	using SmallFlatMap_t = std::map<Key_t, Val_t>;

	enum class RepeatType
	{
		Infinite, // Upon the button being held down, will translate to the key-repeat function activating repeatedly using a delay in between repeats.
		FirstOnly, // Upon the button being held down, will send a single repeat, will not continue translating to repeat after the single repeat.
		None // No key-repeats sent.
	};

	enum class ActionState
	{
		Init, // State indicating ready for new cycle
		KeyDown,
		KeyRepeat,
		KeyUp,
	};

	struct MappingContainer;
	// Concept for range of ButtonDescription type that must be contiguous.
	template<typename T>
	concept MappingRange_c = requires (T & t)
	{
		{ std::same_as<typename T::value_type, MappingContainer> == true };
		{ std::ranges::contiguous_range<T> == true };
	};

	struct TranslationPack;
	// A translator type, wherein you can call GetUpdatedState with a range of virtual keycode integral values, and get a TranslationPack as a result.
	template<typename Poller_t>
	concept InputTranslator_c = requires(Poller_t & t)
	{
		{ t.GetUpdatedState({ 1, 2, 3 }) } -> std::convertible_to<TranslationPack>;
		{ t.GetMappingsRange() } -> std::convertible_to<std::vector<MappingContainer>>;
	};

	template<typename Int_t>
	concept NotBoolIntegral_c = requires(Int_t& t)
	{
		{ std::same_as<Int_t, bool> == false };
		{ std::integral<Int_t> == true };
	};

	/**
	* \brief	DelayTimer manages a non-blocking time delay, it provides functions such as IsElapsed() and Reset(...)
	*/
	class DelayTimer final
	{
		TimePoint_t m_start_time{ Clock_t::now() };
		Nanos_t m_delayTime{}; // this should remain nanoseconds to ensure maximum granularity when Reset() with a different type.
		mutable bool m_has_fired{ false };
	public:
		DelayTimer() = delete;
		explicit DelayTimer(Nanos_t duration) noexcept : m_delayTime(duration) { }
		DelayTimer(const DelayTimer& other) = default;
		DelayTimer(DelayTimer&& other) = default;
		DelayTimer& operator=(const DelayTimer& other) = default;
		DelayTimer& operator=(DelayTimer&& other) = default;
		~DelayTimer() = default;
		/**
		 * \brief	Operator<< overload for ostream specialization, writes more detailed delay details for debugging.
		 */
		friend std::ostream& operator<<(std::ostream& os, const DelayTimer& obj) noexcept
		{
			os << "[DelayTimer]" << '\n'
				<< "m_start_time:" << obj.m_start_time.time_since_epoch() << '\n'
				<< "m_delayTime:" << obj.m_delayTime << '\n'
				<< "m_has_fired:" << obj.m_has_fired << '\n'
				<< "[/DelayTimer]";
			return os;
		}
		/**
		 * \brief	Check for elapsed.
		 * \return	true if timer has elapsed, false otherwise
		 */
		[[nodiscard]]
		bool IsElapsed() const noexcept
		{
			if (Clock_t::now() > (m_start_time + m_delayTime))
			{
				m_has_fired = true;
				return true;
			}
			return false;
		}
		/**
		 * \brief	Reset timer with chrono duration type.
		 * \param delay		Delay in nanoseconds (or any std::chrono duration type)
		 */
		void Reset(const Nanos_t delay) noexcept
		{
			m_start_time = Clock_t::now();
			m_has_fired = false;
			m_delayTime = { delay };
		}
		/**
		 * \brief	Reset timer to last used duration value for a new start point.
		 */
		void Reset() noexcept
		{
			m_start_time = Clock_t::now();
			m_has_fired = false;
		}
		/**
		 * \brief	Gets the current timer period/duration for elapsing.
		 */
		auto GetTimerPeriod() const
		{
			return m_delayTime;
		}
	};

	/**
	 * \brief	Functions called when a state change occurs.
	 */
	struct KeyStateBehaviors final
	{
		Fn_t OnDown; // Key-down
		Fn_t OnUp; // Key-up
		Fn_t OnRepeat; // Key-repeat
		Fn_t OnReset; // Reset after key-up and prior to another key-down can be performed
	};
	static_assert(std::copyable<KeyStateBehaviors>);
	static_assert(std::movable<KeyStateBehaviors>);

	/**
	 * \brief	Controller button to action mapping. This is how a mapping of a controller button to an action is described.
	 */
	struct ButtonDescription final
	{
		/**
		 * \brief	Controller button Virtual Keycode. Can be platform dependent or custom mapping, depends on input poller behavior.
		 */
		int32_t ButtonVirtualKeycode{};

		/**
		 * \brief	Type of key-repeat behavior.
		 */
		RepeatType RepeatingKeyBehavior{};

		/**
		 * \brief	The exclusivity grouping member is intended to allow the user to add different groups of mappings
		 *	that require another mapping from the same group to be "overtaken" or key-up sent before the "overtaking" new mapping
		 *	can perform the key-down.
		 * \remarks		optional, if not in use set to default constructed value or '{}'
		 */
		std::optional<GrpVal_t> ExclusivityGrouping; // TODO one variation of ex. group behavior is to have a priority value associated with the mapping.
	public:
		ButtonDescription(
			const int32_t buttonCode,
			std::optional<RepeatType> repeatBehavior = {},
			std::optional<GrpVal_t> optExclusivityGrouping = {}) noexcept
			: ButtonVirtualKeycode(buttonCode),
			RepeatingKeyBehavior(repeatBehavior.value_or(RepeatType::Infinite)),
			ExclusivityGrouping(optExclusivityGrouping)
		{
		}
	};
	static_assert(std::copyable<ButtonDescription>);
	static_assert(std::movable<ButtonDescription>);

	/**
	 * \brief	Wrapper for button to action mapping state enum, the least I can do is make sure state modifications occur through a managing class,
	 *		and that there exists only one 'current' state, and that it can only be a finite set of possibilities.
	 *		Also contains last sent time (for key-repeat), and delay before first key-repeat timer.
	 * \remarks	This class enforces an invariant that it's state cannot be altered out of sequence.
	 */
	class MappingStateTracker final
	{
		/**
		 * \brief Key Repeat Delay is the time delay a button has in-between activations.
		 */
		static constexpr std::chrono::nanoseconds DefaultKeyRepeatDelay{ std::chrono::microseconds{100'000} };
		ActionState m_currentValue{ ActionState::Init };
	public:
		/**
		 * \brief	This delay is mostly used for in-between key-repeats, but could also be in between other state transitions.
		 */
		DelayTimer LastSentTime{ DefaultKeyRepeatDelay };
		/**
		 * \brief	This is the delay before the first repeat is sent whilst holding the button down.
		 */
		DelayTimer DelayBeforeFirstRepeat{ LastSentTime.GetTimerPeriod() };
	public:
		[[nodiscard]] constexpr bool IsRepeating() const noexcept {
			return m_currentValue == ActionState::KeyRepeat;
		}
		[[nodiscard]] constexpr bool IsDown() const noexcept {
			return m_currentValue == ActionState::KeyDown;
		}
		[[nodiscard]] constexpr bool IsUp() const noexcept {
			return m_currentValue == ActionState::KeyUp;
		}
		[[nodiscard]] constexpr bool IsInitialState() const noexcept {
			return m_currentValue == ActionState::Init;
		}
		constexpr auto SetDown() noexcept
		{
			if (m_currentValue != ActionState::Init)
				return;

			m_currentValue = ActionState::KeyDown;
		}
		constexpr auto SetUp() noexcept
		{
			if (m_currentValue != ActionState::KeyDown && m_currentValue != ActionState::KeyRepeat)
				return;

			m_currentValue = ActionState::KeyUp;
		}
		constexpr auto SetRepeat() noexcept
		{
			if (m_currentValue != ActionState::KeyDown)
				return;

			m_currentValue = ActionState::KeyRepeat;
		}
		constexpr auto SetInitial() noexcept
		{
			if (m_currentValue != ActionState::KeyUp)
				return;

			m_currentValue = ActionState::Init;
		}
	};
	static_assert(std::copyable<MappingStateTracker>);
	static_assert(std::movable<MappingStateTracker>);

	struct MappingContainer final
	{
		/**
		 * \brief	The mapping description.
		 */
		ButtonDescription Button;

		/**
		 * \brief	The events/functions associated with different states.
		 */
		KeyStateBehaviors StateFunctions;

		/**
		 * \brief	Mutable last action performed, with get/set methods.
		*/
		MappingStateTracker LastAction;

	public:
		MappingContainer(
			const ButtonDescription& buttonDescription,
			const KeyStateBehaviors& stateFunctions,
			std::optional<Nanos_t> beforeRepeatDelay = {},
			std::optional<Nanos_t> betweenRepeatDelay = {})
			:
			Button(buttonDescription),
			StateFunctions(stateFunctions)
		{
			if (beforeRepeatDelay)
				LastAction.DelayBeforeFirstRepeat.Reset(beforeRepeatDelay.value());
			if (betweenRepeatDelay)
				LastAction.LastSentTime.Reset(betweenRepeatDelay.value());
		}

		MappingContainer(
			ButtonDescription&& buttonDescription,
			KeyStateBehaviors&& stateFunctions,
			std::optional<Nanos_t> beforeRepeatDelay = {},
			std::optional<Nanos_t> betweenRepeatDelay = {})
			:
			Button(std::move(buttonDescription)),
			StateFunctions(std::move(stateFunctions))
		{
			if (beforeRepeatDelay)
				LastAction.DelayBeforeFirstRepeat.Reset(beforeRepeatDelay.value());
			if (betweenRepeatDelay)
				LastAction.LastSentTime.Reset(betweenRepeatDelay.value());
		}
	};
	static_assert(std::copyable<MappingContainer>);
	static_assert(std::movable<MappingContainer>);

	/**
	 * \brief	TranslationResult holds info from a translated state change, typically the operation to perform (if any) and
	 *		a function to call to advance the state to the next state to continue to receive proper translation results.
	 *	\remarks	The advance state function can be used to ensure the operation to perform occurs BEFORE the mapping advances it's state.
	 *		This does mean that it may be possible to induce some error related to setting the state inappropriately. Without this
	 *		design, it would be possible to, for instance, withhold calling the operation to perform, yet still have the mapping's state updating internally, erroneously.
	 *		I believe this will make calling order-dependent functionality less error-prone.
	 */
	struct TranslationResult final
	{
		// TODO test with std::unique_ptr to Fn_t, it currently is like 18k of stack space.
		// Operation being requested to be performed, callable
		Fn_t OperationToPerform;
		// Function to advance the button mapping to the next state (after operation has been performed)
		Fn_t AdvanceStateFn;
		// Hash of the mapping it refers to
		int32_t MappingVk{};
		// Exclusivity grouping value, if any
		OptGrp_t ExclusivityGrouping;
		// Call operator, calls op fn then advances the state
		void operator()() const
		{
			OperationToPerform();
			AdvanceStateFn();
		}
	};
	static_assert(std::copyable<TranslationResult>);
	static_assert(std::movable<TranslationResult>);

	/**
	 * \brief	TranslationPack is a pack of ranges containing individual TranslationResult structs for processing state changes.
	 * \remarks		If using the provided call operator, it will prioritize key-up requests, then key-down requests, then repeat requests, then updates.
	 *	I figure it should process key-ups and new key-downs with the highest priority, after that keys doing a repeat, and lastly updates.
	 */
	struct TranslationPack final
	{
		void operator()() const
		{
			// Note that there will be a function called if there is a state change,
			// it just may not have any custom behavior attached to it.
			for (const auto& elem : UpRequests)
				elem();
			for (const auto& elem : DownRequests)
				elem();
			for (const auto& elem : RepeatRequests)
				elem();
			for (const auto& elem : UpdateRequests)
				elem();
		}

		SmallVector_t<TranslationResult> UpRequests{}; // key-ups
		SmallVector_t<TranslationResult> DownRequests{}; // key-downs
		SmallVector_t<TranslationResult> RepeatRequests{}; // repeats
		SmallVector_t<TranslationResult> UpdateRequests{}; // resets
		// TODO might wrap the vectors in a struct with a call operator to have individual call operators for range of TranslationResult.
	};
	static_assert(std::copyable<TranslationPack>);
	static_assert(std::movable<TranslationPack>);

	// These are a few 'factory' functions, to create the appropriate TranslationResult for the next mapping state--they are tremendously useful.
	[[nodiscard]] inline auto GetResetTranslationResult(MappingContainer& currentMapping) noexcept -> TranslationResult
	{
		return TranslationResult
		{
			.OperationToPerform = [&currentMapping]()
			{
				if (currentMapping.StateFunctions.OnReset)
					currentMapping.StateFunctions.OnReset();
			},
			.AdvanceStateFn = [&currentMapping]()
			{
				currentMapping.LastAction.SetInitial();
				currentMapping.LastAction.LastSentTime.Reset();
			},
			.MappingVk = currentMapping.Button.ButtonVirtualKeycode,
			.ExclusivityGrouping = currentMapping.Button.ExclusivityGrouping
		};
	}

	[[nodiscard]] inline auto GetRepeatTranslationResult(MappingContainer& currentMapping) noexcept -> TranslationResult
	{
		return TranslationResult
		{
			.OperationToPerform = [&currentMapping]()
			{
				if (currentMapping.StateFunctions.OnRepeat)
					currentMapping.StateFunctions.OnRepeat();
				currentMapping.LastAction.LastSentTime.Reset();
			},
			.AdvanceStateFn = [&currentMapping]()
			{
				currentMapping.LastAction.SetRepeat();
			},
			.MappingVk = currentMapping.Button.ButtonVirtualKeycode,
			.ExclusivityGrouping = currentMapping.Button.ExclusivityGrouping
		};
	}

	[[nodiscard]] inline auto GetOvertakenTranslationResult(MappingContainer& overtakenMapping) noexcept -> TranslationResult
	{
		return TranslationResult
		{
			.OperationToPerform = [&overtakenMapping]()
			{
				if (overtakenMapping.StateFunctions.OnUp)
					overtakenMapping.StateFunctions.OnUp();
			},
			.AdvanceStateFn = [&overtakenMapping]()
			{
				overtakenMapping.LastAction.SetUp();
			},
			.MappingVk = overtakenMapping.Button.ButtonVirtualKeycode,
			.ExclusivityGrouping = overtakenMapping.Button.ExclusivityGrouping
		};
	}

	[[nodiscard]] inline auto GetKeyUpTranslationResult(MappingContainer& currentMapping) noexcept -> TranslationResult
	{
		return TranslationResult
		{
			.OperationToPerform = [&currentMapping]()
			{
				if (currentMapping.StateFunctions.OnUp)
					currentMapping.StateFunctions.OnUp();
			},
			.AdvanceStateFn = [&currentMapping]()
			{
				currentMapping.LastAction.SetUp();
			},
			.MappingVk = currentMapping.Button.ButtonVirtualKeycode,
			.ExclusivityGrouping = currentMapping.Button.ExclusivityGrouping
		};
	}

	[[nodiscard]] inline auto GetInitialKeyDownTranslationResult(MappingContainer& currentMapping) noexcept -> TranslationResult
	{
		return TranslationResult
		{
			.OperationToPerform = [&currentMapping]()
			{
				if (currentMapping.StateFunctions.OnDown)
					currentMapping.StateFunctions.OnDown();
				// Reset timer after activation, to wait for elapsed before another next state translation is returned.
				currentMapping.LastAction.LastSentTime.Reset();
				currentMapping.LastAction.DelayBeforeFirstRepeat.Reset();
			},
			.AdvanceStateFn = [&currentMapping]()
			{
				currentMapping.LastAction.SetDown();
			},
			.MappingVk = currentMapping.Button.ButtonVirtualKeycode,
			.ExclusivityGrouping = currentMapping.Button.ExclusivityGrouping
		};
	}

	// Algorithm functions used by the translator.
	/**
	 * \brief For a single mapping, search the controller state update buffer and produce a TranslationResult appropriate to the current mapping state and controller state.
	 * \param downKeys Wrapper class containing the results of a controller state update polling.
	 * \param singleButton The mapping type for a single virtual key of the controller.
	 * \returns Optional, <c>TranslationResult</c>
	 */
	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForInitialToDown(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find, std::ranges::end;

		if (singleButton.LastAction.IsInitialState())
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK *is* found in the down list, create the down translation.
			if (findResult != end(downKeys))
				return GetInitialKeyDownTranslationResult(singleButton);
		}
		return {};
	}

	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForDownToRepeat(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find, std::ranges::end;
		const bool isDownAndUsesRepeat = singleButton.LastAction.IsDown() &&
			(singleButton.Button.RepeatingKeyBehavior == RepeatType::Infinite || singleButton.Button.RepeatingKeyBehavior == RepeatType::FirstOnly);
		const bool isDelayElapsed = singleButton.LastAction.DelayBeforeFirstRepeat.IsElapsed();
		if (isDownAndUsesRepeat && isDelayElapsed)
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK *is* found in the down list, create the repeat translation.
			if (findResult != end(downKeys))
				return GetRepeatTranslationResult(singleButton);
		}
		return {};
	}

	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForRepeatToRepeat(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find, std::ranges::end;
		const bool isRepeatAndUsesInfinite = singleButton.LastAction.IsRepeating() && singleButton.Button.RepeatingKeyBehavior == RepeatType::Infinite;
		if (isRepeatAndUsesInfinite && singleButton.LastAction.LastSentTime.IsElapsed())
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK *is* found in the down list, create the repeat translation.
			if (findResult != end(downKeys))
				return GetRepeatTranslationResult(singleButton);
		}
		return {};
	}

	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForDownOrRepeatToUp(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find, std::ranges::end;
		if (singleButton.LastAction.IsDown() || singleButton.LastAction.IsRepeating())
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK is not found in the down list, create the up translation.
			if (findResult == end(downKeys))
				return GetKeyUpTranslationResult(singleButton);
		}
		return {};
	}

	// This is the reset translation
	[[nodiscard]] inline auto GetButtonTranslationForUpToInitial(MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		// if the timer has elapsed, update back to the initial state.
		if (singleButton.LastAction.IsUp() && singleButton.LastAction.LastSentTime.IsElapsed())
		{
			return GetResetTranslationResult(singleButton);
		}
		return {};
	}

	/**
	* \brief Returns the indices at which a mapping that matches the 'vk' was found. PRECONDITION: A mapping with the specified VK does exist in the mappingsRange!
	* \param vk Virtual keycode of the presumably 'down' key with which to match MappingContainer mappings.
	* \param mappingsRange The range of MappingContainer mappings for which to return the indices of matching mappings.
	*/
	[[nodiscard]] inline auto GetMappingIndexForVk(const NotBoolIntegral_c auto vk, const std::span<const MappingContainer> mappingsRange) -> std::optional<Index_t>
	{
		using std::ranges::find_if;
		using std::ranges::cend;
		using std::ranges::cbegin;
		using std::ranges::distance;

		const auto findResult = find_if(mappingsRange, [vk](const auto e) { return e.Button.ButtonVirtualKeycode == vk; });
		const bool didFindResult = findResult != cend(mappingsRange);

		if (!didFindResult)
		{
			throw std::runtime_error(
				std::vformat("Did not find mapping with vk: {} in mappings range.\nLocation:\n{}\n\n",
					std::make_format_args(static_cast<int>(vk), std::source_location::current().function_name())));
		}

		return static_cast<Index_t>(distance(cbegin(mappingsRange), findResult));
	}

	/**
	* \brief Returns the iterator at which a mapping that matches the 'vk' was found.
	* \param vk Virtual keycode of the presumably 'down' key with which to match MappingContainer mappings.
	* \param mappingsRange The range of MappingContainer mappings for which to return the indices of matching mappings.
	*/
	[[nodiscard]] inline auto GetMappingByVk(const NotBoolIntegral_c auto vk, std::span<const MappingContainer> mappingsRange) -> std::optional<std::span<const MappingContainer>::iterator>
	{
		using std::ranges::find_if;
		using std::ranges::cend;
		using std::ranges::cbegin;
		using std::ranges::distance;

		const auto findResult = find_if(mappingsRange, [vk](const auto e) { return e.Button.ButtonVirtualKeycode == vk; });
		const bool didFindResult = findResult != cend(mappingsRange);

		if (!didFindResult)
		{
			return {};
		}

		return findResult;
	}

	[[nodiscard]] constexpr auto IsVkInStateUpdate(const NotBoolIntegral_c auto vkToFind, const std::span<const int32_t> downVirtualKeys) noexcept -> bool
	{
		return std::ranges::any_of(downVirtualKeys, [vkToFind](const auto vk) { return vk == vkToFind; });
	}

	[[nodiscard]] constexpr auto IsMappingInRange(const NotBoolIntegral_c auto vkToFind, const std::ranges::range auto& downVirtualKeys) noexcept -> bool
	{
		return std::ranges::any_of(downVirtualKeys, [vkToFind](const auto vk) { return vk == vkToFind; });
	}

	constexpr void EraseValuesFromRange(std::ranges::range auto& theRange, const std::ranges::range auto& theValues) noexcept
	{
		for (const auto& elem : theValues)
		{
			const auto foundPosition = std::ranges::find(theRange, elem);
			if (foundPosition != std::ranges::cend(theRange))
				theRange.erase(foundPosition);
		}
	}

	/**
	 * \brief	Checks a list of mappings for having multiple mappings mapped to a single controller button.
	 * \param	mappingsList Span of controller button to action mappings.
	 * \return	true if good (or empty) mapping list, false if there is a problem.
	 */
	[[nodiscard]] inline bool AreMappingsUniquePerVk(const std::span<const MappingContainer> mappingsList) noexcept
	{
		SmallFlatMap_t<int32_t, bool> mappingTable;
		for (const auto& e : mappingsList)
		{
			if (mappingTable[e.Button.ButtonVirtualKeycode])
			{
				return false;
			}
			mappingTable[e.Button.ButtonVirtualKeycode] = true;
		}
		return true;
	}

	/**
	 * \brief	Checks a list of mappings for having multiple mappings mapped to a single controller button.
	 * \param	mappingsList Span of controller button to action mappings.
	 * \return	true if good (or empty) mapping list, false if there is a problem.
	 */
	[[nodiscard]] inline bool AreMappingVksNonZero(const std::span<const MappingContainer> mappingsList) noexcept
	{
		return !std::ranges::any_of(mappingsList, [](const auto vk) { return vk.ButtonVirtualKeycode == 0; }, &MappingContainer::Button);
	}

	/**
	 * \brief Used to determine if the MappingStateManager is in a state that would require some cleanup before destruction.
	 * \remarks If you add another state for the mapping, make sure to update this.
	 * \return True if mapping needs cleanup, false otherwise.
	 */
	[[nodiscard]] constexpr bool DoesMappingNeedCleanup(const MappingStateTracker& mapping) noexcept
	{
		return mapping.IsDown() || mapping.IsRepeating();
	}

	/**
	 * \brief Encapsulates the mapping buffer, processes controller state updates, returns translation packs.
	 * \remarks If, before destruction, the mappings are in a state other than initial or awaiting reset, then you may wish to
	 *	make use of the <c>GetCleanupActions()</c> function. Not copyable. Is movable.
	 *	<p></p>
	 *	<p>An invariant exists such that: <b>There must be only one mapping per virtual keycode.</b></p>
	 */
	class Translator final
	{
		using MappingVector_t = SmallVector_t<MappingContainer>;
		static_assert(MappingRange_c<MappingVector_t>);

		MappingVector_t m_mappings;

	public:
		Translator() = delete; // no default
		Translator(const Translator& other) = delete; // no copy
		auto operator=(const Translator& other)->Translator & = delete; // no copy-assign

		Translator(Translator&& other) = default; // move-construct
		auto operator=(Translator&& other)->Translator & = default; // move-assign
		~Translator() = default;

		/**
		 * \brief Mapping Vector Ctor, may throw on exclusivity group error, OR more than one mapping per VK.
		 * \param keyMappings mapping vector type
		 * \exception std::runtime_error on exclusivity group error during construction, OR more than one mapping per VK.
		 */
		explicit Translator(MappingVector_t&& keyMappings)
			: m_mappings(std::move(keyMappings))
		{
			if (!AreMappingsUniquePerVk(m_mappings) || !AreMappingVksNonZero(m_mappings))
				throw std::runtime_error("Exception: More than 1 mapping per VK!");
		}
	public:
		[[nodiscard]] auto operator()(SmallVector_t<int32_t> stateUpdate) noexcept -> TranslationPack
		{
			return GetUpdatedState(std::move(stateUpdate));
		}

		[[nodiscard]] auto GetUpdatedState(SmallVector_t<int32_t> stateUpdate) noexcept -> TranslationPack
		{
			TranslationPack translations;
			for (auto& mapping : m_mappings)
			{
				if (const auto upToInitial = GetButtonTranslationForUpToInitial(mapping))
				{
					translations.UpdateRequests.push_back(*upToInitial);
				}
				else if (const auto initialToDown = GetButtonTranslationForInitialToDown(stateUpdate, mapping))
				{
					// Advance to next state.
					translations.DownRequests.push_back(*initialToDown);
				}
				else if (const auto downToFirstRepeat = GetButtonTranslationForDownToRepeat(stateUpdate, mapping))
				{
					translations.RepeatRequests.push_back(*downToFirstRepeat);
				}
				else if (const auto repeatToRepeat = GetButtonTranslationForRepeatToRepeat(stateUpdate, mapping))
				{
					translations.RepeatRequests.push_back(*repeatToRepeat);
				}
				else if (const auto repeatToUp = GetButtonTranslationForDownOrRepeatToUp(stateUpdate, mapping))
				{
					translations.UpRequests.push_back(*repeatToUp);
				}
			}
			return translations;
		}

		[[nodiscard]] auto GetCleanupActions() noexcept -> SmallVector_t<TranslationResult>
		{
			SmallVector_t<TranslationResult> translations;
			for (auto& mapping : m_mappings)
			{
				if (DoesMappingNeedCleanup(mapping.LastAction))
				{
					translations.push_back(GetKeyUpTranslationResult(mapping));
				}
			}
			return translations;
		}

		[[nodiscard]] auto GetMappingsRange() const -> const MappingVector_t&
		{
			return m_mappings;
		}
	};
	static_assert(InputTranslator_c<Translator> == true);
	static_assert(std::movable<Translator> == true);
	static_assert(std::copyable<Translator> == false);

}
