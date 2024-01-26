/*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.

In jurisdictions that recognize copyright laws, the author or authors of this software dedicate any and all copyright interest in the software to the public domain. We make this dedication for the benefit of the public at large and to the detriment of our heirs and successors. We intend this dedication to be an overt act of relinquishment in perpetuity of all present and future rights to this software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to https://unlicense.org
*/
#pragma once
#include <iostream>
#include <span>
#include <ranges>
#include <algorithm>
#include <functional>
#include <optional>
#include <numeric>
#include <vector>
#include <deque>
#include <map>
#include <chrono>
#include <type_traits>
#include <concepts>
#include <source_location>

namespace sds
{
	namespace chron = std::chrono;
	using Index_t = uint32_t;
	using Nanos_t = chron::nanoseconds;
	using Clock_t = chron::steady_clock;
	using TimePoint_t = chron::time_point <Clock_t, Nanos_t>;
	using Fn_t = std::function<void()>;
	using GrpVal_t = int32_t;
	
	template<typename T>
	using SmallVector_t = std::vector<T>;

	template<typename Key_t, typename Val_t>
	using SmallFlatMap_t = std::map<Key_t, Val_t>;

	enum class RepeatType
	{
		// Upon the button being held down, will translate to the key-repeat function activating repeatedly using a delay in between repeats.
		Infinite, 
		// Upon the button being held down, will send a single repeat, will not continue translating to repeat after the single repeat.
		FirstOnly, 
		// No key-repeats sent.
		None 
	};

	enum class ActionState
	{
		Init, // State indicating ready for new cycle
		KeyDown,
		KeyRepeat,
		KeyUp,
	};

	struct MappingContainer;
	struct TranslationPack;
	class Translator;

	// Concept for range of ButtonDescription type that must be contiguous.
	template<typename T>
	concept MappingRange_c = requires (T & t)
	{
		{ std::same_as<typename T::value_type, MappingContainer> == true };
		{ std::ranges::contiguous_range<T> == true };
	};

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

	template<typename GroupInfo_t>
	concept FilterGroupInfo_c = requires(GroupInfo_t & t)
	{
		{ t.IsMappingActivated(1) } -> std::convertible_to<bool>;
		{ t.IsMappingOvertaken(1) } -> std::convertible_to<bool>;
		{ t.IsAnyMappingActivated()	} -> std::convertible_to<bool>;
		{ t.IsMappingActivatedOrOvertaken(1) } -> std::convertible_to<bool>;
		{ t.GetActivatedValue() } -> std::convertible_to<int32_t>;
		{ t.UpdateForNewMatchingGroupingDown(1) } -> std::convertible_to<std::pair<bool, std::optional<int32_t>>>;
		{ t.UpdateForNewMatchingGroupingUp(1) } -> std::convertible_to<std::optional<int32_t>>;
	};

	/**
	* \brief	DelayTimer manages a non-blocking time delay, it provides functions such as IsElapsed() and Reset(...)
	*/
	class DelayTimer
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
	struct KeyStateBehaviors
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
	struct ButtonDescription
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
	class MappingStateTracker
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

	struct MappingContainer
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
	struct TranslationResult
	{
		// TODO test with std::unique_ptr to Fn_t, it currently is like 18k of stack space.
		// Operation being requested to be performed, callable
		Fn_t OperationToPerform;
		// Function to advance the button mapping to the next state (after operation has been performed)
		Fn_t AdvanceStateFn;
		// Hash of the mapping it refers to
		int32_t MappingVk{};
		// Exclusivity grouping value, if any
		std::optional<GrpVal_t> ExclusivityGrouping;
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
	struct TranslationPack
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

	
#pragma region Algos_For_Translator
	// Algorithm functions used by the translator.

	[[nodiscard]] constexpr bool IsNotEnd(const std::ranges::range auto& theRange, const std::ranges::iterator_t<decltype(theRange)>& theIterator) noexcept
	{
		return theIterator != std::ranges::cend(theRange);
	}

	[[nodiscard]] constexpr bool IsEnd(const std::ranges::range auto& theRange, const std::ranges::iterator_t<decltype(theRange)>& theIterator) noexcept
	{
		return theIterator == std::ranges::cend(theRange);
	}

	/**
	 * \brief For a single mapping, search the controller state update buffer and produce a TranslationResult appropriate to the current mapping state and controller state.
	 * \param downKeys Wrapper class containing the results of a controller state update polling.
	 * \param singleButton The mapping type for a single virtual key of the controller.
	 * \returns Optional, <c>TranslationResult</c>
	 */
	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForInitialToDown(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find;

		if (singleButton.LastAction.IsInitialState())
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK *is* found in the down list, create the down translation.
			if (IsNotEnd(downKeys, findResult))
				return GetInitialKeyDownTranslationResult(singleButton);
		}
		return {};
	}

	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForDownToRepeat(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find;

		const bool isDownAndUsesRepeat = 
			singleButton.LastAction.IsDown() 
			&& (singleButton.Button.RepeatingKeyBehavior == RepeatType::Infinite 
			|| singleButton.Button.RepeatingKeyBehavior == RepeatType::FirstOnly);

		const bool isDelayElapsed = singleButton.LastAction.DelayBeforeFirstRepeat.IsElapsed();

		if (isDownAndUsesRepeat && isDelayElapsed)
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK *is* found in the down list, create the repeat translation.
			if (IsNotEnd(downKeys, findResult))
				return GetRepeatTranslationResult(singleButton);
		}
		return {};
	}

	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForRepeatToRepeat(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find;

		const bool isRepeatAndUsesInfinite = singleButton.LastAction.IsRepeating() && singleButton.Button.RepeatingKeyBehavior == RepeatType::Infinite;
		if (isRepeatAndUsesInfinite && singleButton.LastAction.LastSentTime.IsElapsed())
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK *is* found in the down list, create the repeat translation.
			if (IsNotEnd(downKeys, findResult))
				return GetRepeatTranslationResult(singleButton);
		}
		return {};
	}

	template<typename Val_t>
	[[nodiscard]] auto GetButtonTranslationForDownOrRepeatToUp(const SmallVector_t<Val_t>& downKeys, MappingContainer& singleButton) noexcept -> std::optional<TranslationResult>
	{
		using std::ranges::find;

		if (singleButton.LastAction.IsDown() || singleButton.LastAction.IsRepeating())
		{
			const auto findResult = find(downKeys, singleButton.Button.ButtonVirtualKeycode);
			// If VK is not found in the down list, create the up translation.
			if (IsEnd(downKeys, findResult))
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
	* \brief  Optionally returns the indices at which a mapping that matches the 'vk' was found.
	* \param vk Virtual keycode of the presumably 'down' key with which to match MappingContainer mappings.
	* \param mappingsRange The range of MappingContainer mappings for which to return the index of a matching mapping.
	*/
	[[nodiscard]] auto GetMappingIndexForVk(const NotBoolIntegral_c auto vk, const std::span<const MappingContainer> mappingsRange) noexcept -> std::optional<Index_t>
	{
		using std::ranges::find_if;

		const auto findResult = find_if(mappingsRange, [uvk = static_cast<uint32_t>(vk)](const auto e) { return e.Button.ButtonVirtualKeycode == uvk; });
		const bool didFindResult = IsNotEnd(mappingsRange, findResult);

		[[unlikely]]
		if (!didFindResult)
		{
			return {};
		}

		return static_cast<Index_t>(std::distance(mappingsRange.cbegin(), findResult));
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

	[[nodiscard]] constexpr bool DoesMappingHaveExclusivityGroup(const MappingContainer& mapping)
	{
		return mapping.Button.ExclusivityGrouping.has_value();
	}

#pragma endregion Algos_For_Translator

	/**
	 * \brief Encapsulates the mapping buffer, processes controller state updates, returns translation packs.
	 * \remarks If, before destruction, the mappings are in a state other than initial or awaiting reset, then you may wish to
	 *	make use of the <c>GetCleanupActions()</c> function. Not copyable. Is movable.
	 *	<p></p>
	 *	<p>An invariant exists such that: <b>There must be only one mapping per virtual keycode.</b></p>
	 */
	class Translator
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

		[[nodiscard]] auto GetUpdatedState(SmallVector_t<int32_t>&& stateUpdate) noexcept -> TranslationPack
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

	/**
	 * \brief	<para>A logical representation of a mapping's exclusivity group activation status, for this setup a single key in the exclusivity group can be 'activated'
	 *	or have a key-down state at a time. It is exclusively the only key in the group forwarded to the translator for processing of key-down events.</para>
	 * <para>Essentially this is used to ensure only a single key per exclusivity grouping is down at a time, and keys can overtake the current down key. </para>
	 * \remarks This abstraction manages the currently activated key being "overtaken" by another key from the same group and causing a key-up/down to be sent for the currently activated,
	 *	as well as moving the key in line behind the newly activated key. A much needed abstraction.
	 */
	class GroupActivationInfo
	{
		using Elem_t = int32_t;

		// First element of the queue is the activated mapping.
		std::deque<Elem_t> ActivatedValuesQueue;
	public:
		/**
		 * \brief Boolean of the returned pair is whether or not the keydown should be filtered/removed.
		 *	The optional value is (optionally) referring to the mapping to send a new key-up for.
		 * \remarks An <b>precondition</b> is that the mapping's value passed into this has a matching exclusivity grouping!
		 */
		[[nodiscard]] auto UpdateForNewMatchingGroupingDown(const Elem_t newDownVk) noexcept -> std::pair<bool, std::optional<Elem_t>>
		{
			// Filter all of the hashes already activated/overtaken.
			const bool isActivated = IsMappingActivated(newDownVk);
			const bool isOvertaken = IsMappingOvertaken(newDownVk);
			const bool doFilterTheDown = isOvertaken;
			if (isActivated || isOvertaken)
				return std::make_pair(doFilterTheDown, std::optional<Elem_t>{});

			// If any mapping hash is already activated, this new hash will be overtaking it and thus require a key-up for current activated.
			if (IsAnyMappingActivated())
			{
				const auto currentDownValue = ActivatedValuesQueue.front();
				ActivatedValuesQueue.push_front(newDownVk);
				return std::make_pair(false, currentDownValue);
			}

			// New activated mapping case, add to queue in first position and don't filter. No key-up required.
			ActivatedValuesQueue.push_front(newDownVk);
			return std::make_pair(false, std::optional<Elem_t>{});
		}

		/**
		 * \brief The optional value is (optionally) referring to the mapping to send a new key-down for,
		 *	in the event that the currently activated key is key-up'd and there is an overtaken key waiting behind it in the queue.
		 * \remarks An <b>precondition</b> is that the mapping passed into this has a matching exclusivity grouping!
		 */
		auto UpdateForNewMatchingGroupingUp(const Elem_t newUpVk) noexcept -> std::optional<Elem_t>
		{
			// Handle no hashes in queue to update case, and specific new up hash not in queue either.
			if (!IsAnyMappingActivated())
				return {};

			const auto findResult = std::ranges::find(ActivatedValuesQueue, newUpVk);
			const bool isFound = findResult != std::ranges::cend(ActivatedValuesQueue);

			if (isFound)
			{
				const bool isInFirstPosition = findResult == ActivatedValuesQueue.cbegin();

				// Case wherein the currently activated mapping is the one getting a key-up.
				if (isInFirstPosition)
				{
					if (ActivatedValuesQueue.size() > 1)
					{
						// If there is an overtaken queue, key-down the next key in line.
						ActivatedValuesQueue.pop_front();
						// Return the new front hash to be sent a key-down.
						return ActivatedValuesQueue.front();
					}
				}

				// otherwise, just remove it from the queue because it hasn't been key-down'd (it's one of the overtaken, or size is 1).
				ActivatedValuesQueue.erase(findResult);
			}

			return {};
		}

	public:
		[[nodiscard]] bool IsMappingActivated(const Elem_t vk) const noexcept
		{
			if (ActivatedValuesQueue.empty())
				return false;
			return vk == ActivatedValuesQueue.front();
		}
		[[nodiscard]] bool IsMappingOvertaken(const Elem_t vk) const noexcept
		{
			if (ActivatedValuesQueue.empty())
				return false;

			const bool isCurrentActivation = ActivatedValuesQueue.front() == vk;
			const auto findResult = std::ranges::find(ActivatedValuesQueue, vk);
			const bool isFound = findResult != std::ranges::cend(ActivatedValuesQueue);
			return !isCurrentActivation && isFound;
		}
		[[nodiscard]] bool IsAnyMappingActivated() const noexcept {
			return !ActivatedValuesQueue.empty();
		}
		[[nodiscard]] bool IsMappingActivatedOrOvertaken(const Elem_t vk) const noexcept
		{
			const auto findResult = std::ranges::find(ActivatedValuesQueue, vk);
			return findResult != std::ranges::cend(ActivatedValuesQueue);
		}
		[[nodiscard]] auto GetActivatedValue() const noexcept -> Elem_t
		{
			assert(!ActivatedValuesQueue.empty());
			return ActivatedValuesQueue.front();
		}
	};
	static_assert(std::movable<GroupActivationInfo>);
	static_assert(std::copyable<GroupActivationInfo>);

	/**
	 * \brief	May be used to internally filter the poller's translations in order to apply the overtaking behavior.
	 * \remarks This behavior is deviously complex, and modifications are best done to "GroupActivationInfo" only, if at all possible.
	 *	In the event that a single state update contains presently un-handled key-downs for mappings with the same exclusivity grouping,
	 *	it will only process a single overtaking key-down at a time, and will suppress the rest in the state update to be handled on the next iteration.
	 */
	template<typename GroupInfo_t = GroupActivationInfo>
	class OvertakingFilter
	{
		using VirtualCode_t = int32_t;

		// Mapping of exclusivity grouping value to 
		using GroupInfoMap_t = std::map<GrpVal_t, GroupInfo_t>;
		// Mapping of grouping value to mapping indices.
		using GroupIndexMap_t = std::map<GrpVal_t, SmallVector_t<Index_t>>;

		// span to mappings
		std::span<const MappingContainer> m_mappings;

		// map of grouping value to GroupActivationInfo container.
		GroupInfoMap_t m_groupMap;
		GroupIndexMap_t m_groupIndexMap;
	public:
		OvertakingFilter() = delete;

		explicit OvertakingFilter(const InputTranslator_c auto& translator)
		{
			const auto& mappings = translator.GetMappingsRange();
			SetMappingRange(mappings);
			BuildGroupingIndices(mappings);
		}

		// This function is used to filter the controller state updates before they are sent to the translator.
		// It will have an effect on overtaking behavior by modifying the state update buffer, which just contains the virtual keycodes that are reported as down.
		[[nodiscard]] auto GetFilteredButtonState(SmallVector_t<VirtualCode_t>&& stateUpdate) -> SmallVector_t<VirtualCode_t>
		{
			using std::ranges::sort;

			// Sorting provides an ordering to which down states with an already handled exclusivity grouping get filtered out for this iteration.
			//sort(stateUpdate, std::ranges::less{}); // TODO <-- problem for the (current) unit testing, optional anyway

			stateUpdate = FilterStateUpdateForUniqueExclusivityGroups(std::move(stateUpdate));

			auto filteredForDown = FilterDownTranslation(stateUpdate);

			// There appears to be no reason to report additional VKs that will become 'down' after a key is moved to up,
			// because for the key to still be in the overtaken queue, it would need to still be 'down' as well, and thus handled
			// by the down filter.
			FilterUpTranslation(stateUpdate);

			return filteredForDown;
		}

		auto operator()(SmallVector_t<VirtualCode_t> stateUpdate) -> SmallVector_t<VirtualCode_t>
		{
			return GetFilteredButtonState(std::move(stateUpdate));
		}
	private:

		void SetMappingRange(const std::span<const MappingContainer> mappingsList)
		{
			m_mappings = mappingsList;
			m_groupMap = {};

			// Build the map of ex. group information.
			for (const auto& elem : mappingsList)
			{
				if (elem.Button.ExclusivityGrouping)
				{
					const auto grpVal = *elem.Button.ExclusivityGrouping;
					m_groupMap[grpVal] = {};
				}
			}
		}

		// A somewhat important bit of memoization/pre-processing.
		void BuildGroupingIndices(const std::span<const MappingContainer> mappingsList)
		{
			using std::views::enumerate;
			m_groupIndexMap = {};

			// Build the map of ex. group information.
			for (const auto& [index, elem] : enumerate(mappingsList))
			{
				if (elem.Button.ExclusivityGrouping)
				{
					const auto grpVal = *elem.Button.ExclusivityGrouping;
					m_groupIndexMap[grpVal].push_back(index);
				}
			}
		}

		[[nodiscard]] auto FilterDownTranslation(const SmallVector_t<VirtualCode_t>& stateUpdate) -> SmallVector_t<VirtualCode_t>
		{
			using std::views::filter;
			using std::views::transform;

			const auto vkToMappingIndex = [&](const auto vk) -> std::optional<std::size_t>
			{
				using std::ranges::find_if;

				const auto findResult = find_if(m_mappings, [vk](const auto& e) { return e.Button.ButtonVirtualKeycode == vk; });
				const bool didFindResult = IsNotEnd(m_mappings, findResult);
				return didFindResult ? static_cast<std::size_t>(std::distance(m_mappings.cbegin(),findResult)) : std::optional<std::size_t>{};
			};
			const auto optWithValueAndGroup = [&](const auto opt) -> bool
			{
				return opt.has_value() && GetMappingAt(*opt).Button.ExclusivityGrouping.has_value();
			};
			const auto removeOpt = [&](const auto opt)
			{
				return opt.value();
			};

			auto stateUpdateCopy = stateUpdate;
			SmallVector_t<VirtualCode_t> vksToRemoveRange;
			// This appeared (at this time) to be the best option: 
			// input vk List -> xform to mapping index list -> filter results to only include non-empty optional and ex. group -> xform to remove the optional = index list of mappings in the state update that have an ex. group.
			for (const auto& mappingIndex : stateUpdateCopy | transform(vkToMappingIndex) | filter(optWithValueAndGroup) | transform(removeOpt))
			{
				auto& currentMapping = GetMappingAt(mappingIndex);
				auto& currentGroup = m_groupMap[*currentMapping.Button.ExclusivityGrouping];

				const auto& [shouldFilter, upOpt] = currentGroup.UpdateForNewMatchingGroupingDown(currentMapping.Button.ButtonVirtualKeycode);
				if (shouldFilter)
				{
					vksToRemoveRange.push_back(currentMapping.Button.ButtonVirtualKeycode);
				}
				if (upOpt)
				{
					vksToRemoveRange.push_back(*upOpt);
				}
			}

			EraseValuesFromRange(stateUpdateCopy, vksToRemoveRange);

			return stateUpdateCopy;
		}

		// it will process only one key per ex. group per iteration. The others will be filtered out and handled on the next iteration.
		void FilterUpTranslation(const SmallVector_t<VirtualCode_t>& stateUpdate)
		{
			using std::views::filter;
			// filters for all mappings of interest per the current 'down' VK buffer (the UP mappings in this case).
			const auto exGroupAndNotInUpdatePred = [&](const auto& currentMapping)
			{
				const bool hasValue = currentMapping.Button.ExclusivityGrouping.has_value();
				const bool notInUpdate = !IsMappingInRange(currentMapping.Button.ButtonVirtualKeycode, stateUpdate);
				return hasValue && notInUpdate;
			};

			for (const auto& currentMapping : m_mappings | filter(exGroupAndNotInUpdatePred))
			{
				auto& currentGroup = m_groupMap[*currentMapping.Button.ExclusivityGrouping];
				currentGroup.UpdateForNewMatchingGroupingUp(currentMapping.Button.ButtonVirtualKeycode);
			}
		}

	private:
		[[nodiscard]] constexpr auto GetMappingAt(const std::size_t index) noexcept -> const MappingContainer&
		{
			return m_mappings[index];
		}

		[[nodiscard]] constexpr auto GetMappingAt(const std::optional<std::size_t> index) noexcept -> const MappingContainer&
		{
			return m_mappings[index];
		}

		/**
		 * \brief Used to remove VKs with an exclusivity grouping that another state update VK already has. Processed from begin to end, so the first processed VK will be the left-most and
		 *	duplicates to the right will be removed.
		 * \remarks This is essential because processing more than one exclusivity grouping having mapping in a single iteration of a filter will mean the first ex. group vks were not actually processed
		 *	by the translator, yet their state would be updated by the filter incorrectly. Also, VKs in the state update must be unique! One VK per mapping is a hard precondition.
		 * \return "filtered" state update.
		 */
		[[nodiscard]] auto FilterStateUpdateForUniqueExclusivityGroups(SmallVector_t<VirtualCode_t>&& stateUpdate) -> SmallVector_t<VirtualCode_t>
		{
			using std::ranges::find_if;
			using std::ranges::find;
			using std::views::filter;
			using std::views::transform;
			using StateRange_t = std::remove_cvref_t<decltype(stateUpdate)>;

			const auto exGroupPred = [this](const auto vk) -> bool
			{
				const auto index = GetMappingIndexForVk(vk, m_mappings);
				if (index.has_value())
				{
					return GetMappingAt(*index).Button.ExclusivityGrouping.has_value() ? true : false;
				}
				return false;
			};

			// All the vks both in the mappings and has an exclusivity grouping.
			auto exGroupMappings = stateUpdate | filter(exGroupPred) | transform([this](const auto vk) -> std::optional<const MappingContainer&>
				{
					return GetMappingAt(static_cast<uint32_t>(GetMappingIndexForVk(vk, m_mappings)));
				});
			const auto groupSortPred = [&](const auto& lhs, const auto& rhs) -> bool
			{
				return lhs.Button.ExclusivityGrouping.value() < rhs.Button.ExclusivityGrouping.value();
			};
			std::ranges::sort(exGroupMappings, groupSortPred);
			std::ranges::unique(exGroupMappings);
			// todo add predicate for the unique operation and then transform it back to a range of vks.
			//for (const auto elem : m_groupIndexMap)
			//{

			//}

			//std::ranges::sort(exGroupVks, std::ranges::less{});

			// TODO split the mappings list into ex. groups, then sort (if necessary) and unique.
			//const auto splitGroups = m_mappingsList | std::views::split(groupingPred);

			//SmallVector_t<GrpVal_t> groupingValueBuffer;
			//StateRange_t virtualKeycodesToRemove;
			//groupingValueBuffer.reserve(stateUpdate.size());
			//virtualKeycodesToRemove.reserve(stateUpdate.size());

			//for (const auto vk : stateUpdate)
			//{
			//	// TODO simplify this.
			//	//const auto filteredMappingList = flux::ref(m_mappings).map(mappingIndexPred).filter(exGroupPred);//

			//	const auto mappingIndexOpt = GetMappingIndexForVk(vk, m_mappings);
			//	if (mappingIndexOpt.has_value())
			//	{
			//		const auto& foundMappingForVk = GetMappingAt(mappingIndexOpt.value());
			//		if (foundMappingForVk.Button.ExclusivityGrouping)
			//		{
			//			const auto grpVal = foundMappingForVk.Button.ExclusivityGrouping.value();
			//			auto& currentGroup = m_groupMap[grpVal];
			//			if (!currentGroup.IsMappingActivatedOrOvertaken(vk))
			//			{
			//				const auto groupingFindResult = find(groupingValueBuffer, grpVal);

			//				// If already in located, being handled groupings, add to remove buffer.
			//				if (!IsEnd(groupingValueBuffer, groupingFindResult)) // != cend(groupingValueBuffer))
			//					virtualKeycodesToRemove.push_back(vk);
			//				// Otherwise, add this new grouping to the grouping value buffer.
			//				else
			//					groupingValueBuffer.push_back(grpVal);
			//			}
			//		}
			//	}
			//}

			//EraseValuesFromRange(stateUpdate, virtualKeycodesToRemove);

			//return stateUpdate;
		}
	};
	static_assert(std::copyable<OvertakingFilter<>>);
	static_assert(std::movable<OvertakingFilter<>>);
	//static_assert(ValidFilterType_c<OvertakingFilter<>> == true);

}
