#pragma once

// A utility header that is meant to be portable across different projects.

#include <atomic>
#include <memory>
#include <utility>
#include <optional>
#include <vector>
#include <mutex>
#include <future>
#include <format>
#include <winrt/base.h>
#include <Tenkai.hpp>
//#include <robuffer.h>

// Suppress warning C4324: structure was padded due to alignment specifier
// Suppress warning C4456: declaration of 'val' hides previous local declaration
#pragma warning(push)
#pragma warning(disable: 4324 4456)

namespace util {
    namespace sync {
        namespace spsc {
            namespace details {
                template <typename T>
                struct RingBuffer {
                    RingBuffer(size_t size) : size(size), buffer(std::make_unique<T[]>(size)) {}
                    RingBuffer(std::nullptr_t) : size(0), buffer(nullptr) {}
                    RingBuffer(RingBuffer const&) = delete;
                    RingBuffer(RingBuffer&& other) : size(other.size), buffer(std::move(other.buffer)),
                        read_idx(other.read_idx), write_idx(other.write_idx) {
                    }
                    ~RingBuffer() = default;

                    RingBuffer& operator=(RingBuffer other) {
                        std::swap(size, other.size);
                        std::swap(buffer, other.buffer);
                        std::swap(read_idx, other.read_idx);
                        std::swap(write_idx, other.write_idx);
                        return *this;
                    }

                    bool empty() const {
                        return read_idx == write_idx;
                    }

                    bool full() const {
                        return reduce_size(write_idx + 1) == read_idx;
                    }

                    // If push succeeds, the value is moved
                    bool try_push(T& val) {
                        if (full()) {
                            return false;
                        }
                        buffer[write_idx] = std::move(val);
                        write_idx = reduce_size(write_idx + 1);
                        return true;
                    }

                    std::optional<T> pop() {
                        if (empty()) {
                            return std::nullopt;
                        }
                        auto val = std::move(buffer[read_idx]);
                        read_idx = reduce_size(read_idx + 1);
                        return val;
                    }

                private:
                    size_t reduce_size(size_t value) const {
                        if (value >= size) {
                            value -= size;
                        }
                        return value;
                    }

                    std::unique_ptr<T[]> buffer;
                    size_t size;
                    size_t read_idx{};
                    size_t write_idx{};
                };

                template <typename T>
                struct alignas(std::hardware_destructive_interference_size) BufferSide {
                    RingBuffer<T> buffer{ nullptr };
                };

                // Checks if the value has OCCUPIED bit set
                static const uint32_t OCCUPIED_FLAG = 0x80000000;
                static inline bool is_occupied(uint32_t val) {
                    return val & OCCUPIED_FLAG;
                }

                static inline void hint_spin_loop() {
#if defined(_M_IX86)
                    _mm_pause();
#elif defined(_M_X64)
                    _mm_pause();
#elif defined(_M_ARM) 
                    __yield();
#elif defined(_M_ARM64)
                    __yield();
#else
                    // Do nothing
#endif
                }

                template <typename T>
                struct Sender;
                template <typename T>
                struct Receiver;

                template <typename T>
                struct SyncChannelData : std::enable_shared_from_this<SyncChannelData<T>> {
                    static_assert(std::is_nothrow_move_constructible_v<T>, "T must be nothrow move constructible");

                    friend struct Sender<T>;
                    friend struct Receiver<T>;

                    SyncChannelData(size_t bound) : bound(bound) {
                        for (auto& buf : bufs) {
                            buf.buffer = RingBuffer<T>(bound);
                        }
                    }

                    size_t bound;
                    // Have events to process (/ items to consume)?
                    std::atomic_bool is_vacant{ true };
                    BufferSide<T> bufs[2];
                    // When acquired by the sender, it is set to OCCUPIED
                    alignas(std::hardware_destructive_interference_size) std::atomic_uint32_t current_buf_idx{};
                    alignas(std::hardware_destructive_interference_size) uint32_t cached_reader_current_buf_idx {};

                    std::atomic_size_t sender_count{};
                    std::atomic_size_t receiver_count{};

                    bool is_open() const {
                        return sender_count.load(std::memory_order_relaxed) > 0 &&
                            receiver_count.load(std::memory_order_relaxed) > 0;
                    }
                };
            }

            template <typename T>
            struct Sender {
                Sender(std::shared_ptr<details::SyncChannelData<T>> data_) : data(std::move(data_)) {
                    data->sender_count++;
                }
                Sender(std::nullptr_t) : data(nullptr) {}
                Sender(Sender const& other) : data(other.data) {
                    data->sender_count++;
                }
                Sender(Sender&& other) noexcept : data(std::move(other.data)) {
                    other.data = nullptr;
                }
                ~Sender() {
                    if (!data) { return; }

                    if (data->sender_count.fetch_sub(1, std::memory_order_relaxed) == 1) {
                        data->is_vacant.store(false, std::memory_order_release);
                        data->is_vacant.notify_all();
                    }
                }

                Sender& operator=(Sender other) {
                    std::swap(data, other.data);
                    return *this;
                }

                bool is_vacant() const {
                    return data->is_vacant.load(std::memory_order_relaxed);
                }

                std::atomic_bool const& is_vacant_var() const {
                    return data->is_vacant;
                }

                // If send succeeds, the value is moved
                bool send(T& val) const {
                    while (true) {
                        if (!data->is_open()) { return false; }

                        // Write to the next buffer
                        auto buf_idx = data->current_buf_idx.fetch_or(details::OCCUPIED_FLAG, std::memory_order_acquire);
                        auto& buf = data->bufs[1 - buf_idx].buffer;
                        if (!buf.try_push(val)) {
                            // Buffer is full.
                            // Wait for the receiver to flip the buffer
                            data->current_buf_idx.store(buf_idx, std::memory_order_release);
                            data->is_vacant.wait(false, std::memory_order_acquire);
                            continue;
                        }

                        // Notify the receiver
                        data->is_vacant.store(false, std::memory_order_relaxed);
                        data->is_vacant.notify_all();

                        // Release the buffer
                        data->current_buf_idx.store(buf_idx, std::memory_order_release);

                        break;
                    }

                    return true;
                }

                bool try_send(T& val) const {
                    if (!data->is_open()) { return false; }

                    // Write to the next buffer
                    bool succeeded = false;
                    auto buf_idx = data->current_buf_idx.fetch_or(details::OCCUPIED_FLAG, std::memory_order_acquire);
                    auto& buf = data->bufs[1 - buf_idx].buffer;
                    if (buf.try_push(val)) {
                        // Notify the receiver
                        data->is_vacant.store(false, std::memory_order_relaxed);
                        data->is_vacant.notify_all();
                        succeeded = true;
                    }
                    // Release the buffer
                    data->current_buf_idx.store(buf_idx, std::memory_order_release);

                    return succeeded;
                }

                bool is_open() const {
                    return data->is_open();
                }

            private:
                std::shared_ptr<details::SyncChannelData<T>> data;
            };
            template <typename T>
            struct Receiver {
                Receiver(std::shared_ptr<details::SyncChannelData<T>> data_) : data(std::move(data_)) {
                    data->receiver_count++;
                }
                Receiver(std::nullptr_t) : data(nullptr) {}
                Receiver(Receiver const& other) : data(other.data) {
                    data->receiver_count++;
                }
                Receiver(Receiver&& other) noexcept : data(std::move(other.data)) {
                    other.data = nullptr;
                }
                ~Receiver() {
                    if (!data) { return; }

                    if (data->receiver_count.fetch_sub(1, std::memory_order_relaxed) == 1) {
                        // Use `release` to ensure that the sender can see the closed state
                        data->is_vacant.store(true, std::memory_order_release);
                        data->is_vacant.notify_all();

                        // Drain the buffer
                        while (recv()) {}
                    }
                }

                Receiver& operator=(Receiver other) {
                    std::swap(data, other.data);
                    return *this;
                }

                bool is_vacant() const {
                    return data->is_vacant.load(std::memory_order_relaxed);
                }

                std::atomic_bool const& is_vacant_var() const {
                    return data->is_vacant;
                }

                std::optional<T> recv() const {
                    std::optional<T> val = std::nullopt;
                    while (true) {
                        if (val) { break; }

                        // Read from the current buffer
                        auto pure_idx = data->cached_reader_current_buf_idx;
                        auto& buf = data->bufs[pure_idx].buffer;
                        val = buf.pop();
                        if (!val || buf.empty()) {
                            // Current buffer is empty. Prepare to flip the buffer.
                            auto next_buf_idx = 1 - pure_idx;
                            data->is_vacant.store(true, std::memory_order_relaxed);
                            while (true) {
                                // NOTE: Use CAS here since sender's write is fast enough
                                // Try to flip the buffer when it is not occupied by the sender.
                                // After this, sender has either done writing to the buffer or did not write at all.
                                // This ensures that the sender can see `is_vacant` as true.
                                auto copy_pure_idx = pure_idx;
                                if (data->current_buf_idx.compare_exchange_weak(copy_pure_idx, next_buf_idx,
                                    std::memory_order_acq_rel, std::memory_order_relaxed))
                                {
                                    data->cached_reader_current_buf_idx = next_buf_idx;
                                    break;
                                }
                                details::hint_spin_loop();
                            }

                            auto& buf = data->bufs[next_buf_idx].buffer;
                            if (!buf.empty()) {
                                // The sender has written something to the buffer. Start over.
                                data->is_vacant.store(false, std::memory_order_relaxed);
                                continue;
                            }

                            // Nothing to read here. Exit if the sender is closed.
                            bool is_open = data->is_open();
                            data->is_vacant.notify_all();
                            if (!is_open) { return val; }

                            // Sender is alive at this point.
                            // Wait for the sender to write to or close the channel.
                            if (val) { break; }
                            data->is_vacant.wait(true, std::memory_order_acquire);
                            continue;
                        }
                        break;
                    }
                    return val;
                }

                std::optional<T> try_recv() const {
                    std::optional<T> val = std::nullopt;
                    while (true) {
                        if (val) { break; }

                        // Read from the current buffer
                        auto pure_idx = data->cached_reader_current_buf_idx;
                        auto& buf = data->bufs[pure_idx].buffer;
                        val = buf.pop();
                        if (!val || buf.empty()) {
                            // Current buffer is empty. Prepare to flip the buffer.
                            auto next_buf_idx = 1 - pure_idx;
                            data->is_vacant.store(true, std::memory_order_relaxed);
                            while (true) {
                                // NOTE: Use CAS here since sender's write is fast enough
                                // Try to flip the buffer when it is not occupied by the sender.
                                // After this, sender has either done writing to the buffer or did not write at all.
                                // This ensures that the sender can see `is_vacant` as true.
                                auto copy_pure_idx = pure_idx;
                                if (data->current_buf_idx.compare_exchange_weak(copy_pure_idx, next_buf_idx,
                                    std::memory_order_acq_rel, std::memory_order_relaxed))
                                {
                                    data->cached_reader_current_buf_idx = next_buf_idx;
                                    break;
                                }
                                details::hint_spin_loop();
                            }

                            auto& buf = data->bufs[next_buf_idx].buffer;
                            if (!buf.empty()) {
                                // The sender has written something to the buffer. Start over.
                                data->is_vacant.store(false, std::memory_order_relaxed);
                                continue;
                            }

                            // Nothing to read here. Just exit.
                            data->is_vacant.notify_all();
                        }
                        break;
                    }
                    return val;
                }

                bool is_open() const {
                    return data->is_open();
                }

            private:
                std::shared_ptr<details::SyncChannelData<T>> data;
            };

            /// <summary>
            /// A synchronous bounded channel with a single producer and a single consumer.
            /// The sender and receiver can perform blocking send and receive operations, and
            /// are aware of disconnection notifications. Can only be used in at most two threads,
            /// where one thread is the sender and the other is the receiver.
            /// </summary>
            /// <typeparam name="T">Type of message. Must be no-throw constructible.</typeparam>
            /// <param name="bound">The capacity of the channel.</param>
            /// <returns>A sender and a receiver.</returns>
            template <typename T>
            inline std::pair<Sender<T>, Receiver<T>> sync_channel(size_t bound) {
                if (bound == 0) {
                    bound = std::numeric_limits<size_t>::max();
                }
                auto data = std::make_shared<details::SyncChannelData<T>>(bound);
                return {
                    Sender<T>{data},
                    Receiver<T>{data}
                };
            }
        }
    }

    namespace fs {
        inline bool file_exists(const wchar_t* path) {
            return GetFileAttributesW(path) != INVALID_FILE_ATTRIBUTES;
        }

        inline std::wstring to_absolute_path(const wchar_t* path) {
            DWORD len = GetFullPathNameW(path, 0, nullptr, nullptr);
            if (len == 0) {
                return path;
            }
            std::wstring abs_path(len, L'\0');
            if (GetFullPathNameW(path, len, abs_path.data(), nullptr) == 0) {
                return path;
            }
            return abs_path;
        }
    }

    namespace winrt {
        namespace details {
            inline void __stdcall resume_background_callback(void*, void* context) noexcept try {
                auto f_ptr = static_cast<std::function<void()>*>(context);
                std::unique_ptr<std::function<void()>> f(f_ptr);
                (*f)();
            }
            catch (...) {
                // Ignore
            }

            struct __declspec(uuid("905a0fef-bc53-11df-8c49-001e4fc686da")) IBufferByteAccess : ::IUnknown {
                virtual HRESULT __stdcall Buffer(uint8_t** value) = 0;
            };
        }

        template <typename F>
        void resume_background(F f) {
            using namespace ::winrt::impl;
            auto f_ptr = std::make_unique<std::function<void()>>(std::move(f));
            submit_threadpool_callback(details::resume_background_callback, f_ptr.get());
            f_ptr.release();
        }

        template <typename T>
        inline auto apartment_aware_future(std::future<T> fut) {
            struct Awaiter {
                Awaiter(std::future<T> fut) : fut(std::move(fut)) {}
                bool await_ready() const { return fut.wait_for(std::chrono::seconds(0)) == std::future_status::ready; }
                T await_resume() {
                    if constexpr (std::is_same_v<T, void>) {
                        fut.get();
                    }
                    else {
                        return std::move(fut.get());
                    }
                }
                void await_suspend(std::coroutine_handle<> handle) {
                    ::winrt::apartment_context ui_ctx;
                    // FIXME: If too many tasks are queued, this will cause app to deadlock somewhere.
                    //        Use a lighter mechanism instead of `std::future`.
                    resume_background([=] {
                        auto awaiter = ::winrt::operator co_await(ui_ctx);
                        fut.wait();
                        awaiter.await_suspend(handle);
                    });
                }
                std::future<T> fut;
            };
            return Awaiter{ std::move(fut) };
        }

        template <typename T>
        inline auto apartment_aware_task(concurrency::task<T> task) {
            struct Awaiter {
                Awaiter(concurrency::task<T> task) : task(std::move(task)) {}
                bool await_ready() const { return task.is_done(); }
                T await_resume() {
                    if constexpr (std::is_same_v<T, void>) {
                        task.get();
                    }
                    else {
                        return task.get();
                    }
                }
                void await_suspend(std::coroutine_handle<> handle) {
                    ::winrt::apartment_context ui_ctx;
                    task.then([=](concurrency::task<T> completed_task) {
                        auto awaiter = ::winrt::operator co_await(ui_ctx);
                        awaiter.await_suspend(handle);
                    }, concurrency::task_continuation_context::use_default());
                }
                concurrency::task<T> task;
            };
            return Awaiter{ std::move(task) };
        }

        template <typename T>
        void drop_background(T t) {
            resume_background([t = std::move(t)] {
                try {
                    if constexpr (requires { t->Close(); }) {
                        t->Close();
                    }
                    if constexpr (requires { t.Close(); }) {
                        t.Close();
                    }
                }
                catch (...) {
                    // Ignore
                }
            });
        }

        struct fire_forget {
            struct promise_type {
                fire_forget get_return_object() const noexcept { return {}; }
                std::suspend_never initial_suspend() const noexcept { return {}; }
                std::suspend_never final_suspend() const noexcept { return {}; }
                void return_void() const noexcept {}
                void unhandled_exception() const noexcept {
                    auto ex_code = ::winrt::to_hresult();
                    auto ex_msg = ::winrt::to_message();
                    auto msg = std::format(L"fire_forget: Unobserved exception: 0x{:08x} {}", (uint32_t)ex_code, ex_msg);
                    OutputDebugStringW(msg.c_str());
                }
            };
        };

        // TODO: Support both Borrwoed and Owned forms
        /// A buffer wrapper.
        template <typename T>
        struct BufferWrapper : ::winrt::implements<BufferWrapper<T>,
            ::winrt::Windows::Storage::Streams::IBuffer,
            details::IBufferByteAccess>
        {
            BufferWrapper(T const& buffer) : m_buffer(buffer) {}
            uint32_t Capacity() const noexcept {
                return static_cast<uint32_t>(m_buffer.size());
            }
            uint32_t Length() const noexcept {
                return static_cast<uint32_t>(m_buffer.size());
            }
            void Length(uint32_t) {
                throw ::winrt::hresult_not_implemented();
            }
            HRESULT __stdcall Buffer(uint8_t** value) override {
                *value = (uint8_t*)m_buffer.data();
                return S_OK;
            }
        private:
            T const& m_buffer;
        };

        template <typename T>
        auto make_buffer_wrapper(T const& buffer) {
            return ::winrt::make<BufferWrapper<T>>(buffer);
        }

        // Must be run on UI thread
        template<typename Functor, typename T>
        void run_when_loaded(Functor&& functor, T const& elem) {
            using ::winrt::Windows::UI::Xaml::RoutedEventArgs;
            using ::winrt::Windows::Foundation::IInspectable;
            //if constexpr (util::misc::is_pointer_like<T>) {
            if constexpr (false) {
                if (elem->IsLoaded()) { functor(elem); }
                else {
                    auto raw_ptr = ::winrt::get_abi(elem);
                    auto revoke_et = std::make_shared_for_overwrite<::winrt::event_token>();
                    *revoke_et = elem->Loaded(
                        [revoke_et, functor = std::forward<Functor>(functor), raw_ptr](
                            IInspectable const&, RoutedEventArgs const&)
                    {
                        T elem;
                        ::winrt::copy_from_abi(elem, raw_ptr);
                        raw_ptr->Loaded(*revoke_et);
                        functor(std::move(elem));
                    }
                    );
                }
            }
            else {
                constexpr bool is_implementation =
                    !std::is_same_v<decltype(::winrt::Windows::Foundation::IUnknown{}.as<T>()), T > ;
                if constexpr (is_implementation) {
                    return run_when_loaded(std::forward<Functor>(functor), &elem);
                }
                else {
                    if (elem.IsLoaded()) { functor(elem); }
                    else {
                        auto raw_ptr = ::winrt::get_abi(elem);
                        auto revoke_et = std::make_shared_for_overwrite<::winrt::event_token>();
                        *revoke_et = elem.Loaded(
                            [revoke_et, functor = std::forward<Functor>(functor), raw_ptr](
                                IInspectable const&, RoutedEventArgs const&)
                        {
                            T elem{ nullptr };
                            ::winrt::copy_from_abi(elem, raw_ptr);
                            elem.Loaded(*revoke_et);
                            functor(std::move(elem));
                        }
                        );
                    }
                }
            }
        }

        template <typename T>
        auto resume_when_loaded(T const& elem) {
            struct Awaiter {
                Awaiter(T const& elem) : elem(elem), m_succeeded(elem.IsLoaded()) {}
                ~Awaiter() {}
                bool await_ready() const { return m_succeeded; }
                void await_suspend(std::coroutine_handle<> handle) {
                    run_when_loaded([this, handle, se_handle = tenkai::cpp_utils::scope_exit([this, handle] {
                        // HACK: Workarounds a MSVC bug by adding `this->`
                        if (!this->m_succeeded) {
                            handle();
                        }
                    })](auto&&) {
                        m_succeeded = true;
                        handle();
                    }, elem);
                }
                void await_resume() {
                    if (!m_succeeded) {
                        throw ::winrt::hresult_changed_state();
                    }
                }
                T const& elem;
            private:
                bool m_succeeded;
            };
            return Awaiter{ elem };
        }
    }
}

#pragma warning(pop)

// Preludes
using util::winrt::drop_background;
using util::winrt::fire_forget;
