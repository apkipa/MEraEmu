#pragma once

struct SoundPlaybackSession {
    winrt::Windows::Media::Audio::AudioFileInputNode input_node{ nullptr };

    bool is_valid() const noexcept {
        return input_node != nullptr;
    }
};

// Sound playback hub for BGM and SFX playback management
struct SoundPlaybackHub {
    SoundPlaybackHub() : m_data(nullptr) {}
    ~SoundPlaybackHub() {}
    SoundPlaybackHub(std::nullptr_t) : m_data(nullptr) {}
    SoundPlaybackHub(SoundPlaybackHub const&) = default;
    SoundPlaybackHub(SoundPlaybackHub&&) = default;
    SoundPlaybackHub& operator=(SoundPlaybackHub const&) = default;

    operator bool() const noexcept {
        return m_data != nullptr;
    }

    static concurrency::task<SoundPlaybackHub> create_async() {
        winrt::Windows::Media::Audio::AudioGraphSettings settings(winrt::Windows::Media::Render::AudioRenderCategory::GameMedia);
        auto ag_result = co_await winrt::Windows::Media::Audio::AudioGraph::CreateAsync(settings);
        if (ag_result.Status() != winrt::Windows::Media::Audio::AudioGraphCreationStatus::Success) {
            HRESULT hr = ag_result.ExtendedError();
            throw winrt::hresult_error(hr, winrt::format(
                L"Failed to create audio graph: {}, hr = 0x{:08x}",
                to_hstring(ag_result.Status()), static_cast<uint32_t>(hr)
            ));
        }
        auto ag = ag_result.Graph();
        auto result = co_await ag.CreateDeviceOutputNodeAsync();
        if (result.Status() != winrt::Windows::Media::Audio::AudioDeviceNodeCreationStatus::Success) {
            HRESULT hr = result.ExtendedError();
            throw winrt::hresult_error(hr, winrt::format(
                L"Failed to create device output node: {}, hr = 0x{:08x}",
                to_hstring(result.Status()), static_cast<uint32_t>(hr)
            ));
        }
        auto hub = SoundPlaybackHub(std::make_shared<SharedData>());
        ag.UnrecoverableErrorOccurred([weak_data = std::weak_ptr(hub.m_data)](auto&&, auto&& e) {
            if (auto data = weak_data.lock()) {
                SoundPlaybackHub hub(std::move(data));
                hub.exception_boundary([&] {
                    HRESULT hr = E_FAIL;
                    throw winrt::hresult_error(hr, winrt::format(
                        L"Unrecoverable error occurred in audio graph: {}",
                        SoundPlaybackHub::to_hstring(e.Error())
                    ));
                });
            }
        });
        hub.m_data->audio_graph = std::move(ag);
        hub.m_data->device_output_node = result.DeviceOutputNode();
        co_return hub;
    }

    // BGM playback; only one BGM can be played at a time
    void play_bgm(winrt::hstring const& path) noexcept {
        exception_boundary([&] {
            stop_bgm();
            return create_new_session_by_id_async(path, 0);
        });
    }

    void stop_bgm() noexcept {
        exception_boundary([&] {
            if (auto it = m_data->sessions.find(0); it != m_data->sessions.end()) {
                it->second.input_node.Stop();
                drop_background(std::move(it->second.input_node));
                m_data->sessions.erase(it);
            }
            trim();
        });
    }

    // SFX playback; multiple SFX can be played at the same time
    size_t play_sfx(winrt::hstring const& path) noexcept {
        size_t new_id;
        exception_boundary([&] {
            new_id = get_new_session_id();
            return create_new_session_by_id_async(path, new_id, 1);
        });
        return new_id;
    }

    void stop_sfx(size_t id) noexcept {
        exception_boundary([&] {
            if (id == 0) {
                throw winrt::hresult_invalid_argument(L"Reserved ID 0 cannot be used for SFX");
            }
            if (auto it = m_data->sessions.find(id); it != m_data->sessions.end()) {
                it->second.input_node.Stop();
                drop_background(std::move(it->second.input_node));
                m_data->sessions.erase(it);
            }
            trim();
        });
    }

    void stop_all_sfx() noexcept {
        exception_boundary([&] {
            for (auto it = m_data->sessions.begin(); it != m_data->sessions.end();) {
                if (it->first != 0) {
                    it->second.input_node.Stop();
                    drop_background(std::move(it->second.input_node));
                    it = m_data->sessions.erase(it);
                }
                else {
                    ++it;
                }
            }
        });
    }

    void stop_all() noexcept {
        exception_boundary([&] {
            stop_bgm();
            stop_all_sfx();
        });
    }

    void set_bgm_volume(double volume) noexcept {
        exception_boundary([&] {
            if (auto it = m_data->sessions.find(0); it != m_data->sessions.end()) {
                it->second.input_node.OutgoingGain(volume);
            }
        });
    }

    void set_all_sfx_volume(double volume) noexcept {
        exception_boundary([&] {
            for (auto& [id, session] : m_data->sessions) {
                if (id != 0) {
                    session.input_node.OutgoingGain(volume);
                }
            }
        });
    }

    double get_output_volume() const noexcept {
        return m_data->device_output_node.OutgoingGain();
    }

    void set_output_volume(double volume) noexcept {
        m_data->device_output_node.OutgoingGain(volume);
    }

    void register_on_exception(std::move_only_function<void(std::exception_ptr)> fn) {
        m_data->dispatcher_queue = winrt::Windows::System::DispatcherQueue::GetForCurrentThread();
        m_data->on_exception_handler = std::move(fn);
    }

private:
    winrt::Windows::Foundation::IAsyncAction create_new_session_by_id_async(
        winrt::hstring const& path,
        size_t id,
        winrt::Windows::Foundation::IReference<int32_t> loop_count = nullptr
    ) {
        auto& session = m_data->sessions[id];
        if (session.is_valid()) {
            throw winrt::hresult_invalid_argument(L"Session ID already exists");
        }

        auto se_session = tenkai::cpp_utils::ScopeExit([&] {
            m_data->sessions.erase(id);
        });

#if 0
        using namespace winrt;
        using namespace Windows::Foundation;
        using namespace Windows::Storage;
        using namespace Windows::Storage::Streams;

        struct CustomStorageFile : implements<CustomStorageFile,
            IStorageFile,
            IStorageItem
        > {
            CustomStorageFile(hstring const& path) : m_path(path) {}

            FileAttributes Attributes() const noexcept { return {}; }
            DateTime DateCreated() const noexcept { return {}; }
            hstring Path() const noexcept { return m_path; }
            hstring Name() const noexcept { return m_path; }

            IAsyncAction DeleteAsync() const noexcept { return nullptr; }
            IAsyncAction DeleteAsync(StorageDeleteOption) const noexcept { return nullptr; }
            IAsyncOperation<FileProperties::BasicProperties> GetBasicPropertiesAsync() const noexcept { return nullptr; }
            bool IsOfType(StorageItemTypes t) const noexcept { return t == StorageItemTypes::File; }
            IAsyncAction RenameAsync(hstring const&) const noexcept { return nullptr; }
            IAsyncAction RenameAsync(hstring const&, NameCollisionOption) const noexcept { return nullptr; }

            hstring ContentType() const noexcept { return {}; }
            hstring FileType() const noexcept { return {}; }
            IAsyncAction CopyAndReplaceAsync(IStorageFile const&) const noexcept { return nullptr; }
            IAsyncOperation<StorageFile> CopyAsync(IStorageFolder const) const noexcept { return nullptr; }
            IAsyncOperation<StorageFile> CopyAsync(IStorageFolder const&, hstring const) const noexcept { return nullptr; }
            IAsyncOperation<StorageFile> CopyAsync(IStorageFolder const&, hstring const, NameCollisionOption) const noexcept { return nullptr; }
            IAsyncAction MoveAndReplaceAsync(IStorageFile const&) const noexcept { return nullptr; }
            IAsyncAction MoveAsync(IStorageFolder const) const noexcept { return nullptr; }
            IAsyncAction MoveAsync(IStorageFolder const&, hstring const) const noexcept { return nullptr; }
            IAsyncAction MoveAsync(IStorageFolder const&, hstring const, NameCollisionOption) const noexcept { return nullptr; }
            IAsyncOperation<Streams::IRandomAccessStream> OpenAsync(FileAccessMode mode) const noexcept {
                struct Win32FileReadOnlyRandomAccessStream :
                    winrt::implements<Win32FileReadOnlyRandomAccessStream,
                    IRandomAccessStream, IClosable, IInputStream, IOutputStream>
                {
                    Win32FileReadOnlyRandomAccessStream(
                        winrt::file_handle hfile,
                        std::function<winrt::file_handle(winrt::file_handle const&)> file_cloner) :
                        m_hfile(std::move(hfile)), m_file_cloner(std::move(file_cloner)) {
                    }
                    Win32FileReadOnlyRandomAccessStream(winrt::file_handle hfile,
                        std::function<winrt::file_handle(winrt::file_handle const&)> file_cloner,
                        uint64_t position) :
                        Win32FileReadOnlyRandomAccessStream(std::move(hfile), std::move(file_cloner))
                    {
                        this->Seek(position);
                    }
                    ~Win32FileReadOnlyRandomAccessStream() { Close(); }
                    void Close() { m_hfile.close(); }
                    IAsyncOperationWithProgress<IBuffer, uint32_t> ReadAsync(
                        IBuffer buffer, uint32_t count, InputStreamOptions options
                    ) {
                        // TODO: Maybe optimize thread scheduling
                        //co_await winrt::resume_background();
                        buffer.Length(count);
                        auto actual_size = buffer.Length();
                        DWORD dwRead;
                        winrt::check_bool(ReadFile(
                            m_hfile.get(), buffer.data(), actual_size, &dwRead, nullptr));
                        buffer.Length(dwRead);
                        co_return buffer;
                    }
                    IAsyncOperationWithProgress<uint32_t, uint32_t> WriteAsync(IBuffer const& buffer) {
                        throw winrt::hresult_illegal_method_call();
                    }
                    IAsyncOperation<bool> FlushAsync() {
                        co_return true;
                    }
                    uint64_t Size() {
                        LARGE_INTEGER li;
                        winrt::check_bool(GetFileSizeEx(m_hfile.get(), &li));
                        return static_cast<uint64_t>(li.QuadPart);
                    }
                    void Size(uint64_t value) {
                        throw winrt::hresult_illegal_method_call();
                    }
                    ::winrt::Windows::Storage::Streams::IInputStream GetInputStreamAt(uint64_t position) {
                        return winrt::make<Win32FileReadOnlyRandomAccessStream>(
                            m_file_cloner(m_hfile), m_file_cloner, position);
                    }
                    ::winrt::Windows::Storage::Streams::IOutputStream GetOutputStreamAt(uint64_t position) {
                        throw winrt::hresult_illegal_method_call();
                    }
                    uint64_t Position() {
                        LARGE_INTEGER li;
                        winrt::check_bool(SetFilePointerEx(m_hfile.get(), {}, &li, FILE_CURRENT));
                        return static_cast<uint64_t>(li.QuadPart);
                    }
                    void Seek(uint64_t position) {
                        winrt::check_bool(SetFilePointerEx(
                            m_hfile.get(),
                            { .QuadPart = static_cast<LONGLONG>(position) },
                            nullptr,
                            FILE_BEGIN)
                        );
                    }
                    IRandomAccessStream CloneStream() {
                        return winrt::make<Win32FileReadOnlyRandomAccessStream>(
                            m_file_cloner(m_hfile), m_file_cloner);
                    }
                    bool CanRead() { return true; }
                    bool CanWrite() { return false; }
                private:
                    winrt::file_handle m_hfile;
                    std::function<winrt::file_handle(winrt::file_handle const&)> m_file_cloner;
                };
                file_handle fh{
                    CreateFileW(m_path.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr)
                };
                co_return winrt::make<Win32FileReadOnlyRandomAccessStream>(std::move(fh), [](auto const& hfile) {
                    HANDLE newhfile;
                    winrt::check_bool(DuplicateHandle(GetCurrentProcess(), hfile.get(), GetCurrentProcess(), &newhfile, 0, TRUE, DUPLICATE_SAME_ACCESS));
                    return winrt::file_handle(newhfile);
                });
            }
            IAsyncOperation<StorageStreamTransaction> OpenTransactedWriteAsync() const noexcept { return nullptr; }

        private:
            hstring m_path;
        };

        // Load file
        auto real_path = hstring(util::fs::to_absolute_path(path.c_str()));
        auto file = winrt::make<CustomStorageFile>(real_path);
        auto result = co_await m_data->audio_graph.CreateFileInputNodeAsync(file);
        if (result.Status() != winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::Success) {
            HRESULT hr = result.ExtendedError();
            throw winrt::hresult_error(hr, winrt::format(
                L"Failed to create file input node: {}, hr = 0x{:08x}",
                to_hstring(result.Status()), static_cast<uint32_t>(hr)
            ));
        }
        session.input_node = result.FileInputNode();
        bool is_bgm = id == 0;
#endif

        // Load file
        auto file = winrt::Tenkai::Storage::StorageFile::GetFileFromPathUnchecked(path);
        auto result = co_await m_data->audio_graph.CreateFileInputNodeAsync(file);
        if (result.Status() != winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::Success) {
            HRESULT hr = result.ExtendedError();
            throw winrt::hresult_error(hr, winrt::format(
                L"Failed to create file input node: {}, hr = 0x{:08x}",
                to_hstring(result.Status()), static_cast<uint32_t>(hr)
            ));
        }
        session.input_node = result.FileInputNode();
        bool is_bgm = id == 0;

        // Set parameters
        session.input_node.LoopCount(loop_count);
        if (!is_bgm) {
            // Auto-dispose SFX
            session.input_node.FileCompleted([id, weak_data = std::weak_ptr(m_data)](auto&&, auto&&) {
                if (auto data = weak_data.lock()) {
                    SoundPlaybackHub hub(std::move(data));
                    hub.stop_sfx(id);
                }
            });
        }

        // Connect to output
        session.input_node.AddOutgoingConnection(m_data->device_output_node);

        m_data->audio_graph.Start();

        se_session.release();
    }
    /*void create_new_session_by_id(winrt::hstring const& path, size_t id) {
        exception_boundary([&] {
            return create_new_session_by_id_async(path, id);
        });
    }*/
    //winrt::Windows::Foundation::IAsyncOperation<size_t> create_new_session_async(winrt::hstring const& path) {
    //    // NOTE: 0 is reserved for BGM, which is managed separately
    //    auto new_id = m_sessions.empty() ? 1 : m_sessions.rbegin()->first + 1;
    //    co_await create_new_session_by_id_async(path, new_id);
    //    co_return new_id;
    //}
    size_t get_new_session_id() const noexcept {
        // NOTE: 0 is reserved for BGM, which is managed separately
        return m_data->sessions.empty() ? 1 : m_data->sessions.rbegin()->first + 1;
    }
    template <typename F>
    auto exception_boundary(F&& f) {
        using RetT = std::invoke_result_t<F>;

        auto emit_ex_fn = [](std::shared_ptr<SharedData> const& data, std::exception_ptr ex) {
            if (data->on_exception_handler) {
                if (!data->dispatcher_queue || data->dispatcher_queue.HasThreadAccess()) {
                    data->on_exception_handler(std::move(ex));
                }
                else {
                    data->dispatcher_queue.TryEnqueue([data, ex]() {
                        data->on_exception_handler(std::move(ex));
                    });
                }
            }
        };

        try {
            if constexpr (std::is_convertible_v<RetT, winrt::Windows::Foundation::IAsyncInfo>) {
                f().Completed([data = m_data, emit_ex_fn](auto&& op, auto&&) {
                    try {
                        op.GetResults();
                    }
                    catch (...) {
                        emit_ex_fn(data, std::current_exception());
                    }
                });
            }
            else {
                f();
            }
        }
        catch (...) {
            emit_ex_fn(m_data, std::current_exception());
        }
    }
    void trim() {
        if (m_data->sessions.empty()) {
            m_data->audio_graph.Stop();
            //m_data->audio_graph.ResetAllNodes();
        }
    }

    static winrt::hstring to_hstring(winrt::Windows::Media::Audio::AudioFileNodeCreationStatus value) {
        switch (value) {
        case winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::Success: return L"Success";
        case winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::FileNotFound: return L"FileNotFound";
        case winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::InvalidFileType: return L"InvalidFileType";
        case winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::FormatNotSupported: return L"FormatNotSupported";
        case winrt::Windows::Media::Audio::AudioFileNodeCreationStatus::UnknownFailure: return L"UnknownFailure";
        default: return L"<Unknown>";
        }
    }
    static winrt::hstring to_hstring(winrt::Windows::Media::Audio::AudioDeviceNodeCreationStatus value) {
        switch (value) {
        case winrt::Windows::Media::Audio::AudioDeviceNodeCreationStatus::Success: return L"Success";
        case winrt::Windows::Media::Audio::AudioDeviceNodeCreationStatus::DeviceNotAvailable: return L"DeviceNotAvailable";
        case winrt::Windows::Media::Audio::AudioDeviceNodeCreationStatus::FormatNotSupported: return L"FormatNotSupported";
        case winrt::Windows::Media::Audio::AudioDeviceNodeCreationStatus::UnknownFailure: return L"UnknownFailure";
        default: return L"<Unknown>";
        }
    }
    static winrt::hstring to_hstring(winrt::Windows::Media::Audio::AudioGraphCreationStatus value) {
        switch (value) {
        case winrt::Windows::Media::Audio::AudioGraphCreationStatus::Success: return L"Success";
        case winrt::Windows::Media::Audio::AudioGraphCreationStatus::DeviceNotAvailable: return L"DeviceNotAvailable";
        case winrt::Windows::Media::Audio::AudioGraphCreationStatus::FormatNotSupported: return L"FormatNotSupported";
        case winrt::Windows::Media::Audio::AudioGraphCreationStatus::UnknownFailure: return L"UnknownFailure";
        default: return L"<Unknown>";
        }
    }
    static winrt::hstring to_hstring(winrt::Windows::Media::Audio::AudioGraphUnrecoverableError value) {
        switch (value) {
        case winrt::Windows::Media::Audio::AudioGraphUnrecoverableError::None: return L"None";
        case winrt::Windows::Media::Audio::AudioGraphUnrecoverableError::AudioDeviceLost: return L"AudioDeviceLost";
        case winrt::Windows::Media::Audio::AudioGraphUnrecoverableError::AudioSessionDisconnected: return L"AudioSessionDisconnected";
        case winrt::Windows::Media::Audio::AudioGraphUnrecoverableError::UnknownFailure: return L"UnknownFailure";
        default: return L"<Unknown>";
        }
    }

    struct SharedData {
        winrt::Windows::Media::Audio::AudioGraph audio_graph{ nullptr };
        winrt::Windows::Media::Audio::AudioDeviceOutputNode device_output_node{ nullptr };
        std::map<size_t, SoundPlaybackSession> sessions;
        winrt::Windows::System::DispatcherQueue dispatcher_queue{ nullptr };
        std::move_only_function<void(std::exception_ptr)> on_exception_handler;

        ~SharedData() {
            audio_graph.Close();
        }
    };
    std::shared_ptr<SharedData> m_data;

    SoundPlaybackHub(std::shared_ptr<SharedData> data) : m_data(std::move(data)) {}
};
