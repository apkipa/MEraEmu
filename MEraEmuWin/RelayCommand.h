#pragma once

#include "RelayCommand.g.h"

namespace winrt::MEraEmuWin::implementation {
    struct RelayCommand : RelayCommandT<RelayCommand> {
        RelayCommand() : m_handler(nullptr) {}
        RelayCommand(RelayCommandHandler handler) : m_handler(handler) {}
        RelayCommand(std::move_only_function<void(Windows::Foundation::IInspectable const&)> handler) : m_handler(std::move(handler)) {}

        void Execute(Windows::Foundation::IInspectable const& parameter) {
            if (m_handler) {
                m_handler(parameter);
            }
        }

        bool CanExecute(Windows::Foundation::IInspectable const& parameter) {
            return m_handler != nullptr;
        }

        event_token CanExecuteChanged(Windows::Foundation::EventHandler<Windows::Foundation::IInspectable> const& handler) {
            return m_ev_CanExecuteChanged.add(handler);
        }

        void CanExecuteChanged(event_token const& token) noexcept {
            m_ev_CanExecuteChanged.remove(token);
        }

    private:
        // NOTE: Not used; only for keeping the handlers alive
        event<Windows::Foundation::EventHandler<Windows::Foundation::IInspectable>> m_ev_CanExecuteChanged;
        std::move_only_function<void(Windows::Foundation::IInspectable const&)> m_handler;
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct RelayCommand : RelayCommandT<RelayCommand, implementation::RelayCommand> {};
}
