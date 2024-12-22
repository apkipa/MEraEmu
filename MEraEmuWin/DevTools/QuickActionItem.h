#pragma once

#include "DevTools/QuickActionItem.g.h"

#include "ffi/MEraEmuCore.rust.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct QuickActionItem : QuickActionItemT<QuickActionItem> {
        QuickActionItem() = default;

        hstring Title() { return m_title; }
        void Title(hstring const& value) { m_title = value; }
        int32_t Score() { return m_score; }
        void Score(int32_t value) { m_score = value; }
        Windows::UI::Xaml::Input::ICommand Command() { return m_command; }
        void Command(Windows::UI::Xaml::Input::ICommand const& value) { m_command = value; }

        auto& ranges() { return m_ranges; }

    private:
        hstring m_title;
        int32_t m_score;
        Windows::UI::Xaml::Input::ICommand m_command;
        std::vector<RustFuzzyStringMatchEntry> m_ranges;
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct QuickActionItem : QuickActionItemT<QuickActionItem, implementation::QuickActionItem> {};
}
