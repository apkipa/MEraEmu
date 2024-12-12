#pragma once

#include "DevTools/SourcesTabWatchTabItem.g.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct SourcesTabWatchTabItem : SourcesTabWatchTabItemT<SourcesTabWatchTabItem> {
        SourcesTabWatchTabItem() = default;

        void Expression(hstring const& value) { m_expression = value; }
        hstring Expression() const { return m_expression; }
        void BriefValue(hstring const& value) { m_brief_value = value; }
        hstring BriefValue() const { return m_brief_value; }
        void ErrorInfo(hstring const& value) { m_error_info = value; }
        hstring ErrorInfo() const { return m_error_info; }
        void SubItems(Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem> const& value) { m_sub_items = value; }
        Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem> SubItems() const { return m_sub_items; }

    private:
        hstring m_expression;
        hstring m_brief_value;
        hstring m_error_info;
        Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem> m_sub_items{
            winrt::single_threaded_vector<DevTools::SourcesTabWatchTabItem>()
        };
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct SourcesTabWatchTabItem : SourcesTabWatchTabItemT<SourcesTabWatchTabItem, implementation::SourcesTabWatchTabItem> {};
}
