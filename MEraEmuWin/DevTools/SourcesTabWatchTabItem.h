#pragma once

#include "DevTools/SourcesTabWatchTabItem.g.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct SourcesTabWatchTabItem : SourcesTabWatchTabItemT<SourcesTabWatchTabItem> {
        SourcesTabWatchTabItem() = default;

#define PROPERTY_CHANGED_IMPL(mem_name, pub_name, ty)                                                           \
        ty pub_name() const { return mem_name; }                                                                \
        void pub_name(ty const& value) {                                                                        \
            if (mem_name != value) {                                                                            \
                mem_name = value;                                                                               \
                m_ev_PropertyChanged(*this, Windows::UI::Xaml::Data::PropertyChangedEventArgs{ L#pub_name });   \
            }                                                                                                   \
        }

        /*void Expression(hstring const& value) { m_expression = value; }
        hstring Expression() const { return m_expression; }
        void BriefValue(hstring const& value) { m_brief_value = value; }
        hstring BriefValue() const { return m_brief_value; }
        void ErrorInfo(hstring const& value) { m_error_info = value; }
        hstring ErrorInfo() const { return m_error_info; }
        void IsExpanded(bool value) { m_is_expanded = value; }
        bool IsExpanded() const { return m_is_expanded; }
        void SubItems(Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem> const& value) { m_sub_items = value; }
        Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem> SubItems() const { return m_sub_items; }*/

        PROPERTY_CHANGED_IMPL(m_expression, Expression, hstring);
        PROPERTY_CHANGED_IMPL(m_brief_value, BriefValue, hstring);
        PROPERTY_CHANGED_IMPL(m_error_info, ErrorInfo, hstring);
        PROPERTY_CHANGED_IMPL(m_is_expanded, IsExpanded, bool);
        PROPERTY_CHANGED_IMPL(m_sub_items, SubItems, Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem>);

#undef PROPERTY_CHANGED_IMPL

        event_token PropertyChanged(Windows::UI::Xaml::Data::PropertyChangedEventHandler const& handler) {
            return m_ev_PropertyChanged.add(handler);
        }
        void PropertyChanged(event_token const& token) noexcept {
            m_ev_PropertyChanged.remove(token);
        }

    private:
        hstring m_expression;
        hstring m_brief_value;
        hstring m_error_info;
        bool m_is_expanded{ false };
        Windows::Foundation::Collections::IVector<DevTools::SourcesTabWatchTabItem> m_sub_items{
            winrt::single_threaded_observable_vector<DevTools::SourcesTabWatchTabItem>()
        };
        event<Windows::UI::Xaml::Data::PropertyChangedEventHandler> m_ev_PropertyChanged;
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct SourcesTabWatchTabItem : SourcesTabWatchTabItemT<SourcesTabWatchTabItem, implementation::SourcesTabWatchTabItem> {};
}
