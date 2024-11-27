#pragma once

#include "DevTools/SourcesTabCallStackTabItem.g.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct SourcesTabCallStackTabItem : SourcesTabCallStackTabItemT<SourcesTabCallStackTabItem> {
        SourcesTabCallStackTabItem() = default;

        void FuncName(hstring const& value) { m_func_name = value; }
        hstring FuncName() const { return m_func_name; }
        void FileName(hstring const& value) { m_file_name = value; }
        hstring FileName() const { return m_file_name; }
        void LineNumber(uint32_t value) { m_line_number = value; }
        uint32_t LineNumber() const { return m_line_number; }
        void ColumnNumber(uint32_t value) { m_column_number = value; }
        uint32_t ColumnNumber() const { return m_column_number; }
        void SrcSpanStart(uint32_t value) { m_src_span_start = value; }
        uint32_t SrcSpanStart() const { return m_src_span_start; }
        void SrcSpanLen(uint32_t value) { m_src_span_len = value; }
        uint32_t SrcSpanLen() const { return m_src_span_len; }
        void IsCurrent(bool value) { m_is_current = value; }
        bool IsCurrent() const { return m_is_current; }

    private:
        hstring m_func_name;
        hstring m_file_name;
        uint32_t m_line_number;
        uint32_t m_column_number;
        uint32_t m_src_span_start;
        uint32_t m_src_span_len;
        bool m_is_current;
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct SourcesTabCallStackTabItem : SourcesTabCallStackTabItemT<SourcesTabCallStackTabItem, implementation::SourcesTabCallStackTabItem> {};
}
