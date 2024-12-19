#pragma once

#include "DevTools/SourcesTabBreakpointItem.g.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct SourcesTabBreakpointItem : SourcesTabBreakpointItemT<SourcesTabBreakpointItem> {
        SourcesTabBreakpointItem() = default;

        bool IsImplicit() const { return m_is_implicit; }
        void IsImplicit(bool value) { m_is_implicit = value; }
        int32_t MarkerHandle() const { return m_marker_handle; }
        void MarkerHandle(int32_t value) { m_marker_handle = value; }
        bool IsEnabled() const { return m_is_enabled; }
        void IsEnabled(bool value) { m_is_enabled = value; }
        bool IsApplied() const { return m_is_applied; }
        void IsApplied(bool value) { m_is_applied = value; }
        hstring FuncName() const { return m_func_name; }
        void FuncName(hstring const& value) { m_func_name = value; }
        hstring FileName() const { return m_file_name; }
        void FileName(hstring const& value) { m_file_name = value; }
        uint32_t LineNumber() const { return m_line_number; }
        void LineNumber(uint32_t value) { m_line_number = value; }
        uint32_t ColumnNumber() const { return m_column_number; }
        void ColumnNumber(uint32_t value) { m_column_number = value; }
        uint32_t SrcSpanStart() const { return m_src_span_start; }
        void SrcSpanStart(uint32_t value) { m_src_span_start = value; }
        uint32_t SrcSpanLen() const { return m_src_span_len; }
        void SrcSpanLen(uint32_t value) { m_src_span_len = value; }
        uint32_t IpChunk() const { return m_ip_chunk; }
        void IpChunk(uint32_t value) { m_ip_chunk = value; }
        uint32_t IpOffset() const { return m_ip_offset; }
        void IpOffset(uint32_t value) { m_ip_offset = value; }

        auto& original_bytecode() { return m_orig_bytecode; }

    private:
        bool m_is_implicit{ false };
        int32_t m_marker_handle{ 0 };
        bool m_is_enabled{ true };
        bool m_is_applied{ false };
        hstring m_func_name;
        hstring m_file_name;
        uint32_t m_line_number{ 0 };
        uint32_t m_column_number{ 0 };
        uint32_t m_src_span_start{ 0 };
        uint32_t m_src_span_len{ 0 };
        uint32_t m_ip_chunk{ UINT32_MAX };
        uint32_t m_ip_offset{ UINT32_MAX };
        std::vector<uint8_t> m_orig_bytecode;
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct SourcesTabBreakpointItem : SourcesTabBreakpointItemT<SourcesTabBreakpointItem, implementation::SourcesTabBreakpointItem> {};
}
