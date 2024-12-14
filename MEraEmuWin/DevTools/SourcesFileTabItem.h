#pragma once

#include "DevTools/SourcesFileTabItem.g.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct SourcesFileTabItem : SourcesFileTabItemT<SourcesFileTabItem> {
        SourcesFileTabItem() = default;

        void DocType(hstring const& value) { m_doc_type = value; }
        hstring DocType() const { return m_doc_type; }
        void FileName(hstring const& value) { m_file_name = value; }
        hstring FileName() const { return m_file_name; }
        void FullPath(hstring const& value) { m_full_path = value; }
        hstring FullPath() const { return m_full_path; }
        void IsInited(bool value) { m_is_inited = value; }
        bool IsInited() const { return m_is_inited; }

    private:
        hstring m_doc_type;
        hstring m_file_name;
        hstring m_full_path;
        bool m_is_inited{};
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct SourcesFileTabItem : SourcesFileTabItemT<SourcesFileTabItem, implementation::SourcesFileTabItem> {};
}
