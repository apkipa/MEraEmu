#pragma once

#include "DevTools/SourcesFileTabItem.g.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct SourcesFileTabItem : SourcesFileTabItemT<SourcesFileTabItem> {
        SourcesFileTabItem() = default;

        void FileName(hstring const& value) { m_file_name = value; }
        hstring FileName() const { return m_file_name; }
        void FullPath(hstring const& value) { m_full_path = value; }
        hstring FullPath() const { return m_full_path; }

    private:
        hstring m_file_name;
        hstring m_full_path;
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct SourcesFileTabItem : SourcesFileTabItemT<SourcesFileTabItem, implementation::SourcesFileTabItem> {};
}
