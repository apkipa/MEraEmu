#include "pch.h"
#include "CanvasVirtualControl.h"
#include "CanvasVirtualControl.g.cpp"

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::Xaml::Media::Imaging;

/* Design notes:
*
* - The engine presents its output to a VirtualSurfaceImageSource. Since VirtualSurfaceImageSource
* has a hard limit of 2^24 pixels on each dimension, and also the output stretched to become blurry
* when the size is too large (e.g. > 32768 pixels), we need to limit the size of the canvas, moving
* around the canvas as needed. Each time user scrolls, we determine where the new viewport lays
* inside the "split" regions of canvas:
*
* > +-----------------------------------+
* > |       Danger Zone (1024px)        |
* > +-----------------------------------+
* > |                                   |
* > |                                   |
* > |        Safe Zone (30720px)        |
* > |                                   |
* > |                                   |
* > +-----------------------------------+
* > |       Danger Zone (1024px)        |
* > +-----------------------------------+
*
* If the viewport intersects with the Danger Zone, we reposition the canvas so that the viewport is
* in the middle of the Safe Zone again. This way, we can avoid the blurry outcome.
* 
* Con: If the user has a very large screen, then this approach will not work. We may need to consider
*      a different approach in that case.
*/

#define CANVAS_DANGER_ZONE_HEIGHT 1024
#define CANVAS_SAFE_ZONE_HEIGHT ((32768 - CANVAS_DANGER_ZONE_HEIGHT * 2) / 2)
#define CANVAS_TOTAL_HEIGHT (CANVAS_DANGER_ZONE_HEIGHT * 2 + CANVAS_SAFE_ZONE_HEIGHT)

namespace winrt::MEraEmuWin::implementation {
    struct CanvasVirtualControlVsis : implements<CanvasVirtualControlVsis,
        ISurfaceImageSourceNative, IVirtualSurfaceImageSourceNative, ISurfaceImageSourceNativeWithD2D
    > {
        CanvasVirtualControlVsis(CanvasVirtualControl* parent) : m_parent(parent) {}

        void Close() {
            m_parent = nullptr;
        }

#define CHECK_CLOSED() if (!m_parent) { return E_FAIL; }

        // ----- ISurfaceImageSourceNative -----
        STDMETHOD(SetDevice)(IDXGIDevice* device) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->SetDevice(device);
        }
        STDMETHOD(BeginDraw)(RECT updateRect, IDXGISurface** surface, POINT* offset) override {
            CHECK_CLOSED();

            updateRect.top -= m_parent->m_yoffset;
            updateRect.bottom -= m_parent->m_yoffset;

            return m_parent->m_inner_vsis_noref->BeginDraw(updateRect, surface, offset);
        }
        STDMETHOD(ISurfaceImageSourceNative::EndDraw)(void) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->EndDraw();
        }
        // ----- IVirtualSurfaceImageSourceNative -----
        STDMETHOD(Invalidate)(RECT updateRect) override {
            CHECK_CLOSED();

            updateRect.top = std::max(updateRect.top, (LONG)m_parent->m_yoffset);
            updateRect.bottom = std::min(updateRect.bottom, (LONG)m_parent->m_full_height);
            updateRect.top -= m_parent->m_yoffset;
            updateRect.bottom -= m_parent->m_yoffset;
            return m_parent->m_inner_vsis_noref->Invalidate(updateRect);
        }
        STDMETHOD(GetUpdateRectCount)(DWORD* count) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->GetUpdateRectCount(count);
        }
        STDMETHOD(GetUpdateRects)(RECT* updates, DWORD count) override {
            CHECK_CLOSED();

            auto hr = m_parent->m_inner_vsis_noref->GetUpdateRects(updates, count);
            if (SUCCEEDED(hr)) {
                const auto yoffset = m_parent->m_yoffset;
                for (DWORD i = 0; i < count; ++i) {
                    updates[i].top += yoffset;
                    updates[i].bottom += yoffset;
                }
            }
            return hr;
        }
        STDMETHOD(GetVisibleBounds)(RECT* bounds) override {
            CHECK_CLOSED();

            auto hr = m_parent->m_inner_vsis_noref->GetVisibleBounds(bounds);
            if (SUCCEEDED(hr)) {
                bounds->top += m_parent->m_yoffset;
                bounds->bottom += m_parent->m_yoffset;
            }
            return hr;
        }
        STDMETHOD(RegisterForUpdatesNeeded)(IVirtualSurfaceUpdatesCallbackNative* callback) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->RegisterForUpdatesNeeded(callback);
        }
        STDMETHOD(Resize)(INT newWidth, INT newHeight) override {
            CHECK_CLOSED();

            if (newHeight < (int)m_parent->m_yoffset) {
                m_parent->m_yoffset = newHeight;
                m_parent->m_yoffset = (m_parent->m_yoffset / 4) * 4; // Align to 4-pixel boundary
                auto ui_scale = m_parent->XamlRoot().RasterizationScale();
                m_parent->VirtualImage().Margin({ m_parent->m_xoffset / ui_scale, m_parent->m_yoffset / ui_scale, 0, 0 });
            }
            auto real_height = std::max(0, newHeight - (int)m_parent->m_yoffset);
            auto hr = m_parent->m_inner_vsis_noref->Resize(newWidth, real_height);
            if (SUCCEEDED(hr)) {
                // HACK: Must call InvalidateMeasure() for updates to be visible. This is an XAML bug.
                m_parent->VirtualImage().InvalidateMeasure();

                m_parent->m_full_width = newWidth;
                m_parent->m_full_height = newHeight;
            }
            return hr;
        }
        // ----- ISurfaceImageSourceNativeWithD2D -----
        STDMETHOD(SetDevice)(IUnknown* device) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_d2d_noref->SetDevice(device);
        }
        STDMETHOD(BeginDraw)(REFRECT updateRect, REFIID iid, void** updateObject, POINT* offset) override {
            CHECK_CLOSED();

            auto realUpdateRect = updateRect;
            realUpdateRect.top -= m_parent->m_yoffset;
            realUpdateRect.bottom -= m_parent->m_yoffset;

            return m_parent->m_inner_vsis_d2d_noref->BeginDraw(realUpdateRect, iid, updateObject, offset);
        }
        STDMETHOD(ISurfaceImageSourceNativeWithD2D::EndDraw)(void) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_d2d_noref->EndDraw();
        }
        STDMETHOD(SuspendDraw)(void) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_d2d_noref->SuspendDraw();
        }
        STDMETHOD(ResumeDraw)(void) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_d2d_noref->ResumeDraw();
        }

    private:
        CanvasVirtualControl* m_parent;
    };

    CanvasVirtualControl::CanvasVirtualControl() {}
    CanvasVirtualControl::~CanvasVirtualControl() {
        m_canvas_vsis->Close();
    }
    void CanvasVirtualControl::InitializeComponent() {
        CanvasVirtualControlT::InitializeComponent();

        auto vsis = VirtualSurfaceImageSource(0, 0);
        VirtualImage().Source(vsis);
        m_inner_vsis_noref = vsis.as<IVirtualSurfaceImageSourceNative>().get();
        m_inner_vsis_d2d_noref = vsis.as<ISurfaceImageSourceNativeWithD2D>().get();
        m_canvas_vsis = make_self<CanvasVirtualControlVsis>(this);

        EffectiveViewportChanged({ this, &CanvasVirtualControl::OnEffectiveViewportChanged });
    }
    void CanvasVirtualControl::OnEffectiveViewportChanged(IInspectable const&, EffectiveViewportChangedEventArgs const& e) {
        auto effective_viewport = e.EffectiveViewport();

        // TODO: Handle X-axis

        // TODO: This assumption is wrong. We should use the actual scale factor calculated from XAML
        //       layout system. (override MeasureOverride and ArrangeOverride)
        const auto ui_scale = XamlRoot().RasterizationScale();

        const auto danger_top = m_yoffset;
        const auto safe_top = m_yoffset + CANVAS_DANGER_ZONE_HEIGHT;
        const auto safe_bottom = safe_top + CANVAS_SAFE_ZONE_HEIGHT;
        const auto danger_bottom = safe_bottom + CANVAS_DANGER_ZONE_HEIGHT;
        const auto viewport_top = uint32_t(effective_viewport.Y * ui_scale);
        const auto viewport_bottom = uint32_t((effective_viewport.Y + effective_viewport.Height) * ui_scale);
        if (viewport_top < safe_top || viewport_bottom > safe_bottom) {
            // Reposition the canvas
            auto new_middle = int64_t(std::midpoint(viewport_top, viewport_bottom));
            auto new_top = new_middle - CANVAS_TOTAL_HEIGHT / 2;
            if (new_top < 0) {
                new_top = 0;
            }
            new_top = (new_top / 4) * 4; // Align to 4-pixel boundary
            if (m_yoffset != new_top) {
                m_yoffset = new_top;
                VirtualImage().Margin({ m_xoffset / ui_scale, m_yoffset / ui_scale, 0, 0 });
                m_inner_vsis_noref->Resize(0, 0);
                m_inner_vsis_noref->Resize(m_full_width, m_full_height - m_yoffset);
                //VirtualImage().InvalidateMeasure();
            }
        }
    }
    IVirtualSurfaceImageSourceNative* CanvasVirtualControl::GetVsisNative() const noexcept {
        return m_canvas_vsis.get();
    }
}
