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
*/

#define CANVAS_DANGER_ZONE_HEIGHT 1024
#define CANVAS_SAFE_ZONE_HEIGHT 30720
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

            return m_parent->m_inner_vsis_noref->BeginDraw(updateRect, surface, offset);
        }
        STDMETHOD(ISurfaceImageSourceNative::EndDraw)(void) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->EndDraw();
        }
        // ----- IVirtualSurfaceImageSourceNative -----
        STDMETHOD(Invalidate)(RECT updateRect) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->Invalidate(updateRect);
        }
        STDMETHOD(GetUpdateRectCount)(DWORD* count) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->GetUpdateRectCount(count);
        }
        STDMETHOD(GetUpdateRects)(RECT* updates, DWORD count) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->GetUpdateRects(updates, count);
        }
        STDMETHOD(GetVisibleBounds)(RECT* bounds) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->GetVisibleBounds(bounds);
        }
        STDMETHOD(RegisterForUpdatesNeeded)(IVirtualSurfaceUpdatesCallbackNative* callback) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_noref->RegisterForUpdatesNeeded(callback);
        }
        STDMETHOD(Resize)(INT newWidth, INT newHeight) override {
            CHECK_CLOSED();

            // HACK: Must call InvalidateMeasure() for updates to be visible. This is an XAML bug.
            m_parent->VirtualImage().InvalidateMeasure();

            return m_parent->m_inner_vsis_noref->Resize(newWidth, newHeight);
        }
        // ----- ISurfaceImageSourceNativeWithD2D -----
        STDMETHOD(SetDevice)(IUnknown* device) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_d2d_noref->SetDevice(device);
        }
        STDMETHOD(BeginDraw)(REFRECT updateRect, REFIID iid, void** updateObject, POINT* offset) override {
            CHECK_CLOSED();

            return m_parent->m_inner_vsis_d2d_noref->BeginDraw(updateRect, iid, updateObject, offset);
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
    void CanvasVirtualControl::OnEffectiveViewportChanged(IInspectable const&, EffectiveViewportChangedEventArgs const&) {
        // TODO...
    }
    IVirtualSurfaceImageSourceNative* CanvasVirtualControl::GetVsisNative() const noexcept {
        return m_canvas_vsis.get();
    }
}
