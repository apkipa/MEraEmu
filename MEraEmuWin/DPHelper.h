// Tenkai.UWP C++ internal APIs header
// Copyright (c) apkipa. All rights reserved.

#pragma once

// A convenience header to provide macros to make defining DependencyProperty easier.
// To use this header, first define the user parameters below. Then, inside a class
// definition, use the following macros to define a DependencyProperty:
// - DP_DECLARE_PROP(name): Declare a DependencyProperty with the name `name`.
// - DP_DECLARE_PROP_METHOD(name): Declare a static method to get the DependencyProperty.
// - DP_DEFINE_GETSETTER(name, ...): Define the type-safe getter and setter for the DependencyProperty.
//
// Then, outside the class definition, use the following macros. Prior to using these
// macros, define `DP_CLASS` to be the class name that the DependencyProperty is being
// defined for.
// - DP_DEFINE_PROP(name, ...): Define the DependencyProperty with the name `name` and
//   the specified PropertyMetadata.

// ----- Start of User Parameters -----
//#define DP_NAMESPACE ::winrt::Tenkai
//#define DP_CLASS SampleControl1
// ----- End of User Parameters -----

#define DP_DECLARE_PROP(name)   \
    static ::winrt::Windows::UI::Xaml::DependencyProperty m_ ## name ## Property
#define DP_DECLARE_PROP_METHOD(name)                                            \
    static ::winrt::Windows::UI::Xaml::DependencyProperty name ## Property() {  \
        return m_ ## name ## Property;                                          \
    }
#define DP_DEFINE_GETSETTER(name, type)                                         \
    std::remove_cvref_t<type> name() {                                          \
        using RawT = std::remove_cvref_t<type>;                                 \
        return ::winrt::unbox_value<RawT>(GetValue(m_ ## name ## Property));    \
    }                                                                           \
    void name(type value) {                                                     \
        using ::winrt::Windows::Foundation::IInspectable;                       \
        using RawT = std::remove_cvref_t<type>;                                 \
        if constexpr (std::is_base_of_v<IInspectable, RawT>) {                  \
            SetValue(m_ ## name ## Property, box_value(value));                 \
        }                                                                       \
        else {                                                                  \
            if (name() == value) { return; }                                    \
            SetValue(m_ ## name ## Property, box_value(value));                 \
        }                                                                       \
    }
#define DP_DEFINE_PROP_INNER_TY(method, name, ty, ...)                                  \
    ::winrt::Windows::UI::Xaml::DependencyProperty DP_CLASS::m_ ## name ## Property =   \
        ::winrt::Windows::UI::Xaml::DependencyProperty::method(                         \
            L"" #name,                                                                  \
            ::winrt::xaml_typename<ty>(),                                               \
            ::winrt::xaml_typename<DP_NAMESPACE::DP_CLASS>(),                           \
            ::winrt::Windows::UI::Xaml::PropertyMetadata{ __VA_ARGS__ }                 \
        )
#define DP_DEFINE_PROP_INNER(method, name, ...)                                                     \
    DP_DEFINE_PROP_INNER_TY(method, name, decltype(std::declval<DP_CLASS>().name()), __VA_ARGS__)
#define DP_DEFINE_PROP(name, ...) DP_DEFINE_PROP_INNER(Register, name, __VA_ARGS__)
#define DP_DEFINE_PROP_TY(name, ty, ...) DP_DEFINE_PROP_INNER_TY(Register, name, ty, __VA_ARGS__)
#define DP_DEFINE_ATTACHED_PROP(name, ...) DP_DEFINE_PROP_INNER(RegisterAttached, name, __VA_ARGS__)
#define DP_DEFINE_ATTACHED_PROP_TY(name, ty, ...) DP_DEFINE_PROP_INNER_TY(RegisterAttached, name, ty, __VA_ARGS__)
