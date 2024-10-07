use bstr::{ByteSlice, ByteVec};

use crate::{types::*, util::bmatch_caseless};

#[must_use]
pub fn recognize_print_cmd(cmd: &[u8]) -> Option<(EraCmdArgFmt, EraPrintExtendedFlags)> {
    let cmd = cmd.to_ascii_uppercase();
    let mut cmd = cmd.as_slice();
    let arg_fmt;
    let mut flags = EraPrintExtendedFlags::new();
    cmd = cmd.strip_prefix(b"PRINT")?;
    // Handle PRINTPLAIN*
    if cmd.strip_prefix_inplace(b"PLAIN") {
        flags.set_force_plain(true);
        arg_fmt = if cmd.strip_prefix_inplace(b"FORM") {
            EraCmdArgFmt::RawStringForm
        } else {
            EraCmdArgFmt::RawString
        };
        return cmd.is_empty().then_some((arg_fmt, flags));
    }
    // Handle rest
    cmd.strip_prefix_inplace(b"SINGLE")
        .then(|| flags.set_is_single(true));
    if cmd.strip_prefix_inplace(b"V") {
        arg_fmt = EraCmdArgFmt::Expression;
    } else if cmd.strip_prefix_inplace(b"S") {
        arg_fmt = EraCmdArgFmt::ExpressionS;
    } else if cmd.strip_prefix_inplace(b"FORMS") {
        arg_fmt = EraCmdArgFmt::ExpressionSForm;
    } else if cmd.strip_prefix_inplace(b"FORM") {
        arg_fmt = EraCmdArgFmt::RawStringForm;
    } else {
        arg_fmt = EraCmdArgFmt::RawString;
    }
    if cmd.strip_prefix_inplace(b"LC") {
        flags.set_left_pad(true);
    } else if cmd.strip_prefix_inplace(b"C") {
        flags.set_right_pad(true);
    }
    cmd.strip_prefix_inplace(b"K")
        .then(|| flags.set_use_kana(true));
    cmd.strip_prefix_inplace(b"D")
        .then(|| flags.set_ignore_color(true));
    if cmd.strip_prefix_inplace(b"L") {
        flags.set_is_line(true);
    } else if cmd.strip_prefix_inplace(b"W") {
        flags.set_is_line(true);
        flags.set_is_wait(true);
    }
    cmd.is_empty().then_some((arg_fmt, flags))
}

#[must_use]
pub fn recognize_debugprint_cmd(cmd: &[u8]) -> Option<(EraCmdArgFmt, EraPrintExtendedFlags)> {
    let cmd = cmd.to_ascii_uppercase();
    let mut cmd = cmd.as_slice();
    let arg_fmt;
    let mut flags = EraPrintExtendedFlags::new();
    cmd = cmd.strip_prefix(b"DEBUGPRINT")?;
    // Handle PRINTPLAIN*
    if cmd.strip_prefix_inplace(b"PLAIN") {
        flags.set_force_plain(true);
        arg_fmt = if cmd.strip_prefix_inplace(b"FORM") {
            EraCmdArgFmt::RawStringForm
        } else {
            EraCmdArgFmt::RawString
        };
        return cmd.is_empty().then_some((arg_fmt, flags));
    }
    // Handle rest
    cmd.strip_prefix_inplace(b"SINGLE")
        .then(|| flags.set_is_single(true));
    if cmd.strip_prefix_inplace(b"V") {
        arg_fmt = EraCmdArgFmt::Expression;
    } else if cmd.strip_prefix_inplace(b"S") {
        arg_fmt = EraCmdArgFmt::ExpressionS;
    } else if cmd.strip_prefix_inplace(b"FORMS") {
        arg_fmt = EraCmdArgFmt::ExpressionSForm;
    } else if cmd.strip_prefix_inplace(b"FORM") {
        arg_fmt = EraCmdArgFmt::RawStringForm;
    } else {
        arg_fmt = EraCmdArgFmt::RawString;
    }
    if cmd.strip_prefix_inplace(b"LC") {
        flags.set_left_pad(true);
    } else if cmd.strip_prefix_inplace(b"C") {
        flags.set_right_pad(true);
    }
    cmd.strip_prefix_inplace(b"K")
        .then(|| flags.set_use_kana(true));
    cmd.strip_prefix_inplace(b"D")
        .then(|| flags.set_ignore_color(true));
    if cmd.strip_prefix_inplace(b"L") {
        flags.set_is_line(true);
    } else if cmd.strip_prefix_inplace(b"W") {
        flags.set_is_line(true);
        flags.set_is_wait(true);
    }
    cmd.is_empty().then_some((arg_fmt, flags))
}

#[must_use]
pub fn recognize_printdata_cmd(cmd: &[u8]) -> Option<EraPrintExtendedFlags> {
    let cmd = cmd.to_ascii_uppercase();
    let mut cmd = cmd.as_slice();
    let mut flags = EraPrintExtendedFlags::new();
    cmd = cmd.strip_prefix(b"PRINTDATA")?;
    // Handle rest
    cmd.strip_prefix_inplace(b"K")
        .then(|| flags.set_use_kana(true));
    cmd.strip_prefix_inplace(b"D")
        .then(|| flags.set_ignore_color(true));
    if cmd.strip_prefix_inplace(b"L") {
        flags.set_is_line(true);
    } else if cmd.strip_prefix_inplace(b"W") {
        flags.set_is_line(true);
        flags.set_is_wait(true);
    }
    cmd.is_empty().then_some(flags)
}

pub fn parse_int_literal(s: &[u8]) -> Option<i64> {
    let mut s = s;
    let radix = if s.strip_prefix_inplace(b"0x") || s.strip_prefix_inplace(b"0X") {
        16
    } else if s.strip_prefix_inplace(b"0b") || s.strip_prefix_inplace(b"0B") {
        2
    } else if let Some(x) = s.strip_prefix(b"0") {
        if !x.is_empty() {
            s = x;
        }
        // 8
        10
    } else {
        10
    };
    // TODO: Support scientific notation
    let mut num = 0i64;
    // TODO: Optimize with max_safe_digits
    while let [c, rest @ ..] = s {
        let Some(digit) = (*c as char).to_digit(radix) else {
            break;
        };
        num = num.checked_mul(radix as i64)?.checked_add(digit as i64)?;
        s = rest;
    }
    // binary exp
    if s.strip_prefix_inplace(b"p") || s.strip_prefix_inplace(b"P") {
        num = atoi_simd::parse_pos::<u32>(s)
            .ok()
            .and_then(|x| num.checked_shl(x))?;
        s = &[];
    }
    if !s.is_empty() {
        return None;
    }
    Some(num)
}

pub fn int_to_char(v: i64) -> char {
    v.try_into()
        .ok()
        .and_then(char::from_u32)
        .unwrap_or(char::REPLACEMENT_CHARACTER)
}

trait BinarySliceExt {
    fn strip_prefix_inplace(&mut self, prefix: &[u8]) -> bool;
}
impl BinarySliceExt for &[u8] {
    fn strip_prefix_inplace(&mut self, prefix: &[u8]) -> bool {
        match self.strip_prefix(prefix) {
            Some(x) => {
                *self = x;
                true
            }
            None => false,
        }
    }
}

pub fn is_obsolete_var(name: &str) -> bool {
    name.eq_ignore_ascii_case("DITEMTYPE")
        || name.eq_ignore_ascii_case("DA")
        || name.eq_ignore_ascii_case("DB")
        || name.eq_ignore_ascii_case("DC")
        || name.eq_ignore_ascii_case("DD")
        || name.eq_ignore_ascii_case("DE")
        || name.eq_ignore_ascii_case("TA")
        || name.eq_ignore_ascii_case("TB")
}

pub fn is_event_name(name: &str) -> bool {
    let matcher = bmatch_caseless! {
        b"EVENTFIRST" => true,
        b"EVENTTRAIN" => true,
        b"EVENTSHOP" => true,
        b"EVENTBUY" => true,
        b"EVENTCOM" => true,
        b"EVENTTURNEND" => true,
        b"EVENTCOMEND" => true,
        b"EVENTEND" => true,
        b"EVENTLOAD" => true,
        _ => false,
    };
    matcher(name.as_bytes())
}

pub fn is_chara_nodim(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "NO" | "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => true,
        _ => false,
    }
}

pub fn unwrap_str_literal(s: &str) -> String {
    if !matches!(s.as_bytes(), [b'"', .., b'"']) {
        unreachable!("invalid string literal: {:?}", s);
    }
    let s = &s[1..s.len() - 1];
    unescape_str(s)
}

pub fn unescape_str(s: &str) -> String {
    Vec::unescape_bytes(s).into_string_lossy()
}
