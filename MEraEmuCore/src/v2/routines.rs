use bstr::ByteSlice;

use crate::types::*;

#[must_use]
pub fn recognize_print_cmd(cmd: &[u8]) -> Option<(EraCmdArgFmt, PrintExtendedFlags)> {
    let cmd = cmd.to_ascii_uppercase();
    let mut cmd = cmd.as_slice();
    let arg_fmt;
    let mut flags = PrintExtendedFlags::new();
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
pub fn recognize_debugprint_cmd(cmd: &[u8]) -> Option<(EraCmdArgFmt, PrintExtendedFlags)> {
    let cmd = cmd.to_ascii_uppercase();
    let mut cmd = cmd.as_slice();
    let arg_fmt;
    let mut flags = PrintExtendedFlags::new();
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
pub fn recognize_printdata_cmd(cmd: &[u8]) -> Option<PrintExtendedFlags> {
    let cmd = cmd.to_ascii_uppercase();
    let mut cmd = cmd.as_slice();
    let mut flags = PrintExtendedFlags::new();
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
