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

pub fn read_digits_literal<T: num_traits::PrimInt>(radix: u32, s: &mut &[u8]) -> Option<T> {
    if radix < 2 || radix > 36 {
        panic!("invalid radix: {}", radix);
    }
    let radix_as_t = T::from(radix)?;

    let orig_s = s;
    let mut s = *orig_s;

    let mut num = T::zero();
    // TODO: Optimize with max_safe_digits
    while let [c, rest @ ..] = s {
        let Some(digit) = (*c as char).to_digit(radix) else {
            break;
        };
        let digit = T::from(digit)?;
        num = num.checked_mul(&radix_as_t)?.checked_add(&digit)?;
        s = rest;
    }

    *orig_s = s;

    Some(num)
}

pub fn read_nonempty_digits_literal<T: num_traits::PrimInt>(
    radix: u32,
    s: &mut &[u8],
) -> Option<T> {
    let copy_s = *s;
    let num = read_digits_literal(radix, s)?;
    (!std::ptr::addr_eq(*s, copy_s)).then_some(num)
}

/// Reads an integer literal from the input slice. If the input is a valid UTF-8 string,
/// it is guaranteed that the input remains valid UTF-8 after the function call.
pub fn read_int_literal(s: &mut &[u8]) -> Option<i64> {
    let orig_s = s;
    let mut s = *orig_s;

    let radix = if s.strip_prefix_inplace(b"0x") || s.strip_prefix_inplace(b"0X") {
        16
    } else if s.strip_prefix_inplace(b"0b") || s.strip_prefix_inplace(b"0B") {
        2
    } else {
        10
    };
    // TODO: Support scientific notation
    let mut num: i64 = read_nonempty_digits_literal(radix, &mut s)?;
    // binary exp
    if s.strip_prefix_inplace(b"p") || s.strip_prefix_inplace(b"P") {
        let exp = read_nonempty_digits_literal(10, &mut s)?;
        num = num.checked_shl(exp)?;
    }

    *orig_s = s;

    Some(num)
}

pub fn read_int_literal_with_sign(s: &mut &[u8]) -> Option<i64> {
    let orig_s = s;
    let mut s = *orig_s;
    let sign = if s.strip_prefix_inplace(b"-") {
        -1
    } else if s.strip_prefix_inplace(b"+") {
        1
    } else {
        1
    };
    let r = read_int_literal(&mut s).map(|x| x.wrapping_mul(sign))?;
    *orig_s = s;
    Some(r)
}

pub fn read_int_literal_str(s: &mut &str) -> Option<i64> {
    unsafe {
        let mut slice = s.as_bytes();
        let r = read_int_literal(&mut slice)?;
        // SAFETY: The input is valid UTF-8
        *s = std::str::from_utf8_unchecked(slice);
        Some(r)
    }
}

pub fn read_int_literal_str_with_sign(s: &mut &str) -> Option<i64> {
    unsafe {
        let mut slice = s.as_bytes();
        let r = read_int_literal_with_sign(&mut slice)?;
        // SAFETY: The input is valid UTF-8
        *s = std::str::from_utf8_unchecked(slice);
        Some(r)
    }
}

pub fn parse_int_literal(s: &[u8]) -> Option<i64> {
    let mut s = s;
    let r = read_int_literal(&mut s)?;
    s.is_empty().then_some(r)
}

pub fn parse_int_literal_with_sign(s: &[u8]) -> Option<i64> {
    let mut s = s;
    let r = read_int_literal_with_sign(&mut s)?;
    s.is_empty().then_some(r)
}

#[test]
fn test_parse_int_literal_with_sign() -> anyhow::Result<()> {
    assert_eq!(parse_int_literal_with_sign(b"0"), Some(0));
    assert_eq!(parse_int_literal_with_sign(b"1"), Some(1));
    assert_eq!(parse_int_literal_with_sign(b"-1"), Some(-1));
    assert_eq!(parse_int_literal_with_sign(b"+1"), Some(1));
    assert_eq!(parse_int_literal_with_sign(b"0x1"), Some(1));
    assert_eq!(parse_int_literal_with_sign(b"0x10"), Some(16));
    assert_eq!(parse_int_literal_with_sign(b"6"), Some(6));
    assert_eq!(parse_int_literal_with_sign(b""), None);
    assert_eq!(parse_int_literal_with_sign(b"p"), None);
    Ok(())
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

pub fn is_builtin_chara_var(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "ISASSI" | "NO" | "BASE" | "MAXBASE" | "ABL" | "TALENT" | "EXP" | "MARK" | "PALAM"
        | "SOURCE" | "EX" | "CFLAG" | "JUEL" | "RELATION" | "EQUIP" | "TEQUIP" | "STAIN"
        | "GOTJUEL" | "NOWEX" | "DOWNBASE" | "CUP" | "CDOWN" | "TCVAR" | "NAME" | "CALLNAME"
        | "NICKNAME" | "MASTERNAME" | "CSTR" | "CDFLAG" => true,
        _ => false,
    }
}

pub fn is_chara_nodim(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "NO" | "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => true,
        _ => false,
    }
}

pub fn unwrap_str_literal(s: &str) -> String {
    if s.len() < 2 || {
        let s = s.as_bytes();
        [s[0], s[s.len() - 1]] != [b'"', b'"']
    } {
        unreachable!("invalid string literal: {:?}", s);
    }
    unescape_str(&s[1..s.len() - 1])
    // if let [b'"', s @ .., b'"'] = s.as_bytes() {
    //     unescape_str(unsafe { std::str::from_utf8_unchecked(s) })
    // } else {
    //     unreachable!("invalid string literal: {:?}", s);
    // }
}

pub fn unescape_str(s: &str) -> String {
    // Vec::unescape_bytes(s).into_string_lossy()
    let mut out = Vec::new();
    let mut iter = s.bytes();
    while let Some(c) = iter.next() {
        if c != b'\\' {
            out.push(c);
            continue;
        }

        match iter.next() {
            Some(b'n') => out.push(b'\n'),
            Some(b'r') => out.push(b'\r'),
            Some(b't') => out.push(b'\t'),
            // Rest is copied verbatim (so that no invalid UTF-8 is produced)
            Some(c) => out.push(c),
            None => {}
        }
    }
    // SAFETY: Input is valid UTF-8, and we never mess it up
    unsafe { String::from_utf8_unchecked(out) }
}

pub fn unescape_in_place(s: &mut String) {
    // SAFETY: Input is valid UTF-8, and we never mess it up
    let buf = unsafe { s.as_mut_vec() };
    let len = buf.len();
    let mut rd = 0;
    let mut wr = 0;
    while rd < len {
        if buf[rd] != b'\\' {
            buf[wr] = buf[rd];
            rd += 1;
            wr += 1;
            continue;
        }

        match buf.get(rd + 1) {
            Some(b'n') => buf[wr] = b'\n',
            Some(b'r') => buf[wr] = b'\r',
            Some(b't') => buf[wr] = b'\t',
            // Rest is copied verbatim (so that no invalid UTF-8 is produced)
            Some(c) => buf[wr] = *c,
            None => {}
        }
        rd += 2;
        wr += 1;
    }
    unsafe {
        buf.set_len(wr);
    }
    debug_assert!(s.as_bytes().is_utf8());
}

pub fn unescape_to_sink(input: &str, output: &mut String) {
    let output = unsafe { output.as_mut_vec() };
    let mut input = input.as_bytes();

    while let Some(pos) = memchr::memchr(b'\\', input) {
        output.extend_from_slice(&input[..pos]);
        input = &input[pos..];
        if input.len() < 2 {
            return;
        }
        match input[1] {
            b'n' => output.push(b'\n'),
            b'r' => output.push(b'\r'),
            b't' => output.push(b'\t'),
            // Rest is copied verbatim (so that no invalid UTF-8 is produced)
            _ => output.push(input[1]),
        }
        input = &input[2..];
    }
    output.extend_from_slice(input);

    // let output = unsafe { output.as_mut_vec() };
    // let mut iter = input.bytes();
    // while let Some(c) = iter.next() {
    //     if c != b'\\' {
    //         output.push(c);
    //         continue;
    //     }

    //     match iter.next() {
    //         Some(b'n') => output.push(b'\n'),
    //         Some(b'r') => output.push(b'\r'),
    //         Some(b't') => output.push(b'\t'),
    //         // Rest is copied verbatim (so that no invalid UTF-8 is produced)
    //         Some(c) => output.push(c),
    //         None => {}
    //     }
    // }
}

pub fn format_radix(mut num: i64, radix: u32) -> Option<String> {
    // TODO: Optimize the implementation (do not allocate a Vec)
    if radix < 2 || radix > 36 {
        return None;
    }
    let radix = radix as i64;
    let mut buf = Vec::new();
    if num == i64::MIN {
        return Some("-9223372036854775808".to_owned());
    }
    if num < 0 {
        buf.push(b'-');
        num = -num;
    }
    let mut digits = Vec::new();
    while num > 0 {
        digits.push((num % radix) as u8);
        num /= radix;
    }
    if digits.is_empty() {
        digits.push(0);
    }
    for &digit in digits.iter().rev() {
        buf.push(match digit {
            0..=9 => b'0' + digit,
            10..=35 => b'a' + digit - 10,
            _ => unreachable!(),
        });
    }
    Some(String::from_utf8(buf).unwrap())
}

pub fn color_from_name(name: &str) -> Option<u32> {
    let matcher = bmatch_caseless! {
        b"ALICEBLUE" => Some(0xFFF0F8FF),
        b"ANTIQUEWHITE" => Some(0xFFFAEBD7),
        b"AQUA" => Some(0xFF00FFFF),
        b"AQUAMARINE" => Some(0xFF7FFFD4),
        b"AZURE" => Some(0xFFF0FFFF),
        b"BEIGE" => Some(0xFFF5F5DC),
        b"BISQUE" => Some(0xFFFFE4C4),
        b"BLACK" => Some(0xFF000000),
        b"BLANCHEDALMOND" => Some(0xFFFFEBCD),
        b"BLUE" => Some(0xFF0000FF),
        b"BLUEVIOLET" => Some(0xFF8A2BE2),
        b"BROWN" => Some(0xFFA52A2A),
        b"BURLYWOOD" => Some(0xFFDEB887),
        b"CADETBLUE" => Some(0xFF5F9EA0),
        b"CHARTREUSE" => Some(0xFF7FFF00),
        b"CHOCOLATE" => Some(0xFFD2691E),
        b"CORAL" => Some(0xFFFF7F50),
        b"CORNFLOWERBLUE" => Some(0xFF6495ED),
        b"CORNSILK" => Some(0xFFFFF8DC),
        b"CRIMSON" => Some(0xFFDC143C),
        b"CYAN" => Some(0xFF00FFFF),
        b"DARKBLUE" => Some(0xFF00008B),
        b"DARKCYAN" => Some(0xFF008B8B),
        b"DARKGOLDENROD" => Some(0xFFB8860B),
        b"DARKGRAY" => Some(0xFFA9A9A9),
        b"DARKGREEN" => Some(0xFF006400),
        b"DARKKHAKI" => Some(0xFFBDB76B),
        b"DARKMAGENTA" => Some(0xFF8B008B),
        b"DARKOLIVEGREEN" => Some(0xFF556B2F),
        b"DARKORANGE" => Some(0xFFFF8C00),
        b"DARKORCHID" => Some(0xFF9932CC),
        b"DARKRED" => Some(0xFF8B0000),
        b"DARKSALMON" => Some(0xFFE9967A),
        b"DARKSEAGREEN" => Some(0xFF8FBC8F),
        b"DARKSLATEBLUE" => Some(0xFF483D8B),
        b"DARKSLATEGRAY" => Some(0xFF2F4F4F),
        b"DARKTURQUOISE" => Some(0xFF00CED1),
        b"DARKVIOLET" => Some(0xFF9400D3),
        b"DEEPPINK" => Some(0xFFFF1493),
        b"DEEPSKYBLUE" => Some(0xFF00BFFF),
        b"DIMGRAY" => Some(0xFF696969),
        b"DODGERBLUE" => Some(0xFF1E90FF),
        b"FIREBRICK" => Some(0xFFB22222),
        b"FLORALWHITE" => Some(0xFFFFFAF0),
        b"FORESTGREEN" => Some(0xFF228B22),
        b"FUCHSIA" => Some(0xFFFF00FF),
        b"GAINSBORO" => Some(0xFFDCDCDC),
        b"GHOSTWHITE" => Some(0xFFF8F8FF),
        b"GOLD" => Some(0xFFFFD700),
        b"GOLDENROD" => Some(0xFFDAA520),
        b"GRAY" => Some(0xFF808080),
        b"GREEN" => Some(0xFF008000),
        b"GREENYELLOW" => Some(0xFFADFF2F),
        b"HONEYDEW" => Some(0xFFF0FFF0),
        b"HOTPINK" => Some(0xFFFF69B4),
        b"INDIANRED" => Some(0xFFCD5C5C),
        b"INDIGO" => Some(0xFF4B0082),
        b"IVORY" => Some(0xFFFFFFF0),
        b"KHAKI" => Some(0xFFF0E68C),
        b"LAVENDER" => Some(0xFFE6E6FA),
        b"LAVENDERBLUSH" => Some(0xFFFFF0F5),
        b"LAWNGREEN" => Some(0xFF7CFC00),
        b"LEMONCHIFFON" => Some(0xFFFFFACD),
        b"LIGHTBLUE" => Some(0xFFADD8E6),
        b"LIGHTCORAL" => Some(0xFFF08080),
        b"LIGHTCYAN" => Some(0xFFE0FFFF),
        b"LIGHTGOLDENRODYELLOW" => Some(0xFFFAFAD2),
        b"LIGHTGRAY" => Some(0xFFD3D3D3),
        b"LIGHTGREEN" => Some(0xFF90EE90),
        b"LIGHTPINK" => Some(0xFFFFB6C1),
        b"LIGHTSALMON" => Some(0xFFFFA07A),
        b"LIGHTSEAGREEN" => Some(0xFF20B2AA),
        b"LIGHTSKYBLUE" => Some(0xFF87CEFA),
        b"LIGHTSLATEGRAY" => Some(0xFF778899),
        b"LIGHTSTEELBLUE" => Some(0xFFB0C4DE),
        b"LIGHTYELLOW" => Some(0xFFFFFFE0),
        b"LIME" => Some(0xFF00FF00),
        b"LIMEGREEN" => Some(0xFF32CD32),
        b"LINEN" => Some(0xFFFAF0E6),
        b"MAGENTA" => Some(0xFFFF00FF),
        b"MAROON" => Some(0xFF800000),
        b"MEDIUMAQUAMARINE" => Some(0xFF66CDAA),
        b"MEDIUMBLUE" => Some(0xFF0000CD),
        b"MEDIUMORCHID" => Some(0xFFBA55D3),
        b"MEDIUMPURPLE" => Some(0xFF9370DB),
        b"MEDIUMSEAGREEN" => Some(0xFF3CB371),
        b"MEDIUMSLATEBLUE" => Some(0xFF7B68EE),
        b"MEDIUMSPRINGGREEN" => Some(0xFF00FA9A),
        b"MEDIUMTURQUOISE" => Some(0xFF48D1CC),
        b"MEDIUMVIOLETRED" => Some(0xFFC71585),
        b"MIDNIGHTBLUE" => Some(0xFF191970),
        b"MINTCREAM" => Some(0xFFF5FFFA),
        b"MISTYROSE" => Some(0xFFFFE4E1),
        b"MOCCASIN" => Some(0xFFFFE4B5),
        b"NAVAJOWHITE" => Some(0xFFFFDEAD),
        b"NAVY" => Some(0xFF000080),
        b"OLDLACE" => Some(0xFFFDF5E6),
        b"OLIVE" => Some(0xFF808000),
        b"OLIVEDRAB" => Some(0xFF6B8E23),
        b"ORANGE" => Some(0xFFFFA500),
        b"ORANGERED" => Some(0xFFFF4500),
        b"ORCHID" => Some(0xFFDA70D6),
        b"PALEGOLDENROD" => Some(0xFFEEE8AA),
        b"PALEGREEN" => Some(0xFF98FB98),
        b"PALETURQUOISE" => Some(0xFFAFEEEE),
        b"PALEVIOLETRED" => Some(0xFFDB7093),
        b"PAPAYAWHIP" => Some(0xFFFFEFD5),
        b"PEACHPUFF" => Some(0xFFFFDAB9),
        b"PERU" => Some(0xFFCD853F),
        b"PINK" => Some(0xFFFFC0CB),
        b"PLUM" => Some(0xFFDDA0DD),
        b"POWDERBLUE" => Some(0xFFB0E0E6),
        b"PURPLE" => Some(0xFF800080),
        b"RED" => Some(0xFFFF0000),
        b"ROSYBROWN" => Some(0xFFBC8F8F),
        b"ROYALBLUE" => Some(0xFF4169E1),
        b"SADDLEBROWN" => Some(0xFF8B4513),
        b"SALMON" => Some(0xFFFA8072),
        b"SANDYBROWN" => Some(0xFFF4A460),
        b"SEAGREEN" => Some(0xFF2E8B57),
        b"SEASHELL" => Some(0xFFFFF5EE),
        b"SIENNA" => Some(0xFFA0522D),
        b"SILVER" => Some(0xFFC0C0C0),
        b"SKYBLUE" => Some(0xFF87CEEB),
        b"SLATEBLUE" => Some(0xFF6A5ACD),
        b"SLATEGRAY" => Some(0xFF708090),
        b"SNOW" => Some(0xFFFFFAFA),
        b"SPRINGGREEN" => Some(0xFF00FF7F),
        b"STEELBLUE" => Some(0xFF4682B4),
        b"TAN" => Some(0xFFD2B48C),
        b"TEAL" => Some(0xFF008080),
        b"THISTLE" => Some(0xFFD8BFD8),
        b"TOMATO" => Some(0xFFFF6347),
        b"TRANSPARENT" => Some(0x00FFFFFF),
        b"TURQUOISE" => Some(0xFF40E0D0),
        b"VIOLET" => Some(0xFFEE82EE),
        b"WHEAT" => Some(0xFFF5DEB3),
        b"WHITE" => Some(0xFFFFFFFF),
        b"WHITESMOKE" => Some(0xFFF5F5F5),
        b"YELLOW" => Some(0xFFFFFF00),
        b"YELLOWGREEN" => Some(0xFF9ACD32),
        _ => None,
    };
    matcher(name.as_bytes())
}
