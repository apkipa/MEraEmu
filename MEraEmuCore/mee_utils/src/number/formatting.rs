// Reference: https://source.dot.net/#System.Private.CoreLib/src/libraries/System.Private.CoreLib/src/System/Number.Formatting.cs

use anyhow::bail;

pub fn csharp_format_i64(value: i64, format: &str) -> anyhow::Result<String> {
    if format.is_empty() {
        return Ok(value.to_string());
    }
    let (fmt, digits) = parse_format_specifier(format)?;
    let fmt_upper = fmt.to_ascii_uppercase();
    if (fmt_upper == 'G' && digits < 1) || fmt_upper == 'D' {
        return Ok(if value >= 0 {
            let width = digits.max(0) as _;
            format!("{value:0width$}")
        } else {
            let width = (digits + 1).max(0) as _;
            format!("{value:0width$}")
        });
    } else if fmt_upper == 'X' {
        let width = digits.max(0) as _;
        return Ok(if fmt == 'x' {
            format!("{value:0width$x}")
        } else {
            format!("{value:0width$X}")
        });
    } else if fmt_upper == 'B' {
        let width = digits.max(0) as _;
        return Ok(format!("{value:0width$b}"));
    } else {
        let mut digits_cnt = 0;
        let mut commas_cnt = 0;
        let mut has_zero = false;
        let mut has_group_separator = false;
        for ch in format.as_bytes() {
            match ch {
                b'#' => {
                    if commas_cnt > 0 {
                        commas_cnt = 0;
                        has_group_separator = true;
                    }
                    if has_zero {
                        digits_cnt += 1;
                    }
                }
                b'0' => {
                    if commas_cnt > 0 {
                        commas_cnt = 0;
                        has_group_separator = true;
                    }
                    has_zero = true;
                    digits_cnt += 1;
                }
                b',' => {
                    commas_cnt += 1;
                }
                _ => bail!("invalid or unsupported format string"),
            }
        }
        if commas_cnt > 0 {
            bail!("scaling in format string is not implemented for now");
        }
        // Start custom formatting
        let width = digits_cnt.max(1);
        let mut cur_width = 0;
        let mut buf = smallvec::SmallVec::<[u8; 32]>::new();
        let is_negative = value < 0;
        let mut value = value;
        let mut digit_counter = 0;
        while value != 0 || cur_width < width {
            let cur_digit = (value % 10).abs() as u8;
            if digit_counter == 3 && has_group_separator {
                buf.push(b',');
                digit_counter = 0;
            }
            buf.push(b'0' + cur_digit);
            digit_counter += 1;
            value /= 10;
            cur_width += 1;
        }
        if is_negative {
            buf.push(b'-');
        }
        buf.reverse();
        // SAFETY: We only push ASCII characters into buf.
        let string = unsafe { String::from_utf8_unchecked(buf.to_vec()) };
        return Ok(string);
    }
}

fn parse_format_specifier(format: &str) -> anyhow::Result<(char, i32)> {
    let mut c = Default::default();
    let format = format.as_bytes();
    if format.len() > 0 {
        // If the format begins with a symbol, see if it's a standard format
        // with or without a specified number of digits.
        c = format[0] as char;
        if c.is_ascii_alphabetic() {
            // Fast path for sole symbol, e.g. "D"
            if format.len() == 1 {
                return Ok((c, -1));
            }

            if format.len() == 2 {
                // Fast path for symbol and single digit, e.g. "X4"
                let d = format[1] as i32 - '0' as i32;
                if (d as u32) < 10 {
                    return Ok((c, d));
                }
            } else if format.len() == 3 {
                // Fast path for symbol and double digit, e.g. "F12"
                let d1 = format[1] as i32 - '0' as i32;
                let d2 = format[2] as i32 - '0' as i32;
                if (d1 as u32) < 10 && (d2 as u32) < 10 {
                    return Ok((c, d1 * 10 + d2));
                }
            }

            // Fallback for symbol and any length digits.  The digits value must be >= 0 && <= 999_999_999,
            // but it can begin with any number of 0s, and thus we may need to check more than 9
            // digits.  Further, for compat, we need to stop when we hit a null char.
            let mut n = 0;
            let mut i = 1;
            while (i as u32) < (format.len() as u32) && format[i].is_ascii_digit() {
                // Check if we are about to overflow past our limit of 9 digits
                if n >= 100_000_000 {
                    bail!("ThrowHelper.ThrowFormatException_BadFormatSpecifier");
                }
                n = (n * 10) + (format[i] as i32 - '0' as i32);
                i += 1;
            }

            // If we're at the end of the digits rather than having stopped because we hit something
            // other than a digit or overflowed, return the standard format info.
            if (i as u32) >= (format.len() as u32) || format[i] == b'\0' {
                return Ok((c, n));
            }
        }
    }

    // Default empty format to be "G"; custom format is signified with '\0'.
    c = if format.is_empty() || c == '\0' {
        // For compat, treat '\0' as the end of the specifier, even if the specifier extends beyond it.
        'G'
    } else {
        '\0'
    };
    Ok((c, -1))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_g() -> anyhow::Result<()> {
        assert_eq!(csharp_format_i64(42, "")?, "42");
        assert_eq!(csharp_format_i64(42, "G")?, "42");
        assert_eq!(csharp_format_i64(0, "G")?, "0");
        Ok(())
    }

    #[test]
    fn test_d() -> anyhow::Result<()> {
        assert_eq!(csharp_format_i64(1, "D5")?, "00001");
        assert_eq!(csharp_format_i64(-1, "D5")?, "-00001");
        assert_eq!(csharp_format_i64(123, "D2")?, "123");
        assert_eq!(csharp_format_i64(-123, "D2")?, "-123");
        assert_eq!(csharp_format_i64(0, "D2")?, "00");
        assert_eq!(csharp_format_i64(0, "D")?, "0");
        Ok(())
    }

    #[test]
    fn test_x() -> anyhow::Result<()> {
        assert_eq!(csharp_format_i64(213, "x5")?, "000d5");
        assert_eq!(csharp_format_i64(213, "X5")?, "000D5");
        assert_eq!(csharp_format_i64(-3, "x")?, "fffffffffffffffd");
        assert_eq!(csharp_format_i64(0, "x")?, "0");
        assert_eq!(csharp_format_i64(0, "x2")?, "00");
        Ok(())
    }

    #[test]
    fn test_b() -> anyhow::Result<()> {
        assert_eq!(csharp_format_i64(213, "b5")?, "11010101");
        assert_eq!(csharp_format_i64(213, "B5")?, "11010101");
        assert_eq!(
            csharp_format_i64(-3, "b")?,
            "1111111111111111111111111111111111111111111111111111111111111101"
        );
        assert_eq!(csharp_format_i64(0, "b")?, "0");
        assert_eq!(csharp_format_i64(0, "b2")?, "00");
        Ok(())
    }

    #[test]
    fn test_custom() -> anyhow::Result<()> {
        assert_eq!(csharp_format_i64(1234567, "#,###")?, "1,234,567");
        assert_eq!(csharp_format_i64(-1234567, "#,###")?, "-1,234,567");
        assert_eq!(csharp_format_i64(-123456, "#,###")?, "-123,456");
        assert_eq!(csharp_format_i64(1234, "#####")?, "1234");
        assert_eq!(csharp_format_i64(-1234, "00####")?, "-001234");
        assert_eq!(csharp_format_i64(-1234, "#00####")?, "-001234");
        assert_eq!(csharp_format_i64(1234, "#00####")?, "001234");
        Ok(())
    }
}
