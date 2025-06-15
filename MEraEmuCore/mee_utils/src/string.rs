#[cfg(target_arch = "aarch64")]
use std::arch::aarch64::*;
#[cfg(target_arch = "wasm32")]
use std::arch::wasm32::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
use std::mem;
use wide::CmpEq;

/// Helper function to convert a byte array to uppercase ASCII.
#[inline]
pub(crate) fn to_ascii_uppercase_n<const N: usize>(mut s: [u8; N]) -> [u8; N] {
    s.make_ascii_uppercase();
    s
}

pub(crate) fn packed_byte(b: u8) -> u64 {
    // replicate the byte 8 times
    (b as u64) * 0x0101010101010101
}

// Source: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/master/2020/04/30/tolower.cpp
pub(crate) fn to_ascii_lowercase_8(chars: u64) -> u64 {
    let ascii_chars = chars & packed_byte(0x7f);
    let is_not_ascii_chars = chars & packed_byte(0x80);
    // MSB[i] is set if chars[i] is greater or equal than 'A'
    let a = ascii_chars + packed_byte(128 - b'A');
    // MSB[i] is set if chars[i] is greater than 'Z'
    let z = ascii_chars + packed_byte(128 - b'Z' - 1);
    let mut mask_lower = (a ^ z) & packed_byte(0x80);
    mask_lower &= !is_not_ascii_chars; // only transform when ASCII
    chars ^ (mask_lower >> 2)
}

pub(crate) fn to_ascii_uppercase_8(s: u64) -> u64 {
    let ascii_chars = s & packed_byte(0x7f);
    let is_not_ascii_chars = s & packed_byte(0x80);
    // MSB[i] is set if chars[i] is greater or equal than 'a'
    let a = ascii_chars + packed_byte(128 - b'a');
    // MSB[i] is set if chars[i] is greater than 'z'
    let z = ascii_chars + packed_byte(128 - b'z' - 1);
    let mut mask_upper = (a ^ z) & packed_byte(0x80);
    mask_upper &= !is_not_ascii_chars; // only transform when ASCII
    s ^ (mask_upper >> 2)
}

#[inline(always)]
pub(crate) fn to_ascii_uppercase_16_plain(s: [u8; 16]) -> [u8; 16] {
    to_ascii_uppercase_n(s)
}

#[cfg(any(
    target_arch = "wasm32",
    target_arch = "x86_64",
    target_arch = "aarch64"
))]
#[inline]
pub(crate) fn to_ascii_uppercase_16_simd(s: [u8; 16]) -> [u8; 16] {
    // NOTE: Compiler is able to auto-vectorize this.
    to_ascii_uppercase_16_plain(s)
}

fn load_arr_u8_16_simd(s: &[u8; 16]) -> [u8; 16] {
    // TODO: Remove this function when the compiler can optimize loading of 16 bytes.
    #[cfg(target_arch = "x86_64")]
    unsafe {
        use std::arch::x86_64::_mm_loadu_si128;
        let xmm = _mm_loadu_si128(s.as_ptr() as *const _);
        mem::transmute(xmm)
    }
    #[cfg(target_arch = "aarch64")]
    unsafe {
        use std::arch::aarch64::vld1q_u8;
        let v = vld1q_u8(s.as_ptr());
        mem::transmute(v)
    }
    #[cfg(target_arch = "wasm32")]
    unsafe {
        use std::arch::wasm32::v128_load;
        let v = v128_load(s.as_ptr() as *const u8);
        mem::transmute(v)
    }
}

/// Compares two byte slices for ordering, ignoring case.
pub fn cmp_ignore_ascii_case(a: &[u8], b: &[u8]) -> std::cmp::Ordering {
    let l = a.len().min(b.len());

    if l < 8 {
        let mut a_chunk = 0;
        let mut b_chunk = 0;
        if l > 4 {
            a_chunk = (u32::from_be_bytes(a[0..4].try_into().unwrap()) as u64) << 32;
            a_chunk |= u32::from_be_bytes(a[l - 4..l].try_into().unwrap()) as u64;
            b_chunk = (u32::from_be_bytes(b[0..4].try_into().unwrap()) as u64) << 32;
            b_chunk |= u32::from_be_bytes(b[l - 4..l].try_into().unwrap()) as u64;
        } else if l > 2 {
            a_chunk = (u16::from_be_bytes(a[0..2].try_into().unwrap()) as u64) << 16;
            a_chunk |= u16::from_be_bytes(a[l - 2..l].try_into().unwrap()) as u64;
            b_chunk = (u16::from_be_bytes(b[0..2].try_into().unwrap()) as u64) << 16;
            b_chunk |= u16::from_be_bytes(b[l - 2..l].try_into().unwrap()) as u64;
        } else if l > 0 {
            a_chunk = (a[0] as u64) << 8;
            a_chunk |= a[l - 1] as u64;
            b_chunk = (b[0] as u64) << 8;
            b_chunk |= b[l - 1] as u64;
        }
        return match to_ascii_uppercase_8(a_chunk).cmp(&to_ascii_uppercase_8(b_chunk)) {
            std::cmp::Ordering::Equal => a.len().cmp(&b.len()),
            non_eq => non_eq,
        };
    }

    let lhs = &a[..l];
    let rhs = &b[..l];

    let mut i = 0;
    while i < l - 8 {
        let a_chunk = u64::from_be_bytes(lhs[i..i + 8].try_into().unwrap());
        let b_chunk = u64::from_be_bytes(rhs[i..i + 8].try_into().unwrap());
        match to_ascii_uppercase_8(a_chunk).cmp(&to_ascii_uppercase_8(b_chunk)) {
            std::cmp::Ordering::Equal => {}
            non_eq => return non_eq,
        }
        i += 8;
    }

    // Handle the remaining bytes
    let a_chunk = u64::from_be_bytes(lhs[l - 8..l].try_into().unwrap());
    let b_chunk = u64::from_be_bytes(rhs[l - 8..l].try_into().unwrap());
    match to_ascii_uppercase_8(a_chunk).cmp(&to_ascii_uppercase_8(b_chunk)) {
        std::cmp::Ordering::Equal => a.len().cmp(&b.len()),
        non_eq => non_eq,
    }
}

/// Compares two byte slices for equality, ignoring case.
pub fn eq_ignore_ascii_case(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let len = a.len();

    // TODO: Fallback to a safer version for Miri (if we are going to read OOB).

    #[cfg(target_arch = "x86_64")]
    fn load_partial_arr_u8_16_simd(s: &[u8]) -> [u8; 16] {
        let len = s.len();
        assert!(len <= 16);
        unsafe {
            let xmm = if len >= 8 {
                let a = u64::from_ne_bytes(s[0..8].try_into().unwrap());
                let b = u64::from_ne_bytes(s[len - 8..len].try_into().unwrap());
                _mm_set_epi64x(b as i64, a as i64)
            } else if len >= 4 {
                let a = u32::from_ne_bytes(s[0..4].try_into().unwrap());
                let b = u32::from_ne_bytes(s[len - 4..len].try_into().unwrap());
                _mm_set_epi32(0, 0, b as _, a as _)
            } else if len > 0 {
                let a = s[0];
                let b = s[len - 1];
                let mid = s[len / 2];
                _mm_set_epi8(
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a as _, b as _, mid as _,
                )
            } else {
                _mm_setzero_si128()
            };
            mem::transmute(xmm)
        }
    }
    #[cfg(target_arch = "aarch64")]
    fn load_partial_arr_u8_16_simd(s: &[u8]) -> [u8; 16] {
        let len = s.len();
        assert!(len < 16);
        unsafe {
            let xmm = if len >= 8 {
                let a = u64::from_ne_bytes(s[0..8].try_into().unwrap());
                let b = u64::from_ne_bytes(s[len - 8..len].try_into().unwrap());
                vcombine_u64(vcreate_u64(a), vcreate_u64(b))
            } else if len >= 4 {
                let a = u32::from_ne_bytes(s[0..4].try_into().unwrap());
                let b = u32::from_ne_bytes(s[len - 4..len].try_into().unwrap());
                vcombine_u64(vcreate_u64(a as _), vcreate_u64(b as _))
            } else if len > 0 {
                let a = s[0];
                let b = s[len - 1];
                let mid = s[len / 2];
                vcombine_u64(
                    vcreate_u64((a as u64) << 8 | (mid as u64)),
                    vcreate_u64((b as u64) << 8 | (0u64)),
                )
            } else {
                vdupq_n_u64(0)
            };
            mem::transmute(xmm)
        }
    }
    #[cfg(target_arch = "wasm32")]
    fn load_partial_arr_u8_16_simd(s: &[u8]) -> [u8; 16] {
        let len = s.len();
        assert!(len < 16);
        unsafe {
            let xmm = if len >= 8 {
                let a = u64::from_ne_bytes(s[0..8].try_into().unwrap());
                let b = u64::from_ne_bytes(s[len - 8..len].try_into().unwrap());
                u64x2(a, b)
            } else if len >= 4 {
                let a = u32::from_ne_bytes(s[0..4].try_into().unwrap());
                let b = u32::from_ne_bytes(s[len - 4..len].try_into().unwrap());
                u64x2(a as _, b as _)
            } else if len > 0 {
                let a = s[0];
                let b = s[len - 1];
                let mid = s[len / 2];
                u64x2((a as u64) << 8 | (mid as u64), (b as u64) << 8 | (0u64))
            } else {
                u64x2(0, 0)
            };
            mem::transmute(xmm)
        }
    }

    if len < 16 {
        // Read small pieces of the string into 128-bit registers
        let a_chunk = load_partial_arr_u8_16_simd(a);
        let b_chunk = load_partial_arr_u8_16_simd(b);
        to_ascii_uppercase_16_simd(a_chunk) == to_ascii_uppercase_16_simd(b_chunk)
    } else {
        // Read 128-bit chunks of the string into 128-bit registers
        let mut i = 0;
        while i < len - 16 {
            let a_chunk = load_arr_u8_16_simd(a[i..i + 16].try_into().unwrap());
            let b_chunk = load_arr_u8_16_simd(b[i..i + 16].try_into().unwrap());
            if to_ascii_uppercase_16_simd(a_chunk) != to_ascii_uppercase_16_simd(b_chunk) {
                return false;
            }

            i += 16;
        }

        // Handle the remaining bytes
        let a_chunk = load_arr_u8_16_simd(a[len - 16..len].try_into().unwrap());
        let b_chunk = load_arr_u8_16_simd(b[len - 16..len].try_into().unwrap());
        to_ascii_uppercase_16_simd(a_chunk) == to_ascii_uppercase_16_simd(b_chunk)
    }
}

/// Hashes a byte slice using the `rustc-hash` algorithm, treating the bytes as ASCII uppercase.
pub fn rustc_hash_bytes_ascii_uppercase(s: &[u8]) -> u64 {
    // Source: https://github.com/rust-lang/rustc-hash/blob/master/src/lib.rs

    // Nothing special, digits of pi.
    const SEED1: u64 = 0x243f6a8885a308d3;
    const SEED2: u64 = 0x13198a2e03707344;
    const PREVENT_TRIVIAL_ZERO_COLLAPSE: u64 = 0xa4093822299f31d0;

    #[inline]
    fn multiply_mix(x: u64, y: u64) -> u64 {
        #[cfg(target_pointer_width = "64")]
        {
            // We compute the full u64 x u64 -> u128 product, this is a single mul
            // instruction on x86-64, one mul plus one mulhi on ARM64.
            let full = (x as u128) * (y as u128);
            let lo = full as u64;
            let hi = (full >> 64) as u64;

            // The middle bits of the full product fluctuate the most with small
            // changes in the input. This is the top bits of lo and the bottom bits
            // of hi. We can thus make the entire output fluctuate with small
            // changes to the input by XOR'ing these two halves.
            lo ^ hi

            // Unfortunately both 2^64 + 1 and 2^64 - 1 have small prime factors,
            // otherwise combining with + or - could result in a really strong hash, as:
            //     x * y = 2^64 * hi + lo = (-1) * hi + lo = lo - hi,   (mod 2^64 + 1)
            //     x * y = 2^64 * hi + lo =    1 * hi + lo = lo + hi,   (mod 2^64 - 1)
            // Multiplicative hashing is universal in a field (like mod p).
        }

        #[cfg(target_pointer_width = "32")]
        {
            // u64 x u64 -> u128 product is prohibitively expensive on 32-bit.
            // Decompose into 32-bit parts.
            let lx = x as u32;
            let ly = y as u32;
            let hx = (x >> 32) as u32;
            let hy = (y >> 32) as u32;

            // u32 x u32 -> u64 the low bits of one with the high bits of the other.
            let afull = (lx as u64) * (hy as u64);
            let bfull = (hx as u64) * (ly as u64);

            // Combine, swapping low/high of one of them so the upper bits of the
            // product of one combine with the lower bits of the other.
            afull ^ bfull.rotate_right(32)
        }
    }

    #[inline]
    fn uppercase_u64x2(in_a: [u64; 2]) -> [u64; 2] {
        #[cfg(target_arch = "x86_64")]
        unsafe {
            // TODO: Remove this when the compiler can optimize loading of 8x2 bytes in x86_64.
            let mut a = _mm_setzero_si128();
            std::arch::asm!(
                "movq {xmm0}, {a0}",
                "movq {xmm1}, {a1}",
                "punpcklqdq {xmm0}, {xmm1}",
                a0 = in(reg) in_a[0],
                a1 = in(reg) in_a[1],
                xmm0 = out(xmm_reg) a,
                xmm1 = out(xmm_reg) _,
            );
            let a = mem::transmute(a);
            mem::transmute(to_ascii_uppercase_16_simd(a))
        }
        #[cfg(not(target_arch = "x86_64"))]
        unsafe {
            let a: [u8; 16] = mem::transmute(make_arr_u64_2_simd(in_a));
            mem::transmute(to_ascii_uppercase_16_simd(a))
        }
    }

    fn hash_bytes(bytes: &[u8]) -> u64 {
        let len = bytes.len();
        let mut s0 = SEED1;
        let mut s1 = SEED2;

        if len <= 16 {
            // XOR the input into s0, s1.
            if len >= 8 {
                let v0 = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let v1 = u64::from_le_bytes(bytes[len - 8..].try_into().unwrap());
                let [v0, v1] = uppercase_u64x2([v0, v1]);
                s0 ^= v0;
                s1 ^= v1;
            } else if len >= 4 {
                let v0 = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
                let v1 = u32::from_le_bytes(bytes[len - 4..].try_into().unwrap());
                let [v0, v1] = uppercase_u64x2([v0 as u64, v1 as u64]);
                s0 ^= v0;
                s1 ^= v1;
            } else if len > 0 {
                let lo = bytes[0];
                let mid = bytes[len / 2];
                let hi = bytes[len - 1];
                let v0 = lo as u64;
                let v1 = ((hi as u64) << 8) | mid as u64;
                let [v0, v1] = uppercase_u64x2([v0, v1]);
                s0 ^= v0;
                s1 ^= v1;
            }
        } else {
            // Handle bulk (can partially overlap with suffix).
            let mut off = 0;
            while off < len - 16 {
                let xy = load_arr_u8_16_simd(bytes[off..off + 16].try_into().unwrap());
                let [x, y]: [u64; 2] = unsafe { mem::transmute(to_ascii_uppercase_16_simd(xy)) };

                // Replace s1 with a mix of s0, x, and y, and s0 with s1.
                // This ensures the compiler can unroll this loop into two
                // independent streams, one operating on s0, the other on s1.
                //
                // Since zeroes are a common input we prevent an immediate trivial
                // collapse of the hash function by XOR'ing a constant with y.
                let t = multiply_mix(s0 ^ x, PREVENT_TRIVIAL_ZERO_COLLAPSE ^ y);
                s0 = s1;
                s1 = t;
                off += 16;
            }

            let suffix = &bytes[len - 16..];
            let xy = load_arr_u8_16_simd(suffix[0..16].try_into().unwrap());
            let [x, y]: [u64; 2] = unsafe { mem::transmute(to_ascii_uppercase_16_simd(xy)) };
            s0 ^= x;
            s1 ^= y;
        }

        multiply_mix(s0, s1) ^ (len as u64)
    }

    hash_bytes(s)
}

/// Scans a byte slice for line start positions. Line start positions refer to the
/// position of the first byte of each line in the slice, i.e., the position of the
/// newline mark (`\n` and `\r\n`) plus one.
pub fn scan_line_positions_u32(s: &[u8]) -> Vec<u32> {
    assert!(s.len() <= u32::MAX as usize);

    let mut newlines = Vec::with_capacity(s.len() / 64 + 4);
    newlines.push(0);

    let (prefix, middle, tail) = unsafe { s.align_to::<wide::i8x16>() };

    // Prefix
    for (i, &b) in prefix.iter().enumerate() {
        if b == b'\n' {
            let pos = i + 1;
            newlines.push(pos as _);
        }
    }

    // Process the middle part in chunks of 16 bytes
    #[cfg(target_endian = "little")]
    for (i, &chunk) in middle.iter().enumerate() {
        let mask = chunk.cmp_eq(wide::i8x16::splat(b'\n' as _));
        let mut indices = mask.move_mask();

        // Exploits the sparseness of the newline bits to improve performance
        while indices != 0 {
            let index = indices.trailing_zeros() as usize;
            let pos = prefix.len() + i * 16 + index + 1;
            newlines.push(pos as _);
            indices &= indices - 1; // Clear the lowest set bit
        }
    }

    // Tail
    for (i, &b) in tail.iter().enumerate() {
        if b == b'\n' {
            let pos = prefix.len() + middle.len() + i + 1;
            newlines.push(pos as _);
        }
    }

    newlines
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cmp_ignore_ascii_case() {
        assert_eq!(
            cmp_ignore_ascii_case(b"hello", b"HELLO"),
            std::cmp::Ordering::Equal
        );
        assert_eq!(
            cmp_ignore_ascii_case(b"hello", b"world"),
            std::cmp::Ordering::Less
        );
        assert_eq!(
            cmp_ignore_ascii_case(b"Hello World", b"hello world"),
            std::cmp::Ordering::Equal
        );
        assert_eq!(
            cmp_ignore_ascii_case(b"Hello World", b"HelloWorld"),
            std::cmp::Ordering::Less
        );
        assert_eq!(
            cmp_ignore_ascii_case(b"Hello Very Long String", b"hello very long string"),
            std::cmp::Ordering::Equal
        );
    }

    #[test]
    fn test_eq_ignore_ascii_case() {
        assert!(eq_ignore_ascii_case(b"hello", b"HELLO"));
        assert!(!eq_ignore_ascii_case(b"hello", b"world"));
        assert!(eq_ignore_ascii_case(b"Hello World", b"hello world"));
        assert!(!eq_ignore_ascii_case(b"Hello World", b"HelloWorld"));
        assert!(eq_ignore_ascii_case(
            b"Hello Very Long String",
            b"hello very long string"
        ));
    }

    #[test]
    fn test_rustc_hash_bytes_ascii_uppercase() {
        use std::hash::{Hash, Hasher};

        fn hash_a(s: &[u8]) -> u64 {
            let mut hasher = rustc_hash::FxHasher::default();
            s.to_ascii_uppercase().hash(&mut hasher);
            hasher.finish()
        }
        fn hash_b(s: &[u8]) -> u64 {
            let mut hasher = rustc_hash::FxHasher::default();
            hasher.write_usize(s.len());
            hasher.write_u64(rustc_hash_bytes_ascii_uppercase(s));
            hasher.finish()
        }

        let s = b"Hello World";
        assert_eq!(hash_a(s), hash_b(s));
        assert_eq!(hash_a(b"Hello"), hash_b(b"Hello"));
        assert_eq!(hash_a(b"Hello World!"), hash_b(b"Hello World!"));
        assert_eq!(hash_a(b"Hello World!"), hash_b(b"HELLO WORLD!"));
        assert_eq!(
            hash_a(b"Hello Very Long String"),
            hash_b(b"hello very long string")
        );
    }

    #[test]
    fn test_scan_line_positions_u32() {
        let s = b"Hello\nWorld\n";
        assert_eq!(scan_line_positions_u32(s), vec![0, 6, 12]);

        let s = b"Hello\r\nWorld\r\n";
        assert_eq!(scan_line_positions_u32(s), vec![0, 7, 14]);

        let s = b"Hello\nWorld";
        assert_eq!(scan_line_positions_u32(s), vec![0, 6]);
    }
}
