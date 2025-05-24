#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
use std::mem;
use wide::CmpEq;

/// Helper function to convert a byte array to uppercase ASCII.
#[inline]
pub fn to_ascii_uppercase_n<const N: usize>(mut s: [u8; N]) -> [u8; N] {
    s.make_ascii_uppercase();
    s
}

/// Compares two byte slices for equality, ignoring case.
pub fn eq_ignore_ascii_case(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let len = a.len();

    // TODO: Fallback to a safer version for Miri.

    #[cfg(target_arch = "x86_64")]
    // SAFETY: x86_64 arch always has SSE2 support.
    unsafe {
        if len < 16 {
            // Read small pieces of the string into 128-bit registers
            let mut i = 0;
            let mut a_chunk = _mm_setzero_si128();
            let mut b_chunk = _mm_setzero_si128();
            if len & 0x8 != 0 {
                a_chunk = mem::transmute(_mm_load_sd(a.as_ptr().add(i) as *const f64));
                b_chunk = mem::transmute(_mm_load_sd(b.as_ptr().add(i) as *const f64));
                i += 8;
            }
            if len & 0x4 != 0 {
                a_chunk = _mm_insert_epi32::<2>(
                    a_chunk,
                    i32::from_ne_bytes(a.get_unchecked(i..i + 4).try_into().unwrap()),
                );
                b_chunk = _mm_insert_epi32::<2>(
                    b_chunk,
                    i32::from_ne_bytes(b.get_unchecked(i..i + 4).try_into().unwrap()),
                );
                i += 4;
            }
            if len & 0x2 != 0 {
                a_chunk = _mm_insert_epi16::<6>(
                    a_chunk,
                    i16::from_ne_bytes(a.get_unchecked(i..i + 2).try_into().unwrap()) as _,
                );
                b_chunk = _mm_insert_epi16::<6>(
                    b_chunk,
                    i16::from_ne_bytes(b.get_unchecked(i..i + 2).try_into().unwrap()) as _,
                );
                i += 2;
            }
            if len & 0x1 != 0 {
                a_chunk = _mm_insert_epi8::<14>(a_chunk, *a.get_unchecked(i) as _);
                b_chunk = _mm_insert_epi8::<14>(b_chunk, *b.get_unchecked(i) as _);
                // i += 1;
            }
            let a_chunk: [u8; 16] = mem::transmute(a_chunk);
            let b_chunk: [u8; 16] = mem::transmute(b_chunk);
            to_ascii_uppercase_n(a_chunk) == to_ascii_uppercase_n(b_chunk)
        } else {
            // Read 128-bit chunks of the string into 128-bit registers
            let mut i = 0;
            while i < len - 16 {
                let a_chunk = _mm_loadu_si128(a.as_ptr().add(i) as *const _);
                let b_chunk = _mm_loadu_si128(b.as_ptr().add(i) as *const _);
                let a_chunk: [u8; 16] = mem::transmute(a_chunk);
                let b_chunk: [u8; 16] = mem::transmute(b_chunk);
                if to_ascii_uppercase_n(a_chunk) != to_ascii_uppercase_n(b_chunk) {
                    return false;
                }

                i += 16;
            }

            // Handle the remaining bytes
            let a_chunk = _mm_loadu_si128(a.as_ptr().add(len - 16) as *const _);
            let b_chunk = _mm_loadu_si128(b.as_ptr().add(len - 16) as *const _);
            let a_chunk: [u8; 16] = mem::transmute(a_chunk);
            let b_chunk: [u8; 16] = mem::transmute(b_chunk);
            to_ascii_uppercase_n(a_chunk) == to_ascii_uppercase_n(b_chunk)
        }
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

    #[target_feature(enable = "sse2")]
    #[cfg(target_arch = "x86_64")]
    fn hash_bytes(bytes: &[u8]) -> u64 {
        let len = bytes.len();
        let mut s0 = SEED1;
        let mut s1 = SEED2;

        fn uppercase_m128i_to_u64x2(xmm0: __m128i) -> [u64; 2] {
            unsafe {
                let bytes: [u8; 16] = mem::transmute(xmm0);
                let bytes = to_ascii_uppercase_n(bytes);
                mem::transmute(bytes)
            }
        }

        if len <= 16 {
            // XOR the input into s0, s1.
            if len >= 8 {
                let v0 = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let v1 = u64::from_le_bytes(bytes[len - 8..].try_into().unwrap());
                // TODO: Rust generates terrible code when auto vectorizing this,
                //       so use intrinsics explicitly for now.
                let xmm0 = mm_load_128i_from_64(v1 as i64, v0 as i64);
                let xmm0 = uppercase_m128i_to_u64x2(xmm0);
                s0 ^= xmm0[0];
                s1 ^= xmm0[1];
            } else if len >= 4 {
                let v0 = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
                let v1 = u32::from_le_bytes(bytes[len - 4..].try_into().unwrap());
                let xmm0 = mm_load_128i_from_64(v1 as i64, v0 as i64);
                let xmm0 = uppercase_m128i_to_u64x2(xmm0);
                s0 ^= xmm0[0];
                s1 ^= xmm0[1];
            } else if len > 0 {
                let lo = bytes[0];
                let mid = bytes[len / 2];
                let hi = bytes[len - 1];
                let v0 = lo as u64;
                let v1 = ((hi as u64) << 8) | mid as u64;
                let xmm0 = mm_load_128i_from_64(v1 as i64, v0 as i64);
                let xmm0 = uppercase_m128i_to_u64x2(xmm0);
                s0 ^= xmm0[0];
                s1 ^= xmm0[1];
            }
        } else {
            // Handle bulk (can partially overlap with suffix).
            let mut off = 0;
            while off < len - 16 {
                let x = u64::from_le_bytes(bytes[off..off + 8].try_into().unwrap());
                let y = u64::from_le_bytes(bytes[off + 8..off + 16].try_into().unwrap());
                let xmm0 = mm_load_128i_from_64(y as i64, x as i64);
                let [x, y] = uppercase_m128i_to_u64x2(xmm0);

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
            let x = u64::from_le_bytes(suffix[0..8].try_into().unwrap());
            let y = u64::from_le_bytes(suffix[8..16].try_into().unwrap());
            let xmm0 = mm_load_128i_from_64(y as i64, x as i64);
            let [x, y] = uppercase_m128i_to_u64x2(xmm0);
            s0 ^= x;
            s1 ^= y;
        }

        multiply_mix(s0, s1) ^ (len as u64)
    }

    // SAFETY: x86_64 arch always has SSE2 support.
    unsafe { hash_bytes(s) }
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

#[target_feature(enable = "sse2")]
#[cfg(target_arch = "x86_64")]
fn mm_load_128i_from_64(v1: i64, v0: i64) -> __m128i {
    let xmm0 = unsafe { _mm_load_sd(&v0 as *const _ as *const f64) };
    let xmm0 = unsafe { _mm_loadh_pd(xmm0, &v1 as *const _ as *const f64) };
    _mm_castpd_si128(xmm0)
}

#[target_feature(enable = "sse2")]
#[cfg(target_arch = "x86_64")]
fn mm_seth_128i(a: __m128i, h: i64) -> __m128i {
    let a = _mm_castsi128_pd(a);
    let a = unsafe { _mm_loadh_pd(a, &h as *const _ as *const f64) };
    _mm_castpd_si128(a)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq_ignore_ascii_case() {
        assert!(eq_ignore_ascii_case(b"hello", b"HELLO"));
        assert!(!eq_ignore_ascii_case(b"hello", b"world"));
        assert!(eq_ignore_ascii_case(b"Hello World", b"hello world"));
        assert!(!eq_ignore_ascii_case(b"Hello World", b"HelloWorld"));
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
