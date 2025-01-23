use std::{
    borrow::Borrow,
    fmt::Display,
    hash::{Hash, Hasher},
};

/// Case Insensitive wrapper of Ascii strings.
#[derive(Clone, Copy, Debug, Default)]
#[repr(transparent)]
pub struct Ascii<S: ?Sized>(S);
impl<S: AsRef<str>> Ascii<S> {
    pub fn new(value: S) -> Ascii<S> {
        Ascii(value)
    }
    pub fn into_inner(self) -> S {
        self.0
    }
}
impl Ascii<str> {
    pub fn new_str(value: &str) -> &Ascii<str> {
        value.into()
    }
}

impl<S: AsRef<str> + ?Sized> AsRef<str> for Ascii<S> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S: AsRef<str> + ?Sized> Ascii<S> {
    #[inline]
    pub fn as_ascii_ref(&self) -> &Ascii<str> {
        Ascii::new_str(self.0.as_ref())
    }
}

impl<S: AsRef<str> + ?Sized> Hash for Ascii<S> {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        use wide::*;

        // Hash as ASCII lowercase

        // Adapted from: https://github.com/rust-lang/rustc-hash/blob/43e17905ba97af250ea2514c79cc25af0e04dfe5/src/lib.rs#L193
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

        /// A wyhash-inspired non-collision-resistant hash for strings/slices designed
        /// by Orson Peters, with a focus on small strings and small codesize.
        ///
        /// The 64-bit version of this hash passes the SMHasher3 test suite on the full
        /// 64-bit output, that is, f(hash_bytes(b) ^ f(seed)) for some good avalanching
        /// permutation f() passed all tests with zero failures. When using the 32-bit
        /// version of multiply_mix this hash has a few non-catastrophic failures where
        /// there are a handful more collisions than an optimal hash would give.
        ///
        /// We don't bother avalanching here as we'll feed this hash into a
        /// multiplication after which we take the high bits, which avalanches for us.
        #[inline]
        fn hash_bytes(bytes: &[u8]) -> u64 {
            let len = bytes.len();
            let mut s0 = SEED1;
            let mut s1 = SEED2;
            let mut buf = [0u8; 16];

            if len <= 16 {
                let bytes = if len > 0 {
                    bulk_tolower(bytes, &mut buf);
                    &buf[..len]
                } else {
                    bytes
                };

                // XOR the input into s0, s1.
                if len >= 8 {
                    s0 ^= u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                    s1 ^= u64::from_le_bytes(bytes[len - 8..].try_into().unwrap());
                } else if len >= 4 {
                    s0 ^= u32::from_le_bytes(bytes[0..4].try_into().unwrap()) as u64;
                    s1 ^= u32::from_le_bytes(bytes[len - 4..].try_into().unwrap()) as u64;
                } else if len > 0 {
                    let lo = bytes[0];
                    let mid = bytes[len / 2];
                    let hi = bytes[len - 1];
                    s0 ^= lo as u64;
                    s1 ^= ((hi as u64) << 8) | mid as u64;
                }
            } else {
                // Handle bulk (can partially overlap with suffix).
                let mut off = 0;
                while off < len - 16 {
                    bulk_tolower(&bytes[off..off + 16], &mut buf);
                    let bytes = &buf;

                    let x = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                    let y = u64::from_le_bytes(bytes[8..16].try_into().unwrap());

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

                bulk_tolower(&bytes[len - 16..], &mut buf);
                let suffix = &buf;
                s0 ^= u64::from_le_bytes(suffix[0..8].try_into().unwrap());
                s1 ^= u64::from_le_bytes(suffix[8..16].try_into().unwrap());
            }

            multiply_mix(s0, s1) ^ (len as u64)
        }

        /// Bulk conversion of ASCII uppercase to lowercase. Source must not exceed 16 bytes or be empty.
        /// Short inputs are padded with zeroes.
        #[inline]
        fn bulk_tolower(s: &[u8], d: &mut [u8; 16]) {
            let s = bytemuck::cast_slice(s);
            let chunk = i8x16::from(s);
            let v_min = i8x16::from(b'A' as i8);
            let v_max = i8x16::from(b'Z' as i8);
            let mask = !(chunk.cmp_lt(v_min) | chunk.cmp_gt(v_max));
            let chunk = chunk + (mask & i8x16::splat(32));
            let chunk: &[u8] = bytemuck::cast_slice(chunk.as_array_ref());
            d.copy_from_slice(chunk);
        }

        // hasher.write_u64(hash_bytes(self.0.as_ref().as_bytes()));
        let mut s = self.0.as_ref().as_bytes();
        while s.len() >= 8 {
            let mut buf = [0u8; 8];
            for (i, buf) in buf.iter_mut().enumerate() {
                *buf = s[i].to_ascii_lowercase();
            }
            hasher.write_u64(u64::from_ne_bytes(buf));
            s = &s[8..];
        }
        if s.len() >= 4 {
            let mut buf = [0u8; 4];
            for (i, buf) in buf.iter_mut().enumerate() {
                *buf = s[i].to_ascii_lowercase();
            }
            hasher.write_u32(u32::from_ne_bytes(buf));
            s = &s[4..];
        }
        if s.len() >= 2 {
            let mut buf = [0u8; 2];
            for (i, buf) in buf.iter_mut().enumerate() {
                *buf = s[i].to_ascii_lowercase();
            }
            hasher.write_u16(u16::from_ne_bytes(buf));
            s = &s[2..];
        }
        if !s.is_empty() {
            hasher.write_u8(s[0].to_ascii_lowercase());
        }

        // for byte in self.as_ref().bytes().map(|b| b.to_ascii_lowercase()) {
        //     hasher.write_u8(byte);
        // }
    }
}

impl<S: Display> Display for Ascii<S> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}
impl Display for &Ascii<str> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl<S: Borrow<str>> Borrow<Ascii<str>> for Ascii<S> {
    fn borrow(&self) -> &Ascii<str> {
        unsafe { std::mem::transmute(self.0.borrow()) }
    }
}

impl<S: AsRef<str> + ?Sized> PartialEq for Ascii<S> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref().eq_ignore_ascii_case(other.0.as_ref())
    }
}
impl<S: AsRef<str> + ?Sized> Eq for Ascii<S> {}
impl<S: AsRef<str> + ?Sized> PartialOrd for Ascii<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<S: AsRef<str> + ?Sized> Ord for Ascii<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We convert everything to lowercase for comparison
        for (x, y) in self.0.as_ref().bytes().zip(other.0.as_ref().bytes()) {
            let (x, y) = (x.to_ascii_lowercase(), y.to_ascii_lowercase());
            let r = x.cmp(&y);
            if r != std::cmp::Ordering::Equal {
                return r;
            }
        }
        self.0.as_ref().len().cmp(&other.0.as_ref().len())
    }
}

impl From<String> for Ascii<String> {
    fn from(value: String) -> Self {
        Ascii(value)
    }
}
impl From<&str> for &Ascii<str> {
    fn from(value: &str) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
