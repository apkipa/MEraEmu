use mee_engine::*;
use std::hint::black_box;

/// Helper function to convert a byte array to uppercase ASCII.
#[inline]
pub(crate) fn to_ascii_uppercase_n<const N: usize>(mut s: [u8; N]) -> [u8; N] {
    s.make_ascii_uppercase();
    s
}

pub fn to_ascii_uppercase_void(s: [u8; 16]) -> [u8; 16] {
    s
}

#[cfg(not(target_arch = "wasm32"))]
pub fn to_ascii_uppercase_16(s: [u8; 16]) -> [u8; 16] {
    to_ascii_uppercase_n(s)
}

const RUN_TIMES: usize = 100_000_000;

fn bench_to_ascii_uppercase_void() -> String {
    let input = b"Hello, wasmtest1";
    let t0 = chrono::Local::now();
    for i in 0..RUN_TIMES {
        black_box(to_ascii_uppercase_void(*black_box(input)));
    }
    let elapsed = chrono::Local::now() - t0;
    format!("Bench to_ascii_uppercase_void took: {}", elapsed)
}

fn bench_to_ascii_uppercase() -> String {
    let input = b"Hello, wasmtest1";
    let t0 = chrono::Local::now();
    for i in 0..RUN_TIMES {
        black_box(to_ascii_uppercase_n(*black_box(input)));
    }
    let elapsed = chrono::Local::now() - t0;
    format!("Bench to_ascii_uppercase took: {}", elapsed)
}

fn bench_to_ascii_uppercase_simd() -> String {
    let input = b"Hello, wasmtest1";
    let t0 = chrono::Local::now();
    for i in 0..RUN_TIMES {
        black_box(to_ascii_uppercase_16(*black_box(input)));
    }
    let elapsed = chrono::Local::now() - t0;
    format!("Bench to_ascii_uppercase_simd took: {}", elapsed)
}

#[test]
pub fn bench_me() {
    let mut result = String::new();

    result.push_str(bench_to_ascii_uppercase_void().as_str());
    result.push_str("\n");
    result.push_str(bench_to_ascii_uppercase().as_str());
    result.push_str("\n");
    result.push_str(bench_to_ascii_uppercase_simd().as_str());
    result.push_str("\n");

    panic!("{}", result);
}
