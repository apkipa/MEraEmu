use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct SimpleUniformGenerator {
    entropy: u128,
    entropy_size: u32,
}

impl SimpleUniformGenerator {
    pub fn new() -> Self {
        SimpleUniformGenerator {
            entropy: 0,
            entropy_size: 0,
        }
    }

    pub fn gen_range(
        &mut self,
        range: impl std::ops::RangeBounds<u64>,
        mut feeder: impl FnMut() -> u64,
    ) -> u64 {
        use std::ops::Bound;

        let mut get_bits_fn = |bits: u32| -> u64 {
            if bits == 0 {
                return 0;
            }
            if bits > 64 {
                panic!("invalid bits count {}", bits);
            }
            if self.entropy_size < bits {
                self.entropy += (feeder() as u128) << self.entropy_size;
                self.entropy_size += 64;
            }
            let result = self.entropy & ((1u128 << bits) - 1);
            self.entropy >>= bits;
            self.entropy_size -= bits;
            result as _
        };

        let low = match range.start_bound() {
            Bound::Unbounded => u64::MIN,
            Bound::Excluded(&x) => x.checked_add(1).expect("invalid range"),
            Bound::Included(&x) => x,
        };
        let len = match range.end_bound() {
            Bound::Unbounded => match (u64::MAX - low).checked_add(1) {
                Some(x) => x,
                None => return get_bits_fn(64),
            },
            Bound::Excluded(&x) if x > low => x - low,
            Bound::Included(&x) if x >= low => match (x - low).checked_add(1) {
                Some(x) => x,
                None => return get_bits_fn(64),
            },
            _ => panic!("invalid range bounds"),
        };

        // Rejection sampling
        let bits_cnt = (len as u128).next_power_of_two().trailing_zeros();
        loop {
            let data = get_bits_fn(bits_cnt);
            if data < len {
                break low + data;
            }
        }
    }
}
