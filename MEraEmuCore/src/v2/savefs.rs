use anyhow::Context;
use binrw::binrw;

use crate::{
    types::*,
    util::io::{read_utf16_string, write_utf16_string},
};

#[binrw]
#[brw(repr = u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraSaveFileType {
    Normal = 0x00,
    Global = 0x01,
    Var = 0x02,
    CharVar = 0x03,
}

#[binrw]
#[brw(repr = u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraSaveDataType {
    Int = 0x00,
    IntArray = 0x01,
    IntArray2D = 0x02,
    IntArray3D = 0x03,
    Str = 0x10,
    StrArray = 0x11,
    StrArray2D = 0x12,
    StrArray3D = 0x13,

    Separator = 0xfd,
    EOC = 0xfe,
    EOF = 0xff,
}

#[binrw]
#[brw(repr = u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum EraBinaryMark {
    Byte = 0xcf,
    Int16 = 0xd0,
    Int32 = 0xd1,
    Int64 = 0xd2,
    String = 0xd8,
    EoA1 = 0xe0,
    EoA2 = 0xe1,
    Zero = 0xf0,
    ZeroA1 = 0xf1,
    ZeroA2 = 0xf2,
    EoD = 0xff,
}

#[binrw]
#[brw(little, magic = 0x0A1A0A0D41524589u64)]
#[derive(Debug, Clone)]
pub struct EraSaveFileHeader {
    pub version: u32,
    #[bw(try_calc = data.len().try_into())]
    pub data_count: u32,
    #[br(count = data_count)]
    pub data: Vec<u32>,
    pub file_type: EraSaveFileType,
    pub game_code: i64,
    pub game_version: i64,
    #[br(parse_with = read_utf16_string)]
    #[bw(write_with = write_utf16_string)]
    pub save_info: String,
}

pub trait EraSaveFileReadExt {
    fn read_encoded_int(&mut self) -> anyhow::Result<i64>;
    fn try_read_encoded_int_with_mark(&mut self, mark: u8) -> anyhow::Result<Option<i64>>;
    fn read_int_array_0d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_int_array_1d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_int_array_2d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_int_array_3d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_str_array_0d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_str_array_1d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_str_array_2d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_str_array_3d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
    fn read_var(
        &mut self,
        var_type: EraSaveDataType,
        dst: &mut EraVarInfo,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
}

impl<T: std::io::Read> EraSaveFileReadExt for T {
    fn read_encoded_int(&mut self) -> anyhow::Result<i64> {
        use crate::util::io::CSharpBinaryReader;
        let b = self.read_u8()?;
        self.try_read_encoded_int_with_mark(b)?
            .context("invalid binary data")
    }
    fn try_read_encoded_int_with_mark(&mut self, mark: u8) -> anyhow::Result<Option<i64>> {
        use crate::util::io::CSharpBinaryReader;
        let b = mark;
        Ok(if b <= EraBinaryMark::Byte as u8 {
            Some(b.into())
        } else if b == EraBinaryMark::Int16 as u8 {
            Some(self.read_i16()?.into())
        } else if b == EraBinaryMark::Int32 as u8 {
            Some(self.read_i32()?.into())
        } else if b == EraBinaryMark::Int64 as u8 {
            Some(self.read_i64()?)
        } else {
            None
        })
    }
    fn read_int_array_0d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        dst_vals[0] = IntValue {
            val: self.read_encoded_int()?,
        };
        Ok(())
    }
    fn read_int_array_1d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        let save_len = self.read_i32()?;
        dst_vals.fill(Default::default());
        let mut offset = 0;
        loop {
            let bin_mark = self.read_u8()?;
            if let Some(x) = self.try_read_encoded_int_with_mark(bin_mark)? {
                // Don't handle excessive data, drain them instead
                if let Some(dst) = dst_vals.get_mut(offset) {
                    *dst = IntValue { val: x };
                }
                offset += 1;
            } else if bin_mark == EraBinaryMark::EoD as u8 {
                break;
            } else if bin_mark == EraBinaryMark::Zero as u8 {
                offset += self.read_encoded_int()? as usize;
            } else {
                anyhow::bail!("invalid binary data");
            }
        }
        Ok(())
    }
    fn read_int_array_2d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 2 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        // Vertical
        let save_len_0 = self.read_i32()?;
        // Horizontal
        let save_len_1 = self.read_i32()?;
        dst_vals.fill(Default::default());
        let mut offset_0 = 0;
        let mut offset_1 = 0;
        loop {
            let bin_mark = self.read_u8()?;
            if let Some(x) = self.try_read_encoded_int_with_mark(bin_mark)? {
                // Don't handle excessive data, drain them instead
                if offset_0 < dim_0 && offset_1 < dim_1 {
                    let dst_idx = offset_0 * dim_1 + offset_1;
                    let dst = dst_vals.get_mut(dst_idx).unwrap();
                    *dst = IntValue { val: x };
                }
                offset_1 += 1;
            } else if bin_mark == EraBinaryMark::EoD as u8 {
                break;
            } else if bin_mark == EraBinaryMark::Zero as u8 {
                offset_1 += self.read_encoded_int()? as usize;
            } else if bin_mark == EraBinaryMark::ZeroA1 as u8 {
                offset_0 += self.read_encoded_int()? as usize;
                offset_1 = 0;
            } else if bin_mark == EraBinaryMark::EoA1 as u8 {
                offset_0 += 1;
                offset_1 = 0;
            } else {
                anyhow::bail!("invalid binary data");
            }
        }
        Ok(())
    }
    fn read_int_array_3d(
        &mut self,
        dst: &mut ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 3 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let dim_2 = dims[2] as usize;
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        // Vertical
        let save_len_0 = self.read_i32()?;
        // Horizontal
        let save_len_1 = self.read_i32()?;
        let save_len_2 = self.read_i32()?;
        dst_vals.fill(Default::default());
        let mut offset_0 = 0;
        let mut offset_1 = 0;
        let mut offset_2 = 0;
        loop {
            let bin_mark = self.read_u8()?;
            if let Some(x) = self.try_read_encoded_int_with_mark(bin_mark)? {
                // Don't handle excessive data, drain them instead
                if offset_0 < dim_0 && offset_1 < dim_1 && offset_2 < dim_2 {
                    let dst_idx = offset_0 * dim_1 * dim_2 + offset_1 * dim_2 + offset_2;
                    let dst = dst_vals.get_mut(dst_idx).unwrap();
                    *dst = IntValue { val: x };
                }
                offset_2 += 1;
            } else if bin_mark == EraBinaryMark::EoD as u8 {
                break;
            } else if bin_mark == EraBinaryMark::Zero as u8 {
                offset_2 += self.read_encoded_int()? as usize;
            } else if bin_mark == EraBinaryMark::ZeroA1 as u8 {
                offset_1 += self.read_encoded_int()? as usize;
                offset_2 = 0;
            } else if bin_mark == EraBinaryMark::EoA1 as u8 {
                offset_1 += 1;
                offset_2 = 0;
            } else if bin_mark == EraBinaryMark::ZeroA2 as u8 {
                offset_0 += self.read_encoded_int()? as usize;
                offset_1 = 0;
                offset_2 = 0;
            } else if bin_mark == EraBinaryMark::EoA2 as u8 {
                offset_0 += 1;
                offset_1 = 0;
                offset_2 = 0;
            } else {
                anyhow::bail!("invalid binary data");
            }
        }
        Ok(())
    }
    fn read_str_array_0d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        dst_vals[0] = StrValue {
            val: self.read_utf16_string()?.into(),
        };
        Ok(())
    }
    fn read_str_array_1d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        let save_len = self.read_i32()?;
        dst_vals.fill(Default::default());
        let mut offset = 0;
        loop {
            let bin_mark = self.read_u8()?;
            if bin_mark == EraBinaryMark::String as u8 {
                // Don't handle excessive data, drain them instead
                if let Some(dst) = dst_vals.get_mut(offset) {
                    *dst = StrValue {
                        val: self.read_utf16_string()?.into(),
                    };
                }
                offset += 1;
            } else if bin_mark == EraBinaryMark::EoD as u8 {
                break;
            } else if bin_mark == EraBinaryMark::Zero as u8 {
                offset += self.read_encoded_int()? as usize;
            } else {
                anyhow::bail!("invalid binary data");
            }
        }
        Ok(())
    }
    fn read_str_array_2d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 2 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        // Vertical
        let save_len_0 = self.read_i32()?;
        // Horizontal
        let save_len_1 = self.read_i32()?;
        dst_vals.fill(Default::default());
        let mut offset_0 = 0;
        let mut offset_1 = 0;
        loop {
            let bin_mark = self.read_u8()?;
            if bin_mark == EraBinaryMark::String as u8 {
                // Don't handle excessive data, drain them instead
                if offset_0 < dim_0 && offset_1 < dim_1 {
                    let dst_idx = offset_0 * dim_1 + offset_1;
                    let dst = dst_vals.get_mut(dst_idx).unwrap();
                    *dst = StrValue {
                        val: self.read_utf16_string()?.into(),
                    };
                }
                offset_1 += 1;
            } else if bin_mark == EraBinaryMark::EoD as u8 {
                break;
            } else if bin_mark == EraBinaryMark::Zero as u8 {
                offset_1 += self.read_encoded_int()? as usize;
            } else if bin_mark == EraBinaryMark::ZeroA1 as u8 {
                offset_0 += self.read_encoded_int()? as usize;
                offset_1 = 0;
            } else if bin_mark == EraBinaryMark::EoA1 as u8 {
                offset_0 += 1;
                offset_1 = 0;
            } else {
                anyhow::bail!("invalid binary data");
            }
        }
        Ok(())
    }
    fn read_str_array_3d(
        &mut self,
        dst: &mut ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryReader;
        let dims = &dst.dims[(chara_i.is_some() as usize)..];
        let stride = dims.iter().copied().product::<u32>() as usize;
        if dims.len() != 3 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let dim_2 = dims[2] as usize;
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        // Vertical
        let save_len_0 = self.read_i32()?;
        // Horizontal
        let save_len_1 = self.read_i32()?;
        let save_len_2 = self.read_i32()?;
        dst_vals.fill(Default::default());
        let mut offset_0 = 0;
        let mut offset_1 = 0;
        let mut offset_2 = 0;
        loop {
            let bin_mark = self.read_u8()?;
            if bin_mark == EraBinaryMark::String as u8 {
                // Don't handle excessive data, drain them instead
                if offset_0 < dim_0 && offset_1 < dim_1 && offset_2 < dim_2 {
                    let dst_idx = offset_0 * dim_1 * dim_2 + offset_1 * dim_2 + offset_2;
                    let dst = dst_vals.get_mut(dst_idx).unwrap();
                    *dst = StrValue {
                        val: self.read_utf16_string()?.into(),
                    };
                }
                offset_2 += 1;
            } else if bin_mark == EraBinaryMark::EoD as u8 {
                break;
            } else if bin_mark == EraBinaryMark::Zero as u8 {
                offset_2 += self.read_encoded_int()? as usize;
            } else if bin_mark == EraBinaryMark::ZeroA1 as u8 {
                offset_1 += self.read_encoded_int()? as usize;
                offset_2 = 0;
            } else if bin_mark == EraBinaryMark::EoA1 as u8 {
                offset_1 += 1;
                offset_2 = 0;
            } else if bin_mark == EraBinaryMark::ZeroA2 as u8 {
                offset_0 += self.read_encoded_int()? as usize;
                offset_1 = 0;
                offset_2 = 0;
            } else if bin_mark == EraBinaryMark::EoA2 as u8 {
                offset_0 += 1;
                offset_1 = 0;
                offset_2 = 0;
            } else {
                anyhow::bail!("invalid binary data");
            }
        }
        Ok(())
    }
    fn read_var(
        &mut self,
        var_type: EraSaveDataType,
        var: &mut EraVarInfo,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use EraSaveDataType::*;
        match var_type {
            Int | IntArray | IntArray2D | IntArray3D => {
                let mut var_val = var.val.as_arrint_mut().context("variable type mismatch")?;
                var_val.ensure_alloc();
                match var_type {
                    Int => self.read_int_array_0d(&mut var_val, chara_i)?,
                    IntArray => self.read_int_array_1d(&mut var_val, chara_i)?,
                    IntArray2D => self.read_int_array_2d(&mut var_val, chara_i)?,
                    IntArray3D => self.read_int_array_3d(&mut var_val, chara_i)?,
                    _ => unreachable!(),
                }
            }
            Str | StrArray | StrArray2D | StrArray3D => {
                let mut var_val = var.val.as_arrstr_mut().context("variable type mismatch")?;
                var_val.ensure_alloc();
                match var_type {
                    Str => self.read_str_array_0d(&mut var_val, chara_i)?,
                    StrArray => self.read_str_array_1d(&mut var_val, chara_i)?,
                    StrArray2D => self.read_str_array_2d(&mut var_val, chara_i)?,
                    StrArray3D => self.read_str_array_3d(&mut var_val, chara_i)?,
                    _ => unreachable!(),
                }
            }
            _ => anyhow::bail!("invalid var type {var_type:?}"),
        }
        Ok(())
    }
}
