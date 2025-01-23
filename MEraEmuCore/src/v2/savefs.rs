use anyhow::Context;
use binrw::binrw;

use crate::{
    types::*,
    util::io::{read_utf16_string, write_utf16_string},
};

#[binrw]
#[brw(repr = u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
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

impl EraSaveDataType {
    pub fn new_var(is_str: bool, var_dims_cnt: usize) -> anyhow::Result<Self> {
        Ok(if is_str {
            match var_dims_cnt {
                0 => EraSaveDataType::Str,
                1 => EraSaveDataType::StrArray,
                2 => EraSaveDataType::StrArray2D,
                3 => EraSaveDataType::StrArray3D,
                _ => anyhow::bail!(
                    "unsupported array dimension count {} for save",
                    var_dims_cnt
                ),
            }
        } else {
            match var_dims_cnt {
                0 => EraSaveDataType::Int,
                1 => EraSaveDataType::IntArray,
                2 => EraSaveDataType::IntArray2D,
                3 => EraSaveDataType::IntArray3D,
                _ => anyhow::bail!(
                    "unsupported array dimension count {} for save",
                    var_dims_cnt
                ),
            }
        })
    }
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        let _save_len = self.read_i32()?;
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
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
        let _save_len_0 = self.read_i32()?;
        // Horizontal
        let _save_len_1 = self.read_i32()?;
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
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
        let _save_len_0 = self.read_i32()?;
        // Horizontal
        let _save_len_1 = self.read_i32()?;
        let _save_len_2 = self.read_i32()?;
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let dst_vals = if let Some(chara_i) = chara_i {
            &mut dst.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &mut dst.vals[..]
        };
        let _save_len = self.read_i32()?;
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
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
        let _save_len_0 = self.read_i32()?;
        // Horizontal
        let _save_len_1 = self.read_i32()?;
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
        let stride: usize = dims.iter().map(|&x| x as usize).product();
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
        let _save_len_0 = self.read_i32()?;
        // Horizontal
        let _save_len_1 = self.read_i32()?;
        let _save_len_2 = self.read_i32()?;
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

pub trait EraSaveFileWriteExt {
    fn write_encoded_int(&mut self, val: i64) -> anyhow::Result<()>;
    fn write_int_array_0d(&mut self, src: &ArrIntValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_int_array_1d(&mut self, src: &ArrIntValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_int_array_2d(&mut self, src: &ArrIntValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_int_array_3d(&mut self, src: &ArrIntValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_str_array_0d(&mut self, src: &ArrStrValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_str_array_1d(&mut self, src: &ArrStrValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_str_array_2d(&mut self, src: &ArrStrValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_str_array_3d(&mut self, src: &ArrStrValue, chara_i: Option<u32>)
        -> anyhow::Result<()>;
    fn write_var(
        &mut self,
        var_type: EraSaveDataType,
        val: &EraVarInfo,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()>;
}

impl<T: std::io::Write> EraSaveFileWriteExt for T {
    fn write_encoded_int(&mut self, val: i64) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        if val >= 0 && val <= EraBinaryMark::Byte as i64 {
            self.write_u8(val as u8)?;
        } else if val >= i16::MIN as i64 && val <= i16::MAX as i64 {
            self.write_u8(EraBinaryMark::Int16 as u8)?;
            self.write_i16(val as i16)?;
        } else if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
            self.write_u8(EraBinaryMark::Int32 as u8)?;
            self.write_i32(val as i32)?;
        } else {
            self.write_u8(EraBinaryMark::Int64 as u8)?;
            self.write_i64(val)?;
        }
        Ok(())
    }
    fn write_int_array_0d(
        &mut self,
        src: &ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_encoded_int(src_vals[0].val)?;
        Ok(())
    }
    fn write_int_array_1d(
        &mut self,
        src: &ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_i32(src_vals.len() as i32)?;
        let mut zero_count = 0;
        for val in src_vals.iter() {
            if val.val == 0 {
                zero_count += 1;
            } else {
                if zero_count > 0 {
                    self.write_u8(EraBinaryMark::Zero as u8)?;
                    self.write_encoded_int(zero_count)?;
                    zero_count = 0;
                }
                self.write_encoded_int(val.val)?;
            }
        }
        self.write_u8(EraBinaryMark::EoD as u8)?;
        Ok(())
    }
    fn write_int_array_2d(
        &mut self,
        src: &ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 2 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_i32(dim_0 as i32)?;
        self.write_i32(dim_1 as i32)?;
        let mut zeros_count = [0; 2];
        for (i, val) in src_vals.iter().enumerate() {
            let offset_0 = i / dim_1;
            let offset_1 = i % dim_1;
            if val.val == 0 {
                zeros_count[1] += 1;
            } else {
                if zeros_count[0] > 0 {
                    self.write_u8(EraBinaryMark::ZeroA1 as u8)?;
                    self.write_encoded_int(zeros_count[0])?;
                    zeros_count[0] = 0;
                }
                if zeros_count[1] > 0 {
                    self.write_u8(EraBinaryMark::Zero as u8)?;
                    self.write_encoded_int(zeros_count[1])?;
                    zeros_count[1] = 0;
                }
                self.write_encoded_int(val.val)?;
            }
            if offset_1 + 1 == dim_1 {
                // Row ended
                if zeros_count[1] == dim_1 as i64 {
                    zeros_count[0] += 1;
                } else {
                    self.write_u8(EraBinaryMark::EoA1 as u8)?;
                }
                zeros_count[1] = 0;
            }
        }
        self.write_u8(EraBinaryMark::EoD as u8)?;
        Ok(())
    }
    fn write_int_array_3d(
        &mut self,
        src: &ArrIntValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 3 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let dim_2 = dims[2] as usize;
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_i32(dim_0 as i32)?;
        self.write_i32(dim_1 as i32)?;
        self.write_i32(dim_2 as i32)?;
        let mut zeros_count = [0; 3];
        for (i, val) in src_vals.iter().enumerate() {
            let offset_0 = i / (dim_1 * dim_2);
            let offset_1 = (i / dim_2) % dim_1;
            let offset_2 = i % dim_2;
            if val.val == 0 {
                zeros_count[2] += 1;
            } else {
                if zeros_count[0] > 0 {
                    self.write_u8(EraBinaryMark::ZeroA2 as u8)?;
                    self.write_encoded_int(zeros_count[0])?;
                    zeros_count[0] = 0;
                }
                if zeros_count[1] > 0 {
                    self.write_u8(EraBinaryMark::ZeroA1 as u8)?;
                    self.write_encoded_int(zeros_count[1])?;
                    zeros_count[1] = 0;
                }
                if zeros_count[2] > 0 {
                    self.write_u8(EraBinaryMark::Zero as u8)?;
                    self.write_encoded_int(zeros_count[2])?;
                    zeros_count[2] = 0;
                }
                self.write_encoded_int(val.val)?;
            }
            if offset_2 + 1 == dim_2 {
                if zeros_count[2] == dim_2 as i64 {
                    zeros_count[1] += 1;
                } else {
                    self.write_u8(EraBinaryMark::EoA1 as u8)?;
                }
                zeros_count[2] = 0;

                if offset_1 + 1 == dim_1 {
                    if zeros_count[1] == dim_1 as i64 {
                        zeros_count[0] += 1;
                    } else {
                        self.write_u8(EraBinaryMark::EoA2 as u8)?;
                    }
                    zeros_count[1] = 0;
                }
            }
        }
        self.write_u8(EraBinaryMark::EoD as u8)?;
        Ok(())
    }
    fn write_str_array_0d(
        &mut self,
        src: &ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_utf16_string(&src_vals[0].val)?;
        Ok(())
    }
    fn write_str_array_1d(
        &mut self,
        src: &ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 1 {
            anyhow::bail!("invalid dimension");
        }
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_i32(src_vals.len() as i32)?;
        let mut zero_count = 0;
        for val in src_vals.iter() {
            if val.val.is_empty() {
                zero_count += 1;
            } else {
                if zero_count > 0 {
                    self.write_u8(EraBinaryMark::Zero as u8)?;
                    self.write_encoded_int(zero_count)?;
                    zero_count = 0;
                }
                self.write_u8(EraBinaryMark::String as u8)?;
                self.write_utf16_string(&val.val)?;
            }
        }
        self.write_u8(EraBinaryMark::EoD as u8)?;
        Ok(())
    }
    fn write_str_array_2d(
        &mut self,
        src: &ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 2 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_i32(dim_0 as i32)?;
        self.write_i32(dim_1 as i32)?;
        let mut zeros_count = [0; 2];
        for (i, val) in src_vals.iter().enumerate() {
            let offset_0 = i / dim_1;
            let offset_1 = i % dim_1;
            if val.val.is_empty() {
                zeros_count[1] += 1;
            } else {
                if zeros_count[0] > 0 {
                    self.write_u8(EraBinaryMark::ZeroA1 as u8)?;
                    self.write_encoded_int(zeros_count[0])?;
                    zeros_count[0] = 0;
                }
                if zeros_count[1] > 0 {
                    self.write_u8(EraBinaryMark::Zero as u8)?;
                    self.write_encoded_int(zeros_count[1])?;
                    zeros_count[1] = 0;
                }
                self.write_u8(EraBinaryMark::String as u8)?;
                self.write_utf16_string(&val.val)?;
            }
            if offset_1 + 1 == dim_1 {
                if zeros_count[1] == dim_1 as i64 {
                    zeros_count[0] += 1;
                } else {
                    self.write_u8(EraBinaryMark::EoA1 as u8)?;
                }
                zeros_count[1] = 0;
            }
        }
        self.write_u8(EraBinaryMark::EoD as u8)?;
        Ok(())
    }
    fn write_str_array_3d(
        &mut self,
        src: &ArrStrValue,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use crate::util::io::CSharpBinaryWriter;
        let dims = &src.dims[(chara_i.is_some() as usize)..];
        let stride: usize = dims.iter().map(|&x| x as usize).product();
        if dims.len() != 3 {
            anyhow::bail!("invalid dimension");
        }
        let dim_0 = dims[0] as usize;
        let dim_1 = dims[1] as usize;
        let dim_2 = dims[2] as usize;
        let src_vals = if let Some(chara_i) = chara_i {
            &src.vals[(chara_i as usize * stride)..(chara_i as usize * stride + stride)]
        } else {
            &src.vals[..]
        };
        self.write_i32(dim_0 as i32)?;
        self.write_i32(dim_1 as i32)?;
        self.write_i32(dim_2 as i32)?;
        let mut zeros_count = [0; 3];
        for (i, val) in src_vals.iter().enumerate() {
            let offset_0 = i / (dim_1 * dim_2);
            let offset_1 = (i / dim_2) % dim_1;
            let offset_2 = i % dim_2;
            if val.val.is_empty() {
                zeros_count[2] += 1;
            } else {
                if zeros_count[0] > 0 {
                    self.write_u8(EraBinaryMark::ZeroA2 as u8)?;
                    self.write_encoded_int(zeros_count[0])?;
                    zeros_count[0] = 0;
                }
                if zeros_count[1] > 0 {
                    self.write_u8(EraBinaryMark::ZeroA1 as u8)?;
                    self.write_encoded_int(zeros_count[1])?;
                    zeros_count[1] = 0;
                }
                if zeros_count[2] > 0 {
                    self.write_u8(EraBinaryMark::Zero as u8)?;
                    self.write_encoded_int(zeros_count[2])?;
                    zeros_count[2] = 0;
                }
                self.write_u8(EraBinaryMark::String as u8)?;
                self.write_utf16_string(&val.val)?;
            }
            if offset_2 + 1 == dim_2 {
                if zeros_count[2] == dim_2 as i64 {
                    zeros_count[1] += 1;
                } else {
                    self.write_u8(EraBinaryMark::EoA1 as u8)?;
                }
                zeros_count[2] = 0;

                if offset_1 + 1 == dim_1 {
                    if zeros_count[1] == dim_1 as i64 {
                        zeros_count[0] += 1;
                    } else {
                        self.write_u8(EraBinaryMark::EoA2 as u8)?;
                    }
                    zeros_count[1] = 0;
                }
            }
        }
        self.write_u8(EraBinaryMark::EoD as u8)?;
        Ok(())
    }
    fn write_var(
        &mut self,
        var_type: EraSaveDataType,
        val: &EraVarInfo,
        chara_i: Option<u32>,
    ) -> anyhow::Result<()> {
        use EraSaveDataType::*;
        match var_type {
            Int | IntArray | IntArray2D | IntArray3D => {
                let var_val = val.val.as_arrint().context("variable type mismatch")?;
                match var_type {
                    Int => self.write_int_array_0d(var_val, chara_i)?,
                    IntArray => self.write_int_array_1d(var_val, chara_i)?,
                    IntArray2D => self.write_int_array_2d(var_val, chara_i)?,
                    IntArray3D => self.write_int_array_3d(var_val, chara_i)?,
                    _ => unreachable!(),
                }
            }
            Str | StrArray | StrArray2D | StrArray3D => {
                let var_val = val.val.as_arrstr().context("variable type mismatch")?;
                match var_type {
                    Str => self.write_str_array_0d(var_val, chara_i)?,
                    StrArray => self.write_str_array_1d(var_val, chara_i)?,
                    StrArray2D => self.write_str_array_2d(var_val, chara_i)?,
                    StrArray3D => self.write_str_array_3d(var_val, chara_i)?,
                    _ => unreachable!(),
                }
            }
            _ => anyhow::bail!("invalid var type {var_type:?}"),
        }
        Ok(())
    }
}
