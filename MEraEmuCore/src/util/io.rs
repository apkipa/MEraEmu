use std::io::{Seek, Write};

use binrw::{binrw, parser, writer, BinReaderExt, BinResult, BinWriterExt, Endian};
use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use itertools::Itertools;

pub trait CSharpBinaryReader {
    fn read_i64(&mut self) -> std::io::Result<i64>;
    fn read_u64(&mut self) -> std::io::Result<u64>;
    fn read_i32(&mut self) -> std::io::Result<i32>;
    fn read_u32(&mut self) -> std::io::Result<u32>;
    fn read_i16(&mut self) -> std::io::Result<i16>;
    fn read_u16(&mut self) -> std::io::Result<u16>;
    fn read_i8(&mut self) -> std::io::Result<i8>;
    fn read_u8(&mut self) -> std::io::Result<u8>;
    fn read_7_bit_i32(&mut self) -> std::io::Result<i32>;
    fn read_utf16_string(&mut self) -> std::io::Result<String>;
}
pub trait CSharpBinaryWriter {
    fn write_i64(&mut self, val: i64) -> std::io::Result<()>;
    fn write_u64(&mut self, val: u64) -> std::io::Result<()>;
    fn write_i32(&mut self, val: i32) -> std::io::Result<()>;
    fn write_u32(&mut self, val: u32) -> std::io::Result<()>;
    fn write_i16(&mut self, val: i16) -> std::io::Result<()>;
    fn write_u16(&mut self, val: u16) -> std::io::Result<()>;
    fn write_i8(&mut self, val: i8) -> std::io::Result<()>;
    fn write_u8(&mut self, val: u8) -> std::io::Result<()>;
    fn write_7_bit_i32(&mut self, val: i32) -> std::io::Result<()>;
    fn write_utf16_string(&mut self, val: &str) -> std::io::Result<()>;
}

impl<T: std::io::Read> CSharpBinaryReader for T {
    fn read_i64(&mut self) -> std::io::Result<i64> {
        <Self as ReadBytesExt>::read_i64::<LE>(self)
    }
    fn read_u64(&mut self) -> std::io::Result<u64> {
        <Self as ReadBytesExt>::read_u64::<LE>(self)
    }
    fn read_i32(&mut self) -> std::io::Result<i32> {
        <Self as ReadBytesExt>::read_i32::<LE>(self)
    }
    fn read_u32(&mut self) -> std::io::Result<u32> {
        <Self as ReadBytesExt>::read_u32::<LE>(self)
    }
    fn read_i16(&mut self) -> std::io::Result<i16> {
        <Self as ReadBytesExt>::read_i16::<LE>(self)
    }
    fn read_u16(&mut self) -> std::io::Result<u16> {
        <Self as ReadBytesExt>::read_u16::<LE>(self)
    }
    fn read_i8(&mut self) -> std::io::Result<i8> {
        <Self as ReadBytesExt>::read_i8(self)
    }
    fn read_u8(&mut self) -> std::io::Result<u8> {
        <Self as ReadBytesExt>::read_u8(self)
    }
    fn read_7_bit_i32(&mut self) -> std::io::Result<i32> {
        const BYTES_COUNT: usize = 32 / 7;
        const REMAINING_BITS_COUNT: usize = 32 % 7;
        let mut read_bits = 0;
        let mut result: u32 = 0;
        while read_bits < 7 * BYTES_COUNT {
            let cur_byte = ReadBytesExt::read_u8(self)?;
            result += ((cur_byte & 0x7f) as u32) << read_bits;
            if cur_byte <= 0x7f {
                return Ok(result as _);
            }
            read_bits += 7;
        }
        // Read remaining bits
        let cur_byte = ReadBytesExt::read_u8(self)?;
        if cur_byte > ((1 << REMAINING_BITS_COUNT) - 1) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "bad 7-bit Int32",
            ));
        }
        result += (cur_byte as u32) << read_bits;
        Ok(result as _)
    }
    fn read_utf16_string(&mut self) -> std::io::Result<String> {
        let bytes_len = self.read_7_bit_i32()?;
        if bytes_len < 0 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("bad string length (got {bytes_len})"),
            ));
        }
        let bytes_len = bytes_len as u32;
        if bytes_len % 2 != 0 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("bad string length (got {bytes_len})"),
            ));
        }
        if bytes_len == 0 {
            return Ok(String::new());
        }
        let str_len = bytes_len / 2;
        let result: Result<String, _> = (0..str_len)
            .map(|_| ReadBytesExt::read_u16::<LE>(self))
            .process_results(|x| char::decode_utf16(x).collect())?;
        let Ok(result) = result else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "bad UTF-16 string",
            ));
        };
        Ok(result)
    }
}
impl<T: std::io::Write> CSharpBinaryWriter for T {
    fn write_i64(&mut self, val: i64) -> std::io::Result<()> {
        WriteBytesExt::write_i64::<LE>(self, val)
    }
    fn write_u64(&mut self, val: u64) -> std::io::Result<()> {
        WriteBytesExt::write_u64::<LE>(self, val)
    }
    fn write_i32(&mut self, val: i32) -> std::io::Result<()> {
        WriteBytesExt::write_i32::<LE>(self, val)
    }
    fn write_u32(&mut self, val: u32) -> std::io::Result<()> {
        WriteBytesExt::write_u32::<LE>(self, val)
    }
    fn write_i16(&mut self, val: i16) -> std::io::Result<()> {
        WriteBytesExt::write_i16::<LE>(self, val)
    }
    fn write_u16(&mut self, val: u16) -> std::io::Result<()> {
        WriteBytesExt::write_u16::<LE>(self, val)
    }
    fn write_i8(&mut self, val: i8) -> std::io::Result<()> {
        WriteBytesExt::write_i8(self, val)
    }
    fn write_u8(&mut self, val: u8) -> std::io::Result<()> {
        WriteBytesExt::write_u8(self, val)
    }
    fn write_7_bit_i32(&mut self, val: i32) -> std::io::Result<()> {
        let mut val = val as u32;
        while val > 0x7f {
            WriteBytesExt::write_u8(self, (val | 0x80) as _)?;
            val >>= 7;
        }
        WriteBytesExt::write_u8(self, val as _)
    }
    fn write_utf16_string(&mut self, val: &str) -> std::io::Result<()> {
        let bytes_cnt = val.encode_utf16().count() * 2;
        self.write_7_bit_i32(bytes_cnt.try_into().map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, "string too long")
        })?)?;
        for byte in val.encode_utf16() {
            WriteBytesExt::write_u16::<LE>(self, byte)?;
        }
        Ok(())
    }
}

#[parser(reader)]
pub fn read_var_i32() -> BinResult<i32> {
    const BYTES_COUNT: usize = 32 / 7;
    const REMAINING_BITS_COUNT: usize = 32 % 7;
    let mut read_bits = 0;
    let mut result: u32 = 0;
    while read_bits < 7 * BYTES_COUNT {
        let cur_byte = ReadBytesExt::read_u8(reader)?;
        result += ((cur_byte & 0x7f) as u32) << read_bits;
        if cur_byte <= 0x7f {
            return Ok(result as _);
        }
        read_bits += 7;
    }
    // Read remaining bits
    let cur_byte = ReadBytesExt::read_u8(reader)?;
    if cur_byte > ((1 << REMAINING_BITS_COUNT) - 1) {
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "bad VarI32").into());
    }
    result += (cur_byte as u32) << read_bits;
    Ok(result as _)
}

#[writer(writer)]
pub fn write_var_i32(val: i32) -> BinResult<()> {
    let mut val = val as u32;
    while val > 0x7f {
        WriteBytesExt::write_u8(writer, (val | 0x80) as _)?;
        val >>= 7;
    }
    WriteBytesExt::write_u8(writer, val as _)?;
    Ok(())
}

#[parser(reader, endian)]
pub fn read_utf16_string() -> BinResult<String> {
    let bytes_len = read_var_i32(reader, endian, ())?;
    if bytes_len < 0 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("bad string length (got {bytes_len})"),
        )
        .into());
    }
    let bytes_len = bytes_len as u32;
    if bytes_len % 2 != 0 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("bad string length (got {bytes_len})"),
        )
        .into());
    }
    if bytes_len == 0 {
        return Ok(String::new());
    }
    let str_len = bytes_len / 2;
    let result: Result<String, _> = (0..str_len)
        .map(|_| reader.read_type::<u16>(endian))
        .process_results(|x| char::decode_utf16(x).collect())?;
    let Ok(result) = result else {
        return Err(
            std::io::Error::new(std::io::ErrorKind::InvalidData, "bad UTF-16 string").into(),
        );
    };
    Ok(result)
}

pub fn write_utf16_str<W>(val: &str, writer: &mut W, endian: Endian) -> BinResult<()>
where
    W: Write + Seek,
{
    let bytes_cnt = val.encode_utf16().count() * 2;
    write_var_i32(
        bytes_cnt
            .try_into()
            .map_err(|_| std::io::Error::new(std::io::ErrorKind::InvalidData, "string too long"))?,
        writer,
        endian,
        (),
    )?;
    for word in val.encode_utf16() {
        writer.write_type::<u16>(&word, endian)?;
    }
    Ok(())
}

#[writer(writer, endian)]
pub fn write_utf16_string(val: &String) -> BinResult<()> {
    write_utf16_str(val, writer, endian)
}
