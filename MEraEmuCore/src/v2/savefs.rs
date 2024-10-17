use binrw::binrw;

use crate::{
    types::*,
    util::io::{read_utf16_string, write_utf16_string},
};

#[binrw]
#[brw(repr = u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
enum EraSaveFileType {
    Normal = 0x00,
    Global = 0x01,
    Var = 0x02,
    CharVar = 0x03,
}

#[binrw]
#[brw(repr = u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
enum EraSaveDataType {
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
enum EraBinaryMark {
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
struct EraSaveFileHeader {
    version: u32,
    #[bw(try_calc = data.len().try_into())]
    data_count: u32,
    #[br(count = data_count)]
    data: Vec<u32>,
    file_type: EraSaveFileType,
    game_code: i64,
    game_version: i64,
    #[br(parse_with = read_utf16_string)]
    #[bw(write_with = write_utf16_string)]
    save_info: String,
}
