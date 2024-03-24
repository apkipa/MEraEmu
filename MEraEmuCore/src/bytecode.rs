use std::{cell::RefCell, collections::BTreeMap, ops::Deref};

use rclite::Rc;

#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
// TODO: Make bytecode with strongly-typed operands to ease JIT compilation
pub enum EraBytecodePrimaryType {
    Invalid = 0,
    InvalidWithMessage, // Introduced to report compilation errors during execution
    DebugBreak,
    Nop,
    Quit,
    Throw,
    ReturnVoid,
    ReturnInteger,
    ReturnString,
    ReturnNaked, // Merges current frame into previous frame, for syscalls only
    Jump,
    JumpW,
    JumpWW,
    JumpCond,
    JumpCondW,
    JumpCondWW,
    // JumpCondKeep,
    // JumpCondKeepW,
    // JumpCondKeepWW,
    //FunJump,
    /*  FunCall:
        Stack should contain <args...> <function>.
        Note that for dynamic calls, arguments must be packed, and interaction
        with return values are impossible.
    */
    FunCall,
    TryFunCall, // Returns whether function exists
    TryFunCallForce,
    FunExists,
    RestartExecAtFun, // Function must not accept arguments
    GetCallerFuncName,
    // LoadString,
    // LoadStringW,
    // LoadStringWW,
    // LoadInteger,
    // LoadIntegerW,
    // LoadIntegerWW,
    LoadConst,
    LoadConstW,
    LoadConstWW,
    LoadIntegerImm8,
    // LoadIntegerImm16,
    // LoadIntegerImm32,
    // LoadArrayString, // Arrays can be looked up through a string or an integer
    // LoadArrayInteger,
    // LoadBuiltinLocalArray, // Empty string refers to LOCAL@<Current function>
    // LoadBuiltinLocalsArray,
    // LoadBuiltinArgArray,
    // LoadBuiltinArgsArray,
    ConvertToString,  // For string values, this is no-op
    ConvertToInteger, // For integer values, this is no-op
    BuildString,      // Requires an imm8 describing count of strings to concatenate
    PadString,        // <string> <width>, imm8: PadStringFlags
    Pop,
    Duplicate, // Push current value to stack without consuming it
    DuplicateN,
    DuplicateOneN,
    DeepClone, // Replaces current value with a new copy for arrays
    GetGlobal,
    // GetGlobalImmWW,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetMDArrayVal, // Requires an imm8 to describe dimensions count; may be slow
    SetMDArrayVal,
    GetArrayVal,
    SetArrayVal,
    SetArrayValNoRet,
    // HACK: Special bytecode for handling increments & decrements
    IncMDArrayVal,
    DecMDArrayVal,
    BuildArrayIndexFromMD,
    CopyArrayContent,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Negate,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitShiftL,
    BitShiftR,
    CompareL,
    CompareEq,
    CompareLEq,
    // TODO: Do we really need CompareG[Eq] and CompareNEq?
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    GetRandomRange,
    GetRandomMax,
    SplitString,
    MaximumInt,
    MinimumInt,
    ClampInt,
    InRangeInt,
    GetBit,
    SetBit,
    ClearBit,
    InvertBit,
    TimesFloat,
    ReplaceString,
    RepeatString,
    SubString,
    SubStringU,
    StrFind,
    StrFindU,
    StrLen,
    StrLenU,
    CountSubString,
    StrCharAtU,
    FormatIntToStr,
    StringIsValidInteger,
    StringToUpper,
    StringToLower,
    StringToHalf,
    StringToFull,
    BuildBarString,
    EscapeRegexStr,
    EncodeToUnicode,
    UnicodeToStr,
    IntToStrWithBase,
    HtmlTagSplit,
    HtmlToPlainText,
    PowerInt,
    SqrtInt,
    CbrtInt,
    LogInt,
    Log10Int,
    ExponentInt,
    AbsInt,
    GroupMatch,
    ArrayCountMatches,
    CArrayCountMatches,
    SumArray,
    SumCArray,
    MaxArray,
    MaxCArray,
    MinArray,
    MinCArray,
    InRangeArray,
    InRangeCArray,
    ArrayRemove,
    ArraySortAsc,
    ArraySortDesc,
    ArrayMSort,
    ArrayCopy,
    ArrayShift,
    // -----
    Print,
    PrintLine,
    PrintExtended, // Requires an imm8 PrintExtendedFlags
    HtmlPrint,
    PrintButton,
    Wait,
    TWait,
    PrintImg,
    // PrintImg2,
    // PrintImg3,
    PrintImg4,
    GCreate,
    GCreateFromFile,
    GDispose,
    GCreated,
    GDrawSprite,
    GDrawSpriteWithColorMatrix,
    GClear,
    SpriteCreate,
    SpriteDispose,
    SpriteCreated,
    SpriteAnimeCreate,
    SpriteAnimeAddFrame,
    SpriteWidth,
    SpriteHeight,
    CheckFont,
    SaveText,
    LoadText,
    FindElement,
    FindLastElement,
    FindChara,
    FindLastChara,
    VarSet,
    CVarSet,
    GetVarSize,
    GetCharaNum,
    GetHostTimeRaw,
    GetHostTime,
    GetHostTimeS,
    Input, // Extended: EraInputSubBytecodeType
    ReuseLastLine,
    ClearLine,
    //CsvGetProp,
    CsvGetProp2,
    CsvGetNum,
    CharaCsvExists,
    GetPalamLv,
    GetExpLv,
    AddChara,
    AddVoidChara,
    PickUpChara,
    DeleteChara,
    SwapChara,
    AddCopyChara,
    SaveData,
    GetCharaRegNum,
    LoadGlobal,
    SaveGlobal,
    // LoadGame,
    // SaveGame,
    // BeginSystemProcedure,
    // DoTrain,
    ResetData,
    ResetCharaStain,
    SaveChara,
    LoadChara,
    GetConfig,
    GetConfigS,
    KbGetKeyState, // Returns i64 with b15 = <key down>, b0 = <key triggered>
    FindCharaDataFile,
    // -----
    SystemIntrinsics = 192,
    ExtendedBytecode1, // Values >= ExtendedBytecode should do extended lookup
    InvokeStaticJit = 251,
}
impl EraBytecodePrimaryType {
    pub fn from_i(value: u8) -> Self {
        num_traits::FromPrimitive::from_u8(value).unwrap_or(Self::Invalid)
    }
    pub fn try_from_i(value: u8) -> Option<Self> {
        num_traits::FromPrimitive::from_u8(value)
    }
    pub fn to_i(self) -> u8 {
        num_traits::ToPrimitive::to_u8(&self).unwrap()
    }
}
#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraInputSubBytecodeType {
    pub is_string: bool,
    pub is_one: bool,
    pub is_timed: bool,
    pub has_default_value: bool,
    #[skip]
    __: modular_bitfield::specifiers::B4,
}
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraCharaCsvPropType {
    CsvName,
    CsvCallName,
    CsvNickName,
    CsvMasterName,
    CsvBase,
    CsvCStr,
    CsvAbl,
    CsvTalent,
    CsvMark,
    CsvExp,
    CsvRelation,
    CsvJuel,
    CsvEquip,
    CsvCFlag,
}
impl EraCharaCsvPropType {
    // pub fn from_i(value: u8) -> Self {
    //     num_traits::FromPrimitive::from_u8(value).unwrap_or(Self::Invalid)
    // }
    pub fn try_from_i(value: u8) -> Option<Self> {
        num_traits::FromPrimitive::from_u8(value)
    }
    pub fn to_i(self) -> u8 {
        num_traits::ToPrimitive::to_u8(&self).unwrap()
    }
    pub fn try_from_var(var: &str) -> Option<Self> {
        use EraCharaCsvPropType::*;
        let is = |x| var.eq_ignore_ascii_case(x);
        Some(if is("BASE") || is("MAXBASE") {
            CsvBase
        } else if is("CSTR") {
            CsvCStr
        } else if is("ABL") {
            CsvAbl
        } else if is("TALENT") {
            CsvTalent
        } else if is("MARK") {
            CsvMark
        } else if is("EXP") {
            CsvExp
        } else if is("RELATION") {
            CsvRelation
        } else if is("JUEL") {
            CsvJuel
        } else if is("EQUIP") {
            CsvEquip
        } else if is("CFLAG") {
            CsvCFlag
        } else {
            return None;
        })
    }
}
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum EraCsvVarKind {
    CsvAbl,
    CsvExp,
    CsvTalent,
    CsvPalam,
    CsvTrain,
    CsvMark,
    CsvItem,
    CsvBase,
    CsvSource,
    CsvEx,
    CsvStr,
    CsvEquip,
    CsvTEquip,
    CsvFlag,
    CsvTFlag,
    CsvCFlag,
    CsvTCVar,
    CsvCStr,
    CsvStain,
    //CsvStrName,
    CsvTStr,
    CsvSaveStr,
    CsvGlobal,
    CsvGlobals,
}
impl EraCsvVarKind {
    // pub fn from_i(value: u8) -> Self {
    //     num_traits::FromPrimitive::from_u8(value).unwrap_or(Self::Invalid)
    // }
    pub fn try_from_i(value: u8) -> Option<Self> {
        num_traits::FromPrimitive::from_u8(value)
    }
    pub fn to_i(self) -> u8 {
        num_traits::ToPrimitive::to_u8(&self).unwrap()
    }
    pub fn try_from_var(var: &str) -> Option<Self> {
        use EraCsvVarKind::*;
        let is = |x| var.eq_ignore_ascii_case(x);
        Some(if is("ABL") {
            CsvAbl
        } else if is("EXP") {
            CsvExp
        } else if is("TALENT") {
            CsvTalent
        } else if is("PALAM")
            || is("JUEL")
            || is("GOTJUEL")
            || is("UP")
            || is("DOWN")
            || is("CUP")
            || is("CDOWN")
        {
            CsvPalam
        } else if is("TRAIN") {
            CsvTrain
        } else if is("MARK") {
            CsvMark
        } else if is("ITEM") || is("ITEMPRICE") || is("ITEMSALES") {
            CsvItem
        } else if is("BASE") || is("MAXBASE") || is("DOWNBASE") || is("LOSEBASE") {
            CsvBase
        } else if is("SOURCE") {
            CsvSource
        } else if is("EX") || is("NOWEX") {
            CsvEx
        } else if is("STR") {
            CsvStr
        } else if is("EQUIP") {
            CsvEquip
        } else if is("TEQUIP") {
            CsvTEquip
        } else if is("FLAG") {
            CsvFlag
        } else if is("TFLAG") {
            CsvTFlag
        } else if is("CFLAG") {
            CsvCFlag
        } else if is("TCVAR") {
            CsvTCVar
        } else if is("CSTR") {
            CsvCStr
        } else if is("STAIN") {
            CsvStain
        } else if is("TSTR") {
            CsvTStr
        } else if is("SAVESTR") {
            CsvSaveStr
        } else if is("GLOBAL") {
            CsvGlobal
        } else if is("GLOBALS") {
            CsvGlobals
        } else {
            return None;
        })
    }
}

#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone)]
pub enum EraBeginSystemProcedureKind {
    First,
    Title,
    Train,
    AfterTrain,
    AblUp,
    TurnEnd,
    Shop,
}
#[repr(u8)]
#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Debug, Clone, Copy)]
pub enum SystemIntrinsicsKind {
    LoadGamePrintText,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct PrintExtendedFlags {
    pub is_single: bool,
    pub use_kana: bool,
    pub ignore_color: bool,
    pub is_line: bool,
    pub is_wait: bool,
    pub force_plain: bool,
    pub left_pad: bool,
    pub right_pad: bool,
}

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct PadStringFlags {
    pub left_pad: bool,
    pub right_pad: bool,
    __: modular_bitfield::specifiers::B6,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntValue {
    pub val: i64,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrValue {
    pub val: arcstr::ArcStr,
}

// NOTE: Arr*Value are used as variable references
#[derive(Debug, Clone)]
pub struct ArrIntValue {
    pub vals: Vec<IntValue>,
    pub dims: smallvec::SmallVec<[u32; 3]>,
    pub flags: EraValueFlags,
}
#[derive(Debug, Clone)]
pub struct ArrStrValue {
    pub vals: Vec<StrValue>,
    pub dims: smallvec::SmallVec<[u32; 3]>,
    pub flags: EraValueFlags,
}
// pub struct ArrValue {
//     depth: u16,
//     slot: u16,
// }

#[modular_bitfield::bitfield]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub struct EraValueFlags {
    pub is_trap: bool,
    pub is_const: bool,
    __: modular_bitfield::specifiers::B6,
}

// TODO: Compressed array values (extend on demand)?
impl ArrIntValue {
    #[inline(always)]
    pub fn flat_get(&self, idx: usize) -> Option<&IntValue> {
        self.vals.get(idx)
    }
    #[inline(always)]
    pub fn flat_get_mut(&mut self, idx: usize) -> Option<&mut IntValue> {
        self.vals.get_mut(idx)
    }
    #[inline(always)]
    pub fn get(&self, idxs: &[u32]) -> Option<&IntValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get(index)
    }
    #[inline(always)]
    pub fn get_mut(&mut self, idxs: &[u32]) -> Option<&mut IntValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get_mut(index)
    }
    #[inline(always)]
    pub fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        if idxs.len() > self.dims.len() {
            return None;
        }
        if idxs.iter().zip(self.dims.iter()).any(|(&a, &b)| a >= b) {
            return None;
        }
        let (_, idx) =
            self.dims
                .iter()
                .enumerate()
                .rev()
                .fold((1, 0), |(stride, idx), (i, &dim)| {
                    (
                        stride * (dim as usize),
                        idx + idxs.get(i).map(|&x| x as usize).unwrap_or(0) * stride,
                    )
                });
        Some(idx)
    }
    #[inline(always)]
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.resize(size, Default::default());
        }
    }
}
impl ArrStrValue {
    #[must_use]
    #[inline(always)]
    pub fn flat_get(&self, idx: usize) -> Option<&StrValue> {
        self.vals.get(idx)
    }
    #[must_use]
    #[inline(always)]
    pub fn flat_get_mut(&mut self, idx: usize) -> Option<&mut StrValue> {
        self.vals.get_mut(idx)
    }
    #[must_use]
    #[inline(always)]
    pub fn get(&self, idxs: &[u32]) -> Option<&StrValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get(index)
    }
    #[must_use]
    #[inline(always)]
    pub fn get_mut(&mut self, idxs: &[u32]) -> Option<&mut StrValue> {
        let index = self.calc_idx(idxs)?;
        self.vals.get_mut(index)
    }
    #[must_use]
    #[inline(always)]
    pub fn calc_idx(&self, idxs: &[u32]) -> Option<usize> {
        if idxs.len() > self.dims.len() {
            return None;
        }
        if idxs.iter().zip(self.dims.iter()).any(|(&a, &b)| a >= b) {
            return None;
        }
        let (_, idx) =
            self.dims
                .iter()
                .enumerate()
                .rev()
                .fold((1, 0), |(stride, idx), (i, &dim)| {
                    (
                        stride * (dim as usize),
                        idx + idxs.get(i).map(|&x| x as usize).unwrap_or(0) * stride,
                    )
                });
        Some(idx)
    }
    #[inline(always)]
    pub fn ensure_alloc(&mut self) {
        if self.vals.is_empty() {
            let size = self.dims.iter().fold(1, |acc, x| acc * (*x as usize));
            self.vals.resize(size, Default::default());
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueKind {
    Int,
    Str,
    ArrInt,
    ArrStr,
}
impl ValueKind {
    pub fn without_arr(&self) -> Self {
        use ValueKind::*;
        match self {
            Int | ArrInt => Int,
            Str | ArrStr => Str,
        }
    }
    pub fn with_arr(&self) -> Self {
        use ValueKind::*;
        match self {
            Int | ArrInt => ArrInt,
            Str | ArrStr => ArrStr,
        }
    }
    pub fn is_arr(&self) -> bool {
        use ValueKind::*;
        match self {
            ArrInt | ArrStr => true,
            Int | Str => false,
        }
    }
}

//pub type Value = ptr_union::Union4<Box<IntValue>, Box<StrValue>, Box<ArrValue>, Box<()>>;
// pub type ValueInner = ptr_union::Union4<
//     Rc<IntValue>,
//     Rc<StrValue>,
//     Rc<RefCell<ArrIntValue>>,
//     Rc<RefCell<ArrStrValue>>,
// >;
pub type ValueInner = FlatValue;

#[derive(Debug, Clone)]
pub struct Value(ValueInner);
#[derive(Debug, Clone)]
pub enum FlatValue {
    Int(IntValue),
    Str(StrValue),
    ArrInt(Rc<RefCell<ArrIntValue>>),
    ArrStr(Rc<RefCell<ArrStrValue>>),
}
#[derive(Debug, Clone)]
pub enum RefFlatValue<'a> {
    Int(&'a IntValue),
    Str(&'a StrValue),
    ArrInt(&'a Rc<RefCell<ArrIntValue>>),
    ArrStr(&'a Rc<RefCell<ArrStrValue>>),
}
impl RefFlatValue<'_> {
    pub fn kind(&self) -> ValueKind {
        use RefFlatValue::*;
        match self {
            Int(_) => ValueKind::Int,
            Str(_) => ValueKind::Str,
            ArrInt(_) => ValueKind::ArrInt,
            ArrStr(_) => ValueKind::ArrStr,
        }
    }
}

// impl Value {
//     pub fn new_int(val: i64) -> Self {
//         Value(ValueInner::new_a(Rc::new(IntValue { val })).unwrap())
//     }
//     pub fn new_str(val: String) -> Self {
//         Value(ValueInner::new_b(Rc::new(StrValue { val })).unwrap())
//     }
//     pub fn new_int_rc(val: Rc<IntValue>) -> Self {
//         Value(ValueInner::new_a(val).unwrap())
//     }
//     pub fn new_str_rc(val: Rc<StrValue>) -> Self {
//         Value(ValueInner::new_b(val).unwrap())
//     }
//     pub fn new_int_arr(mut dims: Vec<u32>, mut vals: Vec<Rc<IntValue>>) -> Self {
//         if dims.is_empty() {
//             dims.push(1);
//         }
//         let size = dims.iter().fold(1, |acc, x| acc * (*x as usize));
//         //assert_ne!(size, 0, "dimension must not contain zero");
//         vals.resize(size, Rc::new(IntValue { val: 0 }));
//         Value(ValueInner::new_c(Rc::new(RefCell::new(ArrIntValue { vals, dims }))).unwrap())
//     }
//     pub fn new_str_arr(mut dims: Vec<u32>, mut vals: Vec<Rc<StrValue>>) -> Self {
//         if dims.is_empty() {
//             dims.push(1);
//         }
//         let size = dims.iter().fold(1, |acc, x| acc * (*x as usize));
//         //assert_ne!(size, 0, "dimension must not contain zero");
//         vals.resize(size, Rc::new(StrValue { val: String::new() }));
//         Value(ValueInner::new_d(Rc::new(RefCell::new(ArrStrValue { vals, dims }))).unwrap())
//     }
//     pub fn into_unpacked(self) -> FlatValue {
//         use ptr_union::Enum4::*;
//         use FlatValue::*;
//         match self.0.unpack() {
//             A(x) => Int(x),
//             B(x) => Str(x),
//             C(x) => ArrInt(x),
//             D(x) => ArrStr(x),
//         }
//     }
//     pub fn deep_clone(&self) -> Self {
//         // use ptr_union::Enum4::*;
//         // match self.0.clone().unpack() {
//         //     A(x) => Value(ValueInner::new_a(x.deref().clone().into()).unwrap()),
//         //     B(x) => Value(ValueInner::new_b(x.deref().clone().into()).unwrap()),
//         //     C(x) => Value(ValueInner::new_c(x.deref().clone().into()).unwrap()),
//         //     D(x) => Value(ValueInner::new_d(x.deref().clone().into()).unwrap()),
//         // }
//         self.clone().into_unpacked().deep_clone().into_packed()
//     }
//     pub fn kind(&self) -> ValueKind {
//         // TODO: Optimize performance
//         self.clone().into_unpacked().kind()
//     }
// }
impl Value {
    #[inline(always)]
    pub fn new_int(val: i64) -> Self {
        Value(ValueInner::Int(IntValue { val }))
    }
    #[inline(always)]
    pub fn new_str(val: arcstr::ArcStr) -> Self {
        Value(ValueInner::Str(StrValue { val }))
    }
    // pub fn new_int_rc(val: Rc<IntValue>) -> Self {
    //     Value(ValueInner::Int(val.deref().clone()))
    // }
    #[inline(always)]
    pub fn new_int_obj(val: IntValue) -> Self {
        Value(ValueInner::Int(val))
    }
    // pub fn new_str_rc(val: Rc<StrValue>) -> Self {
    //     Value(ValueInner::Str(val.deref().clone()))
    // }
    #[inline(always)]
    pub fn new_str_obj(val: StrValue) -> Self {
        Value(ValueInner::Str(val))
    }
    pub fn new_int_arr(mut dims: smallvec::SmallVec<[u32; 3]>, mut vals: Vec<IntValue>) -> Self {
        if dims.is_empty() {
            dims.push(1);
        }
        let size = dims.iter().fold(1, |acc, x| acc * (*x as usize));
        //assert_ne!(size, 0, "dimension must not contain zero");
        if !vals.is_empty() {
            vals.resize(size, Default::default());
        }
        Value(ValueInner::ArrInt(Rc::new(RefCell::new(ArrIntValue {
            vals,
            dims,
            flags: EraValueFlags::new(),
        }))))
    }
    pub fn new_str_arr(mut dims: smallvec::SmallVec<[u32; 3]>, mut vals: Vec<StrValue>) -> Self {
        if dims.is_empty() {
            dims.push(1);
        }
        let size = dims.iter().fold(1, |acc, x| acc * (*x as usize));
        //assert_ne!(size, 0, "dimension must not contain zero");
        if !vals.is_empty() {
            vals.resize(size, Default::default());
        }
        Value(ValueInner::ArrStr(Rc::new(RefCell::new(ArrStrValue {
            vals,
            dims,
            flags: EraValueFlags::new(),
        }))))
    }
    pub fn new_int_0darr(val: i64) -> Self {
        Self::new_int_arr(smallvec::smallvec![1], vec![IntValue { val }])
    }
    pub fn new_str_0darr(val: arcstr::ArcStr) -> Self {
        Self::new_str_arr(smallvec::smallvec![1], vec![StrValue { val }])
    }
    pub fn into_unpacked(self) -> FlatValue {
        use ptr_union::Enum4::*;
        use FlatValue::*;
        match self.0 {
            Int(x) => Int(x),
            Str(x) => Str(x),
            ArrInt(x) => ArrInt(x),
            ArrStr(x) => ArrStr(x),
        }
    }
    pub fn as_unpacked(&self) -> RefFlatValue {
        use FlatValue::*;
        match &self.0 {
            Int(x) => RefFlatValue::Int(x),
            Str(x) => RefFlatValue::Str(x),
            ArrInt(x) => RefFlatValue::ArrInt(x),
            ArrStr(x) => RefFlatValue::ArrStr(x),
        }
    }
    pub fn deep_clone(&self) -> Self {
        // use ptr_union::Enum4::*;
        // match self.0.clone().unpack() {
        //     A(x) => Value(ValueInner::new_a(x.deref().clone().into()).unwrap()),
        //     B(x) => Value(ValueInner::new_b(x.deref().clone().into()).unwrap()),
        //     C(x) => Value(ValueInner::new_c(x.deref().clone().into()).unwrap()),
        //     D(x) => Value(ValueInner::new_d(x.deref().clone().into()).unwrap()),
        // }
        self.clone().into_unpacked().deep_clone().into_packed()
    }
    pub fn kind(&self) -> ValueKind {
        // TODO: Optimize performance
        //self.clone().into_unpacked().kind()
        self.as_unpacked().kind()
    }
}
impl FlatValue {
    pub fn into_packed(self) -> Value {
        use FlatValue::*;
        Value(match self {
            Int(x) => ValueInner::Int(x),
            Str(x) => ValueInner::Str(x),
            ArrInt(x) => ValueInner::ArrInt(x),
            ArrStr(x) => ValueInner::ArrStr(x),
        })
    }
    pub fn deep_clone(&self) -> Self {
        use FlatValue::*;
        match self {
            // NOTE: We cannot mutate IntValue and StrValue, so it is safe to perform
            //       shallow copies on them
            // Int(x) => Int(x.deref().clone().into()),
            // Str(x) => Str(x.deref().clone().into()),
            Int(x) => Int(x.clone()),
            Str(x) => Str(x.clone()),
            ArrInt(x) => ArrInt(x.deref().clone().into()),
            ArrStr(x) => ArrStr(x.deref().clone().into()),
        }
    }
    pub fn kind(&self) -> ValueKind {
        match self {
            FlatValue::Int(_) => ValueKind::Int,
            FlatValue::Str(_) => ValueKind::Str,
            FlatValue::ArrInt(_) => ValueKind::ArrInt,
            FlatValue::ArrStr(_) => ValueKind::ArrStr,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SourcePosInfo {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct SourcePosLenInfo {
    pub line: u32,
    pub column: u32,
    pub len: u32,
}

impl SourcePosInfo {
    pub fn new(line: u32, column: u32) -> Self {
        SourcePosInfo { line, column }
    }
}

// TODO: Use SourcePosLenInfo instead of SourcePosInfo?
impl SourcePosLenInfo {
    pub fn new(line: u32, column: u32, len: u32) -> Self {
        SourcePosLenInfo { line, column, len }
    }
}

#[derive(Debug, Default)]
pub struct EraCharaInitTemplate {
    pub no: u32,
    pub csv_no: u32,
    pub name: arcstr::ArcStr,
    pub callname: arcstr::ArcStr,
    pub nickname: arcstr::ArcStr,
    pub mastername: arcstr::ArcStr,
    pub maxbase: BTreeMap<u32, i64>,
    pub mark: BTreeMap<u32, i64>,
    pub exp: BTreeMap<u32, i64>,
    pub abl: BTreeMap<u32, i64>,
    pub talent: BTreeMap<u32, i64>,
    pub relation: BTreeMap<u32, i64>,
    pub cflag: BTreeMap<u32, i64>,
    pub equip: BTreeMap<u32, i64>,
    pub juel: BTreeMap<u32, i64>,
    pub cstr: BTreeMap<u32, arcstr::ArcStr>,
}
