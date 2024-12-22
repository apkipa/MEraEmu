use anyhow::Context;
use itertools::{Either, Itertools};
use lending_iterator::higher_kinded_types::*;
use rclite::Rc;
use regex::Regex;
use std::cell::RefCell;

use crate::{
    types::*,
    util::{
        rcstr::{self, ArcStr},
        swap_slice_with_stride,
    },
};

use super::vm::TailVecRef;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum IntrinsicKind {
    HtmlTagSplit,
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
    // ArrayMSort { subs_cnt: u8 },
    ArrayCopy,
    ArrayShift,
    RowAssign,
    ForLoopStep,
    ExtendStrToWidth,
    SplitString,
    FindElement,
    FindLastElement,
    FindChara,
    FindLastChara,
    VarSet,
    CVarSet,
}

#[derive(Debug)]
pub enum EraIntrinsicError {
    IllegalArguments(anyhow::Error),
    GeneralError(anyhow::Error),
}

pub struct EraIntrinsicExecutionData {
    pub consumed_args: usize,
    pub result: Option<Value>,
}

pub type EraIntrinsicResult = Result<EraIntrinsicExecutionData, EraIntrinsicError>;

// #[derive(Debug)]
// pub enum EraIntrinsicResult {
//     IllegalArguments,
//     Success(Value),
//     Failure(anyhow::Error),
// }

impl From<anyhow::Error> for EraIntrinsicError {
    fn from(e: anyhow::Error) -> Self {
        Self::GeneralError(e)
    }
}

pub trait EraIntrinsic {
    // `args` is a list of arguments passed to the intrinsic. Is also
    // a view into the current stack frame.
    fn invoke(&self, args: &[Value]) -> EraIntrinsicResult;
}

pub trait EraIntrinsicExt {
    fn invoke_and_consume(&self, args: &mut TailVecRef<Value>) -> Result<(), EraIntrinsicError>;
}

impl<T: EraIntrinsic + ?Sized> EraIntrinsicExt for T {
    fn invoke_and_consume(&self, args: &mut TailVecRef<Value>) -> Result<(), EraIntrinsicError> {
        let result = self.invoke(args)?;
        args.get_view_mut(args.len() - result.consumed_args..)
            .replace(result.result);
        Ok(())
    }
}

pub const fn get_global_intrinsic(kind: IntrinsicKind) -> Option<&'static dyn EraIntrinsic> {
    macro_rules! reg_intrinsic {
        ($($kind:pat => $method:expr),* $(,)?) => {
            #[allow(unreachable_patterns)]
            match kind {
                $($kind => {
                    const I: &'static dyn EraIntrinsic = &to_intrinsic::<_, (), _>($method);
                    I
                })*
                _ => return None,
            }
        };
    }

    use IntrinsicKind::*;

    // NOTE: -1 means the last dimension (the least significant one), and
    //       0 means the first dimension (usually used for CHARA variables).
    Some(reg_intrinsic! {
        HtmlTagSplit => html_tag_split,
        ArrayCountMatches => |arr, arr_idx, value, start_idx, end_idx| array_count_matches(-1, arr, arr_idx, value, start_idx, end_idx),
        CArrayCountMatches => |arr, arr_idx, value, start_idx, end_idx| array_count_matches(0, arr, arr_idx, value, start_idx, end_idx),
        SumArray => |arr, arr_idx, start_idx, end_idx| array_aggregate::<SumAggregator>(-1, arr, arr_idx, start_idx, end_idx),
        SumCArray => |arr, arr_idx, start_idx, end_idx| array_aggregate::<SumAggregator>(0, arr, arr_idx, start_idx, end_idx),
        MaxArray => |arr, arr_idx, start_idx, end_idx| array_aggregate::<MaxAggregator>(-1, arr, arr_idx, start_idx, end_idx),
        MaxCArray => |arr, arr_idx, start_idx, end_idx| array_aggregate::<MaxAggregator>(0, arr, arr_idx, start_idx, end_idx),
        MinArray => |arr, arr_idx, start_idx, end_idx| array_aggregate::<MinAggregator>(-1, arr, arr_idx, start_idx, end_idx),
        MinCArray => |arr, arr_idx, start_idx, end_idx| array_aggregate::<MinAggregator>(0, arr, arr_idx, start_idx, end_idx),
        InRangeArray => |arr, arr_idx, lower_bound, upper_bound, start_idx, end_idx| array_in_range(-1, arr, arr_idx, lower_bound, upper_bound, start_idx, end_idx),
        InRangeCArray => |arr, arr_idx, lower_bound, upper_bound, start_idx, end_idx| array_in_range(0, arr, arr_idx, lower_bound, upper_bound, start_idx, end_idx),
        ArrayRemove =>  array_remove,
        ArraySortAsc => |arr, arr_idx, start_idx, count| array_sort(true, arr, arr_idx, start_idx, count),
        ArraySortDesc => |arr, arr_idx, start_idx, count| array_sort(false, arr, arr_idx, start_idx, count),
        // ArrayMSort { subs_cnt: u8 },
        ArrayCopy => array_copy,
        ArrayShift => array_shift,
        ForLoopStep => for_loop_step,
        ExtendStrToWidth => extend_str_to_width,
        SplitString => split_string,
        FindElement => |arr, arr_idx, value, start_idx, count| find_element(true, -1, arr, arr_idx, value, start_idx, count),
        FindLastElement => |arr, arr_idx, value, start_idx, count| find_element(false, -1, arr, arr_idx, value, start_idx, count),
        FindChara => |arr, arr_idx, value, start_idx, count| find_element(true, 0, arr, arr_idx, value, start_idx, count),
        FindLastChara => |arr, arr_idx, value, start_idx, count| find_element(false, 0, arr, arr_idx, value, start_idx, count),
        VarSet => |arr, arr_idx, value, start_idx, end_idx| var_set(-1, arr, arr_idx, value, start_idx, end_idx),
        CVarSet => |arr, arr_idx, value, start_idx, end_idx| var_set(0, arr, arr_idx, value, start_idx, end_idx),
    })

    // Some(match kind {
    //     IntrinsicKind::HtmlTagSplit => {
    //         const I: &'static dyn EraIntrinsic = &to_intrinsic::<_, (), _>(html_tag_split);
    //         I
    //     }
    // })
}

pub trait FromValue<'a>: Sized {
    fn from_value(value: &'a Value) -> Option<Self>;
    fn type_name() -> &'static str;
    fn is_no_comsume_args() -> bool {
        false
    }
}

pub trait ValueInto<'a, T>: Sized {
    fn value_into(&'a self) -> Option<T>;
}

impl<'a, T> ValueInto<'a, T> for Value
where
    T: FromValue<'a>,
{
    fn value_into(&'a self) -> Option<T> {
        T::from_value(self)
    }
}

impl<'a> FromValue<'a> for i64 {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_int().map(|x| x.val)
    }

    fn type_name() -> &'static str {
        "Int"
    }
}

impl<'a> FromValue<'a> for &'a str {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_str().map(|x| x.val.as_str())
    }

    fn type_name() -> &'static str {
        "Str"
    }
}

impl<'a> FromValue<'a> for &'a Rc<RefCell<ArrIntValue>> {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_arrint()
    }

    fn type_name() -> &'static str {
        "ArrInt"
    }
}

impl<'a> FromValue<'a> for &'a Rc<RefCell<ArrStrValue>> {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_arrstr()
    }

    fn type_name() -> &'static str {
        "ArrStr"
    }
}

impl<'a> FromValue<'a> for ArcStr {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_str().map(|x| x.val.clone())
    }

    fn type_name() -> &'static str {
        "Str"
    }
}

impl<'a> FromValue<'a> for Rc<RefCell<ArrIntValue>> {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_arrint().cloned()
    }

    fn type_name() -> &'static str {
        "ArrInt"
    }
}

impl<'a> FromValue<'a> for Rc<RefCell<ArrStrValue>> {
    fn from_value(value: &'a Value) -> Option<Self> {
        value.as_arrstr().cloned()
    }

    fn type_name() -> &'static str {
        "ArrStr"
    }
}

impl<'a> FromValue<'a> for usize {
    fn from_value(value: &'a Value) -> Option<Self> {
        let value = i64::from_value(value)?;
        value.try_into().ok()
    }

    fn type_name() -> &'static str {
        "USize"
    }
}

impl<'a, T, U> FromValue<'a> for Either<T, U>
where
    T: FromValue<'a>,
    U: FromValue<'a>,
{
    fn from_value(value: &'a Value) -> Option<Self> {
        T::from_value(value)
            .map(Either::Left)
            .or_else(|| U::from_value(value).map(Either::Right))
    }

    fn type_name() -> &'static str {
        "Either<T, U>"
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct NoConsumeArgs<T>(T);

impl<'a, T: FromValue<'a>> FromValue<'a> for NoConsumeArgs<T> {
    fn from_value(value: &'a Value) -> Option<Self> {
        T::from_value(value).map(NoConsumeArgs)
    }

    fn type_name() -> &'static str {
        T::type_name()
    }

    fn is_no_comsume_args() -> bool {
        true
    }
}

pub trait IntoIntrinsicResult: Sized {
    fn into_intrinsic_result(self) -> Option<Value>;
}

impl IntoIntrinsicResult for Value {
    fn into_intrinsic_result(self) -> Option<Value> {
        Some(self)
    }
}

impl IntoIntrinsicResult for () {
    fn into_intrinsic_result(self) -> Option<Value> {
        None
    }
}

impl IntoIntrinsicResult for i64 {
    fn into_intrinsic_result(self) -> Option<Value> {
        Some(Value::new_int(self))
    }
}

impl IntoIntrinsicResult for ArcStr {
    fn into_intrinsic_result(self) -> Option<Value> {
        Some(Value::new_str(self))
    }
}

impl IntoIntrinsicResult for String {
    fn into_intrinsic_result(self) -> Option<Value> {
        Some(Value::new_str(ArcStr::from(self)))
    }
}

impl IntoIntrinsicResult for bool {
    fn into_intrinsic_result(self) -> Option<Value> {
        Some(Value::new_int(self as _))
    }
}

pub const fn to_intrinsic<T, S, H>(handler: H) -> impl EraIntrinsic
where
    H: EraIntrinsicHandler<T, S> + Copy,
{
    struct Intrinsic<T, S, H> {
        handler: H,
        _phantom: std::marker::PhantomData<(T, S)>,
    }

    impl<T, S, H> EraIntrinsic for Intrinsic<T, S, H>
    where
        H: EraIntrinsicHandler<T, S> + Copy,
    {
        fn invoke(&self, args: &[Value]) -> EraIntrinsicResult {
            self.handler.invoke(args)
        }
    }

    Intrinsic {
        handler,
        _phantom: std::marker::PhantomData,
    }
}

pub trait EraIntrinsicHandler<T, S>: Sized {
    fn invoke(self, args: &[Value]) -> EraIntrinsicResult;
}

macro_rules! impl_handler {
    ($($t:ident $id:expr),*) => {
        impl<R, $($t),*, F, S> EraIntrinsicHandler<(R, $($t),*), S> for F
        where
            F: FnOnce($($t),*) -> anyhow::Result<R>,
            $($t: for<'a> FromValue<'a>,)*
            R: IntoIntrinsicResult,
        {
            fn invoke(self, args: &[Value]) -> EraIntrinsicResult {
                let params_count = [$($id),*].len();
                let args_count = args.len();
                if args_count < params_count {
                    return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                        "Expected {params_count} arguments, got {args_count}",
                    )));
                }
                let args = &args[args_count - params_count..];
                let result = self(
                    $($t::from_value(&args[$id - 1]).ok_or_else(|| {
                        EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                            "Failed to convert argument {} from {} to {}",
                            $id,
                            args[$id - 1].kind(),
                            $t::type_name()
                        ))
                    })?,)*
                )?.into_intrinsic_result();
                Ok(EraIntrinsicExecutionData {
                    consumed_args: params_count - [$($t::is_no_comsume_args() as usize),*].iter().sum::<usize>(),
                    result,
                })
            }
        }
    };
}

impl<R, T1, F, S> EraIntrinsicHandler<(R, T1), S> for F
where
    F: FnOnce(T1) -> anyhow::Result<R>,
    T1: for<'a> FromValue<'a>,
    R: IntoIntrinsicResult,
{
    fn invoke(self, args: &[Value]) -> EraIntrinsicResult {
        let params_count = 1;
        let args_count = args.len();
        if args_count < params_count {
            return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                "Expected {params_count} arguments, got {args_count}",
            )));
        }
        let args = &args[args_count - params_count..];
        let a1 = T1::from_value(&args[0]).ok_or_else(|| {
            EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                "Failed to convert argument 1 from {} to {}",
                args[0].kind(),
                T1::type_name()
            ))
        })?;
        let result = self(a1)?.into_intrinsic_result();
        Ok(EraIntrinsicExecutionData {
            consumed_args: 1,
            result,
        })
    }
}

impl_handler!(T1 1, T2 2);
impl_handler!(T1 1, T2 2, T3 3);
impl_handler!(T1 1, T2 2, T3 3, T4 4);
impl_handler!(T1 1, T2 2, T3 3, T4 4, T5 5);
impl_handler!(T1 1, T2 2, T3 3, T4 4, T5 5, T6 6);
impl_handler!(T1 1, T2 2, T3 3, T4 4, T5 5, T6 6, T7 7);
impl_handler!(T1 1, T2 2, T3 3, T4 4, T5 5, T6 6, T7 7, T8 8);
impl_handler!(T1 1, T2 2, T3 3, T4 4, T5 5, T6 6, T7 7, T8 8, T9 9);
impl_handler!(T1 1, T2 2, T3 3, T4 4, T5 5, T6 6, T7 7, T8 8, T9 9, T10 10);

// fn test_hkt_fn<F, T1: HKT>(f: F)
// where
//     F: FnOnce(Apply!(T1<'_>), i64, i64) -> anyhow::Result<()>,
//     for<'any> Apply!(T1<'any>): FromValue<'any>,
// {
//     let v = Value::new_int(42);
//     f(<Apply!(T1<'_>)>::from_value(&v).unwrap(), 0, 0).unwrap();
// }

// fn test_hkt() {
//     test_hkt_fn::<_, HKT!(&str)>(html_tag_split);
// }

type EitherIntStrArr = Either<Rc<RefCell<ArrIntValue>>, Rc<RefCell<ArrStrValue>>>;
type EitherIntStr = Either<i64, ArcStr>;

pub fn html_tag_split(
    html: ArcStr,
    tags: Rc<RefCell<ArrStrValue>>,
    tags_idx: i64,
    count: Rc<RefCell<ArrIntValue>>,
    count_idx: i64,
) -> anyhow::Result<()> {
    let mut tags = MaskedArr::try_new(tags.borrow_mut(), tags_idx as _, -1)
        .context("invalid indices into array")?;
    let mut count = count.borrow_mut();
    let count_idx = count_idx as usize;
    let mut parts_count: usize = 0;
    for part in crate::util::html::split_html_tags(&html) {
        let part = part.context("found invalid html tag while parsing")?;
        let tags = tags
            .get_mut(parts_count)
            .context("invalid indices into array")?;
        tags.val = part.into();
        parts_count += 1;
    }
    let count = count
        .flat_get_mut(count_idx)
        .context("invalid indices into array")?;
    count.val = parts_count as _;
    Ok(())
}

fn array_count_matches(
    dim_pos: i64,
    arr: EitherIntStrArr,
    arr_idx: i64,
    value: EitherIntStr,
    start_idx: i64,
    end_idx: i64,
) -> anyhow::Result<i64> {
    let start_idx = start_idx.max(0) as usize;
    let end_idx = if end_idx < 0 {
        usize::MAX
    } else {
        end_idx as usize
    };

    match arr {
        Either::Left(arr) => {
            let Either::Left(value) = value else {
                // Treat type mismatch as no matches for counting
                return Ok(0);
            };

            let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(0);
            }
            let mut count = 0;
            for i in start_idx..end_idx {
                let arr = arr.get(i).context("invalid indices into array")?;
                if arr.val == value {
                    count += 1;
                }
            }
            Ok(count)
        }
        Either::Right(arr) => {
            let Either::Right(value) = value else {
                // Treat type mismatch as no matches for counting
                return Ok(0);
            };

            let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(0);
            }
            let mut count = 0;
            for i in start_idx..end_idx {
                let arr = arr.get(i).context("invalid indices into array")?;
                if arr.val == value {
                    count += 1;
                }
            }
            Ok(count)
        }
    }
}

trait Aggregator {
    const INIT: i64;

    fn aggregate(a: i64, b: i64) -> i64;
}

struct SumAggregator;

impl Aggregator for SumAggregator {
    const INIT: i64 = 0;

    fn aggregate(a: i64, b: i64) -> i64 {
        a.wrapping_add(b)
    }
}

struct MaxAggregator;

impl Aggregator for MaxAggregator {
    const INIT: i64 = i64::MIN;

    fn aggregate(a: i64, b: i64) -> i64 {
        a.max(b)
    }
}

struct MinAggregator;

impl Aggregator for MinAggregator {
    const INIT: i64 = i64::MAX;

    fn aggregate(a: i64, b: i64) -> i64 {
        a.min(b)
    }
}

/// Aggregate function for arrays. The function is applied to each element in the array
/// and can be used to query sum, min, max, etc.
fn array_aggregate<AGGREGATOR: Aggregator>(
    dim_pos: i64,
    arr: Rc<RefCell<ArrIntValue>>,
    arr_idx: i64,
    start_idx: i64,
    end_idx: i64,
) -> anyhow::Result<i64> {
    let start_idx = start_idx.max(0) as usize;
    let end_idx = if end_idx < 0 {
        usize::MAX
    } else {
        end_idx as usize
    };

    let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
        .context("invalid indices into array")?;
    let end_idx = end_idx.min(arr.len());
    if start_idx >= end_idx {
        return Ok(AGGREGATOR::INIT);
    }
    let mut count = AGGREGATOR::INIT;
    for i in start_idx..end_idx {
        let arr = arr.get(i).context("invalid indices into array")?;
        count = AGGREGATOR::aggregate(count, arr.val);
    }
    Ok(count)
}

fn array_in_range(
    dim_pos: i64,
    arr: Rc<RefCell<ArrIntValue>>,
    arr_idx: i64,
    lower_bound: i64,
    upper_bound: i64,
    start_idx: i64,
    end_idx: i64,
) -> anyhow::Result<i64> {
    let start_idx = start_idx.max(0) as usize;
    let end_idx = if end_idx < 0 {
        usize::MAX
    } else {
        end_idx as usize
    };

    let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
        .context("invalid indices into array")?;
    let end_idx = end_idx.min(arr.len());
    if start_idx >= end_idx {
        return Ok(0);
    }
    let mut count = 0;
    for i in start_idx..end_idx {
        let arr = arr.get(i).context("invalid indices into array")?;
        if (lower_bound..upper_bound).contains(&arr.val) {
            count += 1;
        }
    }
    Ok(count)
}

fn array_remove(
    arr: EitherIntStrArr,
    arr_idx: i64,
    start_idx: i64,
    count: i64,
) -> anyhow::Result<()> {
    let start_idx = start_idx.max(0) as usize;
    let count = if count < 0 { usize::MAX } else { count as _ };

    match arr {
        Either::Left(arr) => {
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = start_idx.saturating_add(count);
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }
            let arr = arr.as_slice_mut().unwrap();
            arr[start_idx..end_idx].fill(Default::default());
            arr[start_idx..].rotate_left(end_idx - start_idx);
        }
        Either::Right(arr) => {
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = start_idx.saturating_add(count);
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }
            let arr = arr.as_slice_mut().unwrap();
            arr[start_idx..end_idx].fill(Default::default());
            arr[start_idx..].rotate_left(end_idx - start_idx);
        }
    }

    Ok(())
}

fn array_sort(
    is_asc: bool,
    arr: EitherIntStrArr,
    arr_idx: i64,
    start_idx: i64,
    count: i64,
) -> anyhow::Result<()> {
    let start_idx = start_idx.max(0) as usize;
    let count = if count < 0 { usize::MAX } else { count as _ };

    match arr {
        Either::Left(arr) => {
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = start_idx.saturating_add(count);
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }
            let arr = arr.as_slice_mut().unwrap();
            if is_asc {
                arr[start_idx..end_idx].sort_unstable();
            } else {
                arr[start_idx..end_idx].sort_unstable_by(|a, b| b.cmp(a));
            }
        }
        Either::Right(arr) => {
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = start_idx.saturating_add(count);
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }
            let arr = arr.as_slice_mut().unwrap();
            if is_asc {
                arr[start_idx..end_idx].sort_unstable();
            } else {
                arr[start_idx..end_idx].sort_unstable_by(|a, b| b.cmp(a));
            }
        }
    }

    Ok(())
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn array_multi_sort(arrs: &[Value]) -> EraIntrinsicResult {
    if arrs.len() < 1 {
        return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
            "Expected at least 1 array to sort",
        )));
    }

    let indices = match arrs[0].as_unpacked() {
        RefFlatValue::ArrInt(x) => {
            let x = MaskedArr::try_new(x.borrow(), 0, -1).context("invalid indices into array")?;
            let x = x.as_slice().unwrap();
            let mut indices = x
                .iter()
                .take_while(|x| x.val != 0)
                .enumerate()
                .map(|(i, _)| i)
                .collect_vec();
            indices.sort_unstable_by(|&a, &b| x[a].cmp(&x[b]));
            indices
        }
        RefFlatValue::ArrStr(x) => {
            let x = MaskedArr::try_new(x.borrow(), 0, -1).context("invalid indices into array")?;
            let x = x.as_slice().unwrap();
            let mut indices = x
                .iter()
                .take_while(|x| !x.val.is_empty())
                .enumerate()
                .map(|(i, _)| i)
                .collect_vec();
            indices.sort_unstable_by(|&a, &b| x[a].cmp(&x[b]));
            indices
        }
        _ => {
            return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                "Expected array to sort by",
            )));
        }
    };

    // Verify that all arrays have the same length
    for arr in arrs.iter() {
        match arr.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let x = x.borrow().dims[0] as usize;
                if x < indices.len() {
                    return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                        "Array length mismatch",
                    )));
                }
            }
            RefFlatValue::ArrStr(x) => {
                let x = x.borrow().dims[0] as usize;
                if x < indices.len() {
                    return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                        "Array length mismatch",
                    )));
                }
            }
            _ => {
                return Err(EraIntrinsicError::IllegalArguments(anyhow::anyhow!(
                    "Expected array to sort",
                )));
            }
        }
    }

    // Now sort all arrays by the indices
    for arr in arrs.iter() {
        match arr.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let mut x = x.borrow_mut();
                let stride: usize = x.dims[1..].iter().map(|&x| x as usize).product();
                crate::util::apply_permutation_in_place_with_fn(
                    |a, b| {
                        let (a, b) = (a.min(b), a.max(b));
                        let (a, b) = (a * stride, b * stride);
                        let split_pos = a + stride;
                        let (slice1, slice2) = x.get_vals_mut().split_at_mut(split_pos);
                        let b = b - split_pos;
                        slice1[a..a + stride].swap_with_slice(&mut slice2[b..b + stride]);
                    },
                    &indices,
                );
            }
            RefFlatValue::ArrStr(x) => {
                let mut x = x.borrow_mut();
                let stride: usize = x.dims[1..].iter().map(|&x| x as usize).product();
                crate::util::apply_permutation_in_place_with_fn(
                    |a, b| {
                        let (a, b) = (a.min(b), a.max(b));
                        let (a, b) = (a * stride, b * stride);
                        let split_pos = a + stride;
                        let (slice1, slice2) = x.get_vals_mut().split_at_mut(split_pos);
                        let b = b - split_pos;
                        slice1[a..a + stride].swap_with_slice(&mut slice2[b..b + stride]);
                    },
                    &indices,
                );
            }
            _ => unreachable!(),
        }
    }

    Ok(EraIntrinsicExecutionData {
        consumed_args: arrs.len(),
        result: None,
    })
}

fn array_copy(arr_from: EitherIntStrArr, arr_to: EitherIntStrArr) -> anyhow::Result<()> {
    // if arr_from.is_left() != arr_to.is_left() {
    //     return Err(anyhow::anyhow!("Array type mismatch"));
    // }

    match arr_from {
        Either::Left(arr_from) => {
            let arr_from = arr_from.borrow();
            let Either::Left(arr_to) = arr_to else {
                anyhow::bail!("Array type mismatch");
            };
            let mut arr_to = arr_to.borrow_mut();
            let dims_count = arr_from.dims.len().min(arr_to.dims.len());
            let mut dims = EraVarDims::with_capacity(dims_count);
            for i in (0..dims_count).rev() {
                let dim1 = arr_from.dims[arr_from.dims.len() - 1 - i];
                let dim2 = arr_to.dims[arr_to.dims.len() - 1 - i];
                dims.push(dim1.min(dim2));
            }

            let mut idxs = vec![0; dims_count];
            'outer: loop {
                *arr_to.get_mut(&idxs).unwrap() = arr_from.get(&idxs).unwrap().clone();

                // Increment indices
                *idxs.last_mut().unwrap() += 1;
                for i in (0..dims_count).rev() {
                    if idxs[i] == dims[i] {
                        idxs[i] = 0;
                        if i == 0 {
                            break 'outer;
                        }
                        idxs[i - 1] += 1;
                    }
                }
            }
        }
        Either::Right(arr_from) => {
            let arr_from = arr_from.borrow();
            let Either::Right(arr_to) = arr_to else {
                anyhow::bail!("Array type mismatch");
            };
            let mut arr_to = arr_to.borrow_mut();
            let dims_count = arr_from.dims.len().min(arr_to.dims.len());
            let mut dims = EraVarDims::with_capacity(dims_count);
            for i in (0..dims_count).rev() {
                let dim1 = arr_from.dims[arr_from.dims.len() - 1 - i];
                let dim2 = arr_to.dims[arr_to.dims.len() - 1 - i];
                dims.push(dim1.min(dim2));
            }

            let mut idxs = vec![0; dims_count];
            'outer: loop {
                *arr_to.get_mut(&idxs).unwrap() = arr_from.get(&idxs).unwrap().clone();

                // Increment indices
                *idxs.last_mut().unwrap() += 1;
                for i in (0..dims_count).rev() {
                    if idxs[i] == dims[i] {
                        idxs[i] = 0;
                        if i == 0 {
                            break 'outer;
                        }
                        idxs[i - 1] += 1;
                    }
                }
            }
        }
    }

    Ok(())
}

fn array_shift(
    arr: EitherIntStrArr,
    arr_idx: i64,
    shift_count: i64,
    value: EitherIntStr,
    start_idx: i64,
    count: i64,
) -> anyhow::Result<()> {
    let start_idx = start_idx.max(0) as usize;
    let count = if count < 0 { usize::MAX } else { count as _ };
    let abs_shift_count = shift_count.unsigned_abs() as usize;

    match arr {
        Either::Left(arr) => {
            let value = value.left().context("expected integer value")?;
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = start_idx.saturating_add(count);
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }

            let arr = arr.as_slice_mut().unwrap();
            let arr = &mut arr[start_idx..end_idx];
            let value = IntValue { val: value };
            if abs_shift_count >= end_idx - start_idx {
                arr.fill(value);
            } else {
                if shift_count < 0 {
                    arr[..abs_shift_count].fill(value);
                    arr.rotate_left(abs_shift_count);
                } else {
                    arr.rotate_right(abs_shift_count);
                    arr[..abs_shift_count].fill(value);
                }
            }
        }
        Either::Right(arr) => {
            let value = value.right().context("expected string value")?;
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = start_idx.saturating_add(count);
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }

            let arr = arr.as_slice_mut().unwrap();
            let arr = &mut arr[start_idx..end_idx];
            let value = StrValue { val: value };
            if abs_shift_count >= end_idx - start_idx {
                arr.fill(value);
            } else {
                if shift_count < 0 {
                    arr[..abs_shift_count].fill(value);
                    arr.rotate_left(abs_shift_count);
                } else {
                    arr.rotate_right(abs_shift_count);
                    arr[..abs_shift_count].fill(value);
                }
            }
        }
    }

    Ok(())
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn row_assign(arr: EitherIntStrArr, arr_idx: i64, values: &[Value]) -> anyhow::Result<()> {
    match arr {
        Either::Left(arr) => {
            let mut arr = arr.borrow_mut();
            let dim = *arr.dims.last().unwrap() as usize;
            let idx_start = arr_idx as usize % dim;
            let idx_base = arr_idx as usize - idx_start;
            let idx_end = idx_start + values.len();
            if idx_end > dim {
                anyhow::bail!("Index out of bounds");
            }
            for (i, value) in values.iter().enumerate() {
                let idx = idx_base + i;
                let value = match value.as_unpacked() {
                    RefFlatValue::Int(x) => x.val,
                    _ => anyhow::bail!("Expected integer value"),
                };
                arr.vals[idx].val = value;
            }
        }
        Either::Right(arr) => {
            let mut arr = arr.borrow_mut();
            let dim = *arr.dims.last().unwrap() as usize;
            let idx_start = arr_idx as usize % dim;
            let idx_base = arr_idx as usize - idx_start;
            let idx_end = idx_start + values.len();
            if idx_end > dim {
                anyhow::bail!("Index out of bounds");
            }
            for (i, value) in values.iter().enumerate() {
                let idx = idx_base + i;
                let value = match value.as_unpacked() {
                    RefFlatValue::Str(x) => x.val.clone(),
                    _ => anyhow::bail!("Expected string value"),
                };
                arr.vals[idx].val = value;
            }
        }
    }

    Ok(())
}

pub fn for_loop_step(
    NoConsumeArgs(arr): NoConsumeArgs<Rc<RefCell<ArrIntValue>>>,
    NoConsumeArgs(arr_idx): NoConsumeArgs<i64>,
    NoConsumeArgs(end): NoConsumeArgs<i64>,
    NoConsumeArgs(step): NoConsumeArgs<i64>,
) -> anyhow::Result<bool> {
    let mut arr = arr.borrow_mut();
    let var = arr
        .flat_get_mut(arr_idx as _)
        .context("invalid indices into array")?;
    var.val += step;
    Ok(if step < 0 {
        var.val > end
    } else {
        var.val < end
    })
}

pub fn extend_str_to_width(s: ArcStr, width: i64) -> anyhow::Result<ArcStr> {
    use unicode_width::UnicodeWidthChar;
    use unicode_width::UnicodeWidthStr;

    if width < 0 {
        return Ok(s);
    }
    let width = width as usize;
    let orig_width = s.width();
    let repeat_count = orig_width / width;
    // let remainder = orig_width % width;
    let mut buf = String::with_capacity(s.len() + repeat_count * (width + 1));
    for _ in 0..repeat_count {
        buf.push_str(&s);
    }
    let mut residual_chars = s.chars();
    while let Some(c) = residual_chars.next() {
        buf.push(c);
        if buf.width() >= width {
            break;
        }
    }
    if buf.width() > width {
        buf.pop();
    }
    Ok(ArcStr::from(buf))
}

pub fn split_string(
    input: ArcStr,
    separator: ArcStr,
    dest: Rc<RefCell<ArrStrValue>>,
    dest_idx: i64,
    dest_count: Rc<RefCell<ArrIntValue>>,
    dest_count_idx: i64,
) -> anyhow::Result<()> {
    let mut dest = MaskedArr::try_new(dest.borrow_mut(), dest_idx as _, -1)
        .context("invalid indices into array")?;
    let mut dest_count = dest_count.borrow_mut();
    let dest_count_idx = dest_count_idx as usize;
    let mut count = 0;
    for part in input.split(separator.as_str()) {
        let dest = dest.get_mut(count).context("invalid indices into array")?;
        dest.val = part.into();
        count += 1;
    }
    let dest_count = dest_count
        .flat_get_mut(dest_count_idx)
        .context("invalid indices into array")?;
    dest_count.val = count as _;
    Ok(())
}

pub fn find_element(
    is_first: bool,
    dim_pos: i64,
    arr: EitherIntStrArr,
    arr_idx: i64,
    value: EitherIntStr,
    start_idx: i64,
    end_idx: i64,
) -> anyhow::Result<i64> {
    let start_idx = start_idx.max(0) as usize;
    let end_idx = if end_idx < 0 {
        usize::MAX
    } else {
        end_idx as usize
    };

    match arr {
        Either::Left(arr) => {
            let Either::Left(value) = value else {
                // Treat type mismatch as no matches for finding
                return Ok(-1);
            };
            let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(-1);
            }

            let mut iter = arr.iter().skip(start_idx).take(end_idx - start_idx);
            let result = if is_first {
                iter.position(|x| x.val == value)
            } else {
                iter.rposition(|x| x.val == value)
            };
            Ok(result.map(|x| (x + start_idx) as _).unwrap_or(-1))
        }
        Either::Right(arr) => {
            let Either::Right(value) = value else {
                // Treat type mismatch as no matches for finding
                return Ok(-1);
            };
            let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(-1);
            }

            let mut iter = arr.iter().skip(start_idx).take(end_idx - start_idx);
            let result = if is_first {
                iter.position(|x| x.val == value)
            } else {
                iter.rposition(|x| x.val == value)
            };
            Ok(result.map(|x| (x + start_idx) as _).unwrap_or(-1))
        }
    }
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn find_element_with_match(
    is_first: bool,
    arr: EitherIntStrArr,
    arr_idx: i64,
    value: EitherIntStr,
    start_idx: i64,
    end_idx: i64,
    complete_match: bool,
    regex_cache: &mut lru::LruCache<ArcStr, Regex>,
) -> anyhow::Result<i64> {
    let start_idx = start_idx.max(0) as usize;
    let end_idx = if end_idx < 0 {
        usize::MAX
    } else {
        end_idx as usize
    };

    match arr {
        Either::Left(arr) => {
            let Either::Left(value) = value else {
                // Treat type mismatch as no matches for finding
                return Ok(-1);
            };
            let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(-1);
            }

            let mut iter = arr.iter().skip(start_idx).take(end_idx - start_idx);
            let result = if is_first {
                iter.position(|x| x.val == value)
            } else {
                iter.rposition(|x| x.val == value)
            };
            Ok(result.map(|x| (x + start_idx) as _).unwrap_or(-1))
        }
        Either::Right(arr) => {
            let Either::Right(value) = value else {
                // Treat type mismatch as no matches for finding
                return Ok(-1);
            };
            let arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, -1)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(-1);
            }

            // Compile regex
            let re_str = if complete_match {
                &rcstr::format!("^(?:{})$", value)
            } else {
                &value
            };
            let re = regex_cache
                .try_get_or_insert(re_str.clone(), || {
                    // Compile twice to ensure input is safe
                    regex::Regex::new(&value).and_then(|_| regex::Regex::new(&re_str))
                })
                .context("failed to compile regex")?;

            let mut iter = arr.iter().skip(start_idx).take(end_idx - start_idx);
            let result = if is_first {
                iter.position(|x| re.is_match(&x.val))
            } else {
                iter.rposition(|x| re.is_match(&x.val))
            };
            Ok(result.map(|x| (x + start_idx) as _).unwrap_or(-1))
        }
    }
}

pub fn var_set(
    dim_pos: i64,
    arr: EitherIntStrArr,
    arr_idx: i64,
    value: EitherIntStr,
    start_idx: i64,
    end_idx: i64,
) -> anyhow::Result<()> {
    let start_idx = start_idx.max(0) as usize;
    let end_idx = if end_idx < 0 {
        usize::MAX
    } else {
        end_idx as usize
    };

    match arr {
        Either::Left(arr) => {
            let Either::Left(value) = value else {
                // Treat type mismatch as no matches for setting
                return Ok(());
            };
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }

            for i in start_idx..end_idx {
                arr.get_mut(i).context("invalid indices into array")?.val = value.clone();
            }
        }
        Either::Right(arr) => {
            let Either::Right(value) = value else {
                // Treat type mismatch as no matches for setting
                return Ok(());
            };
            let mut arr = MaskedArr::try_new(arr.borrow_mut(), arr_idx as _, dim_pos)
                .context("invalid indices into array")?;
            let end_idx = end_idx.min(arr.len());
            if start_idx >= end_idx {
                return Ok(());
            }

            for i in start_idx..end_idx {
                arr.get_mut(i).context("invalid indices into array")?.val = value.clone();
            }
        }
    }

    Ok(())
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn add_chara<Callback: EraCompilerCallback>(
    ctx: &mut EraCompilerCtx<Callback>,
    chara_reg_slot: u32,
    chara_no: i64,
) -> anyhow::Result<()> {
    let chara_reg_slot = chara_reg_slot as usize;
    let Some(chara_tmpl) = ctx.chara_templates.get(&(chara_no as _)) else {
        anyhow::bail!("no such character template: {}", chara_no);
    };

    for chara_var in ctx.variables.chara_vars_iter() {
        match chara_var.val.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let mut x = x.borrow_mut();
                x.ensure_alloc();
                let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                let start_idx = chara_reg_slot * stride;
                let end_idx = (chara_reg_slot + 1) * stride;
                match chara_var.name.as_ref() {
                    "NO" => {
                        x.vals[start_idx] = IntValue { val: chara_no };
                    }
                    _ => {
                        let src = match chara_var.name.as_ref() {
                            "BASE" => &chara_tmpl.maxbase,
                            "MAXBASE" => &chara_tmpl.maxbase,
                            "MARK" => &chara_tmpl.mark,
                            "EXP" => &chara_tmpl.exp,
                            "ABL" => &chara_tmpl.abl,
                            "TALENT" => &chara_tmpl.talent,
                            "RELATION" => &chara_tmpl.relation,
                            "CFLAG" => &chara_tmpl.cflag,
                            "EQUIP" => &chara_tmpl.equip,
                            "JUEL" => &chara_tmpl.juel,
                            _ => &Default::default(),
                        };
                        x.vals[start_idx..end_idx].fill(Default::default());
                        for (&sk, &sv) in src {
                            x.vals[start_idx + sk as usize] = IntValue { val: sv };
                        }
                    }
                }
            }
            RefFlatValue::ArrStr(x) => {
                let mut x = x.borrow_mut();
                x.ensure_alloc();
                let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                let start_idx = chara_reg_slot * stride;
                let end_idx = (chara_reg_slot + 1) * stride;
                match chara_var.name.as_ref() {
                    "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => {
                        let src = match chara_var.name.as_ref() {
                            "NAME" => &chara_tmpl.name,
                            "CALLNAME" => &chara_tmpl.callname,
                            "NICKNAME" => &chara_tmpl.nickname,
                            "MASTERNAME" => &chara_tmpl.mastername,
                            _ => unreachable!(),
                        };
                        x.vals[start_idx] = StrValue { val: src.clone() };
                    }
                    _ => {
                        let src = match chara_var.name.as_ref() {
                            "CSTR" => &chara_tmpl.cstr,
                            _ => &Default::default(),
                        };
                        x.vals[start_idx..end_idx].fill(Default::default());
                        for (&sk, sv) in src {
                            x.vals[start_idx + sk as usize] = StrValue { val: sv.clone() };
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    Ok(())
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn pick_up_chara<Callback: EraCompilerCallback>(
    chara_nos: &[u32],
    ctx: &mut EraCompilerCtx<Callback>,
) -> anyhow::Result<()> {
    let mut chara_nos = chara_nos.iter().map(|&x| x as usize).collect_vec();

    // FIXME: Adjust TARGET:0 & MASTER:0 accordingly
    for orig_idx in 0..chara_nos.len() {
        let pickup_idx = chara_nos[orig_idx] as usize;
        if orig_idx == pickup_idx {
            continue;
        }
        for chara_var in ctx.variables.chara_vars_iter() {
            match chara_var.val.as_unpacked() {
                RefFlatValue::ArrInt(x) => {
                    let mut x = x.borrow_mut();
                    let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                    // assert!(orig_idx < pickup_idx);
                    // let (left, right) = x.vals.split_at_mut(pickup_idx * stride);
                    // left[(orig_idx * stride)..][..stride].swap_with_slice(&mut right[..stride]);
                    swap_slice_with_stride(&mut x.vals, stride, orig_idx, pickup_idx);
                }
                RefFlatValue::ArrStr(x) => {
                    let mut x = x.borrow_mut();
                    let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                    // assert!(orig_idx < pickup_idx);
                    // let (left, right) = x.vals.split_at_mut(pickup_idx * stride);
                    // left[(orig_idx * stride)..][..stride].swap_with_slice(&mut right[..stride]);
                    swap_slice_with_stride(&mut x.vals, stride, orig_idx, pickup_idx);
                }
                _ => unreachable!(),
            }
        }
        // Update indices as the consequence of swapping
        if let Some(idx) = chara_nos.iter().position(|&x| x == orig_idx) {
            chara_nos[idx] = pickup_idx;
        }
    }

    Ok(())
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn delete_chara<Callback: EraCompilerCallback>(
    chara_nos: &[u32], // NOTE: Sorted
    charas_count: u32,
    ctx: &mut EraCompilerCtx<Callback>,
) -> anyhow::Result<()> {
    let charas_count = charas_count as usize;
    for chara_var in ctx.variables.chara_vars_iter() {
        let mut rd = 0;
        let mut wr = 0;
        let mut chara_nos = chara_nos.iter().map(|&x| x as usize).peekable();

        match chara_var.val.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let mut x = x.borrow_mut();
                let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                while rd < charas_count {
                    if Some(rd) == chara_nos.peek().copied() {
                        // Delete character
                        chara_nos.next();
                    } else {
                        // Retain character
                        swap_slice_with_stride(&mut x.vals, stride, rd, wr);
                        wr += 1;
                    }
                    rd += 1;
                }
                x.vals[wr * stride..].fill(Default::default());
            }
            RefFlatValue::ArrStr(x) => {
                let mut x = x.borrow_mut();
                let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                while rd < charas_count {
                    if Some(rd) == chara_nos.peek().copied() {
                        // Delete character
                        chara_nos.next();
                    } else {
                        // Retain character
                        swap_slice_with_stride(&mut x.vals, stride, rd, wr);
                        wr += 1;
                    }
                    rd += 1;
                }
                x.vals[wr * stride..].fill(Default::default());
            }
            _ => unreachable!(),
        }
    }

    Ok(())
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn swap_chara<Callback: EraCompilerCallback>(
    chara_no1: u32,
    chara_no2: u32,
    ctx: &mut EraCompilerCtx<Callback>,
) -> anyhow::Result<()> {
    let chara_no1 = chara_no1 as usize;
    let chara_no2 = chara_no2 as usize;

    for chara_var in ctx.variables.chara_vars_iter() {
        match chara_var.val.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let mut x = x.borrow_mut();
                let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                swap_slice_with_stride(&mut x.vals, stride, chara_no1, chara_no2);
            }
            RefFlatValue::ArrStr(x) => {
                let mut x = x.borrow_mut();
                let stride: usize = x.dims.iter().skip(1).map(|&x| x as usize).product();
                swap_slice_with_stride(&mut x.vals, stride, chara_no1, chara_no2);
            }
            _ => unreachable!(),
        }
    }

    Ok(())
}

#[derive(Debug)]
pub struct CheckSaveHeaderResult {
    pub status: i64,
    pub timestamp: u64,
    pub save_info: String,
}

fn check_save_header<Callback: EraCompilerCallback>(
    save_header: crate::v2::savefs::EraSaveFileHeader,
    ctx: &mut EraCompilerCtx<Callback>,
) -> anyhow::Result<CheckSaveHeaderResult> {
    use crate::v2::savefs::*;
    use anyhow::bail;

    let version = save_header.version;
    if version != 1808 {
        bail!("unsupported version {version}");
    }
    let save_file_type = save_header.file_type;
    if !matches!(save_file_type, EraSaveFileType::Normal) {
        bail!("invalid save file type {save_file_type:?}");
    }

    let get_var_i32_0d = |name| {
        ctx.variables
            .get_var(name)
            .and_then(|x| x.as_arrint().map(|x| x.borrow().vals[0].val))
    };
    let cur_game_code = get_var_i32_0d("GAMEBASE_GAMECODE").unwrap_or(0);
    let cur_game_ver = get_var_i32_0d("GAMEBASE_VERSION").unwrap_or(0);
    let cur_game_min_ver = get_var_i32_0d("GAMEBASE_ALLOWVERSION").unwrap_or(0);
    // Check game code
    let game_code = save_header.game_code;
    if !(game_code == 0 || game_code == cur_game_code) {
        return Ok(CheckSaveHeaderResult {
            status: 2,
            timestamp: 0,
            save_info: String::new(),
        });
    }
    // Check game version
    let game_ver = save_header.game_version;
    if !(game_ver >= cur_game_min_ver || game_ver == cur_game_ver) {
        return Ok(CheckSaveHeaderResult {
            status: 3,
            timestamp: 0,
            save_info: String::new(),
        });
    }

    let timestamp = 0;
    let save_info = save_header.save_info;
    Ok(CheckSaveHeaderResult {
        status: 0,
        timestamp,
        save_info,
    })
}

#[derive(Debug)]
pub struct LoadDataResult {
    pub file_exists: bool,
    pub charas_count: u32,
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn load_data<Callback: EraCompilerCallback>(
    save_path: &str,
    ctx: &mut EraCompilerCtx<Callback>,
) -> anyhow::Result<LoadDataResult> {
    use crate::util::io::CSharpBinaryReader;
    use crate::v2::savefs::*;
    use anyhow::bail;
    use binrw::BinReaderExt;
    use num_traits::FromPrimitive;

    if !ctx.callback.on_check_host_file_exists(save_path)? {
        return Ok(LoadDataResult {
            file_exists: false,
            charas_count: 0,
        });
    }
    let file = ctx.callback.on_open_host_file(save_path, false)?;
    let mut file = std::io::BufReader::new(file);

    let save_header: EraSaveFileHeader = file.read_le()?;
    _ = check_save_header(save_header, ctx)?;

    reset_data(ctx);

    // Load character variables
    let charas_count = file
        .read_i64()?
        .try_into()
        .context("invalid character count")?;
    for chara_i in 0..charas_count {
        loop {
            let var_type =
                EraSaveDataType::from_u8(file.read_u8()?).context("invalid save data type")?;
            match var_type {
                EraSaveDataType::Separator => continue,
                EraSaveDataType::EOC | EraSaveDataType::EOF => break,
                _ => {
                    let var_name = file.read_utf16_string()?;
                    let var = ctx
                        .variables
                        .get_var_info_by_name(&var_name)
                        .with_context(|| format!("variable `{}` does not exist", var_name))?;
                    if !var.is_charadata {
                        bail!("variable `{}` is not CHARADATA", var_name);
                    }
                    file.read_var(var_type, var, Some(chara_i as _))
                        .with_context(|| format!("read variable `{}` failed", var_name))?;
                }
            }
        }
    }

    // Load normal variables
    loop {
        let var_type =
            EraSaveDataType::from_u8(file.read_u8()?).context("invalid save data type")?;
        match var_type {
            EraSaveDataType::EOF => break,
            _ => {
                let var_name = file.read_utf16_string()?;
                let var = ctx
                    .variables
                    .get_var_info_by_name(&var_name)
                    .with_context(|| format!("variable `{}` does not exist", var_name))?;
                if var.is_charadata {
                    bail!("variable `{}` is CHARADATA", var_name);
                }
                file.read_var(var_type, var, None)
                    .with_context(|| format!("read variable `{}` failed", var_name))?;
            }
        }
    }

    Ok(LoadDataResult {
        file_exists: true,
        charas_count,
    })
}

#[derive(Debug)]
pub struct CheckDataResult {
    pub status: i64,
    pub timestamp: u64,
    pub save_info: String,
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn check_data<Callback: EraCompilerCallback>(
    save_path: &str,
    ctx: &mut EraCompilerCtx<Callback>,
) -> anyhow::Result<CheckDataResult> {
    use crate::v2::savefs::*;
    use binrw::BinReaderExt;

    if !ctx.callback.on_check_host_file_exists(save_path)? {
        return Ok(CheckDataResult {
            status: 1,
            timestamp: 0,
            save_info: String::new(),
        });
    }
    let file = ctx.callback.on_open_host_file(save_path, false)?;
    let mut file = std::io::BufReader::new(file);

    let save_header: EraSaveFileHeader = file.read_le()?;
    let result = check_save_header(save_header, ctx)?;

    Ok(CheckDataResult {
        status: result.status,
        timestamp: result.timestamp,
        save_info: result.save_info,
    })
}

// NOTE: Non-standard intrinsic; must be called manually for now
pub fn reset_data<Callback: EraCompilerCallback>(ctx: &mut EraCompilerCtx<Callback>) {
    // TODO: Fully reset data according to Emuera
    // *self.charas_count = 0;
    for var in ctx.variables.iter() {
        if var.is_const {
            continue;
        }
        match var.val.as_unpacked() {
            RefFlatValue::ArrInt(x) => {
                let mut x = x.borrow_mut();
                let should_reset = !x.flags.is_trap()
                    && (var.is_charadata
                        || !(var.is_global || matches!(var.name.as_ref(), "GLOBAL" | "ITEMPRICE")));
                if should_reset {
                    x.vals.fill(Default::default());
                }
            }
            RefFlatValue::ArrStr(x) => {
                let mut x = x.borrow_mut();
                let should_reset = !x.flags.is_trap()
                    && (var.is_charadata
                        || !(var.is_global || matches!(var.name.as_ref(), "GLOBALS" | "STR")));
                if should_reset {
                    x.vals.fill(Default::default());
                }
            }
            _ => unreachable!(),
        }
    }
    // Restore variables with initializers
    ctx.variables.reinit_variables();
}
