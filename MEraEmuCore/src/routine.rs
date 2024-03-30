pub fn is_chara_var_name(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "ISASSI" | "NO" | "BASE" | "MAXBASE" | "ABL" | "TALENT" | "EXP" | "MARK" | "PALAM"
        | "SOURCE" | "EX" | "CFLAG" | "JUEL" | "RELATION" | "EQUIP" | "TEQUIP" | "STAIN"
        | "GOTJUEL" | "NOWEX" | "DOWNBASE" | "CUP" | "CDOWN" | "TCVAR" | "NAME" | "CALLNAME"
        | "NICKNAME" | "MASTERNAME" | "CSTR" | "CDFLAG" => true,
        _ => false,
    }
}

pub fn is_chara_nodim(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "NO" | "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => true,
        _ => false,
    }
}

pub fn is_csv_var(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "ABL" | "EXP" | "TALENT" | "PALAM" | "JUEL" | "GOTJUEL" | "UP" | "DOWN" | "CUP"
        | "CDOWN" | "TRAIN" | "MARK" | "ITEM" | "ITEMPRICE" | "ITEMSALES" | "BASE" | "MAXBASE"
        | "DOWNBASE" | "LOSEBASE" | "SOURCE" | "EX" | "NOWEX" | "STR" | "EQUIP" | "TEQUIP"
        | "FLAG" | "TFLAG" | "CFLAG" | "TCVAR" | "CSTR" | "STAIN" | "CDFLAG" | "STRNAME"
        | "TSTR" | "SAVESTR" | "GLOBAL" | "GLOBALS" => true,
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub struct CharaVarProps {
    is_string: bool,
    dims: u32,
}

pub fn chara_var_props(name: &str) -> Option<CharaVarProps> {
    let name = name.to_ascii_uppercase();
    Some(match name.as_str() {
        "ISASSI" | "NO" => CharaVarProps {
            is_string: false,
            dims: 0,
        },
        "BASE" | "MAXBASE" | "ABL" | "TALENT" | "EXP" | "MARK" | "PALAM" | "SOURCE" | "EX"
        | "CFLAG" | "JUEL" | "RELATION" | "EQUIP" | "TEQUIP" | "STAIN" | "GOTJUEL" | "NOWEX"
        | "DOWNBASE" | "CUP" | "CDOWN" | "TCVAR" => CharaVarProps {
            is_string: false,
            dims: 1,
        },
        "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => CharaVarProps {
            is_string: true,
            dims: 0,
        },
        "CSTR" => CharaVarProps {
            is_string: true,
            dims: 1,
        },
        "CDFLAG" => CharaVarProps {
            is_string: false,
            dims: 2,
        },
        _ => return None,
    })
}

pub fn is_event_name(name: &str) -> bool {
    let name = name.to_ascii_uppercase();
    match name.as_str() {
        "EVENTFIRST" | "EVENTTRAIN" | "EVENTSHOP" | "EVENTBUY" | "EVENTCOM" | "EVENTTURNEND"
        | "EVENTCOMEND" | "EVENTEND" | "EVENTLOAD" => true,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum VarKindProp {
    Normal,
    Chara,
}

#[derive(Debug, Clone)]
pub struct VarProps {
    kind: VarKindProp,
    dims: u32,
}

pub fn builtin_var_props(name: &str) -> Option<VarProps> {
    // VariableSize.csv?
    todo!()
}

pub fn is_local_or_arg_var(name: &str) -> bool {
    name.eq_ignore_ascii_case("LOCAL")
        || name.eq_ignore_ascii_case("LOCALS")
        || name.eq_ignore_ascii_case("ARG")
        || name.eq_ignore_ascii_case("ARGS")
}

pub fn parse_era_int(val: &str) -> Option<i64> {
    if let Some(x) = val.strip_prefix("0x").or_else(|| val.strip_prefix("0X")) {
        i64::from_str_radix(x, 16).ok()
    } else {
        val.parse::<i64>().ok()
    }
}

pub fn is_obsolete_var(name: &str) -> bool {
    name.eq_ignore_ascii_case("DITEMTYPE")
        || name.eq_ignore_ascii_case("DA")
        || name.eq_ignore_ascii_case("DB")
        || name.eq_ignore_ascii_case("DC")
        || name.eq_ignore_ascii_case("DD")
        || name.eq_ignore_ascii_case("DE")
        || name.eq_ignore_ascii_case("TA")
        || name.eq_ignore_ascii_case("TB")
}
