use crate::bytecode::SourcePosInfo;

fn split_whitespace(mut v: &[u8]) -> &[u8] {
    while let [b' ' | b'\t', rest @ ..] | [rest @ .., b' ' | b'\t'] = v {
        v = rest;
    }
    v
}

pub fn parse_csv<const COL_COUNT: usize>(
    content: &[u8],
) -> Result<Vec<[Box<[u8]>; COL_COUNT]>, (SourcePosInfo, String)> {
    let mut rows: Vec<[Box<[u8]>; COL_COUNT]> = Vec::new();
    for (idx, mut line) in content.split(|x| matches!(x, b'\r' | b'\n')).enumerate() {
        let idx = idx + 1;
        line = split_whitespace(line);
        if let [] | [b';', ..] = line {
            continue;
        }
        let mut failed = false;
        let mut it = line.split(|x| *x == b',');
        let row = std::array::from_fn(|_| {
            it.next()
                .map(|x| {
                    let x: Box<[u8]> = split_whitespace(x).into();
                    x
                })
                .unwrap_or_else(|| {
                    failed = true;
                    Box::from(&b""[..])
                })
        });
        if failed {
            return Err((
                SourcePosInfo {
                    line: idx as _,
                    column: 1,
                },
                "too few columns in CSV".to_owned(),
            ));
        }
        if it.next().is_some() {
            return Err((
                SourcePosInfo {
                    line: idx as _,
                    column: 1,
                },
                "too many columns in CSV".to_owned(),
            ));
        }
        rows.push(row);
    }
    Ok(rows)
}
