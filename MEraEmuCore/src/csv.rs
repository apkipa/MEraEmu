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
    for (idx, mut line) in content.split(|x| matches!(x, b'\n')).enumerate() {
        let idx = idx + 1;
        line = line.strip_suffix(&[b'\r']).unwrap_or(line);
        // Remove comment
        line = line.split(|&x| x == b';').next().unwrap();
        line = split_whitespace(line);
        if line.is_empty() {
            continue;
        }
        let mut failed = false;
        let mut it = line.split(|x| *x == b',').take(COL_COUNT);
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

pub fn parse_csv_loose(
    content: &[u8],
) -> Result<Vec<(u32, Vec<String>)>, std::convert::Infallible> {
    let mut rows = Vec::new();
    for (idx, mut line) in content.split(|x| matches!(x, b'\n')).enumerate() {
        let idx = idx + 1;
        line = line.strip_suffix(&[b'\r']).unwrap_or(line);
        // Remove comment
        line = line.split(|&x| x == b';').next().unwrap();
        line = split_whitespace(line);
        if line.is_empty() {
            continue;
        }

        let items = line
            .split(|x| *x == b',')
            .map(|x| String::from_utf8_lossy(split_whitespace(x)).into_owned())
            .collect();
        rows.push((idx as _, items));
    }
    Ok(rows)
}
