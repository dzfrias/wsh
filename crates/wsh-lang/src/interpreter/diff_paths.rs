use std::path::*;

/// Obtain a relative path from a base.
///
/// This is taken and adapted from [pathdiff](https://crates.io/crates/pathdiff) to fix a Windows
/// verbatim path bug.
pub fn diff_paths<P, B>(path: P, base: B) -> Option<PathBuf>
where
    P: AsRef<Path>,
    B: AsRef<Path>,
{
    let path = path.as_ref();
    let base = base.as_ref();

    if path.is_absolute() != base.is_absolute() {
        if path.is_absolute() {
            Some(PathBuf::from(path))
        } else {
            None
        }
    } else {
        let mut ita = path.components();
        let mut itb = base.components();
        let mut comps: Vec<Component> = vec![];
        loop {
            match (ita.next(), itb.next()) {
                (None, None) => break,
                (Some(a), None) => {
                    comps.push(a);
                    comps.extend(ita.by_ref());
                    break;
                }
                (None, _) => comps.push(Component::ParentDir),
                (Some(a), Some(b)) if comps.is_empty() && a == b => (),
                (Some(a), Some(Component::CurDir)) => comps.push(a),
                (Some(_), Some(Component::ParentDir)) => return None,
                (Some(a), Some(b)) => {
                    if comps.is_empty() {
                        if let (Component::Prefix(a), Component::Prefix(b)) = (a, b) {
                            match (a.kind(), b.kind()) {
                                (Prefix::Disk(a), Prefix::VerbatimDisk(b))
                                | (Prefix::VerbatimDisk(a), Prefix::Disk(b))
                                    if a == b =>
                                {
                                    continue;
                                }
                                (Prefix::VerbatimUNC(a, c), Prefix::UNC(b, d))
                                | (Prefix::UNC(a, c), Prefix::VerbatimUNC(b, d))
                                    if a == b && c == d =>
                                {
                                    continue
                                }
                                _ => {}
                            }
                        }
                    }

                    comps.push(Component::ParentDir);
                    for _ in itb {
                        comps.push(Component::ParentDir);
                    }
                    comps.push(a);
                    comps.extend(ita.by_ref());
                    break;
                }
            }
        }
        Some(comps.iter().map(|c| c.as_os_str()).collect())
    }
}
