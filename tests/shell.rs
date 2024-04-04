use std::{env, fs::File, io::Write, path::Path};

use anyhow::{ensure, Context, Result};
use assert_cmd::Command;
use dir_test::{dir_test, Fixture};
use predicates::prelude::*;
use serde::Deserialize;
use tempdir::TempDir;

#[derive(Debug, Deserialize)]
struct Metadata {
    #[serde(default)]
    stdout: String,
    #[serde(default)]
    stderr: String,

    #[serde(default)]
    disable_windows: bool,
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "**/*.test.wsh",
)]
fn run_test(fixture: Fixture<&str>) -> Result<()> {
    let name = Path::new(fixture.path())
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();
    let (script, metadata) = parse_script(fixture.content())?;
    if cfg!(windows) && metadata.disable_windows {
        return Ok(());
    }
    let tmp_dir = TempDir::new(name).context("error creating temporary directory")?;
    let file_path = tmp_dir.path().join(format!("{name}.wsh"));
    let mut f = File::create(&file_path).context("error creating file for input")?;
    f.write_all(script.as_bytes())
        .context("error writing input to file")?;
    let mut cmd =
        Command::cargo_bin(env!("CARGO_PKG_NAME")).context("error finding crate binary")?;
    cmd.arg(&file_path)
        .current_dir(tmp_dir.path())
        .env("WSH_ENV", "test")
        .env(
            "WASM_PATH",
            env::current_dir()
                .context("error getting cwd")?
                .join("tests/wasm"),
        )
        .assert()
        .try_success()?
        .try_stdout(predicate::eq(metadata.stdout.trim()).trim())?
        .try_stderr(predicate::eq(metadata.stderr.trim()).trim())?;

    drop(f);
    tmp_dir
        .close()
        .context("error closing temporary directory")?;
    Ok(())
}

fn parse_script(contents: &str) -> Result<(&str, Metadata)> {
    let mut lines = contents.lines().map(|line| (line.len() + 1, line));
    ensure!(
        lines.next().is_some_and(|(_, line)| line == "---"),
        "test file contains invalid header line, expected `---`"
    );
    let yaml_end = lines
        .take_while(|(_, line)| {
            if *line == "---" {
                return false;
            }
            true
        })
        .fold(4, |mut acc, (len, _)| {
            acc += len;
            acc
        });
    let yaml = &contents[4..yaml_end];
    let script = &contents[yaml_end + 4..];
    let metadata =
        serde_yaml::from_str::<Metadata>(yaml).context("error reading YAML test metadata")?;
    Ok((script, metadata))
}
