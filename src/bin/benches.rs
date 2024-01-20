use std::{cell::OnceCell, fs, path::PathBuf, rc::Rc};

use anyhow::Result;
use clap::Parser;
use shwasi_engine::{HostFunc, Instance, Store};
use shwasi_wasi::{cap_std, WasiCtxBuilder};

#[derive(Debug, Parser)]
struct Args {
    pub file: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut store = Store::default();
    let now = Rc::new(OnceCell::new());
    {
        let now = now.clone();
        store.define(
            "bench",
            "start",
            HostFunc::wrap(move |_: Instance, _: &mut Store| {
                println!("Benchmark start");
                now.set(std::time::Instant::now()).unwrap();
            }),
        );
    }
    store.define(
        "bench",
        "end",
        HostFunc::wrap(move |_: Instance, _: &mut Store| {
            let delta = std::time::Instant::now() - *now.get().unwrap();
            println!(
                "Benchmark end: {}s {}ms",
                delta.as_secs(),
                delta.as_millis() - (delta.as_secs() as u128) * 1000
            );
        }),
    );
    let dir = cap_std::fs::Dir::open_ambient_dir(".", cap_std::ambient_authority())?;
    let mut ctx = WasiCtxBuilder::new().preopened_dir(dir, ".")?.build();
    shwasi_wasi::sync::snapshots::preview_1::link(&mut store, &mut ctx);

    let binary = fs::read(args.file)?;
    let module = shwasi_parser::Parser::new(&binary).read_module()?;
    let instance = Instance::instantiate(&mut store, module)?;
    let start = instance.get_func::<(), ()>(&store, "_start")?;
    start.call(&mut store, ())?;

    Ok(())
}
