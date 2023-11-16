use std::{collections::HashMap, fs, path::Path};

use anyhow::{bail, ensure, Context, Result};
use shwasi_engine::{Instance, Store, Value};
use shwasi_parser::{validate, Parser};
use tracing::info;
use wast::{
    core::{HeapType, Module, NanPattern, WastArgCore, WastRetCore},
    token::{Id, Index, Span},
    QuoteWat, WastArg, WastDirective, WastExecute, WastInvoke, WastRet, Wat,
};

pub fn run_spectest(name: &str) -> Result<()> {
    let file = Path::new("tests/spectests/testsuite")
        .join(name)
        .with_extension("wast");
    let contents = fs::read_to_string(&file)
        .with_context(|| format!("failed to read file, {}", file.display()))?;
    let tokens = wast::parser::ParseBuffer::new(&contents).context("failed to lex wast file")?;
    let wast = wast::parser::parse::<wast::Wast>(&tokens).context("failed to parse wast file")?;

    let mut store = Store::default();
    store.define("spectest", "print", || {
        println!("print");
    });
    store.define("spectest", "print_i32", |i: i32| {
        println!("print_i32: {i}");
    });
    let ctx = ExecutionContext {
        contents: &contents,
        store,
        ..Default::default()
    };
    execute_directives(wast.directives, ctx)?;

    Ok(())
}

#[derive(Debug, Default)]
struct ExecutionContext<'a> {
    contents: &'a str,

    instances: HashMap<String, Instance>,
    last_instance: Option<Instance>,
    store: Store,
}

impl ExecutionContext<'_> {
    fn line(&self, span: Span) -> usize {
        span.linecol_in(self.contents).0 + 1
    }

    fn invoke(&mut self, invoke: &WastInvoke) -> Result<Vec<Value>> {
        let func = self
            .get_inst(invoke.module)
            .get_func_untyped(&self.store, invoke.name)
            .unwrap_or_else(|err| panic!("{} should be defined, but got {err}", invoke.name));
        let args = convert_args(&invoke.args);
        let results = func.call(&mut self.store, &args).with_context(|| {
            format!(
                "invoke call failed for {} at {span}",
                invoke.name,
                span = self.line(invoke.span)
            )
        })?;

        Ok(results)
    }

    fn get_inst(&mut self, id: Option<Id>) -> Instance {
        id.map(|id| {
            self.instances
                .get(id.name())
                .unwrap_or_else(|| panic!("should have an instance named \"{}\"", id.name()))
        })
        .unwrap_or_else(|| {
            self.last_instance
                .as_ref()
                .expect("should have an instance")
        })
        .clone()
    }
}

fn execute_directives(directives: Vec<WastDirective>, mut ctx: ExecutionContext) -> Result<()> {
    for directive in directives.into_iter() {
        match directive {
            WastDirective::Wat(mut module) => {
                if matches!(
                    module,
                    QuoteWat::QuoteComponent(..) | QuoteWat::Wat(Wat::Component(..))
                ) {
                    unimplemented!("wat component encountered!");
                }

                let wasm = module.encode().expect("module should have a valid form");
                let parser = Parser::new(&wasm);
                let parse_module = parser
                    .read_module()
                    .expect("module should have a valid form");
                let instance = Instance::instantiate(&mut ctx.store, parse_module)
                    .expect("module should be valid");
                if let QuoteWat::Wat(Wat::Module(Module { id: Some(id), .. })) = module {
                    ctx.instances.insert(id.name().to_owned(), instance.clone());
                }
                ctx.last_instance = Some(instance);
            }
            WastDirective::AssertMalformed {
                span,
                mut module,
                message,
            } => {
                let Ok(wasm) = module.encode() else {
                    info!("ignored bad module at {span}", span = ctx.line(span));
                    continue;
                };
                let parser = Parser::new(&wasm);
                ensure!(
                    parser.read_module().is_err(),
                    "module should be malformed: {message} at {span}",
                    span = ctx.line(span)
                );
            }
            WastDirective::AssertInvalid {
                span,
                mut module,
                message,
            } => {
                let wasm = module.encode().expect("module should have a valid form");
                let parser = Parser::new(&wasm);
                let m = parser.read_module().expect("module should parse correctly");
                ensure!(
                    validate(&m).is_err(),
                    "module should not be valid: {message} at {span}",
                    span = ctx.line(span)
                );
            }
            WastDirective::Invoke(invoke) => {
                ctx.invoke(&invoke)?;
            }
            WastDirective::AssertTrap {
                span,
                exec,
                message,
            } => match exec {
                WastExecute::Invoke(invoke) => {
                    let res = ctx.invoke(&invoke);
                    ensure!(
                        res.is_err(),
                        "expected {message} but got no error at {span}",
                        span = ctx.line(span)
                    );
                    info!(
                        "assert trap passed for {} at {span}",
                        invoke.name,
                        span = ctx.line(span)
                    );
                }
                WastExecute::Wat(Wat::Module(mut m)) => {
                    let wasm = m.encode().expect("module should have a valid form");
                    let parser = Parser::new(&wasm);
                    let parse_module = parser
                        .read_module()
                        .expect("module should have a valid form");
                    let res = Instance::instantiate(&mut ctx.store, parse_module);
                    ensure!(
                        res.is_err(),
                        "expected {message} but got no error at {span}",
                        span = ctx.line(span)
                    );
                }
                _ => unimplemented!(),
            },
            WastDirective::AssertReturn {
                span,
                exec,
                results: expect,
            } => match exec {
                WastExecute::Invoke(invoke) => {
                    let results = ctx.invoke(&invoke)?;
                    let expect = convert_results(&expect);
                    ensure!(
                        matches(&results, &expect),
                        "expected {expect:?} but got {results:?} at {span} when running {name}",
                        span = ctx.line(span),
                        name = invoke.name,
                    );
                    info!("assert return passed for {}", invoke.name);
                }
                WastExecute::Get { module, global } => {
                    let val = ctx.get_inst(module).get_global(&ctx.store, global);
                    let Some(val) = val else {
                        bail!("global {global:?} not found from {module:?}");
                    };
                    let expect = convert_results(&expect);
                    ensure!(
                        matches(&[val], &expect),
                        "expected {expect:?} but got {val:?} at {span} when running {name}",
                        span = ctx.line(span),
                        name = global,
                    );
                }
                WastExecute::Wat(..) => unimplemented!(),
            },
            WastDirective::Register {
                span: _,
                name,
                module,
            } => {
                let inst = ctx.get_inst(module);
                inst.export_as(&mut ctx.store, name);
            }
            WastDirective::AssertUnlinkable {
                span,
                mut module,
                message,
            } => {
                let wasm = module.encode().expect("module should have a valid form");
                let parser = Parser::new(&wasm);
                let m = parser.read_module().expect("module should parse correctly");
                ensure!(
                    Instance::instantiate(&mut ctx.store, m).is_err(),
                    "module should not be linkable: {message} at {span}",
                    span = ctx.line(span)
                );
            }

            _ => panic!("unsupported directive: {directive:?}"),
        }
    }

    Ok(())
}

fn convert_results(results: &[WastRet]) -> Vec<Value> {
    results
        .iter()
        .map(|ret| {
            let WastRet::Core(ret) = ret else {
                unimplemented!("core return val encountered!");
            };
            match ret {
                WastRetCore::I32(i32) => Value::I32(*i32 as u32),
                WastRetCore::I64(i64) => Value::I64(*i64 as u64),
                WastRetCore::F32(f32) => Value::F32(match f32 {
                    NanPattern::Value(val) => f32::from_bits(val.bits),
                    NanPattern::CanonicalNan | NanPattern::ArithmeticNan => f32::NAN,
                }),
                WastRetCore::F64(f64) => Value::F64(match f64 {
                    NanPattern::Value(val) => f64::from_bits(val.bits),
                    NanPattern::CanonicalNan | NanPattern::ArithmeticNan => f64::NAN,
                }),
                WastRetCore::RefNull(refty) => match refty.unwrap() {
                    HeapType::Func => Value::Ref(None),
                    HeapType::Extern => Value::ExternRef(None),
                    ty => unimplemented!("heap type of {ty:?} encountered!"),
                },
                WastRetCore::RefExtern(ext) => Value::ExternRef(*ext),
                WastRetCore::RefFunc(ref_) => Value::Ref(ref_.map(|ref_| match ref_ {
                    Index::Num(n, _) => n,
                    Index::Id(_) => unreachable!("id should not be present in an encoded module"),
                })),
                _ => unimplemented!("unsupported return val encountered!"),
            }
        })
        .collect()
}

fn convert_args(args: &[WastArg]) -> Vec<Value> {
    args.iter()
        .map(|arg| {
            let WastArg::Core(arg) = arg else {
                unimplemented!("core arg encountered!");
            };
            match arg {
                WastArgCore::I32(i32) => Value::I32(*i32 as u32),
                WastArgCore::I64(i64) => Value::I64(*i64 as u64),
                WastArgCore::F32(f32) => Value::F32(f32::from_bits(f32.bits)),
                WastArgCore::F64(f64) => Value::F64(f64::from_bits(f64.bits)),
                WastArgCore::RefNull(refty) => match refty {
                    HeapType::Func => Value::Ref(None),
                    HeapType::Extern => Value::ExternRef(None),
                    ty => unimplemented!("heap type of {ty:?} encountered!"),
                },
                WastArgCore::RefExtern(ext) => Value::ExternRef(Some(*ext)),
                WastArgCore::RefHost(_) | WastArgCore::V128(_) => {
                    unimplemented!("unsupported arg encountered!")
                }
            }
        })
        .collect()
}

fn matches(v1: &[Value], v2: &[Value]) -> bool {
    v1.iter().zip(v2).all(|(v1, v2)| match (v1, v2) {
        (Value::I32(i1), Value::I32(i2)) => i1 == i2,
        (Value::I64(i1), Value::I64(i2)) => i1 == i2,
        (Value::F32(f1), Value::F32(f2)) => f1 == f2 || f1.is_nan() && f2.is_nan(),
        (Value::F64(f1), Value::F64(f2)) => f1 == f2 || f1.is_nan() && f2.is_nan(),
        (Value::Ref(r1), Value::Ref(r2)) => r1 == r2,
        (Value::ExternRef(r1), Value::ExternRef(r2)) => r1 == r2,
        _ => false,
    })
}
