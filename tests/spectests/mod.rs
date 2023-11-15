use std::{fs, path::Path};

use anyhow::{ensure, Context, Result};
use shwasi_engine::{Instance, Store, Value};
use shwasi_parser::{validate, Parser};
use wast::{
    core::{HeapType, NanPattern, WastArgCore, WastRetCore},
    token::{Index, Span},
    WastArg, WastDirective, WastExecute, WastInvoke, WastRet,
};

pub fn run_spectest(name: &str) -> Result<()> {
    let file = Path::new("tests/spectests/testsuite")
        .join(name)
        .with_extension("wast");
    let contents = fs::read_to_string(file).context("failed to read file")?;
    let tokens = wast::parser::ParseBuffer::new(&contents).context("failed to lex wast file")?;
    let wast = wast::parser::parse::<wast::Wast>(&tokens).context("failed to parse wast file")?;

    let ctx = ExecutionContext {
        contents: &contents,
        instance: None,
        store: Store::default(),
    };
    execute_directives(wast.directives, ctx)?;

    Ok(())
}

#[derive(Debug)]
struct ExecutionContext<'a> {
    contents: &'a str,

    instance: Option<Instance>,
    store: Store,
}

impl ExecutionContext<'_> {
    fn line(&self, span: Span) -> usize {
        span.linecol_in(self.contents).0 + 1
    }

    fn invoke(&mut self, invoke: &WastInvoke) -> Result<Vec<Value>> {
        let func = self
            .instance
            .as_ref()
            .expect("should have an instance")
            .get_func_untyped(&mut self.store, invoke.name)
            .unwrap_or_else(|err| panic!("{} should be defined, but got {err}", invoke.name));
        let args = convert_args(&invoke.args);
        let results = func.call(&mut self.store, &args).with_context(|| {
            format!(
                "invoke call failed for {} over {}",
                invoke.name,
                self.line(invoke.span)
            )
        })?;

        Ok(results)
    }
}

fn execute_directives(directives: Vec<WastDirective>, mut ctx: ExecutionContext) -> Result<()> {
    for directive in directives.into_iter() {
        match directive {
            WastDirective::Wat(mut module) => {
                let wasm = module.encode().expect("module should have a valid form");
                let parser = Parser::new(&wasm);
                let module = parser
                    .read_module()
                    .expect("module should have a valid form");
                let instance =
                    Instance::instantiate(&mut ctx.store, module).expect("module should be valid");
                ctx.instance = Some(instance);
            }
            WastDirective::AssertMalformed {
                span,
                mut module,
                message,
            } => {
                let Ok(wasm) = module.encode() else {
                    eprintln!("ignored bad module");
                    continue;
                };
                let parser = Parser::new(&wasm);
                ensure!(
                    parser.read_module().is_err(),
                    "module should be malformed: {message} over {span}",
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
                    "module should not be valid: {message} over {span}",
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
                        "expected {message} but got no error over {span}",
                        span = ctx.line(span)
                    );
                    eprintln!("assert trap passed for {}", invoke.name);
                }
                WastExecute::Get { .. } | WastExecute::Wat(..) => unimplemented!(),
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
                        "expected {expect:?} but got {results:?} over {span} when running {name}",
                        span = ctx.line(span),
                        name = invoke.name,
                    );
                    eprintln!("assert return passed for {}", invoke.name);
                }
                // TODO
                WastExecute::Get { .. } => continue,
                WastExecute::Wat(..) => unimplemented!(),
            },

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
                WastRetCore::RefExtern(ext) => Value::ExternRef(ext.map(|ext| ext as usize)),
                WastRetCore::RefFunc(ref_) => Value::Ref(ref_.map(|ref_| match ref_ {
                    Index::Num(n, _) => n as usize,
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
                WastArgCore::RefExtern(ext) => Value::ExternRef(Some(*ext as usize)),
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
