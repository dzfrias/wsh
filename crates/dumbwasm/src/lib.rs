//! # dumbwasm
//!
//! dumbwasm is an extremely simple text-to-[WebAssembly](https://webassembly.org/) transpiler.
//! Unlike the existing WebAssembly text format (WAT), dumbwasm syntax allows for invalid modules
//! to be generated. This makes dumbwasm ideal for writing tests related to WebAssembly!
//!
//! Note that if you're just looking to compile text to WebAssembly, I'd recommend looking using
//! the standard text format. dumbwasm is intended for people who need to manipulate the binary
//! format in ways that WAT doesn't allow.
//!
//! ## Syntax
//!
//! Here is a super simple WebAssembly module, in dumbwasm!
//!
//! ```dumbwasm
//! magic
//! version
//!
//! type {
//!   (anything in parens is skipped by the transpiler)
//!   [
//!     ;; The square brackets automatically count up the number of items (delimited by a comma)!
//!     form.func [] [i32],
//!   ]
//! }
//! function {
//!   ;; Equivalent to writing `[0]`!
//!   1 (number of funcs)
//!   0 (index)
//! }
//! code {
//!   [
//!     ;; This function returns 10 + 20
//!     instrs {
//!       [] (number of locals)
//!       i32.const 10
//!       ;; Base 16 also supported!
//!       i32.const 0xfafa
//!       i32.add
//!       return
//!     },
//!   ]
//! }
//! ```
//!
//! ### Sections
//!
//! You can create a section using the appropriate section name, along with curly braces. This will
//! automatically tally up section size for you, so you don't have to count up all the bytes by
//! hand!
//!
//! Here are the section names:
//!
//! - `custom`
//! - `type`
//! - `import`
//! - `function`
//! - `table`
//! - `memory`
//! - `global`
//! - `export`
//! - `start`
//! - `element`
//! - `code`
//! - `data`
//! - `datacount`
//!
//! ### Instructions
//!
//! Instructions will directly be translated into their binary form. For example, the `unreachable`
//! instruction translates to `0x00`. Instructions can be used anywhere in the module.
//!
//! There's also the `instrs` construct, which (like sections) automatically inserts the size into
//! the binary, to save you from having to count up by hand. It is important to note that `instrs`
//! only does the counting, no more and no less. Instructions are valid anywhere. This is key for
//! things like const initialization expressions, which don't have a size component in the binary
//! format.
//!
//! ### Literals
//!
//! Literals of many forms are allowed in the text format. Primarily, this means numbers, strings,
//! and vectors.
//!
//! For numbers, you can specifiy a type after the literal. For example, `10<i32>` will be written
//! as a signed 32-bit integer, as opposed to unsigned (the default). The type of literals like
//! `-10` and `10.2` are inferred from the literal itself. This syntax is also applicable to hex
//! and binary literal syntax!
//!
//! For strings, you can just put whatever you want in quotes! "hello world" will be encoded with
//! its size as the first bytes, followed by UTF-8.
//!
//! ### Aliases
//!
//! Almost everything else in dumbwasm is just an alias for a number that you'd have to write by
//! hand if you were to make a WebAssembly binary. For example, `i32` directly translates to the
//! number `0x7f`, no matter the context! You could even put `0x7f` in place of the magic value!
//!
//! Below is a table of such translations (not including instructions):
//!
//! | Syntax       | Output | Usage               |
//! |--------------|--------|---------------------|
//! | i32          | 0x7f   | Type                |
//! | i64          | 0x7e   | Type                |
//! | f32          | 0x7d   | Type                |
//! | f64          | 0x7c   | Type                |
//! | funcref      | 0x70   | Type                |
//! | externref    | 0x6f   | Type                |
//! | extern.func  | 0x00   | Import/export type  |
//! | extern.table | 0x01   | Import/export type  |
//! | extern.mem   | 0x02   | Import/export type  |
//! | form.func    | 0x60   | Type section form   |
//! | magic        | \0asm  | Beginning of module |
//! | version      | 1      | Beginning of module |
//! | true         | 0x01   | Generic             |
//! | false        | 0x00   | Generic             |

use crate::{lexer::Token, writer::Writer};
use logos::Logos;
pub use writer::{WriteError, WriteErrorKind};

mod lexer;
mod writer;

pub fn parse(input: &str) -> Result<Vec<u8>, WriteError> {
    let lexer = Token::lexer(input);
    let writer = Writer::new(lexer);
    writer.write()
}
