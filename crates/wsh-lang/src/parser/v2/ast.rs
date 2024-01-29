use core::fmt;
use std::{iter::FusedIterator, ops::Deref};

use smol_str::SmolStr;

/// The abstract syntax tree of the wsh language.
#[derive(Debug)]
pub struct Ast {
    /// The internal representation of nodes. The AST revolves around NodeInfo being small (around
    /// 16 bytes), taking a data-oriented approach. This design was largely inspired by the Zig
    /// compiler's ideas; read more at this link: https://mitchellh.com/zig/parser
    nodes: Vec<NodeInfo>,
    /// Holds any extra data a node might have. This is an arbitrary vector of bytes, and can be
    /// treated however is seen fit depending on the node kind.
    extra_data: Vec<u32>,
    strings: Vec<AstString>,
}

#[derive(Debug)]
pub struct Node {
    kind: NodeKind,
    offset: usize,
}

#[derive(Debug)]
pub enum NodeKind {
    Root(Root),

    // Statements
    Command(Command),
    Pipeline(Pipeline),
    Assignment(Assignment),
    Export(Export),

    // Expressions
    String(AstString),
    Number(Number),
    Boolean(Boolean),
    Ident(Ident),
    EnvVar(EnvVar),
    Binop(Binop),
    Unop(Unop),
    LastStatus(LastStatus),
    HomeDir(HomeDir),
}

#[derive(Debug)]
pub struct Root {
    pub stmts: NodeArray,
}

#[derive(Debug)]
pub struct Command {
    pub exprs: NodeArray,
    pub merge_stderr: bool,
}

#[derive(Debug)]
pub struct Pipeline {
    pub cmds: NodeArray,
    pub end: Option<PipelineEnd>,
}

#[derive(Debug)]
pub struct Assignment {
    pub name: Ident,
    pub value: NodeIndex,
}

#[derive(Debug)]
pub struct PipelineEnd {
    pub kind: PipelineEndKind,
    pub file: NodeIndex,
}

#[derive(Debug)]
#[repr(u8)]
pub enum PipelineEndKind {
    Write,
    Append,
}

#[derive(Debug)]
pub struct Binop {
    pub lhs: NodeIndex,
    pub rhs: NodeIndex,
    pub op: BinopKind,
}

#[derive(Debug)]
pub struct Export {
    pub name: Ident,
    pub value: NodeIndex,
}

#[derive(Debug)]
pub struct Unop {
    pub expr: NodeIndex,
    pub op: UnopKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinopKind {
    Add,
    Sub,
    Div,
    Mul,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnopKind {
    Neg,
    Bang,
    Sign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug)]
pub struct LastStatus;

#[derive(Debug)]
pub struct HomeDir;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Number(f64);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct NodeIndex(u32);

impl NodeIndex {
    pub(super) fn raw(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct EnvVar(Ident);

impl Deref for EnvVar {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// The string type in the AST.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AstString(SmolStr);

impl AstString {
    pub fn new(s: &str) -> Self {
        AstString(SmolStr::new(s))
    }
}

impl Deref for AstString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// An identifier.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ident(AstString);

impl Ident {
    pub fn new(s: &str) -> Self {
        Ident(AstString::new(s))
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct NodeArray {
    ptr: DataIndex,
    len: u32,
}

impl Ast {
    /// Create a new, empty AST.
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            extra_data: vec![],
            strings: vec![],
        }
    }

    /// Get a node from the AST, at the given node index.
    pub fn get(&self, node: NodeIndex) -> Node {
        let NodeInfo {
            kind,
            offset,
            p1,
            p2,
        } = self.nodes[node.0 as usize];

        macro_rules! binop {
            ($kind:ident) => {{
                NodeKind::Binop(Binop {
                    lhs: NodeIndex(p1),
                    rhs: NodeIndex(p2),
                    op: BinopKind::$kind,
                })
            }};
        }

        let node_kind = match kind {
            NodeInfoKind::Root => {
                let stmts = self.deserialize::<NodeArray>(DataIndex(p1));
                NodeKind::Root(Root { stmts })
            }
            NodeInfoKind::Command => {
                let exprs = self.deserialize::<NodeArray>(DataIndex(p1));
                NodeKind::Command(Command {
                    exprs,
                    merge_stderr: p2 == 1,
                })
            }
            NodeInfoKind::Pipeline => {
                let cmds = self.deserialize::<NodeArray>(DataIndex(p1));
                NodeKind::Pipeline(Pipeline { cmds, end: None })
            }
            NodeInfoKind::PipelineWithEnd => {
                let cmds = self.deserialize::<NodeArray>(DataIndex(p1));
                let end = self.deserialize::<PipelineEnd>(DataIndex(p2));
                NodeKind::Pipeline(Pipeline {
                    cmds,
                    end: Some(end),
                })
            }
            NodeInfoKind::Assignment => {
                let name = self.strings[p1 as usize].clone();
                NodeKind::Assignment(Assignment {
                    name: Ident(name),
                    value: NodeIndex(p2),
                })
            }
            NodeInfoKind::Export => {
                let name = self.strings[p1 as usize].clone();
                NodeKind::Export(Export {
                    name: Ident(name),
                    value: NodeIndex(p2),
                })
            }

            NodeInfoKind::Number => {
                let raw = (p2 as u64) | (p1 as u64) << 32;
                NodeKind::Number(Number(f64::from_bits(raw)))
            }
            NodeInfoKind::String => {
                let s = self.strings[p1 as usize].clone();
                NodeKind::String(s)
            }
            NodeInfoKind::EnvVar => {
                let s = self.strings[p1 as usize].clone();
                NodeKind::EnvVar(EnvVar(Ident(s)))
            }
            NodeInfoKind::Ident => {
                let s = self.strings[p1 as usize].clone();
                NodeKind::Ident(Ident(s))
            }
            NodeInfoKind::Boolean => NodeKind::Boolean(if p1 == 0 {
                Boolean::False
            } else {
                Boolean::True
            }),
            NodeInfoKind::LastStatus => NodeKind::LastStatus(LastStatus),
            NodeInfoKind::HomeDir => NodeKind::HomeDir(HomeDir),

            NodeInfoKind::Add => binop!(Add),
            NodeInfoKind::Sub => binop!(Sub),
            NodeInfoKind::Mul => binop!(Mul),
            NodeInfoKind::Div => binop!(Div),
            NodeInfoKind::Lt => binop!(Lt),
            NodeInfoKind::Gt => binop!(Gt),
            NodeInfoKind::Le => binop!(Le),
            NodeInfoKind::Ge => binop!(Ge),
            NodeInfoKind::Eq => binop!(Eq),
            NodeInfoKind::Ne => binop!(Ne),
            NodeInfoKind::Neg => NodeKind::Unop(Unop {
                expr: NodeIndex(p1),
                op: UnopKind::Neg,
            }),
            NodeInfoKind::Bang => NodeKind::Unop(Unop {
                expr: NodeIndex(p1),
                op: UnopKind::Bang,
            }),
            NodeInfoKind::Sign => NodeKind::Unop(Unop {
                expr: NodeIndex(p1),
                op: UnopKind::Sign,
            }),
        };

        Node {
            kind: node_kind,
            offset: offset as usize,
        }
    }

    /// Get the root node of the AST.
    pub fn root(&self) -> Root {
        let NodeKind::Root(root) = self.get(NodeIndex(self.nodes.len() as u32 - 1)).kind else {
            panic!("root should always be the last node!");
        };

        root
    }

    /// Add a node to the AST. This will return a handle to the node.
    // TODO: make unsafe
    pub(super) fn add(&mut self, node: NodeInfo) -> NodeIndex {
        self.nodes.push(node);
        NodeIndex(self.nodes.len() as u32 - 1)
    }

    /// Serialize a given object. This is useful for adding nodes to the AST.
    pub(super) fn serialize<T: Serialize>(&mut self, data: T::From) -> DataIndex {
        T::to_ast(data, self)
    }

    /// Allocate a string, return a handle to it.
    pub(super) fn alloc_string(&mut self, s: &str) -> StringIndex {
        self.strings.push(AstString::new(s));
        StringIndex(self.strings.len() as u32 - 1)
    }

    fn alloc_data(&mut self, data: u32) -> DataIndex {
        self.extra_data.push(data);
        DataIndex(self.extra_data.len() as u32 - 1)
    }

    fn deserialize<T: Deserialize>(&self, idx: DataIndex) -> T {
        T::from_ast(self, idx)
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
}

pub(super) trait Serialize {
    type From;

    fn to_ast(from: Self::From, ast: &mut Ast) -> DataIndex;
}

trait Deserialize {
    fn from_ast(ast: &Ast, index: DataIndex) -> Self;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(super) struct StringIndex(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(super) struct DataIndex(u32);

impl StringIndex {
    pub fn raw(self) -> u32 {
        self.0
    }
}

impl DataIndex {
    pub fn raw(self) -> u32 {
        self.0
    }
}

impl NodeArray {
    pub fn iter<'ast>(&self, ast: &'ast Ast) -> NodeArrayIter<'ast> {
        NodeArrayIter {
            ast,
            current: self.ptr,
            end: self.ptr.0 + self.len,
        }
    }
}

impl Deserialize for NodeArray {
    fn from_ast(ast: &Ast, index: DataIndex) -> Self {
        let len = ast.extra_data[index.0 as usize];
        Self {
            ptr: DataIndex(index.raw() + 1),
            len,
        }
    }
}

impl Serialize for NodeArray {
    type From = Vec<NodeIndex>;

    fn to_ast(from: Self::From, ast: &mut Ast) -> DataIndex {
        let i = ast.alloc_data(from.len() as u32);
        if from.is_empty() {
            ast.alloc_data(0);
            return i;
        }
        for idx in from {
            ast.alloc_data(idx.raw());
        }
        i
    }
}

impl Serialize for PipelineEnd {
    type From = PipelineEnd;

    fn to_ast(from: Self::From, ast: &mut Ast) -> DataIndex {
        let kind = ast.alloc_data(from.kind as u32);
        ast.alloc_data(from.file.raw());
        kind
    }
}

impl Deserialize for PipelineEnd {
    fn from_ast(ast: &Ast, index: DataIndex) -> Self {
        let kind = match ast.extra_data[index.raw() as usize] {
            0 => PipelineEndKind::Write,
            1 => PipelineEndKind::Append,
            _ => unreachable!(),
        };
        let file = NodeIndex(ast.extra_data[index.raw() as usize + 1]);
        Self { kind, file }
    }
}

#[derive(Debug)]
pub struct NodeArrayIter<'ast> {
    ast: &'ast Ast,
    current: DataIndex,
    end: u32,
}

impl<'ast> Iterator for NodeArrayIter<'ast> {
    type Item = NodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 == self.end {
            return None;
        }

        let idx = self
            .ast
            .extra_data
            .get(self.current.0 as usize)
            .copied()
            .map(NodeIndex)
            .expect("all extra data indices should be valid by construction!");
        self.current = DataIndex(self.current.0 + 1);
        Some(idx)
    }
}

impl FusedIterator for NodeArrayIter<'_> {}

#[derive(Debug, Clone)]
pub(super) struct NodeInfo {
    pub kind: NodeInfoKind,
    pub offset: u32,
    pub p1: u32,
    pub p2: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum NodeInfoKind {
    /// `a; b; c ...`. `[a, b, c, ..] = NodeArray<Stmt>[p1]`
    Root,

    /// `a b c ... (%)d?`. `[a, b, c, ...] = NodeArray<Expr>[p1], `d = Bool[p2]`
    Command,
    /// `a | b | c ...`. `[a, b, c, ..] = NodeArray<Expr>[p1]`
    Pipeline,
    /// `a | b | c ... > d`. `[a, b, c, ..] = NodeArray<Expr>[p1], d = PipelineEnd[p2]`
    PipelineWithEnd,
    /// `a = p2`. `a = String[p1]`
    Assignment,
    /// `export a = p2`. `a = String[p1]`
    Export,

    /// `a`. `a = String[p1]`
    String,
    /// `a`. `a = String[p1]`
    Ident,
    /// `a`. `a = Bool[p1]`
    Boolean,
    /// `?`
    LastStatus,
    /// `~`
    HomeDir,
    /// `$a`. `a = String[p1]`
    EnvVar,

    /// `a`. a = p1 | p2
    Number,
    /// `p1 + p2`
    Add,
    /// `p1 - p2`
    Sub,
    /// `p1 * p2`
    Mul,
    /// `p1 / p2`
    Div,
    /// `p1 < p2`
    Lt,
    /// `p1 > p2`
    Gt,
    /// `p1 <= p2`
    Le,
    /// `p1 >= p2`
    Ge,
    /// `p1 == p2`
    Eq,
    /// `p1 != p2`
    Ne,

    /// `-p1`
    Neg,
    /// `!p1`
    Bang,
    /// `+p1`
    Sign,
}

impl Node {
    pub fn new(kind: NodeKind, offset: usize) -> Self {
        Self { kind, offset }
    }

    pub fn kind(&self) -> &NodeKind {
        &self.kind
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_node(
            f: &mut fmt::Formatter<'_>,
            ast: &Ast,
            node: NodeIndex,
            mut indent: usize,
        ) -> fmt::Result {
            let node = ast.get(node).kind;
            writeln!(f, "{:indent$}{node}", "")?;
            indent += 2;
            match node {
                NodeKind::Command(Command { exprs: nodes, .. })
                | NodeKind::Root(Root { stmts: nodes })
                | NodeKind::Pipeline(Pipeline {
                    cmds: nodes,
                    end: None,
                }) => {
                    for node in nodes.iter(ast) {
                        write_node(f, ast, node, indent)?;
                    }
                }
                NodeKind::Pipeline(Pipeline {
                    cmds,
                    end: Some(end),
                }) => {
                    for node in cmds.iter(ast) {
                        write_node(f, ast, node, indent)?;
                    }
                    writeln!(f, "{:indent$}{}", "", end.kind)?;
                    write_node(f, ast, end.file, indent + 2)?;
                }
                NodeKind::Assignment(Assignment { name, value })
                | NodeKind::Export(Export { name, value }) => {
                    indent += 2;
                    writeln!(f, "{:indent$}{name}", "")?;
                    write_node(f, ast, value, indent)?;
                }
                NodeKind::Binop(Binop { lhs, rhs, op: _ }) => {
                    write_node(f, ast, lhs, indent)?;
                    write_node(f, ast, rhs, indent)?;
                }
                NodeKind::Unop(Unop { expr, op: _ }) => {
                    write_node(f, ast, expr, indent)?;
                }
                _ => {}
            }

            Ok(())
        }

        let mut indent = 0;
        let root = self.root();
        writeln!(f, "{root}")?;
        indent += 2;
        for node in root.stmts.iter(self) {
            write_node(f, self, node, indent)?;
        }

        Ok(())
    }
}

impl fmt::Display for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! fmt {
            ($($kind:ident),*) => {
                match self {
                    $(
                        NodeKind::$kind(o) => o.fmt(f),
                     )*
                }
            };
        }
        fmt!(
            Root, Command, Number, Binop, String, Ident, Unop, Pipeline, Boolean, LastStatus,
            EnvVar, Assignment, HomeDir, Export
        )
    }
}

impl fmt::Display for Root {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ROOT")
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "COMMAND")?;
        if self.merge_stderr {
            write!(f, " (merge_stderr=true)")?;
        }

        Ok(())
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BINOP `{}`", self.op)
    }
}

impl fmt::Display for Unop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UNOP `{}`", self.op)
    }
}

impl fmt::Display for BinopKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinopKind::Add => write!(f, "+"),
            BinopKind::Sub => write!(f, "-"),
            BinopKind::Div => write!(f, "/"),
            BinopKind::Mul => write!(f, "*"),
            BinopKind::Lt => write!(f, "<"),
            BinopKind::Gt => write!(f, ">"),
            BinopKind::Le => write!(f, "<="),
            BinopKind::Ge => write!(f, ">="),
            BinopKind::Eq => write!(f, "=="),
            BinopKind::Ne => write!(f, "!="),
        }
    }
}

impl fmt::Display for UnopKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnopKind::Neg => write!(f, "-"),
            UnopKind::Bang => write!(f, "!"),
            UnopKind::Sign => write!(f, "+"),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NUMBER `{}`", self.0)
    }
}

impl fmt::Display for AstString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "STRING `{}`", self.0)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IDENT `{}`", self.0 .0)
    }
}

impl fmt::Display for Pipeline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PIPELINE")
    }
}

impl fmt::Display for PipelineEndKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PipelineEndKind::Write => write!(f, "WRITE"),
            PipelineEndKind::Append => write!(f, "APPEND"),
        }
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "BOOLEAN `{}`",
            match self {
                Boolean::True => true,
                Boolean::False => false,
            }
        )
    }
}

impl fmt::Display for LastStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LAST_STATUS")
    }
}

impl fmt::Display for HomeDir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HOME_DIR")
    }
}

impl fmt::Display for EnvVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ENV_VAR `{}`", self.0 .0 .0)
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ASSIGNMENT")
    }
}

impl fmt::Display for Export {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EXPORT")
    }
}
