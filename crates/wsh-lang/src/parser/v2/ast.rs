use core::fmt;
use std::{iter::FusedIterator, marker::PhantomData, ops::Deref};

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
    If(If),
    While(While),
    Break(Break),
    Continue(Continue),

    // Expressions
    String(StringHandle),
    Number(Number),
    Boolean(Boolean),
    Ident(IdentHandle),
    EnvVar(EnvVarHandle),
    Binop(Binop),
    Unop(Unop),
    LastStatus(LastStatus),
    HomeDir(HomeDir),
    Capture(Capture),
}

#[derive(Debug)]
pub struct Root {
    pub stmts: DataHandle<DataArray<NodeHandle>>,
}

#[derive(Debug)]
pub struct Command {
    pub exprs: DataHandle<DataArray<NodeHandle>>,
    pub merge_stderr: bool,
}

#[derive(Debug)]
pub struct Pipeline {
    pub cmds: DataHandle<DataArray<NodeHandle>>,
    pub end: Option<PipelineEnd>,
    pub env: DataHandle<DataArray<EnvSet>>,
}

#[derive(Debug)]
pub struct PipelineEnd {
    pub kind: PipelineEndKind,
    pub file: NodeHandle,
}

#[derive(Debug)]
pub struct Assignment {
    pub name: IdentHandle,
    pub value: NodeHandle,
}

#[derive(Debug)]
pub struct EnvSet {
    pub name: IdentHandle,
    pub value: NodeHandle,
}

#[derive(Debug)]
pub struct If {
    pub condition: NodeHandle,
    pub body: DataHandle<DataArray<NodeHandle>>,
    pub else_body: Option<DataHandle<DataArray<NodeHandle>>>,
}

#[derive(Debug)]
pub struct While {
    pub condition: NodeHandle,
    pub body: DataHandle<DataArray<NodeHandle>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PipelineEndKind {
    Write,
    Append,
}

#[derive(Debug)]
pub struct Binop {
    pub lhs: NodeHandle,
    pub rhs: NodeHandle,
    pub op: BinopKind,
}

#[derive(Debug)]
pub struct Capture {
    pub pipeline: NodeHandle,
}

#[derive(Debug)]
pub struct Export {
    pub name: IdentHandle,
    pub value: NodeHandle,
}

#[derive(Debug)]
pub struct Unop {
    pub expr: NodeHandle,
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

#[derive(Debug)]
pub struct Break;

#[derive(Debug)]
pub struct Continue;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Number(f64);

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
pub struct DataArray<T>
where
    T: ExactSizeDeserialize,
{
    ptr: DataHandle<T>,
    len: u32,
    _phantom: PhantomData<T>,
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

    /// Get the root node of the AST.
    pub fn root(&self) -> Root {
        let NodeKind::Root(root) = self.get_node(NodeHandle(self.nodes.len() as u32 - 1)).kind
        else {
            panic!("root should always be the last node!");
        };

        root
    }

    /// Add a node to the AST. This will return a handle to the node.
    ///
    /// # Safety
    /// This function blindly accpets any NodeInfo. NodeInfo contains context-sensitive information
    /// about the type being added into the AST, and varies per node. See [`NodeInfoKind`] for
    /// descriptions of each type's serialization into the AST. Failure to properly serialize
    /// breaks the invariants of this type and the corresponding node type.
    pub(super) unsafe fn add(&mut self, node: NodeInfo) -> NodeHandle {
        self.nodes.push(node);
        NodeHandle(self.nodes.len() as u32 - 1)
    }

    /// Serialize a given object. This is useful for adding nodes to the AST.
    pub(super) fn serialize<T: Serialize>(&mut self, data: T) -> DataHandle<T> {
        data.serialize(self)
    }

    /// Allocate a string, returning a handle to it.
    pub(super) fn alloc_string(&mut self, s: &str) -> StringHandle {
        self.strings.push(AstString::new(s));
        StringHandle(self.strings.len() as u32 - 1)
    }

    /// Allocate an ident, returning a handle to it.
    pub(super) fn alloc_ident(&mut self, s: &str) -> IdentHandle {
        IdentHandle(self.alloc_string(s).raw())
    }

    /// Allocate an env var, returning a handle to it.
    pub(super) fn alloc_env(&mut self, s: &str) -> EnvVarHandle {
        EnvVarHandle(self.alloc_string(s).raw())
    }

    fn get_node(&self, node: NodeHandle) -> Node {
        let NodeInfo {
            kind,
            offset,
            p1,
            p2,
        } = self.nodes[node.0 as usize];

        macro_rules! binop {
            ($kind:ident) => {{
                NodeKind::Binop(Binop {
                    lhs: NodeHandle(p1),
                    rhs: NodeHandle(p2),
                    op: BinopKind::$kind,
                })
            }};
        }

        let node_kind = match kind {
            NodeInfoKind::Root => NodeKind::Root(Root {
                stmts: DataHandle::new(p1),
            }),
            NodeInfoKind::Command => NodeKind::Command(Command {
                exprs: DataHandle::new(p1),
                merge_stderr: p2 == 1,
            }),
            NodeInfoKind::Pipeline => NodeKind::Pipeline(Pipeline {
                cmds: DataHandle::new(p1),
                env: DataHandle::new(p2),
                end: None,
            }),
            NodeInfoKind::PipelineWithEnd => {
                let (end, env) =
                    DataHandle::<(PipelineEnd, DataHandle<DataArray<EnvSet>>)>::new(p2).deref(self);
                NodeKind::Pipeline(Pipeline {
                    cmds: DataHandle::new(p1),
                    end: Some(end),
                    env,
                })
            }
            NodeInfoKind::Assignment => NodeKind::Assignment(Assignment {
                name: IdentHandle(p1),
                value: NodeHandle(p2),
            }),
            NodeInfoKind::Export => NodeKind::Export(Export {
                name: IdentHandle(p1),
                value: NodeHandle(p2),
            }),
            NodeInfoKind::If => NodeKind::If(If {
                condition: NodeHandle(p1),
                body: DataHandle::new(p2),
                else_body: None,
            }),
            NodeInfoKind::While => NodeKind::While(While {
                condition: NodeHandle(p1),
                body: DataHandle::new(p2),
            }),
            NodeInfoKind::IfElse => {
                let (body, else_body) = DataHandle::<(
                    DataHandle<DataArray<NodeHandle>>,
                    DataHandle<DataArray<NodeHandle>>,
                )>::new(p2)
                .deref(self);
                NodeKind::If(If {
                    condition: NodeHandle(p1),
                    body,
                    else_body: Some(else_body),
                })
            }
            NodeInfoKind::Break => NodeKind::Break(Break),
            NodeInfoKind::Continue => NodeKind::Continue(Continue),

            NodeInfoKind::Number => {
                let raw = (p2 as u64) | (p1 as u64) << 32;
                NodeKind::Number(Number(f64::from_bits(raw)))
            }
            NodeInfoKind::String => NodeKind::String(StringHandle(p1)),
            NodeInfoKind::EnvVar => NodeKind::EnvVar(EnvVarHandle(p1)),
            NodeInfoKind::Ident => NodeKind::Ident(IdentHandle(p1)),
            NodeInfoKind::Boolean => NodeKind::Boolean(if p1 == 0 {
                Boolean::False
            } else {
                Boolean::True
            }),
            NodeInfoKind::LastStatus => NodeKind::LastStatus(LastStatus),
            NodeInfoKind::HomeDir => NodeKind::HomeDir(HomeDir),
            NodeInfoKind::Capture => NodeKind::Capture(Capture {
                pipeline: NodeHandle(p1),
            }),

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
                expr: NodeHandle(p1),
                op: UnopKind::Neg,
            }),
            NodeInfoKind::Bang => NodeKind::Unop(Unop {
                expr: NodeHandle(p1),
                op: UnopKind::Bang,
            }),
            NodeInfoKind::Sign => NodeKind::Unop(Unop {
                expr: NodeHandle(p1),
                op: UnopKind::Sign,
            }),
        };

        Node {
            kind: node_kind,
            offset: offset as usize,
        }
    }

    fn alloc_data(&mut self, data: u32) -> u32 {
        self.extra_data.push(data);
        self.extra_data.len() as u32 - 1
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
}

pub(super) trait Serialize: Sized {
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self>;
}

pub trait Deserialize: Sized {
    fn deserialize(ast: &Ast, index: DataHandle<Self>) -> Self;
}

impl<T: Serialize> Serialize for &T {
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self> {
        DataHandle::new((*self).serialize(ast).raw())
    }
}

/// AST types that have a statically known size in the `extra_data` store should implement this,
/// and give their size in u32s.
pub trait ExactSizeDeserialize: Deserialize {
    fn size() -> usize;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StringHandle(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct IdentHandle(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EnvVarHandle(u32);

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct DataHandle<T>(u32, PhantomData<T>);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct NodeHandle(u32);

impl<T> Clone for DataHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for DataHandle<T> {}

impl StringHandle {
    pub(super) fn raw(self) -> u32 {
        self.0
    }
    pub fn deref(self, ast: &Ast) -> &AstString {
        &ast.strings[self.raw() as usize]
    }
}

impl<T> DataHandle<T> {
    fn new(raw: u32) -> Self {
        Self(raw, PhantomData)
    }
    pub(super) fn raw(self) -> u32 {
        self.0
    }
    pub fn deref(self, ast: &Ast) -> T
    where
        T: Deserialize,
    {
        T::deserialize(ast, self)
    }
}

impl IdentHandle {
    pub(super) fn raw(self) -> u32 {
        self.0
    }
    pub fn deref(self, ast: &Ast) -> &Ident {
        let s = &ast.strings[self.raw() as usize];
        // SAFETY: the internal structure of an Ident is the same as an AstString
        unsafe { std::mem::transmute(s) }
    }
}

impl EnvVarHandle {
    pub(super) fn raw(self) -> u32 {
        self.0
    }
    pub fn deref(self, ast: &Ast) -> &EnvVar {
        let s = &ast.strings[self.raw() as usize];
        // SAFETY: the internal structure of an EnvVar is the same as an AstString
        unsafe { std::mem::transmute(s) }
    }
}

impl NodeHandle {
    pub(super) fn raw(self) -> u32 {
        self.0
    }
    pub fn deref(self, ast: &Ast) -> Node {
        ast.get_node(self)
    }
}

impl<T: ExactSizeDeserialize> Deserialize for DataArray<T> {
    fn deserialize(ast: &Ast, index: DataHandle<DataArray<T>>) -> Self {
        let len = ast.extra_data[index.0 as usize];
        Self {
            ptr: DataHandle::new(index.raw() + 1),
            len,
            _phantom: PhantomData,
        }
    }
}

impl<T: ExactSizeDeserialize> DataArray<T> {
    pub fn iter<'ast>(&self, ast: &'ast Ast) -> DataArrayIter<'ast, T> {
        DataArrayIter {
            ast,
            current: self.ptr,
            end: self.ptr.0 + self.len * T::size() as u32,
            _phantom: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(super) fn from_iter<I, U>(ast: &mut Ast, iter: I) -> DataHandle<Self>
    where
        I: IntoIterator<Item = T, IntoIter = U>,
        U: ExactSizeIterator<Item = T>,
        T: Serialize,
    {
        let iter = iter.into_iter();
        let i = ast.alloc_data(iter.len() as u32);
        if iter.len() == 0 {
            ast.alloc_data(0);
            return DataHandle::new(i);
        }
        for t in iter {
            ast.serialize(t);
        }
        DataHandle::new(i)
    }
}

impl<T, U> Deserialize for (T, U)
where
    T: ExactSizeDeserialize,
    U: ExactSizeDeserialize,
{
    fn deserialize(ast: &Ast, index: DataHandle<Self>) -> Self {
        let idx = DataHandle::new(index.raw());
        let t = T::deserialize(ast, idx);
        let idx = DataHandle::new(index.raw() + T::size() as u32);
        let u = U::deserialize(ast, idx);
        (t, u)
    }
}

impl<T, U> Serialize for (T, U)
where
    T: Serialize,
    U: Serialize,
{
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self> {
        let i = self.0.serialize(ast);
        self.1.serialize(ast);
        DataHandle::new(i.raw())
    }
}

impl Serialize for PipelineEnd {
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self> {
        let kind = self.kind as u32;
        let i = ast.alloc_data(kind);
        ast.alloc_data(self.file.raw());
        DataHandle::new(i)
    }
}

impl Deserialize for PipelineEnd {
    fn deserialize(ast: &Ast, index: DataHandle<Self>) -> Self {
        let data = ast.extra_data[index.raw() as usize];
        let kind = match data {
            0 => PipelineEndKind::Write,
            1 => PipelineEndKind::Append,
            _ => panic!("invalid deserialization"),
        };
        let handle = ast.extra_data[index.raw() as usize + 1];
        Self {
            kind,
            file: NodeHandle(handle),
        }
    }
}

impl ExactSizeDeserialize for PipelineEnd {
    fn size() -> usize {
        2
    }
}

impl<T> Deserialize for DataHandle<T> {
    fn deserialize(ast: &Ast, index: DataHandle<Self>) -> Self {
        Self::new(ast.extra_data[index.raw() as usize])
    }
}

impl<T> Serialize for DataHandle<T> {
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self> {
        DataHandle::new(ast.alloc_data(self.raw()))
    }
}

impl<T> ExactSizeDeserialize for DataHandle<T> {
    fn size() -> usize {
        1
    }
}

impl Deserialize for NodeHandle {
    fn deserialize(ast: &Ast, index: DataHandle<Self>) -> Self {
        Self(ast.extra_data[index.raw() as usize])
    }
}

impl ExactSizeDeserialize for NodeHandle {
    fn size() -> usize {
        1
    }
}

impl Serialize for NodeHandle {
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self> {
        DataHandle::new(ast.alloc_data(self.raw()))
    }
}

impl Serialize for EnvSet {
    fn serialize(&self, ast: &mut Ast) -> DataHandle<Self> {
        let i = ast.alloc_data(self.name.raw());
        ast.alloc_data(self.value.raw());
        DataHandle::new(i)
    }
}

impl Deserialize for EnvSet {
    fn deserialize(ast: &Ast, index: DataHandle<Self>) -> Self {
        let name = ast.extra_data[index.raw() as usize];
        let value = ast.extra_data[index.raw() as usize + 1];
        Self {
            name: IdentHandle(name),
            value: NodeHandle(value),
        }
    }
}

impl ExactSizeDeserialize for EnvSet {
    fn size() -> usize {
        2
    }
}

#[derive(Debug)]
pub struct DataArrayIter<'ast, T>
where
    T: ExactSizeDeserialize,
{
    ast: &'ast Ast,
    current: DataHandle<T>,
    end: u32,
    _phantom: PhantomData<T>,
}

impl<'ast, T: ExactSizeDeserialize> Iterator for DataArrayIter<'ast, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 == self.end {
            return None;
        }

        let t = T::deserialize(self.ast, self.current);
        self.current = DataHandle::new(self.current.0 + T::size() as u32);
        Some(t)
    }
}

impl<T: ExactSizeDeserialize> FusedIterator for DataArrayIter<'_, T> {}

#[derive(Debug, Clone)]
pub(super) struct NodeInfo {
    pub kind: NodeInfoKind,
    pub offset: u32,
    pub p1: u32,
    pub p2: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum NodeInfoKind {
    /// `a; b; c ...`. `[a, b, c, ..] = DataArray<Stmt>[p1]`
    Root,

    /// `a b c ... (%)d?`. `[a, b, c, ...] = DataArray<Expr>[p1], `d = Bool[p2]`
    Command,
    /// `a | b | c ...`. `[a, b, c, ..] = DataArray<Expr>[p1]`
    Pipeline,
    /// `a | b | c ... > d`. `[a, b, c, ..] = DataArray<Expr>[p1], d = PipelineEnd[p2]`
    PipelineWithEnd,
    /// `a = p2`. `a = String[p1]`
    Assignment,
    /// `export a = p2`. `a = String[p1]`
    Export,
    /// `if p1 then p2 end`
    If,
    /// `if p1 then a else b end`. `(a, b) = Tuple[DataArray<Stmt>; 2]`
    IfElse,
    /// `while a do b end`
    While,
    /// `break`
    Break,
    /// `continue`
    Continue,

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
    /// `\`p1\``
    Capture,

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

/// A special formatter for AST nodes. This is just a wrapper around [`std::fmt::Formatter`], but
/// has `indent` and `unindent` methods that allow for visual nesting.
struct AstFormatter<'a, 'b> {
    fmt: &'a mut fmt::Formatter<'b>,
    indent: usize,
}

impl AstFormatter<'_, '_> {
    pub fn indent(&mut self) {
        self.indent += 2;
    }

    pub fn unindent(&mut self) {
        self.indent -= 2;
    }

    pub fn writeln<D>(&mut self, d: D) -> fmt::Result
    where
        D: fmt::Display,
    {
        let indent = self.indent;
        writeln!(self.fmt, "{:indent$}{d}", "")
    }
}

/// The main trait used to display nodes of the AST. Every node should implement this.
trait AstDisplay {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result;
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ast_fmt = AstFormatter { fmt: f, indent: 0 };
        let root = self.root();
        root.ast_fmt(self, &mut ast_fmt)
    }
}

impl AstDisplay for Node {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        macro_rules! fmt {
            ($($kind:ident),*) => {
                match &self.kind {
                    $(
                        NodeKind::$kind(o) => o.ast_fmt(ast, f),
                     )*
                }
            };
        }
        fmt!(
            Root, Command, Number, Binop, String, Ident, Unop, Pipeline, Boolean, LastStatus,
            EnvVar, Assignment, HomeDir, Export, If, While, Break, Continue, Capture
        )
    }
}

impl AstDisplay for Root {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("ROOT")?;
        f.indent();
        for stmt in self.stmts.deref(ast).iter(ast) {
            let node = ast.get_node(stmt);
            node.ast_fmt(ast, f)?;
        }
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for Command {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        if self.merge_stderr {
            f.writeln("COMMAND (merge_stderr=true)")?;
        } else {
            f.writeln("COMMAND")?;
        }
        f.indent();
        for expr in self.exprs.deref(ast).iter(ast) {
            let node = ast.get_node(expr);
            node.ast_fmt(ast, f)?;
        }
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for Binop {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!("BINOP `{}`", self.op))?;
        f.indent();
        ast.get_node(self.lhs).ast_fmt(ast, f)?;
        ast.get_node(self.rhs).ast_fmt(ast, f)?;
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for Unop {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!("UNOP `{}`", self.op))?;
        f.indent();
        ast.get_node(self.expr).ast_fmt(ast, f)?;
        f.unindent();
        Ok(())
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

impl AstDisplay for Number {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!("NUMBER `{}`", self.0))
    }
}

impl AstDisplay for AstString {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!("STRING `{}`", self.0))
    }
}

impl AstDisplay for Ident {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!("IDENT `{}`", &*self.0))
    }
}

impl AstDisplay for Pipeline {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("PIPELINE")?;
        f.indent();
        for cmd in self.cmds.deref(ast).iter(ast) {
            ast.get_node(cmd).ast_fmt(ast, f)?;
        }
        if let Some(end) = &self.end {
            f.writeln(match end.kind {
                PipelineEndKind::Write => "WRITE",
                PipelineEndKind::Append => "APPEND",
            })?;
            f.indent();
            ast.get_node(end.file).ast_fmt(ast, f)?;
            f.unindent();
        }
        if !self.env.deref(ast).is_empty() {
            f.writeln("ENV")?;
            f.indent();
            for env_set in self.env.deref(ast).iter(ast) {
                f.writeln("ENV_SET")?;
                f.indent();
                env_set.name.deref(ast).ast_fmt(ast, f)?;
                env_set.value.deref(ast).ast_fmt(ast, f)?;
                f.unindent();
            }
            f.unindent();
        }
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for Boolean {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!(
            "BOOLEAN `{}`",
            match self {
                Boolean::True => "true",
                Boolean::False => "false",
            }
        ))
    }
}

impl AstDisplay for LastStatus {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("LAST_STATUS")
    }
}

impl AstDisplay for HomeDir {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("HOME_DIR")
    }
}

impl AstDisplay for EnvVar {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln(format_args!("ENV_VAR `{}`", &*self.0))
    }
}

impl AstDisplay for Assignment {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("ASSIGNMENT")?;
        f.indent();
        self.name.deref(ast).ast_fmt(ast, f)?;
        ast.get_node(self.value).ast_fmt(ast, f)?;
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for Export {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("EXPORT")?;
        f.indent();
        self.name.deref(ast).ast_fmt(ast, f)?;
        self.value.deref(ast).ast_fmt(ast, f)?;
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for StringHandle {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        (*self).deref(ast).ast_fmt(ast, f)
    }
}

impl AstDisplay for IdentHandle {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        (*self).deref(ast).ast_fmt(ast, f)
    }
}

impl AstDisplay for EnvVarHandle {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        (*self).deref(ast).ast_fmt(ast, f)
    }
}

impl AstDisplay for If {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("IF")?;
        f.indent();
        self.condition.deref(ast).ast_fmt(ast, f)?;
        f.writeln("BODY")?;
        f.indent();
        for stmt in self.body.deref(ast).iter(ast) {
            stmt.deref(ast).ast_fmt(ast, f)?;
        }
        f.unindent();
        if let Some(else_body) = self.else_body {
            f.writeln("ELSE")?;
            f.indent();
            for stmt in else_body.deref(ast).iter(ast) {
                stmt.deref(ast).ast_fmt(ast, f)?;
            }
            f.unindent();
        }
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for While {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("WHILE")?;
        f.indent();
        self.condition.deref(ast).ast_fmt(ast, f)?;
        f.writeln("BODY")?;
        f.indent();
        for stmt in self.body.deref(ast).iter(ast) {
            stmt.deref(ast).ast_fmt(ast, f)?;
        }
        f.unindent();
        f.unindent();
        Ok(())
    }
}

impl AstDisplay for Break {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("BREAK")
    }
}

impl AstDisplay for Continue {
    fn ast_fmt(&self, _ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("CONTINUE")
    }
}

impl AstDisplay for Capture {
    fn ast_fmt(&self, ast: &Ast, f: &mut AstFormatter) -> fmt::Result {
        f.writeln("CAPTURE")?;
        f.indent();
        self.pipeline.deref(ast).ast_fmt(ast, f)?;
        f.unindent();
        Ok(())
    }
}
