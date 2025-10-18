#![feature(let_chains)]
#![feature(trait_alias)]

pub mod ast;
mod ffi;
pub mod ffi_ext;
mod syntax;
mod token;

use std::{
    collections::{HashMap, hash_map::Entry},
    ffi::c_char,
    fmt, hash, iter,
    marker::PhantomData,
    ops::{self, Not},
    pin::Pin,
};

use cxx::{SharedPtr, UniquePtr};
pub use ffi::CxxSV;
use itertools::{Either, Itertools};
use smol_str::SmolStr;
pub use syntax::{
    SyntaxKind, TokenKind, TriviaKind,
    cursor::SyntaxCursor,
    iter::{
        SyntaxAncestors, SyntaxChildren, SyntaxElemPreorder, SyntaxIdxChildren, SyntaxNodePreorder,
        WalkEvent,
    },
};

pub struct SVInt {
    _ptr: UniquePtr<ffi::SVInt>,
}

pub struct SVLogic {
    _ptr: UniquePtr<ffi::SVLogic>,
}

pub struct SourceLocation {
    _ptr: UniquePtr<ffi::SourceLocation>,
}

pub struct SourceRange {
    _ptr: UniquePtr<ffi::SourceRange>,
}

#[derive(Clone, Copy)]
pub struct SyntaxNode<'a> {
    _ptr: Pin<&'a ffi::SyntaxNode>,
}

#[derive(Clone, Copy)]
pub struct SyntaxToken<'a> {
    _ptr: Pin<&'a ffi::SyntaxToken>,
}

#[derive(Clone)]
pub struct SyntaxTree {
    _ptr: SharedPtr<ffi::SyntaxTree>,
}

#[derive(Clone, Copy)]
pub struct SyntaxTrivia<'a> {
    _ptr: Pin<&'a ffi::SyntaxTrivia>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TimeUnit {
    Seconds,
    Milliseconds,
    Microseconds,
    Nanoseconds,
    Picoseconds,
    Femtoseconds,
}

impl fmt::Display for TimeUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TimeUnit::Seconds => write!(f, "s"),
            TimeUnit::Milliseconds => write!(f, "ms"),
            TimeUnit::Microseconds => write!(f, "us"),
            TimeUnit::Nanoseconds => write!(f, "ns"),
            TimeUnit::Picoseconds => write!(f, "ps"),
            TimeUnit::Femtoseconds => write!(f, "fs"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralBase {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bit {
    L,
    H,
    X,
    Z,
}

impl fmt::Display for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bit::L => write!(f, "0"),
            Bit::H => write!(f, "1"),
            Bit::X => write!(f, "x"),
            Bit::Z => write!(f, "z"),
        }
    }
}

impl SourceLocation {
    const NO_LOCATION: usize = (1usize << 36) - 1;

    #[inline]
    pub fn from_unique_ptr(_ptr: UniquePtr<ffi::SourceLocation>) -> Option<Self> {
        _ptr.is_null().not().then(|| SourceLocation { _ptr })
    }

    #[inline]
    pub fn offset(&self) -> Option<usize> {
        let offset = self._ptr.offset();
        (offset == Self::NO_LOCATION).not().then_some(offset)
    }

    #[inline]
    pub fn buffer_id(&self) -> Option<source_manager::BufferId> {
        self._ptr
            .as_ref()
            .map(|loc| source_manager::BufferId(ffi::SourceLocation_buffer(loc)))
            .filter(|id| id.is_valid())
    }
}

impl fmt::Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceLocation").field("offset", &self.offset()).finish()
    }
}

impl PartialEq for SourceLocation {
    fn eq(&self, other: &Self) -> bool {
        self.offset() == other.offset()
    }
}

impl Eq for SourceLocation {}

impl SourceRange {
    #[inline]
    fn from_unique_ptr(_ptr: UniquePtr<ffi::SourceRange>) -> Option<Self> {
        _ptr.is_null().not().then(|| SourceRange { _ptr })
    }

    #[inline]
    pub fn start(&self) -> usize {
        self._ptr.start()
    }

    #[inline]
    pub fn end(&self) -> usize {
        self._ptr.end()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start() >= self.end()
    }
}

impl fmt::Debug for SourceRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start(), self.end())
    }
}

impl PartialEq for SourceRange {
    fn eq(&self, other: &Self) -> bool {
        self.start() == other.start() && self.end() == other.end()
    }
}

impl Eq for SourceRange {}

impl Into<ops::Range<usize>> for SourceRange {
    fn into(self) -> ops::Range<usize> {
        let start = self.start();
        let end = self.end();
        start..end
    }
}

pub struct LookupLocation {
    _ptr: UniquePtr<ffi::LookupLocation>,
}

impl LookupLocation {
    #[inline]
    pub fn max() -> Self {
        LookupLocation { _ptr: ffi::LookupLocation::max() }
    }

    #[inline]
    pub fn before(symbol: SymbolRef<'_>) -> Self {
        LookupLocation { _ptr: ffi::LookupLocation::before(symbol.ptr) }
    }

    #[inline]
    pub fn after(symbol: SymbolRef<'_>) -> Self {
        LookupLocation { _ptr: ffi::LookupLocation::after(symbol.ptr) }
    }

    #[inline]
    pub fn as_ref(&self) -> Option<&ffi::LookupLocation> {
        self._ptr.as_ref()
    }
}

#[derive(Clone, Copy)]
pub struct SymbolRef<'a> {
    ptr: &'a ffi::Symbol,
}

impl<'a> SymbolRef<'a> {
    #[inline]
    pub fn kind(self) -> u16 {
        self.ptr.kind()
    }

    #[inline]
    pub fn next_sibling(self) -> Option<SymbolRef<'a>> {
        unsafe { self.ptr.getNextSibling().as_ref() }.map(SymbolRef::new)
    }

    #[inline]
    pub fn parent_scope(self) -> Option<ScopeRef<'a>> {
        unsafe { self.ptr.getParentScope().as_ref() }.map(ScopeRef::new)
    }

    #[inline]
    pub fn raw(self) -> &'a ffi::Symbol {
        self.ptr
    }

    #[inline]
    fn new(ptr: &'a ffi::Symbol) -> Self {
        SymbolRef { ptr }
    }
}

#[derive(Clone, Copy)]
pub struct ScopeRef<'a> {
    ptr: &'a ffi::Scope,
}

impl<'a> ScopeRef<'a> {
    #[inline]
    pub fn as_symbol(self) -> SymbolRef<'a> {
        let ptr = unsafe { ffi::Scope_asSymbol(self.ptr).as_ref().expect("scope symbol") };
        SymbolRef::new(ptr)
    }

    #[inline]
    pub fn members(self) -> ScopeMemberIter<'a> {
        ScopeMemberIter { current: self.ptr.getFirstMember(), _marker: PhantomData }
    }

    #[inline]
    pub fn find_member(self, name: &str) -> Option<SymbolRef<'a>> {
        unsafe { self.ptr.find(CxxSV::new(name)).as_ref() }.map(SymbolRef::new)
    }

    #[inline]
    pub fn lookup_name(
        self,
        name: &str,
        location: Option<&LookupLocation>,
    ) -> Option<SymbolRef<'a>> {
        let mut temp_holder: Option<LookupLocation> = None;
        let loc_ref = match location {
            Some(loc) => loc.as_ref(),
            None => {
                temp_holder = Some(LookupLocation::max());
                temp_holder.as_ref().and_then(|loc| loc.as_ref())
            }
        }?;
        unsafe { self.ptr.lookupName(CxxSV::new(name), loc_ref).as_ref() }.map(SymbolRef::new)
    }

    #[inline]
    pub fn raw(self) -> &'a ffi::Scope {
        self.ptr
    }

    #[inline]
    fn new(ptr: &'a ffi::Scope) -> Self {
        ScopeRef { ptr }
    }
}

pub struct ScopeMemberIter<'a> {
    current: *const ffi::Symbol,
    _marker: PhantomData<&'a ffi::Scope>,
}

impl<'a> Iterator for ScopeMemberIter<'a> {
    type Item = SymbolRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        if current.is_null() {
            return None;
        }
        unsafe {
            let sym = &*current;
            self.current = sym.getNextSibling();
            Some(SymbolRef::new(sym))
        }
    }
}

#[derive(Clone, Copy)]
pub struct RootSymbol<'a> {
    ptr: &'a ffi::RootSymbol,
}

impl<'a> RootSymbol<'a> {
    #[inline]
    pub fn as_scope(self) -> ScopeRef<'a> {
        let scope = unsafe { ffi::RootSymbol_asScope(self.ptr).as_ref().expect("root scope") };
        ScopeRef::new(scope)
    }

    #[inline]
    pub fn as_symbol(self) -> SymbolRef<'a> {
        let sym = unsafe { ffi::RootSymbol_asSymbol(self.ptr).as_ref().expect("root symbol") };
        SymbolRef::new(sym)
    }

    #[inline]
    fn new(ptr: &'a ffi::RootSymbol) -> Self {
        RootSymbol { ptr }
    }
}

pub struct SourceManagerRef<'a> {
    ptr: *mut ffi::SourceManager,
    _marker: PhantomData<&'a mut ffi::SourceManager>,
}

impl<'a> SourceManagerRef<'a> {
    #[inline]
    fn new(ptr: *mut ffi::SourceManager) -> Option<Self> {
        (!ptr.is_null()).then_some(SourceManagerRef { ptr, _marker: PhantomData })
    }

    #[inline]
    fn as_pin(&mut self) -> Pin<&mut ffi::SourceManager> {
        unsafe { Pin::new_unchecked(&mut *self.ptr) }
    }

    #[inline]
    fn as_ref(&self) -> &ffi::SourceManager {
        unsafe { &*self.ptr }
    }

    #[inline]
    pub fn assign_text(&mut self, text: &str) -> Option<source_manager::BufferId> {
        let id = ffi::SourceManager_assignText(self.as_pin(), CxxSV::new(text));
        (id != 0).then_some(source_manager::BufferId(id))
    }

    #[inline]
    pub fn assign_text_with_path(
        &mut self,
        path: &str,
        text: &str,
    ) -> Option<source_manager::BufferId> {
        let id = ffi::SourceManager_assignTextWithPath(
            self.as_pin(),
            CxxSV::new(path),
            CxxSV::new(text),
        );
        (id != 0).then_some(source_manager::BufferId(id))
    }

    #[inline]
    pub fn source_text(&self, buffer: source_manager::BufferId) -> Option<String> {
        if !buffer.is_valid() {
            return None;
        }
        let mut text = ffi::SourceManager_getSourceText(self.as_ref(), buffer.raw());
        if text.ends_with('\0') {
            text.pop();
        }
        Some(text)
    }

    #[inline]
    pub fn make_location(
        &self,
        buffer: source_manager::BufferId,
        offset: text_size::TextSize,
    ) -> Option<SourceLocation> {
        if !buffer.is_valid() {
            return None;
        }
        let offset_u32: u32 = offset.into();
        let ptr = ffi::SourceManager_makeLocation(buffer.raw(), offset_u32 as usize);
        SourceLocation::from_unique_ptr(ptr)
    }

    #[inline]
    pub fn file_name(&self, loc: &SourceLocation) -> String {
        self.as_ref().getFileName(loc._ptr.as_ref().expect("source location"))
    }

    #[inline]
    pub fn line_number(&self, loc: &SourceLocation) -> u32 {
        self.as_ref().getLineNumber(loc._ptr.as_ref().expect("source location"))
    }

    #[inline]
    pub fn column_number(&self, loc: &SourceLocation) -> u32 {
        self.as_ref().getColumnNumber(loc._ptr.as_ref().expect("source location"))
    }
}

impl SVLogic {
    #[inline]
    pub fn is_unknown(&self) -> bool {
        self._ptr.isUnknown()
    }

    #[inline]
    pub fn char(&self) -> c_char {
        self._ptr.toChar()
    }

    #[inline]
    pub fn bit(&self) -> Bit {
        const X: u8 = 1 << 7;
        const Z: u8 = 1 << 6;
        match self._ptr.value() {
            0 => Bit::L,
            1 => Bit::H,
            X => Bit::X,
            Z => Bit::Z,
            _ => unreachable!(),
        }
    }
}

impl SVInt {
    #[inline]
    pub fn is_signed(&self) -> bool {
        self._ptr.isSigned()
    }

    #[inline]
    pub fn has_unknown(&self) -> bool {
        self._ptr.hasUnknown()
    }

    #[inline]
    pub fn get_bit_width(&self) -> usize {
        self._ptr.getBitWidth() as usize
    }

    #[inline]
    pub fn is_single_word(&self) -> bool {
        const CHAR_BIT: usize = core::ffi::c_char::BITS as usize;
        const BITS_PER_WORD: usize = core::mem::size_of::<u64>() * CHAR_BIT;
        self.get_bit_width() <= BITS_PER_WORD && !self.has_unknown()
    }

    #[inline]
    pub fn get_single_word(&self) -> Option<u64> {
        self.is_single_word().then(|| unsafe { *self._ptr.getRawPtr() })
    }

    #[inline]
    pub fn logic_eq(&self, other: &SVInt) -> SVLogic {
        let logic = self._ptr.eq(&other._ptr);
        SVLogic { _ptr: logic }
    }

    #[inline]
    pub fn serialize(&self, base: usize) -> String {
        self._ptr.toString(base)
    }
}

unsafe impl Send for SVInt {}

unsafe impl Sync for SVInt {}

impl fmt::Debug for SVInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SVInt").field("to_string", &self.to_string()).finish()
    }
}

impl Clone for SVInt {
    fn clone(&self) -> Self {
        SVInt { _ptr: self._ptr.clone() }
    }
}

impl PartialEq for SVInt {
    fn eq(&self, other: &Self) -> bool {
        let logic = self.logic_eq(other);
        logic.bit() == Bit::H
    }
}

impl Eq for SVInt {}

impl hash::Hash for SVInt {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self._ptr.getRawPtr().hash(state)
    }
}

impl ToString for SVInt {
    fn to_string(&self) -> String {
        self._ptr.toString(10)
    }
}

impl fmt::Debug for SyntaxTrivia<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxTrivia")
            .field("kind", &self.kind())
            .field("raw_text", &self.get_raw_text())
            .finish()
    }
}

pub trait ChildrenIter<It> = DoubleEndedIterator<Item = It> + ExactSizeIterator + Clone;

impl SyntaxTrivia<'_> {
    #[inline]
    fn from_raw_ptr(_ptr: *const ffi::SyntaxTrivia) -> Option<Self> {
        assert!(_ptr.is_null().not());
        Some(SyntaxTrivia { _ptr: unsafe { Pin::new_unchecked(&*_ptr) } })
    }

    #[inline]
    pub fn get_raw_text(&self) -> CxxSV {
        self._ptr.getRawText()
    }

    #[inline]
    pub fn kind(&self) -> TriviaKind {
        TriviaKind::from_id(self._ptr.kind())
    }
}

impl<'a> SyntaxToken<'a> {
    #[inline]
    fn from_raw_ptr(_ptr: *const ffi::SyntaxToken) -> Option<Self> {
        _ptr.is_null().not().then(|| SyntaxToken { _ptr: unsafe { Pin::new_unchecked(&*_ptr) } })
    }

    #[inline]
    pub fn is_missing(&self) -> bool {
        self._ptr.isMissing()
    }

    #[inline]
    pub fn range(&self) -> Option<SourceRange> {
        SourceRange::from_unique_ptr(self._ptr.range())
    }

    #[inline]
    pub fn value_text(&self) -> CxxSV {
        self._ptr.valueText()
    }

    #[inline]
    pub fn raw_text(&self) -> CxxSV {
        self._ptr.rawText()
    }

    #[inline]
    pub fn kind(&self) -> TokenKind {
        TokenKind::from_id(self._ptr.kind())
    }

    #[inline]
    pub fn int(&self) -> Option<SVInt> {
        matches!(self.kind(), TokenKind::INTEGER_LITERAL)
            .then(|| SVInt { _ptr: self._ptr.intValue() })
    }

    #[inline]
    pub fn bits(&self) -> Option<SVLogic> {
        matches!(self.kind(), TokenKind::UNBASED_UNSIZED_LITERAL)
            .then(|| SVLogic { _ptr: self._ptr.bitValue() })
    }

    #[inline]
    pub fn real(&self) -> Option<f64> {
        matches!(self.kind(), TokenKind::REAL_LITERAL | TokenKind::TIME_LITERAL)
            .then(|| self._ptr.realValue())
    }

    #[inline]
    pub fn base(&self) -> Option<LiteralBase> {
        matches!(self.kind(), TokenKind::INTEGER_BASE)
            .then(|| unsafe { std::mem::transmute::<u8, LiteralBase>(self._ptr.base()) })
    }

    #[inline]
    pub fn time_unit(&self) -> Option<TimeUnit> {
        matches!(self.kind(), TokenKind::TIME_LITERAL)
            .then(|| unsafe { std::mem::transmute::<u8, TimeUnit>(self._ptr.unit()) })
    }

    #[inline]
    pub fn trivia_count(&self) -> usize {
        self._ptr.trivia_count()
    }

    #[inline]
    pub fn trivia_at(&self, idx: usize) -> Option<SyntaxTrivia<'a>> {
        SyntaxTrivia::from_raw_ptr(self._ptr.trivia(idx))
    }

    #[inline]
    pub fn trivias(&self) -> impl ChildrenIter<SyntaxTrivia<'a>> + use<'a> {
        SyntaxTriviaIter { tok: *self, idx: 0, total: self.trivia_count() }
    }

    #[inline]
    pub fn trivias_with_loc(
        &self,
    ) -> impl ChildrenIter<((usize, usize), SyntaxTrivia<'a>)> + use<'a> {
        let Some(range) = self.range() else {
            return Either::Left(iter::empty());
        };
        let start = range.start();
        let locs = self
            .trivias()
            .rev()
            .scan(start, |state: &mut usize, trivia| {
                let end = *state;
                *state -= trivia.get_raw_text().to_string().len();
                Some(((*state, end), trivia))
            })
            .collect_vec();
        Either::Right(locs.into_iter().rev())
    }
}

impl fmt::Debug for SyntaxToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxToken")
            .field("kind", &self.kind())
            .field("range", &self.range())
            .field("value_text", &self.value_text())
            .finish()
    }
}

impl PartialEq for SyntaxToken<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind() == other.kind() && self.range() == other.range()
    }
}

impl Eq for SyntaxToken<'_> {}

#[derive(Debug, Clone)]
pub struct SyntaxTriviaIter<'a> {
    tok: SyntaxToken<'a>,
    idx: usize,
    total: usize,
}

impl<'a> Iterator for SyntaxTriviaIter<'a> {
    type Item = SyntaxTrivia<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.total {
            let trivia = self.tok.trivia_at(self.idx).unwrap();
            self.idx += 1;
            Some(trivia)
        } else {
            None
        }
    }
}

impl<'a> DoubleEndedIterator for SyntaxTriviaIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.idx < self.total {
            self.total -= 1;
            let trivia = self.tok.trivia_at(self.total).unwrap();
            Some(trivia)
        } else {
            None
        }
    }
}

impl<'a> ExactSizeIterator for SyntaxTriviaIter<'a> {
    fn len(&self) -> usize {
        self.total - self.idx
    }
}

impl<'a> SyntaxNode<'a> {
    #[inline]
    fn from_raw_ptr(_ptr: *const ffi::SyntaxNode) -> Option<Self> {
        _ptr.is_null().not().then(|| SyntaxNode { _ptr: unsafe { Pin::new_unchecked(&*_ptr) } })
    }

    #[inline]
    pub fn walk(&self) -> SyntaxCursor<'a> {
        SyntaxCursor::new(*self)
    }

    #[inline]
    pub fn range(&self) -> Option<SourceRange> {
        SourceRange::from_unique_ptr(self._ptr.range())
    }

    #[inline]
    pub fn child_node(&self, idx: usize) -> Option<SyntaxNode<'a>> {
        SyntaxNode::from_raw_ptr(self._ptr.childNode(idx))
    }

    // not-null
    #[inline]
    pub fn child_token(&self, idx: usize) -> Option<SyntaxToken<'a>> {
        SyntaxToken::from_raw_ptr(self._ptr.childToken(idx))
            .filter(|tok| tok.kind() != TokenKind::UNKNOWN)
    }

    #[inline]
    pub fn child_count(&self) -> usize {
        self._ptr.getChildCount()
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        SyntaxKind::from_id(self._ptr.kind())
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode<'a>> {
        SyntaxNode::from_raw_ptr(self._ptr.parent())
    }

    #[inline]
    pub fn child(&self, idx: usize) -> Option<SyntaxElement<'a>> {
        // TODO: we have to visit twice to get the child, this is not efficient
        if idx >= self.child_count() {
            None
        } else if let Some(node) = self.child_node(idx) {
            Some(SyntaxElement::Node(node))
        } else {
            self.child_token(idx)
                .map(|tok| SyntaxElement::Token(SyntaxTokenWithParent { parent: *self, tok }))
        }
    }

    #[inline]
    pub fn children_with_idx(&self) -> SyntaxIdxChildren<'a> {
        SyntaxIdxChildren::new(*self)
    }

    #[inline]
    pub fn children(&self) -> SyntaxChildren<'a> {
        SyntaxChildren::new(*self)
    }

    #[inline]
    pub fn elem_preorder(&self) -> SyntaxElemPreorder<'a> {
        SyntaxElemPreorder::new(*self)
    }

    #[inline]
    pub fn node_preorder(&self) -> SyntaxNodePreorder<'a> {
        SyntaxNodePreorder::new(*self)
    }

    #[inline]
    pub fn first_token(&self) -> Option<SyntaxTokenWithParent<'a>> {
        let mut cursor = self.walk();

        while cursor.to_tok_with_parent().is_none() {
            if cursor.goto_first_child() {
                continue;
            }

            while !cursor.goto_next_sibling() {
                if !cursor.goto_parent() {
                    unreachable!("Tree has no tokens");
                }
            }
        }

        cursor.to_tok_with_parent()
    }
}

impl fmt::Debug for SyntaxNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxNode")
            .field("kind", &self.kind())
            .field("range", &self.range())
            .field("child_count", &self.child_count())
            .finish()
    }
}

impl PartialEq for SyntaxNode<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Just compare pointer
        std::ptr::eq(Pin::as_ref(&self._ptr).get_ref(), Pin::as_ref(&other._ptr).get_ref())
    }
}

impl Eq for SyntaxNode<'_> {}

impl hash::Hash for SyntaxNode<'_> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        let ptr = Pin::as_ref(&self._ptr).get_ref() as *const ffi::SyntaxNode;
        ptr.hash(state)
    }
}

impl SyntaxTree {
    #[inline]
    pub fn from_text(text: &str, name: &str, path: &str) -> SyntaxTree {
        SyntaxTree {
            _ptr: ffi::SyntaxTree::fromText(CxxSV::new(text), CxxSV::new(name), CxxSV::new(path)),
        }
    }

    #[inline]
    pub fn root(&self) -> Option<SyntaxNode> {
        SyntaxNode::from_raw_ptr(self._ptr.root())
    }
}

unsafe impl Send for SyntaxTree {}

unsafe impl Sync for SyntaxTree {}

impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("<SyntaxTree>").finish()
    }
}

impl PartialEq for SyntaxTree {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let a = self._ptr.as_ref().unwrap();
        let b = other._ptr.as_ref().unwrap();
        std::ptr::eq(std::ptr::from_ref(a), std::ptr::from_ref(b))
    }
}

impl Eq for SyntaxTree {}

/// 与默认 `SourceManager` 交互的辅助 API。
pub mod source_manager {
    use text_size::TextSize;

    use super::{CxxSV, ffi};

    /// 由 `SourceManager` 分配的缓冲区 ID。
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct BufferId(pub u32);

    impl BufferId {
        /// 返回底层的 `u32` 标识。
        #[inline]
        pub fn raw(self) -> u32 {
            self.0
        }

        /// 判断 ID 是否有效。
        #[inline]
        pub fn is_valid(self) -> bool {
            self.0 != 0
        }
    }

    /// 将内存中的文本注册到默认 `SourceManager`，返回缓冲区 ID。
    #[inline]
    pub fn assign_text(text: &str) -> Option<BufferId> {
        let id = ffi::SourceManager_assignTextDefault(CxxSV::new(text));
        (id != 0).then_some(BufferId(id))
    }

    /// 将文本以指定路径注册到默认 `SourceManager`。
    #[inline]
    pub fn assign_text_with_path(path: &str, text: &str) -> Option<BufferId> {
        let id = ffi::SourceManager_assignTextWithPathDefault(CxxSV::new(path), CxxSV::new(text));
        (id != 0).then_some(BufferId(id))
    }

    /// 从默认 `SourceManager` 中读取指定缓冲区的源文本。
    #[inline]
    pub fn source_text(buffer: BufferId) -> Option<String> {
        if !buffer.is_valid() {
            return None;
        }
        let mut text = ffi::SourceManager_getSourceTextDefault(buffer.raw());
        if text.ends_with('\0') {
            text.pop();
        }
        Some(text)
    }

    /// 创建给定缓冲区与偏移处的 `SourceLocation`。
    #[inline]
    pub fn make_location(buffer: BufferId, offset: TextSize) -> Option<crate::SourceLocation> {
        if !buffer.is_valid() {
            return None;
        }
        let offset_u32: u32 = offset.into();
        let ptr = ffi::SourceManager_makeLocation(buffer.raw(), offset_u32 as usize);
        crate::SourceLocation::from_unique_ptr(ptr)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxTokenWithParent<'a> {
    pub parent: SyntaxNode<'a>,
    pub tok: SyntaxToken<'a>,
}

impl<'a> std::ops::Deref for SyntaxTokenWithParent<'a> {
    type Target = SyntaxToken<'a>;

    fn deref(&self) -> &Self::Target {
        &self.tok
    }
}

impl PartialEq for SyntaxTokenWithParent<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && self.tok == other.tok
    }
}

impl Eq for SyntaxTokenWithParent<'_> {}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxElement<'a> {
    Node(SyntaxNode<'a>),
    Token(SyntaxTokenWithParent<'a>),
}

impl<'a> SyntaxElement<'a> {
    pub fn from_node(node: SyntaxNode) -> SyntaxElement {
        SyntaxElement::Node(node)
    }

    pub fn from_token<'b>(tok_with_parent: SyntaxTokenWithParent<'b>) -> SyntaxElement<'b> {
        SyntaxElement::Token(tok_with_parent)
    }

    pub fn as_node(&self) -> Option<SyntaxNode<'a>> {
        match self {
            SyntaxElement::Node(node) => Some(*node),
            SyntaxElement::Token(_) => None,
        }
    }

    pub fn as_tok_with_parent(&self) -> Option<SyntaxTokenWithParent<'a>> {
        match self {
            SyntaxElement::Token(tok_with_parent) => Some(*tok_with_parent),
            SyntaxElement::Node(_) => None,
        }
    }

    pub fn as_token(&self) -> Option<SyntaxToken<'a>> {
        match self {
            SyntaxElement::Token(tok_with_parent) => Some(tok_with_parent.tok),
            SyntaxElement::Node(_) => None,
        }
    }

    pub fn child_count(&self) -> usize {
        match self {
            SyntaxElement::Node(node) => node.child_count(),
            SyntaxElement::Token { .. } => 0,
        }
    }

    pub fn child(&self, idx: usize) -> Option<SyntaxElement<'a>> {
        match self {
            SyntaxElement::Node(node) => node.child(idx),
            SyntaxElement::Token { .. } => None,
        }
    }

    pub fn range(&self) -> Option<SourceRange> {
        match self {
            SyntaxElement::Node(node) => node.range(),
            SyntaxElement::Token(tok) => tok.range(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode<'a>> {
        match self {
            SyntaxElement::Node(node) => node.parent(),
            SyntaxElement::Token(tok) => Some(tok.parent),
        }
    }

    pub fn kind(&self) -> SyntaxElementKind {
        match self {
            SyntaxElement::Node(node) => SyntaxElementKind::Node(node.kind()),
            SyntaxElement::Token(tok) => SyntaxElementKind::Token(tok.kind()),
        }
    }

    pub fn children_with_idx(
        &self,
    ) -> Either<SyntaxIdxChildren<'a>, iter::Empty<(usize, SyntaxElement<'a>)>> {
        match self {
            SyntaxElement::Node(node) => Either::Left(node.children_with_idx()),
            SyntaxElement::Token(_) => Either::Right(iter::empty()),
        }
    }

    pub fn children(&self) -> Either<SyntaxChildren<'a>, iter::Empty<SyntaxElement<'a>>> {
        match self {
            SyntaxElement::Node(node) => Either::Left(node.children()),
            SyntaxElement::Token(_) => Either::Right(iter::empty()),
        }
    }
}

impl<'a> From<SyntaxNode<'a>> for SyntaxElement<'a> {
    fn from(node: SyntaxNode<'a>) -> SyntaxElement<'a> {
        SyntaxElement::Node(node)
    }
}

impl<'a> From<SyntaxTokenWithParent<'a>> for SyntaxElement<'a> {
    fn from(tok_with_parent: SyntaxTokenWithParent<'a>) -> SyntaxElement<'a> {
        SyntaxElement::Token(tok_with_parent)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElementKind {
    Node(SyntaxKind),
    Token(TokenKind),
}

impl SyntaxElementKind {
    pub fn is_list(&self) -> bool {
        match self {
            SyntaxElementKind::Node(kind) => kind.is_list(),
            SyntaxElementKind::Token(_) => false,
        }
    }
}

impl From<SyntaxKind> for SyntaxElementKind {
    fn from(kind: SyntaxKind) -> SyntaxElementKind {
        SyntaxElementKind::Node(kind)
    }
}

impl From<TokenKind> for SyntaxElementKind {
    fn from(kind: TokenKind) -> SyntaxElementKind {
        SyntaxElementKind::Token(kind)
    }
}

pub struct Compilation {
    _ptr: UniquePtr<ffi::Compilation>,
    tracked: HashMap<SmolStr, TrackedTree>,
    anonymous: Vec<SyntaxTree>,
}

struct TrackedTree {
    version: u64,
    tree: SyntaxTree,
}

pub struct PackageSymbol<'a> {
    ptr: *const ffi::PackageSymbol,
    _marker: PhantomData<&'a ffi::PackageSymbol>,
}

impl<'a> PackageSymbol<'a> {
    fn from_ptr(ptr: *const ffi::PackageSymbol) -> Option<Self> {
        (!ptr.is_null()).then_some(Self { ptr, _marker: PhantomData })
    }

    pub fn scope(&self) -> ScopeRef<'a> {
        let pkg = unsafe { &*self.ptr };
        let scope_ptr = unsafe { ffi::PackageSymbol_asScope(pkg).as_ref().expect("package scope") };
        ScopeRef::new(scope_ptr)
    }
}

impl Compilation {
    pub fn new() -> Self {
        Compilation {
            _ptr: ffi::Compilation::new(),
            tracked: HashMap::new(),
            anonymous: Vec::new(),
        }
    }

    pub fn add_syntax_tree(&mut self, tree: SyntaxTree) {
        self.push_tree(&tree);
        self.anonymous.push(tree);
    }

    pub fn upsert_syntax_tree(
        &mut self,
        path: impl Into<SmolStr>,
        version: u64,
        tree: SyntaxTree,
    ) -> bool {
        let path = path.into();
        match self.tracked.entry(path) {
            Entry::Occupied(mut entry) => {
                if entry.get().version == version {
                    return false;
                }
                entry.insert(TrackedTree { version, tree });
            }
            Entry::Vacant(entry) => {
                entry.insert(TrackedTree { version, tree });
            }
        }
        self.rebuild_all();
        true
    }

    pub fn remove_syntax_tree(&mut self, path: &str) -> bool {
        if self.tracked.remove(path).is_some() {
            self.rebuild_all();
            true
        } else {
            false
        }
    }

    pub fn root(&mut self) -> Option<RootSymbol<'_>> {
        unsafe { ffi::Compilation::getRoot(self._ptr.as_mut().unwrap()).as_ref() }
            .map(|root| RootSymbol::new(root))
    }

    pub fn source_manager(&mut self) -> Option<SourceManagerRef<'_>> {
        let ptr = ffi::Compilation::getSourceManager(self._ptr.as_mut().unwrap());
        if let Some(sm) = SourceManagerRef::new(ptr) {
            return Some(sm);
        }
        let default = ffi::SourceManager_getDefault();
        SourceManagerRef::new(default)
    }

    pub fn get_package(&self, name: &str) -> Option<PackageSymbol<'_>> {
        let ptr = ffi::Compilation::getPackage(self._ptr.as_ref().unwrap(), CxxSV::new(name));
        PackageSymbol::from_ptr(ptr)
    }

    fn push_tree(&mut self, tree: &SyntaxTree) {
        ffi::Compilation::add_syntax_tree(self._ptr.as_mut().unwrap(), tree._ptr.clone());
    }

    fn rebuild_all(&mut self) {
        let mut new_ptr = ffi::Compilation::new();

        for tree in &self.anonymous {
            ffi::Compilation::add_syntax_tree(new_ptr.as_mut().unwrap(), tree._ptr.clone());
        }

        let mut tracked: Vec<_> = self.tracked.iter().collect();
        tracked.sort_by(|(a, _), (b, _)| a.cmp(b));
        for (_, entry) in tracked {
            ffi::Compilation::add_syntax_tree(new_ptr.as_mut().unwrap(), entry.tree._ptr.clone());
        }

        self._ptr = new_ptr;
    }
}

#[cfg(test)]
mod tests;
