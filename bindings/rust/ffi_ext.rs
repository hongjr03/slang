//! FFI 扩展 - 提供安全的辅助函数和类型转换
//!
//! 这个模块封装了 slang FFI 的常用模式，提供更符合 Rust 习惯的 API。

use text_size::{TextRange, TextSize};

use crate::{
    ScopeMemberIter, ScopeRef, SymbolRef,
    ffi::{SourceLocation, SourceRange},
};

/// FFI 操作的结果类型
pub type FfiResult<T> = Result<T, FfiError>;

/// FFI 错误类型
#[derive(Debug, thiserror::Error)]
pub enum FfiError {
    #[error("Null pointer encountered")]
    NullPointer,

    #[error("Invalid symbol kind: {0}")]
    InvalidSymbolKind(u16),

    #[error("FFI call failed: {0}")]
    CallFailed(String),

    #[error("Type mismatch")]
    TypeMismatch,
}

/// 安全解引用指针的宏
#[macro_export]
macro_rules! safe_deref {
    ($ptr:expr) => {{
        if $ptr.is_null() {
            return Err($crate::ffi_ext::FfiError::NullPointer);
        }
        unsafe { &*$ptr }
    }};
}

/// 尝试安全解引用指针的宏
#[macro_export]
macro_rules! try_deref {
    ($ptr:expr) => {{ if $ptr.is_null() { None } else { Some(unsafe { &*$ptr }) } }};
}

/// SourceRange 扩展
pub trait SourceRangeExt {
    /// 转换为 TextRange
    fn to_text_range(&self) -> TextRange;
}

impl SourceRangeExt for SourceRange {
    fn to_text_range(&self) -> TextRange {
        TextRange::new(TextSize::from(self.start() as u32), TextSize::from(self.end() as u32))
    }
}

/// SourceLocation 扩展
pub trait SourceLocationExt {
    /// 转换为 TextSize
    fn to_text_size(&self) -> TextSize;
}

impl SourceLocationExt for SourceLocation {
    fn to_text_size(&self) -> TextSize {
        TextSize::from(self.offset() as u32)
    }
}

/// Symbol 迭代器
pub struct SymbolIterator<'a> {
    inner: ScopeMemberIter<'a>,
}

impl<'a> SymbolIterator<'a> {
    /// 创建新的符号迭代器
    pub fn new(scope: ScopeRef<'a>) -> Self {
        Self { inner: scope.members() }
    }
}

impl<'a> Iterator for SymbolIterator<'a> {
    type Item = SymbolRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// Scope 成员迭代扩展
pub trait ScopeExt<'a> {
    /// 迭代所有成员
    fn members(&self) -> SymbolIterator<'a>;

    /// 迭代指定类型的成员
    fn members_of_kind(&self, kind: u16) -> Box<dyn Iterator<Item = SymbolRef<'a>> + 'a>;

    /// 查找直接子成员
    fn find_member(&self, name: &str) -> Option<SymbolRef<'a>>;

    /// 按名称执行完整查找
    fn lookup_name(&self, name: &str) -> Option<SymbolRef<'a>>;
}

impl<'a> ScopeExt<'a> for ScopeRef<'a> {
    fn members(&self) -> SymbolIterator<'a> {
        SymbolIterator::new(*self)
    }

    fn members_of_kind(&self, kind: u16) -> Box<dyn Iterator<Item = SymbolRef<'a>> + 'a> {
        Box::new(ScopeRef::members(*self).filter(move |sym| sym.kind() == kind))
    }

    fn find_member(&self, name: &str) -> Option<SymbolRef<'a>> {
        ScopeRef::find_member(*self, name)
    }

    fn lookup_name(&self, name: &str) -> Option<SymbolRef<'a>> {
        ScopeRef::lookup_name(*self, name, None)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_source_range_conversion() {
        // 这个测试需要实际的 SourceRange 实例
        // 暂时留空，实际测试需要集成到 slang 的测试中
    }
}
