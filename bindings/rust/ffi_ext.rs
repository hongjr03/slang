//! FFI 扩展 - 提供安全的辅助函数和类型转换
//!
//! 这个模块封装了 slang FFI 的常用模式，提供更符合 Rust 习惯的 API。

use std::marker::PhantomData;

use text_size::{TextRange, TextSize};

use crate::ffi::{SourceLocation, SourceRange, Symbol};

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
    ($ptr:expr) => {{
        if $ptr.is_null() {
            None
        } else {
            Some(unsafe { &*$ptr })
        }
    }};
}

/// SourceRange 扩展
pub trait SourceRangeExt {
    /// 转换为 TextRange
    fn to_text_range(&self) -> TextRange;
}

impl SourceRangeExt for SourceRange {
    fn to_text_range(&self) -> TextRange {
        TextRange::new(
            TextSize::from(self.start() as u32),
            TextSize::from(self.end() as u32),
        )
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
    current: *const Symbol,
    _phantom: PhantomData<&'a Symbol>,
}

impl<'a> SymbolIterator<'a> {
    /// 创建新的符号迭代器
    pub fn new(first: *const Symbol) -> Self {
        Self {
            current: first,
            _phantom: PhantomData,
        }
    }
}

impl<'a> Iterator for SymbolIterator<'a> {
    type Item = &'a Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.is_null() {
            return None;
        }
        
        unsafe {
            let symbol = &*self.current;
            self.current = symbol.getNextSibling();
            Some(symbol)
        }
    }
}

/// Scope 成员迭代扩展
pub trait ScopeExt {
    /// 迭代所有成员
    fn members(&self) -> SymbolIterator<'_>;
    
    /// 迭代指定类型的成员
    fn members_of_kind(&self, kind: u16) -> impl Iterator<Item = &Symbol>;
}

impl ScopeExt for crate::ffi::Scope {
    fn members(&self) -> SymbolIterator<'_> {
        SymbolIterator::new(self.getFirstMember())
    }
    
    fn members_of_kind(&self, kind: u16) -> impl Iterator<Item = &Symbol> {
        self.members().filter(move |sym| sym.kind() == kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_range_conversion() {
        // 这个测试需要实际的 SourceRange 实例
        // 暂时留空，实际测试需要集成到 slang 的测试中
    }
}