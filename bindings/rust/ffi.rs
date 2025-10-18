#![allow(non_snake_case)]
#![allow(clippy::module_inception)]

mod cxx_sv;

pub use std::pin::Pin;

use cxx::{SharedPtr, UniquePtr};
pub use cxx_sv::CxxSV;
pub use slang_ffi::*;

#[cxx::bridge]
mod slang_ffi {
    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/include/slang/text/SourceLocation.h");

        type SourceLocation;

        fn offset(self: &SourceLocation) -> usize;

        type SourceRange;

        #[namespace = "wrapper"]
        fn source_range_start(range: &SourceRange) -> usize;

        #[namespace = "wrapper"]
        fn source_range_end(range: &SourceRange) -> usize;
    }

    impl UniquePtr<SourceLocation> {}

    impl UniquePtr<SourceRange> {}

    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/include/slang/numeric/SVInt.h");

        #[cxx_name = "logic_t"]
        type SVLogic;

        fn isUnknown(self: &SVLogic) -> bool;

        fn toChar(self: &SVLogic) -> c_char;

        #[namespace = "wrapper"]
        fn logic_t_value(logic: &SVLogic) -> u8;

        type SVInt;

        fn isSigned(self: &SVInt) -> bool;

        fn hasUnknown(self: &SVInt) -> bool;

        fn getBitWidth(self: &SVInt) -> u32;

        fn getRawPtr(self: &SVInt) -> *const u64;

        #[namespace = "wrapper"]
        fn SVInt_toString(svint: &SVInt, base: usize) -> String;

        #[namespace = "wrapper"]
        fn SVInt_clone(svint: &SVInt) -> UniquePtr<SVInt>;

        #[namespace = "wrapper"]
        fn SVInt_eq(lhs: &SVInt, rhs: &SVInt) -> UniquePtr<SVLogic>;
    }

    impl UniquePtr<SVLogic> {}

    impl UniquePtr<SVInt> {}

    #[namespace = "slang::parsing"]
    unsafe extern "C++" {
        include!("slang/include/slang/parsing/Token.h");

        #[cxx_name = "Trivia"]
        type SyntaxTrivia;

        fn getRawText(self: &SyntaxTrivia) -> CxxSV;

        #[namespace = "wrapper::parsing"]
        fn SyntaxTrivia_kind(trivia: &SyntaxTrivia) -> u8;

        #[cxx_name = "Token"]
        type SyntaxToken;

        fn isMissing(self: &SyntaxToken) -> bool;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_range(tok: &SyntaxToken) -> UniquePtr<SourceRange>;

        fn valueText(self: &SyntaxToken) -> CxxSV; // excapedText

        fn rawText(self: &SyntaxToken) -> CxxSV; // rawText

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_kind(tok: &SyntaxToken) -> u16;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_intValue(tok: &SyntaxToken) -> UniquePtr<SVInt>;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_bitValue(tok: &SyntaxToken) -> UniquePtr<SVLogic>;

        fn realValue(self: &SyntaxToken) -> f64;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_base(tok: &SyntaxToken) -> u8;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_unit(tok: &SyntaxToken) -> u8;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_trivia_count(tok: &SyntaxToken) -> usize;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_trivia(tok: &SyntaxToken, idx: usize) -> *const SyntaxTrivia;
    }

    impl UniquePtr<SyntaxTrivia> {}

    impl UniquePtr<SyntaxToken> {}

    #[namespace = "slang::syntax"]
    unsafe extern "C++" {
        include!("slang/include/slang/syntax/SyntaxNode.h");

        type SyntaxNode;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_range(node: &SyntaxNode) -> UniquePtr<SourceRange>;

        fn childNode(self: &SyntaxNode, idx: usize) -> *const SyntaxNode;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_childToken(node: &SyntaxNode, idx: usize) -> *const SyntaxToken;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_parent(node: &SyntaxNode) -> *const SyntaxNode;

        fn getChildCount(self: &SyntaxNode) -> usize;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_kind(node: &SyntaxNode) -> u16;
    }

    #[namespace = "slang::syntax"]
    unsafe extern "C++" {
        include!("slang/bindings/rust/ffi/wrapper.h");
        include!("slang/include/slang/syntax/SyntaxTree.h");

        type SyntaxTree;

        #[namespace = "wrapper::syntax"]
        fn SyntaxTree_fromText(text: CxxSV, name: CxxSV, path: CxxSV) -> SharedPtr<SyntaxTree>;

        #[namespace = "wrapper::syntax"]
        fn SyntaxTree_root(tree: &SyntaxTree) -> *const SyntaxNode;
    }

    impl SharedPtr<SyntaxTree> {}

    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/include/slang/text/SourceManager.h");

        type SourceManager;

        #[namespace = "wrapper"]
        fn SourceManager_getFileName(sm: &SourceManager, loc: &SourceLocation) -> String;

        #[namespace = "wrapper"]
        fn SourceManager_getLineNumber(sm: &SourceManager, loc: &SourceLocation) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_getColumnNumber(sm: &SourceManager, loc: &SourceLocation) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_makeLocationDefault(
            buffer_id: u32,
            offset: usize,
        ) -> UniquePtr<SourceLocation>;

        #[namespace = "wrapper"]
        fn SourceLocation_buffer(loc: &SourceLocation) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_assignTextDefault(text: CxxSV) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_assignTextWithPathDefault(path: CxxSV, text: CxxSV) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_getSourceTextDefault(buffer_id: u32) -> String;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/Compilation.h");
        include!("slang/include/slang/ast/symbols/CompilationUnitSymbols.h");

        type Compilation;
        type PackageSymbol;

        #[namespace = "wrapper::ast"]
        fn Compilation_new() -> UniquePtr<Compilation>;

        #[namespace = "wrapper::ast"]
        fn Compilation_add_syntax_tree(
            compilation: Pin<&mut Compilation>,
            tree: SharedPtr<SyntaxTree>,
        );

        #[namespace = "wrapper::ast"]
        fn Compilation_getSourceManager(comp: Pin<&mut Compilation>) -> *const SourceManager;

        #[namespace = "wrapper::ast"]
        fn Compilation_getRoot(comp: Pin<&mut Compilation>) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Compilation_getPackage(comp: &Compilation, name: CxxSV) -> *const PackageSymbol;
    }

    impl UniquePtr<Compilation> {}

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        #[namespace = "wrapper::ast"]
        fn PackageSymbol_asScope(pkg: &PackageSymbol) -> *const Scope;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/Symbol.h");
        include!("slang/include/slang/ast/Scope.h");

        type Symbol;
        type Scope;

        #[namespace = "wrapper::ast"]
        fn Symbol_kind(symbol: &Symbol) -> u16;

        fn getParentScope(self: &Symbol) -> *const Scope;

        fn getSyntax(self: &Symbol) -> *const SyntaxNode;

        #[namespace = "wrapper::ast"]
        fn Symbol_getNextSibling(symbol: &Symbol) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Scope_asSymbol(scope: &Scope) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Scope_getFirstMember(scope: &Scope) -> *const Symbol;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/Lookup.h");

        type LookupLocation;

        #[namespace = "wrapper::ast"]
        fn LookupLocation_max() -> UniquePtr<LookupLocation>;

        #[namespace = "wrapper::ast"]
        fn LookupLocation_before(symbol: &Symbol) -> UniquePtr<LookupLocation>;

        #[namespace = "wrapper::ast"]
        fn LookupLocation_after(symbol: &Symbol) -> UniquePtr<LookupLocation>;

        type LookupResult;

        #[namespace = "wrapper::ast"]
        fn LookupResult_new() -> UniquePtr<LookupResult>;

        #[namespace = "wrapper::ast"]
        fn LookupResult_found(result: &LookupResult) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn LookupResult_hasError(result: &LookupResult) -> bool;
    }

    impl UniquePtr<LookupLocation> {}
    impl UniquePtr<LookupResult> {}

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        #[namespace = "wrapper::ast"]
        fn Scope_lookupName(scope: &Scope, name: CxxSV, location: &LookupLocation)
        -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Scope_find(scope: &Scope, name: CxxSV) -> *const Symbol;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/types/Type.h");
        include!("slang/include/slang/ast/types/DeclaredType.h");

        type Type;
        type DeclaredType;

        fn isIntegral(self: &Type) -> bool;
        fn isFloating(self: &Type) -> bool;
        fn isNumeric(self: &Type) -> bool;
        fn isAggregate(self: &Type) -> bool;

        #[namespace = "wrapper::ast"]
        fn Type_toString(ty: &Type) -> String;

        #[namespace = "wrapper::ast"]
        fn Type_kind(ty: &Type) -> u16;

        #[namespace = "wrapper::ast"]
        fn DeclaredType_getType(dt: &DeclaredType) -> *const Type;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/symbols/ValueSymbol.h");

        type ValueSymbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_asValueSymbol(symbol: &Symbol) -> *const ValueSymbol;

        #[namespace = "wrapper::ast"]
        fn ValueSymbol_getType(vs: &ValueSymbol) -> *const DeclaredType;
    }

    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/include/slang/numeric/ConstantValue.h");

        type ConstantValue;

        fn isInteger(self: &ConstantValue) -> bool;
        fn isReal(self: &ConstantValue) -> bool;
        fn isUnpacked(self: &ConstantValue) -> bool;
        fn isString(self: &ConstantValue) -> bool;

        #[namespace = "wrapper"]
        fn ConstantValue_integer(cv: &ConstantValue) -> UniquePtr<SVInt>;

        #[namespace = "wrapper"]
        fn ConstantValue_real(cv: &ConstantValue) -> f64;

        #[namespace = "wrapper"]
        fn ConstantValue_str(cv: &ConstantValue) -> String;
    }

    impl UniquePtr<ConstantValue> {}

    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/diagnostics/Diagnostics.h");

        type Diagnostic;
    }

    // StringView
    unsafe extern "C++" {
        include!("slang/bindings/rust/ffi/string_view.h");

        #[namespace = "std"]
        #[cxx_name = "string_view"]
        type CxxSV<'a> = crate::CxxSV<'a>;
    }
}

macro_rules! forward_functions {
    (fn $name:ident(&self $(, $arg:ident: $arg_ty:ty)*) -> $ret:ty |> $ffi_fn:ident; $($tt:tt)*) => {
        #[inline]
        pub fn $name(&self $(, $arg: $arg_ty)*) -> $ret {
            slang_ffi::$ffi_fn(self $(, $arg)*)
        }
        forward_functions!($($tt)*);
    };
    (fn $name:ident($($arg:ident: $arg_ty:ty),*) -> $ret:ty |> $ffi_fn:ident; $($tt:tt)*) => {
        #[inline]
        pub fn $name($($arg: $arg_ty),*) -> $ret {
            slang_ffi::$ffi_fn($($arg),*)
        }
        forward_functions!($($tt)*);
    };
    () => {};
}

macro_rules! impl_functions {
    (impl $type:ty { $($tt:tt)* }) => {
        impl $type {forward_functions!($($tt)*); }
    };
}

impl_functions! {
    impl SourceRange {
        fn start(&self) -> usize |> source_range_start;
        fn end(&self) -> usize |> source_range_end;
    }
}

impl_functions! {
    impl SyntaxTree {
        fn fromText(text: CxxSV, name: CxxSV, path: CxxSV) -> SharedPtr<SyntaxTree> |> SyntaxTree_fromText;
        fn root(&self) -> *const SyntaxNode |> SyntaxTree_root;
    }
}

impl_functions! {
    impl SyntaxNode {
        fn range(&self) -> UniquePtr<SourceRange> |> SyntaxNode_range;
        fn kind(&self) -> u16 |> SyntaxNode_kind;
        fn childToken(&self, idx: usize) -> *const SyntaxToken |> SyntaxNode_childToken;
        fn parent(&self) -> *const SyntaxNode |> SyntaxNode_parent;
    }
}

impl_functions! {
    impl SyntaxToken {
        fn range(&self) -> UniquePtr<SourceRange> |> SyntaxToken_range;
    }
}

impl_functions! {
    impl SyntaxTrivia {
        fn kind(&self) -> u8 |> SyntaxTrivia_kind;
    }
}

impl_functions! {
    impl SyntaxToken {
        fn kind(&self) -> u16 |> SyntaxToken_kind;
        fn intValue(&self) -> UniquePtr<SVInt> |> SyntaxToken_intValue;
        fn bitValue(&self) -> UniquePtr<SVLogic> |> SyntaxToken_bitValue;
        fn base(&self) -> u8 |> SyntaxToken_base;
        fn unit(&self) -> u8 |> SyntaxToken_unit;
        fn trivia_count(&self) -> usize |> SyntaxToken_trivia_count;
        fn trivia(&self, idx: usize) -> *const SyntaxTrivia |> SyntaxToken_trivia;
    }
}

impl_functions! {
    impl SVLogic {
        fn value(&self) -> u8 |> logic_t_value;
    }
}

impl_functions! {
    impl SVInt {
        fn clone(&self) -> UniquePtr<SVInt> |> SVInt_clone;
        fn toString(&self, base: usize) -> String |> SVInt_toString;
        fn eq(&self, rhs: &SVInt) -> UniquePtr<SVLogic> |> SVInt_eq;
    }
}

impl_functions! {
    impl Compilation {
        fn new() -> UniquePtr<Compilation> |> Compilation_new;
        fn add_syntax_tree(self_: Pin<&mut Compilation>, tree: SharedPtr<SyntaxTree>) -> () |> Compilation_add_syntax_tree;
        fn getSourceManager(self_: Pin<&mut Compilation>) -> *const SourceManager |> Compilation_getSourceManager;
        fn getRoot(self_: Pin<&mut Compilation>) -> *const Symbol |> Compilation_getRoot;
        fn getPackage(&self, name: CxxSV) -> *const PackageSymbol |> Compilation_getPackage;
    }
}

impl_functions! {
    impl PackageSymbol {
        fn asScope(&self) -> *const Scope |> PackageSymbol_asScope;
    }
}

impl_functions! {
    impl SourceManager {
        fn getFileName(&self, loc: &SourceLocation) -> String |> SourceManager_getFileName;
        fn getLineNumber(&self, loc: &SourceLocation) -> u32 |> SourceManager_getLineNumber;
        fn getColumnNumber(&self, loc: &SourceLocation) -> u32 |> SourceManager_getColumnNumber;
    }
}

impl_functions! {
    impl Symbol {
        fn kind(&self) -> u16 |> Symbol_kind;
        fn getNextSibling(&self) -> *const Symbol |> Symbol_getNextSibling;
    }
}

impl_functions! {
    impl Scope {
        fn asSymbol(&self) -> *const Symbol |> Scope_asSymbol;
        fn getFirstMember(&self) -> *const Symbol |> Scope_getFirstMember;
        fn lookupName(&self, name: CxxSV, location: &LookupLocation) -> *const Symbol |> Scope_lookupName;
        fn find(&self, name: CxxSV) -> *const Symbol |> Scope_find;
    }
}

impl_functions! {
    impl LookupLocation {
        fn max() -> UniquePtr<LookupLocation> |> LookupLocation_max;
        fn before(symbol: &Symbol) -> UniquePtr<LookupLocation> |> LookupLocation_before;
        fn after(symbol: &Symbol) -> UniquePtr<LookupLocation> |> LookupLocation_after;
    }
}

impl_functions! {
    impl LookupResult {
        fn new() -> UniquePtr<LookupResult> |> LookupResult_new;
        fn found(&self) -> *const Symbol |> LookupResult_found;
        fn hasError(&self) -> bool |> LookupResult_hasError;
    }
}

impl_functions! {
    impl Type {
        fn toString(&self) -> String |> Type_toString;
        fn kind(&self) -> u16 |> Type_kind;
    }
}

impl_functions! {
    impl Symbol {
        fn asValueSymbol(&self) -> *const ValueSymbol |> Symbol_asValueSymbol;
    }
}

impl_functions! {
    impl ValueSymbol {
        fn getType(&self) -> *const DeclaredType |> ValueSymbol_getType;
    }
}

impl_functions! {
    impl DeclaredType {
        fn getType(&self) -> *const Type |> DeclaredType_getType;
    }
}

impl_functions! {
    impl ConstantValue {
        fn integer(&self) -> UniquePtr<SVInt> |> ConstantValue_integer;
        fn real(&self) -> f64 |> ConstantValue_real;
        fn str(&self) -> String |> ConstantValue_str;
    }
}
