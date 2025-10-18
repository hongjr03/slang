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

        #[namespace = "wrapper"]
        fn SourceManager_assignText(sm: Pin<&mut SourceManager>, text: CxxSV) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_assignTextWithPath(
            sm: Pin<&mut SourceManager>,
            path: CxxSV,
            text: CxxSV,
        ) -> u32;

        #[namespace = "wrapper"]
        fn SourceManager_getSourceText(sm: &SourceManager, buffer_id: u32) -> String;

        #[namespace = "wrapper"]
        fn SourceManager_makeLocation(buffer_id: u32, offset: usize) -> UniquePtr<SourceLocation>;

        #[namespace = "wrapper"]
        fn SourceManager_getDefault() -> *mut SourceManager;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/Compilation.h");
        include!("slang/include/slang/ast/symbols/CompilationUnitSymbols.h");

        type Compilation;
        type PackageSymbol;
        type RootSymbol;

        #[namespace = "wrapper::ast"]
        fn Compilation_new() -> UniquePtr<Compilation>;

        #[namespace = "wrapper::ast"]
        fn Compilation_add_syntax_tree(
            compilation: Pin<&mut Compilation>,
            tree: SharedPtr<SyntaxTree>,
        );

        #[namespace = "wrapper::ast"]
        fn Compilation_getSourceManager(comp: Pin<&mut Compilation>) -> *mut SourceManager;

        #[namespace = "wrapper::ast"]
        fn Compilation_getRoot(comp: Pin<&mut Compilation>) -> *const RootSymbol;

        #[namespace = "wrapper::ast"]
        fn Compilation_getPackage(comp: &Compilation, name: CxxSV) -> *const PackageSymbol;
    }

    impl UniquePtr<Compilation> {}

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        #[namespace = "wrapper::ast"]
        fn PackageSymbol_asScope(pkg: &PackageSymbol) -> *const Scope;

        #[namespace = "wrapper::ast"]
        fn RootSymbol_asScope(root: &RootSymbol) -> *const Scope;

        #[namespace = "wrapper::ast"]
        fn RootSymbol_asSymbol(root: &RootSymbol) -> *const Symbol;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/Symbol.h");
        include!("slang/include/slang/ast/Scope.h");
        include!("slang/include/slang/ast/symbols/ParameterSymbols.h");
        include!("slang/include/slang/ast/symbols/VariableSymbols.h");
        include!("slang/include/slang/ast/symbols/SubroutineSymbols.h");

        type Symbol;
        type Scope;
        type ParameterSymbol;
        type FieldSymbol;
        type SubroutineSymbol;
        type FormalArgumentSymbol;
        type ValueSymbol;
        type VariableSymbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_kind(symbol: &Symbol) -> u16;

        #[namespace = "wrapper::ast"]
        fn Symbol_name(symbol: &Symbol) -> String;

        fn getParentScope(self: &Symbol) -> *const Scope;

        fn getSyntax(self: &Symbol) -> *const SyntaxNode;

        #[namespace = "wrapper::ast"]
        fn Symbol_getNextSibling(symbol: &Symbol) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_isType(symbol: &Symbol) -> bool;

        #[namespace = "wrapper::ast"]
        fn Symbol_isValue(symbol: &Symbol) -> bool;

        #[namespace = "wrapper::ast"]
        fn Symbol_asType(symbol: &Symbol) -> *const Type;

        #[namespace = "wrapper::ast"]
        fn Symbol_getDeclaredType(symbol: &Symbol) -> *const DeclaredType;

        #[namespace = "wrapper::ast"]
        fn Symbol_asScope(symbol: &Symbol) -> *const Scope;

        #[namespace = "wrapper::ast"]
        fn Scope_asSymbol(scope: &Scope) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Scope_getFirstMember(scope: &Scope) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_asParameterSymbol(symbol: &Symbol) -> *const ParameterSymbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_asFieldSymbol(symbol: &Symbol) -> *const FieldSymbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_asSubroutineSymbol(symbol: &Symbol) -> *const SubroutineSymbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_asFormalArgumentSymbol(symbol: &Symbol) -> *const FormalArgumentSymbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_asVariableSymbol(symbol: &Symbol) -> *const VariableSymbol;

        #[namespace = "wrapper::ast"]
        fn ParameterSymbol_asValueSymbol(symbol: &ParameterSymbol) -> *const ValueSymbol;

        #[namespace = "wrapper::ast"]
        fn ParameterSymbol_isLocalParam(symbol: &ParameterSymbol) -> bool;

        #[namespace = "wrapper::ast"]
        fn ParameterSymbol_isPortParam(symbol: &ParameterSymbol) -> bool;

        #[namespace = "wrapper::ast"]
        fn ParameterSymbol_getValue(symbol: &ParameterSymbol) -> *const ConstantValue;

        #[namespace = "wrapper::ast"]
        fn FieldSymbol_asValueSymbol(symbol: &FieldSymbol) -> *const ValueSymbol;

        #[namespace = "wrapper::ast"]
        fn FieldSymbol_bitOffset(symbol: &FieldSymbol) -> u64;

        #[namespace = "wrapper::ast"]
        fn FieldSymbol_fieldIndex(symbol: &FieldSymbol) -> u32;

        #[namespace = "wrapper::ast"]
        fn FieldSymbol_randMode(symbol: &FieldSymbol) -> u8;

        #[namespace = "wrapper::ast"]
        fn VariableSymbol_lifetime(symbol: &VariableSymbol) -> u8;

        #[namespace = "wrapper::ast"]
        fn VariableSymbol_flags(symbol: &VariableSymbol) -> u16;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_asSymbol(symbol: &SubroutineSymbol) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_asScope(symbol: &SubroutineSymbol) -> *const Scope;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_kind(symbol: &SubroutineSymbol) -> u8;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_methodFlags(symbol: &SubroutineSymbol) -> u32;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_visibility(symbol: &SubroutineSymbol) -> u8;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_hasOutputArgs(symbol: &SubroutineSymbol) -> bool;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_isVirtual(symbol: &SubroutineSymbol) -> bool;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_getReturnType(symbol: &SubroutineSymbol) -> *const Type;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_argumentCount(symbol: &SubroutineSymbol) -> usize;

        #[namespace = "wrapper::ast"]
        fn SubroutineSymbol_argumentAt(
            symbol: &SubroutineSymbol,
            index: usize,
        ) -> *const FormalArgumentSymbol;

        #[namespace = "wrapper::ast"]
        fn FormalArgumentSymbol_asValueSymbol(symbol: &FormalArgumentSymbol) -> *const ValueSymbol;

        #[namespace = "wrapper::ast"]
        fn FormalArgumentSymbol_direction(symbol: &FormalArgumentSymbol) -> u8;
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

        #[namespace = "wrapper::ast"]
        fn Lookup_isVisibleFrom(symbol: &Symbol, scope: &Scope) -> bool;
    }

    impl UniquePtr<LookupLocation> {}
    impl UniquePtr<LookupResult> {}

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        #[namespace = "wrapper::ast"]
        fn Scope_lookupName(scope: &Scope, name: CxxSV, location: &LookupLocation)
        -> *const Symbol;

        #[namespace = "wrapper::ast"]
        unsafe fn Scope_lookupNameWithFlags(
            scope: &Scope,
            name: CxxSV,
            location: *const LookupLocation,
            flags: u32,
        ) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        fn Scope_find(scope: &Scope, name: CxxSV) -> *const Symbol;

        #[namespace = "wrapper::ast"]
        unsafe fn Scope_getVisibleSymbols(
            scope: &Scope,
            view: &Scope,
            location: *const LookupLocation,
            flags: u32,
        ) -> Vec<usize>;
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
        fn isSigned(self: &Type) -> bool;
        fn isFourState(self: &Type) -> bool;
        fn isArray(self: &Type) -> bool;
        fn isStruct(self: &Type) -> bool;
        fn isString(self: &Type) -> bool;
        fn isNull(self: &Type) -> bool;
        fn hasFixedRange(self: &Type) -> bool;
        fn getBitWidth(self: &Type) -> u32;
        fn getBitstreamWidth(self: &Type) -> u64;
        fn getSelectableWidth(self: &Type) -> u64;

        #[namespace = "wrapper::ast"]
        fn Type_toString(ty: &Type) -> String;

        #[namespace = "wrapper::ast"]
        fn Type_kind(ty: &Type) -> u16;

        #[namespace = "wrapper::ast"]
        fn Type_fieldCount(ty: &Type) -> usize;

        #[namespace = "wrapper::ast"]
        fn Type_getField(ty: &Type, index: usize) -> *const FieldSymbol;

        #[namespace = "wrapper::ast"]
        fn DeclaredType_getType(dt: &DeclaredType) -> *const Type;

        #[namespace = "wrapper::ast"]
        fn Type_getDefaultValue(ty: &Type) -> UniquePtr<ConstantValue>;

        #[namespace = "wrapper::ast"]
        fn Type_getIntegralFlags(ty: &Type) -> u8;
    }

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/include/slang/ast/symbols/ValueSymbol.h");

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
        fn isShortReal(self: &ConstantValue) -> bool;
        fn isString(self: &ConstantValue) -> bool;
        fn isNullHandle(self: &ConstantValue) -> bool;
        fn isUnbounded(self: &ConstantValue) -> bool;
        fn isUnpacked(self: &ConstantValue) -> bool;
        fn isQueue(self: &ConstantValue) -> bool;
        fn isMap(self: &ConstantValue) -> bool;
        fn hasUnknown(self: &ConstantValue) -> bool;
        fn isTrue(self: &ConstantValue) -> bool;
        fn isFalse(self: &ConstantValue) -> bool;
        fn size(self: &ConstantValue) -> usize;
        fn empty(self: &ConstantValue) -> bool;
    }

    #[namespace = "wrapper::numeric"]
    unsafe extern "C++" {
        fn ConstantValue_isValid(value: &ConstantValue) -> bool;
        fn ConstantValue_integer(value: &ConstantValue) -> UniquePtr<SVInt>;
        fn ConstantValue_str(value: &ConstantValue) -> String;
        fn ConstantValue_real(value: &ConstantValue) -> f64;
        fn ConstantValue_shortReal(value: &ConstantValue) -> f32;
        fn ConstantValue_toString(value: &ConstantValue, exact_unknowns: bool) -> String;
        fn ConstantValue_clone(value: &ConstantValue) -> UniquePtr<ConstantValue>;
    }

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
        fn getSourceManager(self_: Pin<&mut Compilation>) -> *mut SourceManager |> Compilation_getSourceManager;
        fn getRoot(self_: Pin<&mut Compilation>) -> *const RootSymbol |> Compilation_getRoot;
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
        fn isType(&self) -> bool |> Symbol_isType;
        fn isValue(&self) -> bool |> Symbol_isValue;
        fn asType(&self) -> *const Type |> Symbol_asType;
        fn getDeclaredType(&self) -> *const DeclaredType |> Symbol_getDeclaredType;
        fn asScope(&self) -> *const Scope |> Symbol_asScope;
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
        fn fieldCount(&self) -> usize |> Type_fieldCount;
        fn getField(&self, index: usize) -> *const FieldSymbol |> Type_getField;
    }
}

impl_functions! {
    impl Symbol {
        fn asValueSymbol(&self) -> *const ValueSymbol |> Symbol_asValueSymbol;
        fn asParameterSymbol(&self) -> *const ParameterSymbol |> Symbol_asParameterSymbol;
        fn asFieldSymbol(&self) -> *const FieldSymbol |> Symbol_asFieldSymbol;
        fn asSubroutineSymbol(&self) -> *const SubroutineSymbol |> Symbol_asSubroutineSymbol;
        fn asFormalArgumentSymbol(&self) -> *const FormalArgumentSymbol |> Symbol_asFormalArgumentSymbol;
    }
}

impl_functions! {
    impl ValueSymbol {
        fn getType(&self) -> *const DeclaredType |> ValueSymbol_getType;
    }
}

impl_functions! {
    impl ParameterSymbol {
        fn asValueSymbol(&self) -> *const ValueSymbol |> ParameterSymbol_asValueSymbol;
        fn isLocalParam(&self) -> bool |> ParameterSymbol_isLocalParam;
        fn isPortParam(&self) -> bool |> ParameterSymbol_isPortParam;
    }
}

impl_functions! {
    impl FieldSymbol {
        fn asValueSymbol(&self) -> *const ValueSymbol |> FieldSymbol_asValueSymbol;
        fn bitOffset(&self) -> u64 |> FieldSymbol_bitOffset;
        fn fieldIndex(&self) -> u32 |> FieldSymbol_fieldIndex;
        fn randMode(&self) -> u8 |> FieldSymbol_randMode;
    }
}

impl_functions! {
    impl SubroutineSymbol {
        fn asSymbol(&self) -> *const Symbol |> SubroutineSymbol_asSymbol;
        fn asScope(&self) -> *const Scope |> SubroutineSymbol_asScope;
        fn kind(&self) -> u8 |> SubroutineSymbol_kind;
        fn methodFlags(&self) -> u32 |> SubroutineSymbol_methodFlags;
        fn visibility(&self) -> u8 |> SubroutineSymbol_visibility;
        fn hasOutputArgs(&self) -> bool |> SubroutineSymbol_hasOutputArgs;
        fn isVirtual(&self) -> bool |> SubroutineSymbol_isVirtual;
        fn getReturnType(&self) -> *const Type |> SubroutineSymbol_getReturnType;
        fn argumentCount(&self) -> usize |> SubroutineSymbol_argumentCount;
        fn argumentAt(&self, index: usize) -> *const FormalArgumentSymbol
            |> SubroutineSymbol_argumentAt;
    }
}

impl_functions! {
    impl FormalArgumentSymbol {
        fn asValueSymbol(&self) -> *const ValueSymbol |> FormalArgumentSymbol_asValueSymbol;
        fn direction(&self) -> u8 |> FormalArgumentSymbol_direction;
    }
}

impl_functions! {
    impl DeclaredType {
        fn getType(&self) -> *const Type |> DeclaredType_getType;
    }
}
