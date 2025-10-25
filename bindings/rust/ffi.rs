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
        include!("slang/text/SourceLocation.h");

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
        include!("slang/numeric/SVInt.h");

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
        include!("slang/parsing/Token.h");

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
        include!("slang/syntax/SyntaxNode.h");

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
        include!("slang/syntax/SyntaxTree.h");

        type SyntaxTree;

        #[namespace = "wrapper::syntax"]
        fn SyntaxTree_fromText(text: CxxSV, name: CxxSV, path: CxxSV) -> SharedPtr<SyntaxTree>;

        #[namespace = "wrapper::syntax"]
        fn SyntaxTree_root(tree: &SyntaxTree) -> *const SyntaxNode;
    }

    impl SharedPtr<SyntaxTree> {}

    #[namespace = "slang::ast"]
    unsafe extern "C++" {
        include!("slang/ast/Compilation.h");
        include!("slang/ast/Symbol.h");
        include!("slang/ast/Scope.h");
        include!("slang/ast/symbols/CompilationUnitSymbols.h");
        include!("slang/ast/types/Type.h");
        include!("slang/ast/types/AllTypes.h");

        type Compilation;

        #[namespace = "wrapper::ast"]
        fn Compilation_new() -> UniquePtr<Compilation>;
        #[namespace = "wrapper::ast"]
        fn Compilation_add_syntax_tree(
            compilation: Pin<&mut Compilation>,
            tree: SharedPtr<SyntaxTree>,
        );
        #[namespace = "wrapper::ast"]
        fn Compilation_get_root(compilation: Pin<&mut Compilation>) -> *const RootSymbol;
        #[namespace = "wrapper::ast"]
        fn Compilation_get_module_ports(
            compilation: Pin<&mut Compilation>,
            module_name: &str,
        ) -> Vec<String>;

        type Symbol;

        #[namespace = "wrapper::ast"]
        fn Symbol_get_name(symbol: &Symbol) -> String;
        #[namespace = "wrapper::ast"]
        fn Symbol_get_kind(symbol: &Symbol) -> u16;
        #[namespace = "wrapper::ast"]
        fn Symbol_as_scope(symbol: &Symbol) -> *const Scope;
        #[namespace = "wrapper::ast"]
        fn Symbol_get_parent_scope(symbol: &Symbol) -> *const Symbol;
        #[namespace = "wrapper::ast"]
        fn Symbol_as_type(symbol: &Symbol) -> *const Type;
        #[namespace = "wrapper::ast"]
        fn Symbol_get_type(symbol: &Symbol) -> *const Type;
        #[namespace = "wrapper::ast"]
        fn Symbol_get_location(symbol: &Symbol) -> UniquePtr<SourceRange>;
        #[namespace = "wrapper::ast"]
        fn Symbol_has_location(symbol: &Symbol) -> bool;
        #[namespace = "wrapper::ast"]
        fn Symbol_is_definition(symbol: &Symbol) -> bool;
        #[namespace = "wrapper::ast"]
        fn Symbol_as_definition(symbol: &Symbol) -> *const DefinitionSymbol;

        type Scope;

        #[namespace = "wrapper::ast"]
        fn Scope_as_symbol(scope: &Scope) -> &Symbol;
        #[namespace = "wrapper::ast"]
        fn Scope_find(scope: &Scope, name: &str) -> *const Symbol;
        #[namespace = "wrapper::ast"]
        fn Scope_lookup_name(scope: &Scope, name: &str) -> *const Symbol;
        #[namespace = "wrapper::ast"]
        fn Scope_member_count(scope: &Scope) -> usize;
        #[namespace = "wrapper::ast"]
        fn Scope_member_at(scope: &Scope, index: usize) -> *const Symbol;

        type Type;

        #[namespace = "wrapper::ast"]
        fn Type_get_canonical(type_: &Type) -> &Type;
        #[namespace = "wrapper::ast"]
        fn Type_get_kind(type_: &Type) -> u16;
        #[namespace = "wrapper::ast"]
        fn Type_as_symbol(type_: &Type) -> &Symbol;
        #[namespace = "wrapper::ast"]
        fn Type_as_scope(type_: &Type) -> *const Scope;
        #[namespace = "wrapper::ast"]
        fn Type_is_integral(type_: &Type) -> bool;
        #[namespace = "wrapper::ast"]
        fn Type_is_aggregate(type_: &Type) -> bool;
        #[namespace = "wrapper::ast"]
        fn Type_is_struct(type_: &Type) -> bool;
        #[namespace = "wrapper::ast"]
        fn Type_is_class(type_: &Type) -> bool;
        #[namespace = "wrapper::ast"]
        fn Type_to_string(type_: &Type) -> String;

        #[cxx_name = "RootSymbol"]
        type RootSymbol;

        #[cxx_name = "DefinitionSymbol"]
        type DefinitionSymbol;

        #[cxx_name = "InstanceSymbol"]
        type InstanceSymbol;

        #[cxx_name = "InstanceBodySymbol"]
        type InstanceBodySymbol;

        #[namespace = "wrapper::ast"]
        fn RootSymbol_top_instances_count(root: &RootSymbol) -> usize;

        #[namespace = "wrapper::ast"]
        fn RootSymbol_top_instance_at(root: &RootSymbol, index: usize) -> *const InstanceSymbol;

        #[namespace = "wrapper::ast"]
        fn DefinitionSymbol_create_default_instance(
            compilation: Pin<&mut Compilation>,
            definition: &DefinitionSymbol,
        ) -> *const InstanceSymbol;

        #[namespace = "wrapper::ast"]
        fn InstanceSymbol_get_body(instance: &InstanceSymbol) -> *const Scope;

        #[namespace = "wrapper::ast"]
        fn InstanceBodySymbol_as_scope(body: &InstanceBodySymbol) -> *const Scope;
    }

    impl UniquePtr<Compilation> {}

    impl UniquePtr<Symbol> {}

    impl UniquePtr<Scope> {}

    impl UniquePtr<Type> {}

    impl UniquePtr<RootSymbol> {}

    impl UniquePtr<DefinitionSymbol> {}

    impl UniquePtr<InstanceSymbol> {}

    impl UniquePtr<InstanceBodySymbol> {}

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
        fn get_root(self_: Pin<&mut Compilation>) -> *const RootSymbol |> Compilation_get_root;
    }
}

impl_functions! {
    impl Symbol {
        fn get_name(&self) -> String |> Symbol_get_name;
        fn get_kind(&self) -> u16 |> Symbol_get_kind;
        fn as_scope(&self) -> *const Scope |> Symbol_as_scope;
        fn get_parent_scope(&self) -> *const Symbol |> Symbol_get_parent_scope;
        fn as_type(&self) -> *const Type |> Symbol_as_type;
        fn get_type(&self) -> *const Type |> Symbol_get_type;
        fn get_location(&self) -> UniquePtr<SourceRange> |> Symbol_get_location;
        fn has_location(&self) -> bool |> Symbol_has_location;
    }
}

impl_functions! {
    impl Scope {
        fn as_symbol(&self) -> &Symbol |> Scope_as_symbol;
        fn find(&self, name: &str) -> *const Symbol |> Scope_find;
        fn lookup_name(&self, name: &str) -> *const Symbol |> Scope_lookup_name;
        fn member_count(&self) -> usize |> Scope_member_count;
        fn member_at(&self, index: usize) -> *const Symbol |> Scope_member_at;
    }
}

impl_functions! {
    impl Type {
        fn get_canonical(&self) -> &Type |> Type_get_canonical;
        fn get_kind(&self) -> u16 |> Type_get_kind;
        fn as_symbol(&self) -> &Symbol |> Type_as_symbol;
        fn as_scope(&self) -> *const Scope |> Type_as_scope;
        fn is_integral(&self) -> bool |> Type_is_integral;
        fn is_aggregate(&self) -> bool |> Type_is_aggregate;
        fn is_struct(&self) -> bool |> Type_is_struct;
        fn is_class(&self) -> bool |> Type_is_class;
        fn type_string(&self) -> String |> Type_to_string;
    }
}
