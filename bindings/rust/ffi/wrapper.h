#pragma once
#include "rust/cxx.h"
#include <cassert>
#include <cstddef>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>

#include "slang/ast/Compilation.h"
#include "slang/ast/Scope.h"
#include "slang/ast/Symbol.h"
#include "slang/ast/symbols/CompilationUnitSymbols.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/ValueSymbol.h"
#include "slang/ast/types/AllTypes.h"
#include "slang/ast/types/Type.h"
#include "slang/diagnostics/Diagnostics.h"
#include "slang/diagnostics/LookupDiags.h"
#include "slang/numeric/SVInt.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/syntax/SyntaxTree.h"
#include "slang/text/SourceLocation.h"

namespace wrapper {
using SyntaxTrivia = ::slang::parsing::Trivia;
using SyntaxToken = ::slang::parsing::Token;
using SVInt = ::slang::SVInt;
using logic_t = ::slang::logic_t;
using SourceRange = ::slang::SourceRange;
using SyntaxTree = ::slang::syntax::SyntaxTree;
using SyntaxNode = ::slang::syntax::SyntaxNode;
using Compilation = ::slang::ast::Compilation;
using Symbol = ::slang::ast::Symbol;
using Scope = ::slang::ast::Scope;
using RootSymbol = ::slang::ast::RootSymbol;
using Type = ::slang::ast::Type;
using Diagnostic = ::slang::Diagnostic;
using DefinitionSymbol = ::slang::ast::DefinitionSymbol;
using InstanceSymbol = ::slang::ast::InstanceSymbol;
using InstanceBodySymbol = ::slang::ast::InstanceBodySymbol;

// SourceRange
inline static size_t source_range_start(const slang::SourceRange& range) {
    return range.start().offset();
}

inline static size_t source_range_end(const slang::SourceRange& range) {
    return range.end().offset();
}

inline static uint8_t logic_t_value(const slang::logic_t& logic) {
    return logic.value;
}

inline static rust::string SVInt_toString(const SVInt& svint, size_t base) {
    switch (base) {
        case 2:
            return rust::String(svint.toString(slang::LiteralBase::Binary, false));
        case 8:
            return rust::String(svint.toString(slang::LiteralBase::Octal, false));
        case 16:
            return rust::String(svint.toString(slang::LiteralBase::Hex, false));
        case 10:
            return rust::String(svint.toString(slang::LiteralBase::Decimal, false));
        default:
            assert(false);
    }
}

inline static std::unique_ptr<slang::SVInt> SVInt_clone(const SVInt& svint) {
    return std::make_unique<SVInt>(svint);
}

inline static std::unique_ptr<slang::logic_t> SVInt_eq(const SVInt& lhs, const SVInt& rhs) {
    return std::make_unique<logic_t>(lhs == rhs);
}

namespace parsing {
// Trivia
inline static uint8_t SyntaxTrivia_kind(const SyntaxTrivia& trivia) {
    return static_cast<uint8_t>(trivia.kind);
}

// Token
inline static size_t SyntaxToken_trivia_count(const SyntaxToken& token) {
    return token.trivia().size();
}

inline static const SyntaxTrivia* SyntaxToken_trivia(const SyntaxToken& token, size_t index) {
    return &token.trivia()[index];
}

inline static uint16_t SyntaxToken_kind(const SyntaxToken& token) {
    return static_cast<uint16_t>(token.kind);
}

inline static std::unique_ptr<SourceRange> SyntaxToken_range(const SyntaxToken& token) {
    auto range = token.range();
    return range == SourceRange::NoLocation ? nullptr : std::make_unique<SourceRange>(range);
}

inline static std::unique_ptr<SVInt> SyntaxToken_intValue(const SyntaxToken& token) {
    return std::make_unique<SVInt>(token.intValue());
}

inline static std::unique_ptr<logic_t> SyntaxToken_bitValue(const SyntaxToken& token) {
    return std::make_unique<logic_t>(token.bitValue());
}

inline static uint8_t SyntaxToken_base(const SyntaxToken& token) {
    return static_cast<uint8_t>(token.numericFlags().base());
}

inline static uint8_t SyntaxToken_unit(const SyntaxToken& token) {
    return static_cast<uint8_t>(token.numericFlags().unit());
}
} // namespace parsing

namespace syntax {
inline static std::shared_ptr<SyntaxTree> SyntaxTree_fromText(std::string_view text,
                                                              std::string_view name,
                                                              std::string_view path) {
    return SyntaxTree::fromText(text, name, path);
}

inline static const SyntaxNode* SyntaxTree_root(const SyntaxTree& tree) {
    return &tree.root();
}

inline static std::unique_ptr<SourceRange> SyntaxNode_range(const SyntaxNode& node) {
    auto range = node.sourceRange();
    return range == SourceRange::NoLocation ? nullptr : std::make_unique<SourceRange>(range);
}

inline static const SyntaxToken* SyntaxNode_childToken(const SyntaxNode& node, size_t index) {
    // Since the function returns a const ptr, so we garentee the node won't be modified.
    return (const_cast<SyntaxNode&>(node)).childTokenPtr(index);
}

inline static const SyntaxNode* SyntaxNode_parent(const SyntaxNode& node) {
    return node.parent;
}

inline static uint16_t SyntaxNode_kind(const SyntaxNode& node) {
    return static_cast<uint16_t>(node.kind);
}
} // namespace syntax

namespace ast {
inline static std::unique_ptr<Compilation> Compilation_new() {
    return std::unique_ptr<Compilation>(new Compilation());
}

inline static void Compilation_add_syntax_tree(Compilation& compilation,
                                               std::shared_ptr<SyntaxTree> tree) {
    compilation.addSyntaxTree(tree);
}

inline static const slang::ast::RootSymbol* Compilation_get_root(Compilation& compilation) {
    return &compilation.getRoot();
}

// Get ports from a module definition by name
// Returns a vector of "name|type" strings for each port
inline static rust::Vec<rust::String> Compilation_get_module_ports(
    Compilation& compilation,
    rust::Str module_name
) {
    rust::Vec<rust::String> result;
    
    // Find the definition
    auto def_result = compilation.getDefinition(
        std::string_view(module_name.data(), module_name.size()),
        compilation.getRoot(),
        slang::SourceRange::NoLocation,
        slang::DiagCode()
    );
    
    if (!def_result.definition || 
        def_result.definition->kind != slang::ast::SymbolKind::Definition) {
        return result;
    }
    
    auto& definition = def_result.definition->as<DefinitionSymbol>();
    
    // Create a default instance to get the elaborated ports
    auto& instance = slang::ast::InstanceSymbol::createDefault(compilation, definition);
    auto& body = instance.body;
    
    // Iterate through members to find ports
    for (auto& member : body.members()) {
        // Check if it's a port
        if (member.kind != slang::ast::SymbolKind::Port &&
            member.kind != slang::ast::SymbolKind::MultiPort &&
            member.kind != slang::ast::SymbolKind::InterfacePort) {
            continue;
        }
        
        // Get port name
        std::string port_name(member.name);
        
        // Get port type if available
        std::string port_type;
        if (auto valSym = member.as_if<slang::ast::ValueSymbol>()) {
            port_type = valSym->getType().toString();
        }
        
        // Encode as "name|type" format
        result.push_back(rust::String(port_name + "|" + port_type));
    }
    
    return result;
}

inline static size_t RootSymbol_top_instances_count(const RootSymbol& root) {
    return root.topInstances.size();
}

inline static const slang::ast::InstanceSymbol* RootSymbol_top_instance_at(const RootSymbol& root,
                                                                           size_t index) {
    if (index >= root.topInstances.size()) {
        return nullptr;
    }
    return root.topInstances[index];
}

inline static const slang::ast::Scope* InstanceSymbol_get_body(
    const slang::ast::InstanceSymbol& instance) {
    return &instance.body;
}

inline static rust::Vec<std::unique_ptr<Diagnostic>> Compilation_get_all_diagnostics(
    const Compilation& compilation) {
}

// Symbol methods
inline static rust::String Symbol_get_name(const Symbol& symbol) {
    return rust::String(std::string(symbol.name));
}

inline static uint16_t Symbol_get_kind(const Symbol& symbol) {
    return static_cast<uint16_t>(symbol.kind);
}

inline static const Scope* Symbol_as_scope(const Symbol& symbol) {
    return symbol.as_if<Scope>();
}

inline static const Symbol* Symbol_get_parent_scope(const Symbol& symbol) {
    auto scope = symbol.getParentScope();
    return scope ? &scope->asSymbol() : nullptr;
}

// Scope methods
inline static const Symbol& Scope_as_symbol(const Scope& scope) {
    return scope.asSymbol();
}

inline static const Symbol* Scope_find(const Scope& scope, rust::Str name) {
    return scope.find(std::string_view(name.data(), name.size()));
}

inline static const Symbol* Scope_lookup_name(const Scope& scope, rust::Str name) {
    return scope.lookupName(std::string_view(name.data(), name.size()));
}

inline static size_t Scope_member_count(const Scope& scope) {
    size_t count = 0;
    for (auto& member : scope.members()) {
        (void)member;
        count++;
    }
    return count;
}

inline static const Symbol* Scope_member_at(const Scope& scope, size_t index) {
    auto it = scope.members().begin();
    std::advance(it, index);
    return it != scope.members().end() ? &*it : nullptr;
}

// Type methods
inline static const Type* Symbol_as_type(const Symbol& symbol) {
    return symbol.as_if<Type>();
}

inline static const Type* Symbol_get_type(const Symbol& symbol) {
    if (auto valSym = symbol.as_if<::slang::ast::ValueSymbol>()) {
        return &valSym->getType();
    }
    return nullptr;
}

inline static const Type& Type_get_canonical(const Type& type) {
    return type.getCanonicalType();
}

inline static uint16_t Type_get_kind(const Type& type) {
    return static_cast<uint16_t>(type.kind);
}

inline static const Symbol& Type_as_symbol(const Type& type) {
    return type;
}

inline static const Scope* Type_as_scope(const Type& type) {
    return type.as_if<Scope>();
}

inline static bool Type_is_integral(const Type& type) {
    return type.isIntegral();
}

inline static bool Type_is_aggregate(const Type& type) {
    return type.isAggregate();
}

inline static bool Type_is_struct(const Type& type) {
    return type.isStruct();
}

inline static bool Type_is_class(const Type& type) {
    return type.isClass();
}

inline static rust::String Type_to_string(const Type& type) {
    return rust::String(type.toString());
}

// Symbol location APIs
inline static std::unique_ptr<SourceRange> Symbol_get_location(const Symbol& symbol) {
    auto loc = symbol.location;
    if (!loc) {
        return nullptr;
    }
    return std::make_unique<SourceRange>(SourceRange(loc, loc));
}

inline static bool Symbol_has_location(const Symbol& symbol) {
    return symbol.location.operator bool();
}

// DefinitionSymbol APIs
inline static bool Symbol_is_definition(const Symbol& symbol) {
    return symbol.kind == ::slang::ast::SymbolKind::Definition;
}

inline static const DefinitionSymbol* Symbol_as_definition(const Symbol& symbol) {
    if (symbol.kind == ::slang::ast::SymbolKind::Definition) {
        return &symbol.as<DefinitionSymbol>();
    }
    return nullptr;
}

// Create default instance from definition
inline static const InstanceSymbol* DefinitionSymbol_create_default_instance(
    Compilation& compilation, const DefinitionSymbol& definition) {
    return &InstanceSymbol::createDefault(compilation, definition);
}

// Get the body from an instance (already exists but let's ensure it's documented)
// inline static const InstanceBodySymbol* InstanceSymbol_get_body(const InstanceSymbol& instance) {
//     return &instance.body;
// }

// InstanceBodySymbol is a Scope, so we can use it directly
inline static const Scope* InstanceBodySymbol_as_scope(const InstanceBodySymbol& body) {
    return &body;
}
} // namespace ast

namespace diagnostics {
inline static uint16_t code(const Diagnostic& diag) {
    return diag.code.getCode();
}
} // namespace diagnostics
} // namespace wrapper
