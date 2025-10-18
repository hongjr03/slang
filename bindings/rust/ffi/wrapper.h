#pragma once
#include "rust/cxx.h"
#include <cassert>
#include <compare>
#include <cstddef>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>

#include "slang/ast/Compilation.h"
#include "slang/ast/Scope.h"
#include "slang/ast/Symbol.h"
#include "slang/ast/SemanticFacts.h"
#include "slang/ast/symbols/CompilationUnitSymbols.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/MemberSymbols.h"
#include "slang/ast/symbols/ParameterSymbols.h"
#include "slang/ast/symbols/PortSymbols.h"
#include "slang/ast/symbols/SubroutineSymbols.h"
#include "slang/ast/symbols/VariableSymbols.h"
#include "slang/ast/symbols/ValueSymbol.h"
#include "slang/ast/types/DeclaredType.h"
#include "slang/ast/types/Type.h"
#include "slang/ast/types/AllTypes.h"
#include "slang/diagnostics/Diagnostics.h"
#include "slang/numeric/ConstantValue.h"
#include "slang/numeric/SVInt.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/syntax/SyntaxTree.h"
#include "slang/text/SourceLocation.h"
#include "slang/text/SourceManager.h"

namespace wrapper {
using SyntaxTrivia = ::slang::parsing::Trivia;
using SyntaxToken = ::slang::parsing::Token;
using SVInt = ::slang::SVInt;
using logic_t = ::slang::logic_t;
using SourceRange = ::slang::SourceRange;
using SyntaxTree = ::slang::syntax::SyntaxTree;
using SyntaxNode = ::slang::syntax::SyntaxNode;
using Compilation = ::slang::ast::Compilation;

// SourceRange
inline static size_t source_range_start(const slang::SourceRange& range) {
    return range.start().offset();
}

inline static size_t source_range_end(const slang::SourceRange& range) {
    return range.end().offset();
}

inline static std::unique_ptr<SourceRange> source_range_from_locations(
    const slang::SourceLocation& start, const slang::SourceLocation& end) {
    if (!start.valid() || !end.valid())
        return nullptr;
    return std::make_unique<SourceRange>(start, end);
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

// SourceManager wrappers
inline static rust::String SourceManager_getFileName(const ::slang::SourceManager& sm,
                                                     const ::slang::SourceLocation& loc) {
    std::string_view sv = sm.getFileName(loc);
    return rust::String(std::string(sv));
}

inline static uint32_t SourceManager_getLineNumber(const ::slang::SourceManager& sm,
                                                   const ::slang::SourceLocation& loc) {
    return sm.getLineNumber(loc);
}

inline static uint32_t SourceManager_getColumnNumber(const ::slang::SourceManager& sm,
                                                     const ::slang::SourceLocation& loc) {
    return sm.getColumnNumber(loc);
}

inline static std::unique_ptr<slang::SourceLocation> SourceManager_makeLocationDefault(
    uint32_t buffer_id, size_t offset) {
    if (buffer_id == 0)
        return nullptr;
    std::string_view empty_view{};
    auto loc = ::slang::SourceLocation(::slang::BufferID(buffer_id, empty_view), offset);
    return std::make_unique<slang::SourceLocation>(loc);
}

inline static uint32_t SourceLocation_buffer(const ::slang::SourceLocation& loc) {
    return loc.buffer().getId();
}

inline static uint32_t SourceManager_assignTextDefault(std::string_view text) {
    auto& sm = ::slang::syntax::SyntaxTree::getDefaultSourceManager();
    auto buffer = sm.assignText(text);
    return buffer.id.getId();
}

inline static uint32_t SourceManager_assignTextWithPathDefault(std::string_view path,
                                                               std::string_view text) {
    auto& sm = ::slang::syntax::SyntaxTree::getDefaultSourceManager();
    auto buffer = sm.assignText(path, text);
    return buffer.id.getId();
}

inline static rust::String SourceManager_getSourceTextDefault(uint32_t buffer_id) {
    if (buffer_id == 0) {
        return rust::String(std::string());
    }
    auto& sm = ::slang::syntax::SyntaxTree::getDefaultSourceManager();
    std::string_view empty_view{};
    auto view = sm.getSourceText(::slang::BufferID(buffer_id, empty_view));
    return rust::String(std::string(view));
}

inline static uint32_t SourceManager_assignText(::slang::SourceManager& sm, std::string_view text) {
    auto buffer = sm.assignText(text);
    return buffer.id.getId();
}

inline static uint32_t SourceManager_assignTextWithPath(::slang::SourceManager& sm,
                                                        std::string_view path,
                                                        std::string_view text) {
    auto buffer = sm.assignText(path, text);
    return buffer.id.getId();
}

inline static rust::String SourceManager_getSourceText(const ::slang::SourceManager& sm,
                                                       uint32_t buffer_id) {
    if (buffer_id == 0) {
        return rust::String(std::string());
    }
    std::string_view empty_view{};
    auto view = sm.getSourceText(::slang::BufferID(buffer_id, empty_view));
    return rust::String(std::string(view));
}

inline static std::unique_ptr<slang::SourceLocation> SourceManager_makeLocation(uint32_t buffer_id,
                                                                                size_t offset) {
    if (buffer_id == 0) {
        return nullptr;
    }
    std::string_view empty_view{};
    auto loc = ::slang::SourceLocation(::slang::BufferID(buffer_id, empty_view), offset);
    return std::make_unique<slang::SourceLocation>(loc);
}

inline static ::slang::SourceManager* SourceManager_getDefault() {
    auto& sm = ::slang::syntax::SyntaxTree::getDefaultSourceManager();
    return &sm;
}

namespace ast {
inline static std::unique_ptr<Compilation> Compilation_new() {
    return std::unique_ptr<Compilation>(new Compilation());
}

inline static void Compilation_add_syntax_tree(Compilation& compilation,
                                               std::shared_ptr<SyntaxTree> tree) {
    compilation.addSyntaxTree(tree);
}

inline static ::slang::SourceManager* Compilation_getSourceManager(Compilation& comp) {
    auto sm = comp.getSourceManager();
    if (!sm)
        return nullptr;
    return const_cast<::slang::SourceManager*>(sm);
}

inline static const ::slang::ast::RootSymbol* Compilation_getRoot(Compilation& comp) {
    return &comp.getRoot();
}

inline static const ::slang::ast::PackageSymbol* Compilation_getPackage(const Compilation& comp,
                                                                        std::string_view name) {
    return comp.getPackage(name);
}

inline static const ::slang::ast::Scope* PackageSymbol_asScope(
    const ::slang::ast::PackageSymbol& pkg) {
    return static_cast<const ::slang::ast::Scope*>(&pkg);
}

inline static const ::slang::ast::Scope* RootSymbol_asScope(const ::slang::ast::RootSymbol& root) {
    return static_cast<const ::slang::ast::Scope*>(&root);
}

inline static const ::slang::ast::Symbol* RootSymbol_asSymbol(
    const ::slang::ast::RootSymbol& root) {
    return static_cast<const ::slang::ast::Symbol*>(&root);
}

// Symbol wrappers
inline static uint16_t Symbol_kind(const ::slang::ast::Symbol& symbol) {
    return static_cast<uint16_t>(symbol.kind);
}

inline static const ::slang::ast::Symbol* Symbol_getNextSibling(
    const ::slang::ast::Symbol& symbol) {
    return symbol.getNextSibling();
}

inline static bool Symbol_isType(const ::slang::ast::Symbol& symbol) {
    return symbol.isType();
}

inline static bool Symbol_isValue(const ::slang::ast::Symbol& symbol) {
    return symbol.isValue();
}

inline static const ::slang::ast::Type* Symbol_asType(const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::Type>();
}

inline static const ::slang::ast::DeclaredType* Symbol_getDeclaredType(
    const ::slang::ast::Symbol& symbol) {
    return symbol.getDeclaredType();
}

inline static const ::slang::ast::Scope* Symbol_asScope(const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::Scope>();
}

// Scope wrappers
inline static const ::slang::ast::Symbol* Scope_asSymbol(const ::slang::ast::Scope& scope) {
    const ::slang::ast::Symbol& sym = scope.asSymbol();
    return &sym;
}

inline static rust::String Symbol_name(const ::slang::ast::Symbol& symbol) {
    return rust::String(std::string(symbol.name));
}

inline static const ::slang::ast::Symbol* Scope_getFirstMember(const ::slang::ast::Scope& scope) {
    return scope.getFirstMember();
}

inline static const ::slang::ast::ParameterSymbol* Symbol_asParameterSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::ParameterSymbol>();
}

inline static const ::slang::ast::FieldSymbol* Symbol_asFieldSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::FieldSymbol>();
}

inline static const ::slang::ast::SubroutineSymbol* Symbol_asSubroutineSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::SubroutineSymbol>();
}

inline static const ::slang::ast::FormalArgumentSymbol* Symbol_asFormalArgumentSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::FormalArgumentSymbol>();
}

inline static const ::slang::ast::VariableSymbol* Symbol_asVariableSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::VariableSymbol>();
}

inline static const ::slang::ast::PortSymbol* Symbol_asPortSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::PortSymbol>();
}

inline static const ::slang::ast::MultiPortSymbol* Symbol_asMultiPortSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::MultiPortSymbol>();
}

// LookupLocation wrappers
inline static std::unique_ptr<::slang::ast::LookupLocation> LookupLocation_max() {
    return std::make_unique<::slang::ast::LookupLocation>(::slang::ast::LookupLocation::max);
}

inline static std::unique_ptr<::slang::ast::LookupLocation> LookupLocation_before(
    const ::slang::ast::Symbol& symbol) {
    return std::make_unique<::slang::ast::LookupLocation>(
        ::slang::ast::LookupLocation::before(symbol));
}

inline static std::unique_ptr<::slang::ast::LookupLocation> LookupLocation_after(
    const ::slang::ast::Symbol& symbol) {
    return std::make_unique<::slang::ast::LookupLocation>(
        ::slang::ast::LookupLocation::after(symbol));
}

// LookupResult wrappers
inline static std::unique_ptr<::slang::ast::LookupResult> LookupResult_new() {
    return std::make_unique<::slang::ast::LookupResult>();
}

inline static const ::slang::ast::Symbol* LookupResult_found(
    const ::slang::ast::LookupResult& result) {
    return result.found;
}

inline static bool LookupResult_hasError(const ::slang::ast::LookupResult& result) {
    return result.hasError();
}

// Scope lookup wrappers
inline static const ::slang::ast::Symbol* Scope_lookupName(
    const ::slang::ast::Scope& scope, std::string_view name,
    const ::slang::ast::LookupLocation& location) {
    return scope.lookupName(name, location);
}

inline static const ::slang::ast::Symbol* Scope_lookupNameWithFlags(
    const ::slang::ast::Scope& scope, std::string_view name,
    const ::slang::ast::LookupLocation* location, uint32_t rawFlags) {
    auto flags = slang::bitmask<::slang::ast::LookupFlags>(
        static_cast<::slang::ast::LookupFlags>(rawFlags));
    if (!location)
        return scope.lookupName(name, ::slang::ast::LookupLocation::max, flags);
    return scope.lookupName(name, *location, flags);
}

inline static uint8_t VariableSymbol_lifetime(const ::slang::ast::VariableSymbol& symbol) {
    return static_cast<uint8_t>(symbol.lifetime);
}

inline static uint16_t VariableSymbol_flags(const ::slang::ast::VariableSymbol& symbol) {
    return static_cast<uint16_t>(symbol.flags.bits());
}

inline static const ::slang::ast::Symbol* Scope_find(const ::slang::ast::Scope& scope,
                                                     std::string_view name) {
    return scope.find(name);
}

inline static rust::Vec<slang::uintptr_t> Scope_getVisibleSymbols(
    const ::slang::ast::Scope& scope, const ::slang::ast::Scope& view,
    const ::slang::ast::LookupLocation* location, uint32_t rawFlags) {
    rust::Vec<slang::uintptr_t> out;
    slang::flat_hash_set<const ::slang::ast::Symbol*> seen;
    auto flags = slang::bitmask<::slang::ast::LookupFlags>(
        static_cast<::slang::ast::LookupFlags>(rawFlags));

    auto tryAdd = [&](const ::slang::ast::Symbol& candidate) {
        const ::slang::ast::Symbol* symbol = &candidate;
        if (symbol->kind == ::slang::ast::SymbolKind::TransparentMember)
            symbol = &symbol->as<::slang::ast::TransparentMemberSymbol>().wrapped;

        if (symbol->name.empty())
            return;

        if (!::slang::ast::Lookup::isVisibleFrom(*symbol, view))
            return;

        if (flags.has(::slang::ast::LookupFlags::Type)) {
            if (!symbol->isType() && symbol->kind != ::slang::ast::SymbolKind::TypeParameter &&
                symbol->kind != ::slang::ast::SymbolKind::GenericClassDef) {
                return;
            }
        }
        else {
            switch (symbol->kind) {
                case ::slang::ast::SymbolKind::Subroutine:
                case ::slang::ast::SymbolKind::InstanceArray:
                case ::slang::ast::SymbolKind::Sequence:
                case ::slang::ast::SymbolKind::Property:
                case ::slang::ast::SymbolKind::Checker:
                case ::slang::ast::SymbolKind::UninstantiatedDef:
                    break;
                case ::slang::ast::SymbolKind::Instance:
                    if (!symbol->as<::slang::ast::InstanceSymbol>().isInterface())
                        return;
                    break;
                default:
                    if (!symbol->isValue())
                        return;
                    break;
            }
        }

        if (symbol->kind == ::slang::ast::SymbolKind::Subroutine &&
            symbol->as<::slang::ast::SubroutineSymbol>().flags.has(
                ::slang::ast::MethodFlags::Constructor)) {
            return;
        }

        if (::slang::ast::VariableSymbol::isKind(symbol->kind) &&
            symbol->as<::slang::ast::VariableSymbol>().flags.has(
                ::slang::ast::VariableFlags::CompilerGenerated)) {
            return;
        }

        if (location && !flags.has(::slang::ast::LookupFlags::AllowDeclaredAfter)) {
            auto parentScope = symbol->getParentScope();
            if (parentScope && parentScope == location->getScope()) {
                auto beforeLoc = ::slang::ast::LookupLocation::before(*symbol);
                if (beforeLoc > *location)
                    return;
            }
        }

        if (seen.emplace(symbol).second)
            out.push_back(reinterpret_cast<slang::uintptr_t>(symbol));
    };

    for (auto& member : scope.members())
        tryAdd(member);

    if (auto* importData = scope.getWildcardImportData()) {
        for (auto* import : importData->wildcardImports) {
            if (!import)
                continue;
            if (const auto* package = import->getPackage()) {
                for (auto& member : package->members())
                    tryAdd(member);
            }
        }
    }

    return out;
}

inline static const ::slang::ast::ValueSymbol* ParameterSymbol_asValueSymbol(
    const ::slang::ast::ParameterSymbol& symbol) {
    return static_cast<const ::slang::ast::ValueSymbol*>(&symbol);
}

inline static bool ParameterSymbol_isLocalParam(const ::slang::ast::ParameterSymbol& symbol) {
    return symbol.isLocalParam();
}

inline static bool ParameterSymbol_isPortParam(const ::slang::ast::ParameterSymbol& symbol) {
    return symbol.isPortParam();
}

inline static const ::slang::ConstantValue* ParameterSymbol_getValue(
    const ::slang::ast::ParameterSymbol& symbol) {
    return &symbol.getValue();
}

inline static const ::slang::ast::ValueSymbol* FieldSymbol_asValueSymbol(
    const ::slang::ast::FieldSymbol& symbol) {
    return static_cast<const ::slang::ast::ValueSymbol*>(&symbol);
}

inline static uint64_t FieldSymbol_bitOffset(const ::slang::ast::FieldSymbol& symbol) {
    return symbol.bitOffset;
}

inline static uint32_t FieldSymbol_fieldIndex(const ::slang::ast::FieldSymbol& symbol) {
    return symbol.fieldIndex;
}

inline static uint8_t FieldSymbol_randMode(const ::slang::ast::FieldSymbol& symbol) {
    return static_cast<uint8_t>(symbol.randMode);
}

inline static const ::slang::ast::Symbol* SubroutineSymbol_asSymbol(
    const ::slang::ast::SubroutineSymbol& symbol) {
    return static_cast<const ::slang::ast::Symbol*>(&symbol);
}

inline static const ::slang::ast::Scope* SubroutineSymbol_asScope(
    const ::slang::ast::SubroutineSymbol& symbol) {
    return static_cast<const ::slang::ast::Scope*>(&symbol);
}

inline static uint8_t SubroutineSymbol_kind(const ::slang::ast::SubroutineSymbol& symbol) {
    return static_cast<uint8_t>(symbol.subroutineKind);
}

inline static uint32_t SubroutineSymbol_methodFlags(const ::slang::ast::SubroutineSymbol& symbol) {
    return static_cast<uint32_t>(symbol.flags.bits());
}

inline static uint8_t SubroutineSymbol_visibility(const ::slang::ast::SubroutineSymbol& symbol) {
    return static_cast<uint8_t>(symbol.visibility);
}

inline static bool SubroutineSymbol_hasOutputArgs(const ::slang::ast::SubroutineSymbol& symbol) {
    return symbol.hasOutputArgs();
}

inline static bool SubroutineSymbol_isVirtual(const ::slang::ast::SubroutineSymbol& symbol) {
    return symbol.isVirtual();
}

inline static const ::slang::ast::Type* SubroutineSymbol_getReturnType(
    const ::slang::ast::SubroutineSymbol& symbol) {
    return &symbol.getReturnType();
}

inline static size_t SubroutineSymbol_argumentCount(
    const ::slang::ast::SubroutineSymbol& symbol) {
    return symbol.getArguments().size();
}

inline static const ::slang::ast::FormalArgumentSymbol* SubroutineSymbol_argumentAt(
    const ::slang::ast::SubroutineSymbol& symbol, size_t index) {
    auto args = symbol.getArguments();
    if (index >= args.size())
        return nullptr;
    return args[index];
}

inline static const ::slang::ast::ValueSymbol* FormalArgumentSymbol_asValueSymbol(
    const ::slang::ast::FormalArgumentSymbol& symbol) {
    return static_cast<const ::slang::ast::ValueSymbol*>(&symbol);
}

inline static uint8_t FormalArgumentSymbol_direction(
    const ::slang::ast::FormalArgumentSymbol& symbol) {
    return static_cast<uint8_t>(symbol.direction);
}

inline static uint8_t FormalArgumentSymbol_lifetime(
    const ::slang::ast::FormalArgumentSymbol& symbol) {
    return static_cast<uint8_t>(symbol.lifetime);
}

inline static uint8_t PortSymbol_direction(const ::slang::ast::PortSymbol& symbol) {
    return static_cast<uint8_t>(symbol.direction);
}

inline static uint8_t MultiPortSymbol_direction(
    const ::slang::ast::MultiPortSymbol& symbol) {
    return static_cast<uint8_t>(symbol.direction);
}

inline static size_t Type_fieldCount(const ::slang::ast::Type& type) {
    const auto& canonical = type.getCanonicalType();
    switch (canonical.kind) {
        case ::slang::ast::SymbolKind::PackedStructType: {
            auto& packed = static_cast<const ::slang::ast::PackedStructType&>(canonical);
            size_t count = 0;
            for (auto member = packed.getFirstMember(); member; member = member->getNextSibling())
                if (member->kind == ::slang::ast::SymbolKind::Field)
                    count++;
            return count;
        }
        case ::slang::ast::SymbolKind::UnpackedStructType: {
            auto& structType = static_cast<const ::slang::ast::UnpackedStructType&>(canonical);
            return structType.fields.size();
        }
        default:
            return 0;
    }
}

inline static const ::slang::ast::FieldSymbol* Type_getField(const ::slang::ast::Type& type,
                                                             size_t index) {
    const auto& canonical = type.getCanonicalType();
    switch (canonical.kind) {
        case ::slang::ast::SymbolKind::PackedStructType: {
            auto& packed = static_cast<const ::slang::ast::PackedStructType&>(canonical);
            size_t current = 0;
            for (auto member = packed.getFirstMember(); member; member = member->getNextSibling()) {
                if (member->kind == ::slang::ast::SymbolKind::Field) {
                    if (current == index)
                        return static_cast<const ::slang::ast::FieldSymbol*>(member);
                    current++;
                }
            }
            return nullptr;
        }
        case ::slang::ast::SymbolKind::UnpackedStructType: {
            auto& structType = static_cast<const ::slang::ast::UnpackedStructType&>(canonical);
            return index < structType.fields.size() ? structType.fields[index] : nullptr;
        }
        default:
            return nullptr;
    }
}

inline static std::unique_ptr<::slang::ConstantValue> Type_getDefaultValue(
    const ::slang::ast::Type& type) {
    return std::make_unique<::slang::ConstantValue>(type.getDefaultValue());
}

inline static uint8_t Type_getIntegralFlags(const ::slang::ast::Type& type) {
    auto flags = type.getIntegralFlags();
    return static_cast<uint8_t>(flags.bits());
}

inline static bool Lookup_isVisibleFrom(const ::slang::ast::Symbol& symbol,
                                        const ::slang::ast::Scope& scope) {
    return ::slang::ast::Lookup::isVisibleFrom(symbol, scope);
}

// Type wrappers
inline static rust::String Type_toString(const ::slang::ast::Type& ty) {
    return rust::String(ty.toString());
}

inline static uint16_t Type_kind(const ::slang::ast::Type& ty) {
    return static_cast<uint16_t>(ty.kind);
}

// ValueSymbol wrappers
inline static const ::slang::ast::ValueSymbol* Symbol_asValueSymbol(
    const ::slang::ast::Symbol& symbol) {
    return symbol.as_if<::slang::ast::ValueSymbol>();
}

inline static const ::slang::ast::DeclaredType* ValueSymbol_getType(
    const ::slang::ast::ValueSymbol& vs) {
    return vs.getDeclaredType();
}

inline static const ::slang::ast::Type* DeclaredType_getType(const ::slang::ast::DeclaredType& dt) {
    const ::slang::ast::Type& ty = dt.getType();
    return &ty;
}

} // namespace ast

namespace numeric {
inline static bool ConstantValue_isValid(const ::slang::ConstantValue& value) {
    return static_cast<bool>(value);
}

inline static std::unique_ptr<::slang::SVInt> ConstantValue_integer(
    const ::slang::ConstantValue& value) {
    if (!value.isInteger())
        return nullptr;
    return std::make_unique<::slang::SVInt>(value.integer());
}

inline static rust::String ConstantValue_str(const ::slang::ConstantValue& value) {
    if (!value.isString())
        return rust::String();
    return rust::String(value.str());
}

inline static double ConstantValue_real(const ::slang::ConstantValue& value) {
    if (!value.isReal())
        return 0.0;
    return static_cast<double>(value.real());
}

inline static float ConstantValue_shortReal(const ::slang::ConstantValue& value) {
    if (!value.isShortReal())
        return 0.0f;
    return static_cast<float>(value.shortReal());
}

inline static rust::String ConstantValue_toString(const ::slang::ConstantValue& value,
                                                  bool exact_unknowns) {
    return rust::String(value.toString(
        ::slang::SVInt::DefaultStringAbbreviationThresholdBits, exact_unknowns, false));
}

inline static std::unique_ptr<::slang::ConstantValue> ConstantValue_clone(
    const ::slang::ConstantValue& value) {
    return std::make_unique<::slang::ConstantValue>(value);
}
} // namespace numeric

namespace ast {
inline static rust::Vec<std::unique_ptr<::slang::Diagnostic>> Compilation_get_all_diagnostics(
    Compilation& compilation) {
    rust::Vec<std::unique_ptr<::slang::Diagnostic>> out;
    const ::slang::Diagnostics& diags = compilation.getAllDiagnostics();
    for (const auto& d : diags) {
        out.push_back(std::make_unique<::slang::Diagnostic>(d));
    }
    return out;
}
} // namespace ast

namespace diagnostics {
inline static uint16_t code(const ::slang::Diagnostic& diag) {
    return diag.code.getCode();
}
} // namespace diagnostics
} // namespace wrapper
