#pragma once
#include <cassert>
#include <cstddef>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/numeric/SVInt.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/text/SourceLocation.h"
#include "slang/text/SourceManager.h"
#include "slang/ast/Compilation.h"
#include "slang/ast/Symbol.h"
#include "slang/ast/Scope.h"
#include "slang/ast/types/Type.h"
#include "slang/ast/types/DeclaredType.h"
#include "slang/ast/symbols/ValueSymbol.h"
#include "slang/numeric/ConstantValue.h"
#include "slang/diagnostics/Diagnostics.h"
#include "rust/cxx.h"

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
  }

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
  }

  // SourceManager wrappers
  inline static rust::String SourceManager_getFileName(const ::slang::SourceManager& sm, const ::slang::SourceLocation& loc) {
    std::string_view sv = sm.getFileName(loc);
    return rust::String(std::string(sv));
  }

  inline static uint32_t SourceManager_getLineNumber(const ::slang::SourceManager& sm, const ::slang::SourceLocation& loc) {
    return sm.getLineNumber(loc);
  }

  inline static uint32_t SourceManager_getColumnNumber(const ::slang::SourceManager& sm, const ::slang::SourceLocation& loc) {
    return sm.getColumnNumber(loc);
  }

  inline static uint32_t SourceManager_assignTextDefault(std::string_view text) {
    auto& sm = ::slang::syntax::SyntaxTree::getDefaultSourceManager();
    auto buffer = sm.assignText(text);
    return buffer.id.getId();
  }

  inline static uint32_t SourceManager_assignTextWithPathDefault(std::string_view path, std::string_view text) {
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

  namespace ast {
    inline static std::unique_ptr<Compilation> Compilation_new() {
        return std::unique_ptr<Compilation>(new Compilation());
    }

    inline static void Compilation_add_syntax_tree(Compilation& compilation, std::shared_ptr<SyntaxTree> tree) {
        compilation.addSyntaxTree(tree);
    }

    inline static const ::slang::SourceManager* Compilation_getSourceManager(Compilation& comp) {
        return comp.getSourceManager();
    }

    inline static const ::slang::ast::Symbol* Compilation_getRoot(Compilation& comp) {
        // RootSymbol inherits from Symbol, use reinterpret_cast for incomplete type
        return reinterpret_cast<const ::slang::ast::Symbol*>(&comp.getRoot());
    }

    // Symbol wrappers
    inline static uint16_t Symbol_kind(const ::slang::ast::Symbol& symbol) {
        return static_cast<uint16_t>(symbol.kind);
    }

    inline static const ::slang::ast::Symbol* Symbol_getNextSibling(const ::slang::ast::Symbol& symbol) {
        return symbol.getNextSibling();
    }

    // Scope wrappers
    inline static const ::slang::ast::Symbol* Scope_asSymbol(const ::slang::ast::Scope& scope) {
        const ::slang::ast::Symbol& sym = scope.asSymbol();
        return &sym;
    }

    inline static const ::slang::ast::Symbol* Scope_getFirstMember(const ::slang::ast::Scope& scope) {
        return scope.getFirstMember();
    }

    // LookupLocation wrappers
    inline static std::unique_ptr<::slang::ast::LookupLocation> LookupLocation_max() {
        return std::make_unique<::slang::ast::LookupLocation>(::slang::ast::LookupLocation::max);
    }

    inline static std::unique_ptr<::slang::ast::LookupLocation> LookupLocation_before(const ::slang::ast::Symbol& symbol) {
        return std::make_unique<::slang::ast::LookupLocation>(::slang::ast::LookupLocation::before(symbol));
    }

    inline static std::unique_ptr<::slang::ast::LookupLocation> LookupLocation_after(const ::slang::ast::Symbol& symbol) {
        return std::make_unique<::slang::ast::LookupLocation>(::slang::ast::LookupLocation::after(symbol));
    }

    // LookupResult wrappers
    inline static std::unique_ptr<::slang::ast::LookupResult> LookupResult_new() {
        return std::make_unique<::slang::ast::LookupResult>();
    }

    inline static const ::slang::ast::Symbol* LookupResult_found(const ::slang::ast::LookupResult& result) {
        return result.found;
    }

    inline static bool LookupResult_hasError(const ::slang::ast::LookupResult& result) {
        return result.hasError();
    }

    // Scope lookup wrappers
    inline static const ::slang::ast::Symbol* Scope_lookupName(
        const ::slang::ast::Scope& scope,
        std::string_view name,
        const ::slang::ast::LookupLocation& location
    ) {
        return scope.lookupName(name, location);
    }

    inline static const ::slang::ast::Symbol* Scope_find(
        const ::slang::ast::Scope& scope,
        std::string_view name
    ) {
        return scope.find(name);
    }

    // Type wrappers
    inline static rust::String Type_toString(const ::slang::ast::Type& ty) {
        return rust::String(ty.toString());
    }

    inline static uint16_t Type_kind(const ::slang::ast::Type& ty) {
        return static_cast<uint16_t>(ty.kind);
    }

    // ValueSymbol wrappers
    inline static const ::slang::ast::ValueSymbol* Symbol_asValueSymbol(const ::slang::ast::Symbol& symbol) {
        return symbol.as_if<::slang::ast::ValueSymbol>();
    }

    inline static const ::slang::ast::DeclaredType* ValueSymbol_getType(const ::slang::ast::ValueSymbol& vs) {
        return vs.getDeclaredType();
    }

    inline static const ::slang::ast::Type* DeclaredType_getType(const ::slang::ast::DeclaredType& dt) {
        const ::slang::ast::Type& ty = dt.getType();
        return &ty;
    }

  }

  // ConstantValue wrappers
  inline static std::unique_ptr<SVInt> ConstantValue_integer(const ::slang::ConstantValue& cv) {
      return std::make_unique<SVInt>(cv.integer());
  }

  inline static double ConstantValue_real(const ::slang::ConstantValue& cv) {
      return cv.real();
  }

  inline static rust::String ConstantValue_str(const ::slang::ConstantValue& cv) {
      return rust::String(std::string(cv.str()));
  }

  namespace ast {
  inline static rust::Vec<std::unique_ptr<::slang::Diagnostic>> Compilation_get_all_diagnostics(Compilation& compilation) {
      rust::Vec<std::unique_ptr<::slang::Diagnostic>> out;
      const ::slang::Diagnostics& diags = compilation.getAllDiagnostics();
      for (const auto& d : diags) {
        out.push_back(std::make_unique<::slang::Diagnostic>(d));
      }
      return out;
    }
  }

  namespace diagnostics {
  inline static uint16_t code(const ::slang::Diagnostic& diag) {
    return diag.code.getCode();
  }
  }
}
