#pragma once
#include <cassert>
#include <cstddef>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/syntax/SyntaxKind.h"
#include "slang/numeric/SVInt.h"
#include "slang/parsing/LexerFacts.h"
#include "slang/parsing/Preprocessor.h"
#include "slang/parsing/TokenKind.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/text/SourceLocation.h"
#include "slang/text/SourceManager.h"
#include "slang/ast/Compilation.h"
#include "slang/diagnostics/Diagnostics.h"
#include "slang/util/Bag.h"
#include "rust/cxx.h"

namespace wrapper {
  using SyntaxTrivia = ::slang::parsing::Trivia;
  using SyntaxToken = ::slang::parsing::Token;
  using SVInt = ::slang::SVInt;
  using logic_t = ::slang::logic_t;
  using SourceRange = ::slang::SourceRange;
  using SyntaxTree = ::slang::syntax::SyntaxTree;
  using SyntaxNode = ::slang::syntax::SyntaxNode;
  using DefineDirectiveSyntax = ::slang::syntax::DefineDirectiveSyntax;
  using Compilation = ::slang::ast::Compilation;
  using Diagnostic = ::slang::Diagnostic;

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
    inline static rust::Vec<rust::String> verilog_2005_keywords() {
      rust::Vec<rust::String> out;
      auto* table = slang::parsing::LexerFacts::getKeywordTable(
          slang::parsing::KeywordVersion::v1364_2005);
      if (!table)
        return out;
      for (const auto& it : *table) {
        out.push_back(rust::String(std::string(it.first)));
      }
      return out;
    }

    inline static rust::Vec<rust::String> keyword_table_for_version(rust::Str version) {
      rust::Vec<rust::String> out;
      std::string_view v(version.data(), version.size());
      auto opt = slang::parsing::LexerFacts::getKeywordVersion(v);
      if (!opt)
        return out;
      auto* table = slang::parsing::LexerFacts::getKeywordTable(*opt);
      if (!table)
        return out;
      for (const auto& it : *table) {
        out.push_back(rust::String(std::string(it.first)));
      }
      return out;
    }

    inline static rust::String token_kind_text(uint16_t kind_id) {
      auto kind = static_cast<slang::parsing::TokenKind>(kind_id);
      return rust::String(
          std::string(slang::parsing::LexerFacts::getTokenKindText(kind)));
    }

    inline static uint16_t directive_kind(rust::Str directive, bool enableLegacyProtect) {
      std::string_view v(directive.data(), directive.size());
      auto kind = slang::parsing::LexerFacts::getDirectiveKind(v, enableLegacyProtect);
      return static_cast<uint16_t>(kind);
    }

    inline static rust::String directive_text(uint16_t kind_id) {
      auto kind = static_cast<slang::syntax::SyntaxKind>(kind_id);
      return rust::String(
          std::string(slang::parsing::LexerFacts::getDirectiveText(kind)));
    }

    // Trivia
    inline static uint8_t SyntaxTrivia_kind(const SyntaxTrivia& trivia) {
      return static_cast<uint8_t>(trivia.kind);
    }

    inline static const SyntaxNode* SyntaxTrivia_syntax(const SyntaxTrivia& trivia) {
      return trivia.syntax();
    }

    inline static std::string_view SyntaxTrivia_directive_token_raw_text(const SyntaxTrivia& trivia) {
      if (trivia.kind != slang::parsing::TriviaKind::Directive)
        return {};
      auto* node = trivia.syntax();
      if (!node)
        return {};
      return node->getFirstToken().rawText();
    }

    inline static std::unique_ptr<SourceRange> SyntaxTrivia_directive_token_range(const SyntaxTrivia& trivia) {
      if (trivia.kind != slang::parsing::TriviaKind::Directive)
        return nullptr;
      auto* node = trivia.syntax();
      if (!node)
        return nullptr;
      auto range = node->getFirstToken().range();
      return range == SourceRange::NoLocation ? nullptr : std::make_unique<SourceRange>(range);
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

    inline static uint16_t SyntaxToken_directive_kind(const SyntaxToken& token) {
      return static_cast<uint16_t>(token.directiveKind());
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

    inline static std::shared_ptr<SyntaxTree> syntax_tree_from_text_with_options(
        std::string_view text, std::string_view name, std::string_view path,
        const rust::Vec<rust::String>& predefines,
        const rust::Vec<rust::String>& include_paths) {
      if (predefines.empty() && include_paths.empty()) {
        return SyntaxTree::fromText(text, name, path);
      }

      slang::Bag options;
      slang::parsing::PreprocessorOptions ppOptions;
      // Keep in sync with base_db::preprocessor::PREDEFINE_SOURCE_NAME.
      ppOptions.predefineSource = "<vizsla_config>";

      ppOptions.predefines.reserve(predefines.size());
      for (const auto& entry : predefines) {
        ppOptions.predefines.emplace_back(std::string(entry.data(), entry.size()));
      }

      ppOptions.additionalIncludePaths.reserve(include_paths.size());
      for (const auto& entry : include_paths) {
        ppOptions.additionalIncludePaths.emplace_back(std::string(entry.data(), entry.size()));
      }

      options.set(ppOptions);
      return SyntaxTree::fromText(text, options, name, path);
    }

    inline static const SyntaxNode* SyntaxTree_root(const SyntaxTree& tree) {
      return &tree.root();
    }

    inline static size_t SyntaxTree_defined_macro_count(const SyntaxTree& tree) {
      return tree.getDefinedMacros().size();
    }

    inline static const DefineDirectiveSyntax* SyntaxTree_defined_macro(const SyntaxTree& tree,
                                                                        size_t index) {
      auto macros = tree.getDefinedMacros();
      if (index >= macros.size()) {
        return nullptr;
      }
      return macros[index];
    }

    inline static bool DefineDirectiveSyntax_location(
        const SyntaxTree& tree, const DefineDirectiveSyntax& syntax, rust::String& file,
        size_t& start, size_t& end) {
      auto range = syntax.sourceRange();
      if (range == SourceRange::NoLocation) {
        file = rust::String();
        start = 0;
        end = 0;
        return false;
      }

      auto startLoc = range.start();
      auto endLoc = range.end();
      auto& sourceManager = tree.sourceManager();
      auto& fullPath = sourceManager.getFullPath(startLoc.buffer());
      if (!fullPath.empty()) {
        file = rust::String(fullPath.string());
        start = startLoc.offset();
        end = endLoc.offset();
        return true;
      }

      auto fileName = sourceManager.getFileName(startLoc);
      file = rust::String(std::string(fileName));
      start = startLoc.offset();
      end = endLoc.offset();
      return true;
    }

    inline static const SyntaxToken* DefineDirectiveSyntax_name(
        const DefineDirectiveSyntax& syntax) {
      return &syntax.name;
    }

    inline static std::unique_ptr<SourceRange> DefineDirectiveSyntax_range(
        const DefineDirectiveSyntax& syntax) {
      auto range = syntax.sourceRange();
      return range == SourceRange::NoLocation ? nullptr : std::make_unique<SourceRange>(range);
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

  namespace ast {
    inline static std::unique_ptr<Compilation> Compilation_new() {
        return std::unique_ptr<Compilation>(new Compilation());
    }

    inline static void Compilation_add_syntax_tree(Compilation& compilation, std::shared_ptr<SyntaxTree> tree) {
        compilation.addSyntaxTree(tree);
    }

    inline static rust::Vec<std::unique_ptr<Diagnostic>> Compilation_get_all_diagnostics(const Compilation& compilation) {

    }
  }

  namespace diagnostics {
    inline static uint16_t code(const Diagnostic& diag) {
        return diag.code.getCode();
    }
  }
}
