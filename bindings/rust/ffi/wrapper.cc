#include "slang/bindings/rust/ffi.rs.h"

namespace wrapper {
namespace {

::RawSyntaxDiagnostic to_rust_syntax_diagnostic(const Diagnostic& diag,
                                                slang::DiagnosticEngine& engine) {
  ::RawSyntaxDiagnostic rust_diag;
  rust_diag.code = diag.code.getCode();
  rust_diag.subsystem = static_cast<uint16_t>(diag.code.getSubsystem());
  rust_diag.severity = static_cast<uint8_t>(engine.getSeverity(diag.code, diag.location));
  rust_diag.message = rust::String(engine.formatMessage(diag));
  rust_diag.primary_range_start = 0;
  rust_diag.primary_range_end = 0;
  rust_diag.has_primary_range = false;
  rust_diag.location = 0;
  rust_diag.has_location = false;

  if (!diag.ranges.empty() && diag.ranges.front() != SourceRange::NoLocation) {
    rust_diag.primary_range_start = diag.ranges.front().start().offset();
    rust_diag.primary_range_end = diag.ranges.front().end().offset();
    rust_diag.has_primary_range = true;
  }

  if (diag.location.valid()) {
    rust_diag.location = diag.location.offset();
    rust_diag.has_location = true;
  }

  return rust_diag;
}

} // namespace

namespace syntax {

rust::Vec<::RawSyntaxDiagnostic> SyntaxTree_diagnostics(const SyntaxTree& tree) {
  auto& diags = const_cast<SyntaxTree&>(tree).diagnostics();
  slang::DiagnosticEngine engine(tree.sourceManager());
  rust::Vec<::RawSyntaxDiagnostic> rust_diags;
  rust_diags.reserve(diags.size());
  for (const auto& diag : diags)
    rust_diags.emplace_back(to_rust_syntax_diagnostic(diag, engine));
  return rust_diags;
}

} // namespace syntax

namespace ast {

rust::Vec<::RawSyntaxDiagnostic> Compilation_semantic_diagnostics(const Compilation& compilation) {
  auto& diags = const_cast<Compilation&>(compilation).getSemanticDiagnostics();
  auto source_manager = compilation.getSourceManager();
  SLANG_ASSERT(source_manager);
  slang::DiagnosticEngine engine(*source_manager);
  rust::Vec<::RawSyntaxDiagnostic> rust_diags;
  rust_diags.reserve(diags.size());
  for (const auto& diag : diags)
    rust_diags.emplace_back(to_rust_syntax_diagnostic(diag, engine));
  return rust_diags;
}

} // namespace ast
} // namespace wrapper
