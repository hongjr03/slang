use ast::{
    AstNode, CompilationUnit, Expression, LiteralExpression, Member, Name, PrimaryExpression,
};
use expect_test::expect;
use smol_str::SmolStr;
use text_size::{TextRange, TextSize};

use super::*;
use crate::{ArgumentDirection, RandMode, SubroutineKind, Visibility};
fn get_test_tree() -> SyntaxTree {
    SyntaxTree::from_text("module A(input a); wire x; endmodule;", "source", "")
}

fn get_empty_tree() -> SyntaxTree {
    SyntaxTree::from_text("", "source", "")
}

fn get_multi_module_tree() -> SyntaxTree {
    SyntaxTree::from_text("module A; endmodule; module B; endmodule;", "source", "")
}

fn get_tree_with_trivia() -> SyntaxTree {
    SyntaxTree::from_text(
        r#"
module A();

C #(.l(1)) c();
endmodule

"#,
        "source",
        "",
    )
}

fn get_complex_tree() -> SyntaxTree {
    SyntaxTree::from_text(
        r#"
module A(
  input a,
  /decode,
  output b,
);
endmodule;"#,
        "source",
        "",
    )
}

fn dfs(node: SyntaxNode, depth: usize, ans: &mut String) {
    let range = node.range();
    let kind = node.kind();
    let child_count = node.child_count();
    if let Some(range) = range {
        *ans +=
            &format!("{:indent$}{kind:?} {range:?} (cnt: {child_count})\n", "", indent = depth * 2);
    } else {
        assert!(kind == SyntaxKind::UNKNOWN || kind.is_list());
        *ans += &format!("{:indent$}{kind:?} (cnt: {child_count})\n", "", indent = depth * 2);
    }

    for i in 0..child_count {
        if let Some(node) = node.child_node(i) {
            dfs(node, depth + 1, ans);
        } else if let Some(tok) = node.child_token(i) {
            tok.trivias_with_loc().for_each(|((start, end), trivia)| {
                *ans += &format!(
                    "{:indent$}{:?} {start}..{end} (trivia)\n",
                    "",
                    trivia.kind(),
                    indent = (depth + 1) * 2
                );
            });

            if let Some(range) = tok.range() {
                *ans += &format!(
                    "{:indent$}{:?} {range:?}\n",
                    "",
                    tok.kind(),
                    indent = (depth + 1) * 2
                );
            } else {
                *ans += &format!("{:indent$}{:?}\n", "", tok.kind(), indent = (depth + 1) * 2);
            }
        }
    }
}

#[test]
fn parse() {
    let tree = get_test_tree();
    let root = tree.root().unwrap();
    let mut ans = String::new();
    dfs(root, 0, &mut ans);

    let expected = expect![[r#"
        CompilationUnit 0..37 (cnt: 2)
          SyntaxList 0..37 (cnt: 2)
            ModuleDeclaration 0..36 (cnt: 5)
              SyntaxList (cnt: 0)
              ModuleHeader 0..18 (cnt: 7)
                ModuleKeyword 0..6
                Whitespace 6..7 (trivia)
                Identifier 7..8
                SyntaxList (cnt: 0)
                AnsiPortList 8..17 (cnt: 3)
                  OpenParenthesis 8..9
                  SeparatedList 9..16 (cnt: 1)
                    ImplicitAnsiPort 9..16 (cnt: 3)
                      SyntaxList (cnt: 0)
                      VariablePortHeader 9..15 (cnt: 4)
                        InputKeyword 9..14
                        ImplicitType 15..15 (cnt: 3)
                          SyntaxList (cnt: 0)
                          Placeholder 15..15
                      Declarator 15..16 (cnt: 3)
                        Whitespace 14..15 (trivia)
                        Identifier 15..16
                        SyntaxList (cnt: 0)
                  CloseParenthesis 16..17
                Semicolon 17..18
              SyntaxList 19..26 (cnt: 1)
                NetDeclaration 19..26 (cnt: 8)
                  SyntaxList (cnt: 0)
                  Whitespace 18..19 (trivia)
                  WireKeyword 19..23
                  ImplicitType 24..24 (cnt: 3)
                    SyntaxList (cnt: 0)
                    Placeholder 24..24
                  SeparatedList 24..25 (cnt: 1)
                    Declarator 24..25 (cnt: 3)
                      Whitespace 23..24 (trivia)
                      Identifier 24..25
                      SyntaxList (cnt: 0)
                  Semicolon 25..26
              Whitespace 26..27 (trivia)
              EndModuleKeyword 27..36
            EmptyMember 36..37 (cnt: 3)
              SyntaxList (cnt: 0)
              TokenList (cnt: 0)
              Semicolon 36..37
          EndOfFile 37..37
    "#]];
    expected.assert_eq(&ans);
}

#[test]
fn multiple_module() {
    let tree = get_multi_module_tree();
    let root = tree.root().unwrap();
    let mut ans = String::new();
    dfs(root, 0, &mut ans);

    let expected = expect![[r#"
        CompilationUnit 0..41 (cnt: 2)
          SyntaxList 0..41 (cnt: 4)
            ModuleDeclaration 0..19 (cnt: 5)
              SyntaxList (cnt: 0)
              ModuleHeader 0..9 (cnt: 7)
                ModuleKeyword 0..6
                Whitespace 6..7 (trivia)
                Identifier 7..8
                SyntaxList (cnt: 0)
                Semicolon 8..9
              SyntaxList (cnt: 0)
              Whitespace 9..10 (trivia)
              EndModuleKeyword 10..19
            EmptyMember 19..20 (cnt: 3)
              SyntaxList (cnt: 0)
              TokenList (cnt: 0)
              Semicolon 19..20
            ModuleDeclaration 21..40 (cnt: 5)
              SyntaxList (cnt: 0)
              ModuleHeader 21..30 (cnt: 7)
                Whitespace 20..21 (trivia)
                ModuleKeyword 21..27
                Whitespace 27..28 (trivia)
                Identifier 28..29
                SyntaxList (cnt: 0)
                Semicolon 29..30
              SyntaxList (cnt: 0)
              Whitespace 30..31 (trivia)
              EndModuleKeyword 31..40
            EmptyMember 40..41 (cnt: 3)
              SyntaxList (cnt: 0)
              TokenList (cnt: 0)
              Semicolon 40..41
          EndOfFile 41..41
    "#]];
    expected.assert_eq(&ans);
}

#[test]
fn no_location() {
    let tree = get_empty_tree();
    let root = tree.root().unwrap();
    let node = root.child_node(0).unwrap();
    let range = node.range();
    assert!(range.is_none());
}

#[test]
fn kind() {
    let tree = get_test_tree();
    let root = tree.root().unwrap();

    assert_eq!(root.kind(), SyntaxKind::COMPILATION_UNIT);
}

#[test]
fn test_partial_eq_syntax_node() {
    let tree = get_complex_tree();
    let root = tree.root().unwrap();
    let node = root.child_node(0).unwrap();
    let node2 = root.child_node(0).unwrap();
    assert!(node == node2);
}

#[test]
fn parse_complex() {
    let tree = get_complex_tree();
    let root = tree.root().unwrap();
    let mut ans = String::new();
    dfs(root, 0, &mut ans);

    let expected = expect![[r#"
        CompilationUnit 1..58 (cnt: 2)
          SyntaxList 1..58 (cnt: 2)
            ModuleDeclaration 1..57 (cnt: 5)
              SyntaxList (cnt: 0)
              ModuleHeader 1..47 (cnt: 7)
                EndOfLine 0..1 (trivia)
                ModuleKeyword 1..7
                Whitespace 7..8 (trivia)
                Identifier 8..9
                SyntaxList (cnt: 0)
                AnsiPortList 9..46 (cnt: 3)
                  OpenParenthesis 9..10
                  SeparatedList 13..44 (cnt: 6)
                    ImplicitAnsiPort 13..20 (cnt: 3)
                      SyntaxList (cnt: 0)
                      VariablePortHeader 13..19 (cnt: 4)
                        EndOfLine 10..11 (trivia)
                        Whitespace 11..13 (trivia)
                        InputKeyword 13..18
                        ImplicitType 19..19 (cnt: 3)
                          SyntaxList (cnt: 0)
                          Placeholder 19..19
                      Declarator 19..20 (cnt: 3)
                        Whitespace 18..19 (trivia)
                        Identifier 19..20
                        SyntaxList (cnt: 0)
                    Comma 20..21
                    ImplicitAnsiPort 24..21 (cnt: 3)
                      SyntaxList (cnt: 0)
                      VariablePortHeader 24..24 (cnt: 4)
                        ImplicitType 24..24 (cnt: 3)
                          SyntaxList (cnt: 0)
                          Placeholder 24..24
                      Declarator 21..21 (cnt: 3)
                        Identifier 21..21
                        SyntaxList (cnt: 0)
                    SkippedTokens 31..31 (trivia)
                    Comma 31..32
                    ImplicitAnsiPort 35..43 (cnt: 3)
                      SyntaxList (cnt: 0)
                      VariablePortHeader 35..42 (cnt: 4)
                        EndOfLine 32..33 (trivia)
                        Whitespace 33..35 (trivia)
                        OutputKeyword 35..41
                        ImplicitType 42..42 (cnt: 3)
                          SyntaxList (cnt: 0)
                          Placeholder 42..42
                      Declarator 42..43 (cnt: 3)
                        Whitespace 41..42 (trivia)
                        Identifier 42..43
                        SyntaxList (cnt: 0)
                    Comma 43..44
                  EndOfLine 44..45 (trivia)
                  CloseParenthesis 45..46
                Semicolon 46..47
              SyntaxList (cnt: 0)
              EndOfLine 47..48 (trivia)
              EndModuleKeyword 48..57
            EmptyMember 57..58 (cnt: 3)
              SyntaxList (cnt: 0)
              TokenList (cnt: 0)
              Semicolon 57..58
          EndOfFile 58..58
    "#]];
    expected.assert_eq(&ans);
}

#[test]
fn test_literals() {
    let tree = SyntaxTree::from_text(
        r#"
module A();
  wire y = 3s;
  wire x = 7'b0010xx10;
  wire z = 12;
  wire p = 'x;
  wire empty = ;
endmodule;"#,
        "source",
        "",
    );
    let root = tree.root().unwrap();

    let unit = CompilationUnit::cast(root).unwrap();
    let module = match unit.members().children().next().unwrap() {
        Member::ModuleDeclaration(module) => module,
        _ => unreachable!("expected module declaration"),
    };
    let mut literals = module.members().children().map(|member| {
        let Member::NetDeclaration(decl) = member else {
            unreachable!("expected net declaration");
        };
        let decl = decl.declarators().children().next().unwrap();
        let lit = decl.initializer().unwrap().expr();
        lit
    });

    let Expression::PrimaryExpression(PrimaryExpression::LiteralExpression(
        LiteralExpression::TimeLiteralExpression(time_lit),
    )) = literals.next().unwrap()
    else {
        unreachable!("expected time literal");
    };
    let time_tok = time_lit.child_token(0).unwrap();
    assert_eq!(time_tok.time_unit(), Some(TimeUnit::Seconds));
    assert_eq!(time_tok.base(), None);
    assert!((time_tok.real().unwrap() - 3.0) < f64::EPSILON);

    let Expression::PrimaryExpression(PrimaryExpression::IntegerVectorExpression(vec_lit)) =
        literals.next().unwrap()
    else {
        unreachable!("expected integer vector literal");
    };
    assert_eq!(vec_lit.size().unwrap().int().unwrap().get_single_word(), Some(7));
    assert_eq!(vec_lit.base().unwrap().base(), Some(LiteralBase::Bin));
    assert_eq!(vec_lit.value().unwrap().int().unwrap().serialize(2), "10xx10");

    let Expression::PrimaryExpression(PrimaryExpression::LiteralExpression(
        LiteralExpression::IntegerLiteralExpression(int_lit),
    )) = literals.next().unwrap()
    else {
        unreachable!("expected integer literal");
    };
    let int_tok = int_lit.child_token(0).unwrap();
    let svint = int_tok.int().unwrap();
    assert_eq!(svint.to_string(), "12");
    assert_eq!(svint.get_single_word(), Some(12));

    let svint2 = svint.clone();
    assert_eq!(svint, svint2);

    let Expression::PrimaryExpression(PrimaryExpression::LiteralExpression(
        LiteralExpression::UnbasedUnsizedLiteralExpression(unbased_lit),
    )) = literals.next().unwrap()
    else {
        unreachable!("expected unbased unsized literal");
    };
    let unbased_tok = unbased_lit.child_token(0).unwrap();
    assert_eq!(unbased_tok.bits().unwrap().bit(), Bit::X);

    let Expression::Name(Name::IdentifierName(name)) = literals.next().unwrap() else {
        unreachable!("expected identifier");
    };
    assert!(name.identifier().unwrap().value_text().to_string().is_empty());
}

#[test]
fn test_trivia() {
    let tree = get_tree_with_trivia();
    let root = tree.root().unwrap();
    let mut ans = String::new();
    dfs(root, 0, &mut ans);

    let expected = expect![[r#"
        CompilationUnit 1..41 (cnt: 2)
          SyntaxList 1..39 (cnt: 1)
            ModuleDeclaration 1..39 (cnt: 5)
              SyntaxList (cnt: 0)
              ModuleHeader 1..12 (cnt: 7)
                EndOfLine 0..1 (trivia)
                ModuleKeyword 1..7
                Whitespace 7..8 (trivia)
                Identifier 8..9
                SyntaxList (cnt: 0)
                AnsiPortList 9..11 (cnt: 3)
                  OpenParenthesis 9..10
                  SeparatedList (cnt: 0)
                  CloseParenthesis 10..11
                Semicolon 11..12
              SyntaxList 14..29 (cnt: 1)
                HierarchyInstantiation 14..29 (cnt: 5)
                  SyntaxList (cnt: 0)
                  EndOfLine 12..13 (trivia)
                  EndOfLine 13..14 (trivia)
                  Identifier 14..15
                  ParameterValueAssignment 16..24 (cnt: 4)
                    Whitespace 15..16 (trivia)
                    Hash 16..17
                    OpenParenthesis 17..18
                    SeparatedList 18..23 (cnt: 1)
                      NamedParamAssignment 18..23 (cnt: 5)
                        Dot 18..19
                        Identifier 19..20
                        OpenParenthesis 20..21
                        IntegerLiteralExpression 21..22 (cnt: 1)
                          IntegerLiteral 21..22
                        CloseParenthesis 22..23
                    CloseParenthesis 23..24
                  SeparatedList 25..28 (cnt: 1)
                    HierarchicalInstance 25..28 (cnt: 4)
                      InstanceName 25..26 (cnt: 2)
                        Whitespace 24..25 (trivia)
                        Identifier 25..26
                        SyntaxList (cnt: 0)
                      OpenParenthesis 26..27
                      SeparatedList (cnt: 0)
                      CloseParenthesis 27..28
                  Semicolon 28..29
              EndOfLine 29..30 (trivia)
              EndModuleKeyword 30..39
          EndOfLine 39..40 (trivia)
          EndOfLine 40..41 (trivia)
          EndOfFile 41..41
    "#]];
    expected.assert_eq(&ans);
}

#[test]
fn test_compilation() {
    let mut compilation = Compilation::new();
    let tree = get_test_tree();
    compilation.add_syntax_tree(tree).expect("add syntax tree");
}

#[test]
fn source_manager_assign_text_roundtrip() {
    let text = "module top; endmodule";
    let buffer = source_manager::assign_text(text).expect("buffer id");
    let roundtrip = source_manager::source_text(buffer).expect("text");
    assert_eq!(roundtrip, text);
}

#[test]
fn source_manager_assign_text_with_path() {
    let text = "module foo; endmodule";
    let buffer = source_manager::assign_text_with_path("virtual/foo.sv", text).expect("buffer id");
    assert_ne!(buffer.raw(), 0);
    let roundtrip = source_manager::source_text(buffer).expect("text");
    assert_eq!(roundtrip, text);
}

#[test]
fn source_manager_location_roundtrip() {
    let text = "module bar; endmodule";
    let buffer = source_manager::assign_text(text).expect("buffer id");
    let offset = TextSize::from(7u32);
    let loc = source_manager::make_location(buffer, offset).expect("location");
    assert_eq!(loc.offset(), Some(7));
    assert_eq!(loc.buffer_id(), Some(buffer));
}

#[test]
fn compilation_get_package_and_scope_lookup() {
    let tree = SyntaxTree::from_text("package foo; int bar; int baz; endpackage", "source", "");

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let package = compilation.get_package("foo").expect("get package").expect("package symbol");
    let scope = package.scope().expect("package scope");

    assert!(scope.find_member("bar").is_some());
    assert!(scope.lookup_name("baz", None).is_some());
}

#[test]
fn compilation_root_scope_iteration() {
    let tree = SyntaxTree::from_text("module foo; endmodule", "source", "");
    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let root = compilation.root().expect("root symbol");
    let scope = root.as_scope().expect("root scope");

    let mut members = scope.members();
    assert!(members.next().is_some(), "expected root scope to contain members");
}

#[test]
fn compilation_upsert_rebuilds_on_version_change() {
    let mut compilation = Compilation::new();

    let tree_v1 = SyntaxTree::from_text("module foo; endmodule", "foo", "");
    assert!(compilation.upsert_syntax_tree("foo.sv", 1, tree_v1).expect("upsert tree"));

    // Calling root finalizes current compilation; subsequent upserts should
    // rebuild.
    assert!(compilation.root().is_ok());

    let tree_same = SyntaxTree::from_text("module foo; endmodule", "foo", "");
    assert!(!compilation.upsert_syntax_tree("foo.sv", 1, tree_same).expect("upsert tree"));

    let tree_v2 = SyntaxTree::from_text("module foo; wire x; endmodule", "foo", "");
    assert!(compilation.upsert_syntax_tree("foo.sv", 2, tree_v2).expect("upsert tree"));

    assert!(compilation.remove_syntax_tree("foo.sv").expect("remove tree"));
    assert!(!compilation.remove_syntax_tree("missing.sv").expect("remove tree"));
}

#[test]
fn source_manager_instance_assign_text() {
    let mut compilation = Compilation::new();
    let mut sm = compilation.source_manager().expect("source manager");

    let buffer = sm.assign_text("module inline; endmodule").expect("buffer id");
    let text = sm.source_text(buffer).expect("source text");
    assert_eq!(text, "module inline; endmodule");

    let loc = sm.make_location(buffer, TextSize::from(0u32)).expect("location");
    assert_eq!(loc.offset(), Some(0));
}

#[test]
fn scope_visible_symbols_collected() {
    let tree = SyntaxTree::from_text("package foo; int bar; int baz; endpackage", "source", "");

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let package = compilation.get_package("foo").expect("get package").expect("package symbol");
    let scope = package.scope().expect("package scope");

    let names: std::collections::HashSet<_> = scope
        .visible_symbols()
        .expect("visible symbols")
        .into_iter()
        .map(|sym| sym.name())
        .collect();

    for expected in [SmolStr::new("bar"), SmolStr::new("baz")] {
        let sym = scope.lookup_name(expected.as_str(), None).expect("visible symbol");
        assert!(sym.is_value(), "{} should be value", expected);
        assert!(names.contains(&expected), "missing symbol {expected}");
    }
}

#[test]
fn scope_visible_symbols_include_imports() {
    let tree = SyntaxTree::from_text(
        r#"
package pkg;
  int imported_var;
  typedef int imported_typedef;
endpackage

package user;
  import pkg::*;
  int own_var;
endpackage
"#,
        "source",
        "",
    );

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let package = compilation.get_package("user").expect("get package").expect("user package");
    let scope = package.scope().expect("package scope");

    let names: std::collections::HashSet<_> = scope
        .visible_symbols()
        .expect("visible symbols")
        .into_iter()
        .map(|sym| sym.name())
        .collect();

    for expected in [SmolStr::new("imported_var"), SmolStr::new("own_var")] {
        let sym = scope.lookup_name(expected.as_str(), None).expect("value symbol");
        assert!(sym.is_value(), "{} should be value", expected);
        assert!(names.contains(&expected), "missing symbol {expected}");
    }

    let type_symbols =
        scope.visible_symbols_with(scope, None, LookupFlags::TYPE).expect("type symbols");
    let type_names: std::collections::HashSet<_> =
        type_symbols.iter().map(|sym| sym.name()).collect();

    assert!(type_names.contains(&SmolStr::new("imported_typedef")), "missing imported type");
    let imported_type = type_symbols
        .iter()
        .copied()
        .find(|sym| sym.name() == SmolStr::new("imported_typedef"))
        .expect("imported typedef symbol");
    assert!(imported_type.is_type(), "imported_typedef should be type");
    assert!(imported_type.as_type().is_some(), "imported_typedef should cast to type");
}

#[test]
fn scope_lookup_hierarchy() {
    let tree = SyntaxTree::from_text(
        r#"
module leaf;
  int value;
endmodule

module mid;
  leaf u_leaf();
endmodule

module top;
  mid u_mid();
endmodule
"#,
        "hierarchy",
        "",
    );

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let root = compilation.root().expect("root symbol");
    let root_scope = root.as_scope().expect("root scope");

    let leaf_instance = root_scope.lookup_hierarchy("top.u_mid.u_leaf").expect("leaf instance");
    assert_eq!(leaf_instance.name(), SmolStr::new("u_leaf"));
}

macro_rules! lookup_symbol {
    ($scope:expr, $path:expr, $desc:expr,as_variable) => {
        $scope.lookup_hierarchy($path).expect($desc).as_variable().expect("variable symbol")
    };
    ($scope:expr, $path:expr, $desc:expr,as_type) => {
        $scope.lookup_hierarchy($path).expect($desc).as_type().expect("type symbol")
    };
    ($scope:expr, $path:expr, $desc:expr,as_subroutine) => {
        $scope.lookup_hierarchy($path).expect($desc).as_subroutine().expect("subroutine symbol")
    };
    ($scope:expr, $path:expr, $desc:expr,as_value_symbol) => {
        $scope.lookup_hierarchy($path).expect($desc).as_value_symbol().expect("value symbol")
    };
}

macro_rules! assert_var_lifetime {
    ($scope:expr, $path:expr, $expected_lifetime:expr, $desc:expr) => {
        let var = lookup_symbol!($scope, $path, $desc, as_variable);
        assert_eq!(var.lifetime(), $expected_lifetime, "lifetime mismatch for {}", $desc);
    };
}

macro_rules! assert_var_flags {
    ($scope:expr, $path:expr, $flag:expr, $has_flag:expr, $desc:expr) => {
        let var = lookup_symbol!($scope, $path, $desc, as_variable);
        if $has_flag {
            assert!(var.flags().contains($flag), "flag should be set for {}", $desc);
        } else {
            assert!(!var.flags().contains($flag), "flag should not be set for {}", $desc);
        }
    };
}

#[test]
fn symbol_wrappers_parameter_field_method() {
    let tree = SyntaxTree::from_text(
        r#"
package foo;
  localparam int LP = 42;
  typedef struct packed {
    int field;
  } st_t;
  st_t inst;
  function automatic int add(input int a, output int b);
    return a + b;
  endfunction
endpackage
"#,
        "source",
        "",
    );

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let package = compilation.get_package("foo").expect("get package").expect("package symbol");
    let scope = package.scope().expect("package scope");

    let lp_symbol = scope.lookup_name("LP", None).expect("parameter symbol");
    assert!(lp_symbol.is_value());
    assert!(lp_symbol.ty().is_some());
    let parameter = lp_symbol.as_parameter().expect("parameter wrapper");
    assert!(parameter.is_local());
    assert!(!parameter.is_port());
    let param_value = parameter.value().expect("parameter value");
    assert!(param_value.is_valid());
    assert_eq!(param_value.to_string(false), "42");
    let param_svint = param_value.as_svint().expect("parameter as svint");
    assert_eq!(param_svint.to_string(), "42");
    let _param_type = parameter
        .declared_type()
        .expect("parameter declared type")
        .and_then(|dt| dt.ty())
        .expect("parameter type");

    let typedef_symbol = scope.lookup_name("st_t", None).expect("typedef symbol");
    assert!(typedef_symbol.is_type());
    let typedef_type = typedef_symbol.as_type().expect("typedef type");
    assert!(typedef_type.to_string().contains("struct"));
    let inst_symbol = scope.lookup_name("inst", None).expect("instance symbol");
    let inst_value = inst_symbol.as_value_symbol().expect("value symbol");
    let ty = inst_value.ty().expect("instance type");
    let fields = ty.fields().expect("fields");
    assert_eq!(fields.len(), 1);
    let field = fields[0];
    assert_eq!(field.rand_mode(), Some(RandMode::None));
    let field_type = field
        .value_symbol()
        .expect("field value symbol")
        .declared_type()
        .and_then(|dt| dt.ty())
        .expect("field type");
    assert_eq!(field_type.to_string(), "int");
    assert_eq!(field_type.bit_width(), Some(32));
    assert!(field_type.is_signed());
    assert!(field_type.integral_flags().contains(IntegralFlags::SIGNED));
    let default_value = field_type.default_value().expect("default value");
    assert!(default_value.is_valid());
    assert!(default_value.is_integer());
    assert_eq!(default_value.as_svint().expect("default as svint").to_string(), "0");

    let add_symbol = scope.lookup_name("add", None).expect("function symbol");
    let subroutine = lookup_symbol!(scope, "add", "function symbol", as_subroutine);
    assert_eq!(subroutine.kind(), Some(SubroutineKind::Function));
    assert_eq!(subroutine.visibility(), Some(Visibility::Public));
    assert_eq!(subroutine.return_type().expect("return type").to_string(), "int");

    let args = subroutine.arguments().expect("arguments");
    assert_eq!(args.len(), 2);
    assert_eq!(args[0].direction(), Some(ArgumentDirection::In));
    assert_eq!(args[1].direction(), Some(ArgumentDirection::Out));
    assert_eq!(
        args[0]
            .declared_type()
            .expect("arg declared type")
            .and_then(|dt| dt.ty())
            .expect("arg type")
            .to_string(),
        "int"
    );
}

#[test]
fn symbol_direction_and_lifetime() {
    let tree = SyntaxTree::from_text(
        r#"
module top(
  input logic a,
  output logic b,
  inout wire c
);
  int module_var;

  function automatic void foo(input int arg_in, ref int arg_ref);
    automatic int local_auto;
    static int local_static;
  endfunction
endmodule

"#,
        "dir-life",
        "",
    );

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let root = compilation.root().expect("root symbol");
    let scope = root.as_scope().expect("root scope");

    let module_var = scope.lookup_hierarchy("top.module_var").expect("module var");
    assert_eq!(module_var.lifetime(), Some(VariableLifetime::Static));
    assert!(module_var.direction().is_none());

    let foo_symbol = scope.lookup_hierarchy("top.foo").expect("foo function");
    let foo_subroutine = foo_symbol.as_subroutine().expect("foo subroutine");
    let foo_scope = foo_subroutine.scope().expect("foo scope");

    let arg_in = foo_scope.lookup_name("arg_in", None).expect("arg_in");
    assert_eq!(arg_in.direction(), Some(ArgumentDirection::In));
    assert_eq!(arg_in.lifetime(), Some(VariableLifetime::Automatic));

    let arg_ref = foo_scope.lookup_name("arg_ref", None).expect("arg_ref");
    assert_eq!(arg_ref.direction(), Some(ArgumentDirection::Ref));

    let local_auto = foo_scope.lookup_name("local_auto", None).expect("local_auto");
    assert_eq!(local_auto.lifetime(), Some(VariableLifetime::Automatic));
    assert!(local_auto.direction().is_none());

    let local_static = foo_scope.lookup_name("local_static", None).expect("local_static");
    assert_eq!(local_static.lifetime(), Some(VariableLifetime::Static));
}

#[test]
fn doc_comment_extraction() {
    let tree = SyntaxTree::from_text(
        r#"
module top;
  /// Adds foo.
  /// Multiline doc.
  int foo;

  /**
   * Block doc summary
   *
   * more detail
   */
  function int bar();
    return 0;
  endfunction

  // regular comment
  int baz;
endmodule
"#,
        "doc-test",
        "",
    );

    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let root = compilation.root().expect("root");
    let scope = root.as_scope().expect("root scope");

    let foo = scope.lookup_hierarchy("top.foo").expect("foo symbol");
    assert_eq!(foo.doc_comment().as_deref(), Some("Adds foo.\nMultiline doc."));
    assert_eq!(foo.leading_comment_lines().unwrap(), vec!["Adds foo.", "Multiline doc."]);

    let bar = scope.lookup_hierarchy("top.bar").expect("bar symbol");
    assert_eq!(bar.doc_comment().as_deref(), Some("Block doc summary\n\nmore detail"));

    let baz = scope.lookup_hierarchy("top.baz").expect("baz symbol");
    assert!(baz.doc_comment().is_none());
    assert_eq!(baz.leading_comment_lines().unwrap(), vec!["regular comment"]);
}

#[test]
fn source_range_from_text_range() {
    let tree = SyntaxTree::from_text("module p; endmodule", "buffer", "");
    let mut compilation = Compilation::new();
    compilation.add_syntax_tree(tree).expect("add syntax tree");

    let mut source_manager = compilation.source_manager().expect("source manager");
    let buffer = source_manager.assign_text("module p; endmodule").expect("buffer id");

    let range = TextRange::new(TextSize::from(0), TextSize::from(6));
    let source_range =
        source_manager.source_range_from_text_range(buffer, range).expect("source range");

    assert_eq!(source_range.start(), 0);
    assert_eq!(source_range.end(), 6);
}
