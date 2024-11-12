use ast::{
    AstNode, CompilationUnit, Expression, LiteralExpression, Member, Name, PrimaryExpression,
};
use expect_test::expect;
use itertools::Itertools;

use super::*;

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
// testA
// testB
wire a = 1;
"#,
        "source",
        "",
    )
}

fn get_complex_tree() -> SyntaxTree {
    SyntaxTree::from_text(
        r#"
module A (input a,);
endmodule"#,
        "source",
        "",
    )
}

fn dfs(node: SyntaxNode, depth: usize, ans: &mut String) {
    let range = node.range();
    let kind = node.kind();
    let child_count = node.child_count();
    if let Some(range) = range {
        *ans += &format!(
            "{:indent$}{kind:?} {range:?} (cnt: {child_count})\n",
            "",
            indent = depth * 2
        );
    } else {
        assert!(kind == SyntaxKind::UNKNOWN || kind.is_list());
        *ans += &format!(
            "{:indent$}{kind:?} (cnt: {child_count})\n",
            "",
            indent = depth * 2
        );
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
        CompilationUnit 1..31 (cnt: 2)
          SyntaxList 1..31 (cnt: 1)
            ModuleDeclaration 1..31 (cnt: 5)
              SyntaxList (cnt: 0)
              ModuleHeader 1..21 (cnt: 7)
                EndOfLine 0..1 (trivia)
                ModuleKeyword 1..7
                Whitespace 7..8 (trivia)
                Identifier 8..9
                SyntaxList (cnt: 0)
                AnsiPortList 10..20 (cnt: 3)
                  Whitespace 9..10 (trivia)
                  OpenParenthesis 10..11
                  SeparatedList 11..19 (cnt: 2)
                    ImplicitAnsiPort 11..18 (cnt: 3)
                      SyntaxList (cnt: 0)
                      VariablePortHeader 11..17 (cnt: 4)
                        InputKeyword 11..16
                        ImplicitType 17..17 (cnt: 3)
                          SyntaxList (cnt: 0)
                          Placeholder 17..17
                      Declarator 17..18 (cnt: 3)
                        Whitespace 16..17 (trivia)
                        Identifier 17..18
                        SyntaxList (cnt: 0)
                    Comma 18..19
                  CloseParenthesis 19..20
                Semicolon 20..21
              SyntaxList (cnt: 0)
              EndOfLine 21..22 (trivia)
              EndModuleKeyword 22..31
          EndOfFile 31..31
    "#]];
    expected.assert_eq(&ans);
}

#[test]
fn test_literals() {
    let tree = SyntaxTree::from_text(
        r#"
module A();
  wire y = 3s;
  wire x = 3'b101;
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
    assert_eq!(
        vec_lit.size().unwrap().int().unwrap().get_single_word(),
        Some(3)
    );
    assert_eq!(vec_lit.base().unwrap().base(), Some(LiteralBase::Bin));
    assert_eq!(
        vec_lit.value().unwrap().int().unwrap().get_single_word(),
        Some(5)
    );

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
    assert!(name
        .identifier()
        .unwrap()
        .value_text()
        .to_string()
        .is_empty());
}

#[test]
fn test_trivia() {
    let tree = get_tree_with_trivia();
    let root = tree.root().unwrap();
    let mut ans = String::new();
    dfs(root, 0, &mut ans);

    let expected = expect![[r#"
        CompilationUnit 19..31 (cnt: 2)
          SyntaxList 19..30 (cnt: 1)
            NetDeclaration 19..30 (cnt: 8)
              SyntaxList (cnt: 0)
              EndOfLine 0..1 (trivia)
              LineComment 1..9 (trivia)
              EndOfLine 9..10 (trivia)
              LineComment 10..18 (trivia)
              EndOfLine 18..19 (trivia)
              WireKeyword 19..23
              ImplicitType 24..24 (cnt: 3)
                SyntaxList (cnt: 0)
                Placeholder 24..24
              SeparatedList 24..29 (cnt: 1)
                Declarator 24..29 (cnt: 3)
                  Whitespace 23..24 (trivia)
                  Identifier 24..25
                  SyntaxList (cnt: 0)
                  EqualsValueClause 26..29 (cnt: 2)
                    Whitespace 25..26 (trivia)
                    Equals 26..27
                    IntegerLiteralExpression 28..29 (cnt: 1)
                      Whitespace 27..28 (trivia)
                      IntegerLiteral 28..29
              Semicolon 29..30
          EndOfLine 30..31 (trivia)
          EndOfFile 31..31
    "#]];
    expected.assert_eq(&ans);

    let wire = root.child_node(0).unwrap().child_node(0).unwrap();
    let trivias = wire
        .child_token(1)
        .unwrap()
        .trivias_with_loc()
        .map(|((start, end), trivia)| format!("{:?} {start:?}..{end:?}", trivia.kind()))
        .join("\n");
    let ans = expect![[r#"
        EndOfLine 0..1
        LineComment 1..9
        EndOfLine 9..10
        LineComment 10..18
        EndOfLine 18..19"#]];
    ans.assert_eq(&trivias);
}
