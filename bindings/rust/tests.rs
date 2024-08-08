use ast::{
    AstNode, CompilationUnit, Expression, LiteralExpression, Member, Name, PrimaryExpression,
};
use expect_test::expect;

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

fn get_complex_tree() -> SyntaxTree {
    SyntaxTree::from_text(
        r#"
module A();
always_comb begin
  wire x = 3;
end
endmodule;
"#,
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
            "{:indent$}{kind:?} | st: {} | ed: {} | cnt: {child_count}\n",
            "",
            range.start(),
            range.end(),
            indent = depth * 2
        );
    } else {
        assert!(kind == SyntaxKind::UNKNOWN || kind.is_list());
        *ans += &format!(
            "{:indent$}{kind:?} | cnt: {child_count}\n",
            "",
            indent = depth * 2
        );
    }

    for i in 0..child_count {
        if let Some(node) = node.child_node(i) {
            dfs(node, depth + 1, ans);
        } else if let Some(tok) = node.child_token(i) {
            if let Some(range) = tok.range() {
                *ans += &format!(
                    "{:indent$}kind: {:?} | st: {} | ed: {}\n",
                    "",
                    tok.kind(),
                    range.start(),
                    range.end(),
                    indent = (depth + 1) * 2
                );
            } else {
                *ans += &format!(
                    "{:indent$}kind: {:?}\n",
                    "",
                    tok.kind(),
                    indent = (depth + 1) * 2
                );
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
        CompilationUnit | st: 0 | ed: 37 | cnt: 2
          SyntaxList | st: 0 | ed: 37 | cnt: 2
            ModuleDeclaration | st: 0 | ed: 36 | cnt: 5
              SyntaxList | cnt: 0
              ModuleHeader | st: 0 | ed: 18 | cnt: 7
                kind: ModuleKeyword | st: 0 | ed: 6
                kind: Identifier | st: 7 | ed: 8
                SyntaxList | cnt: 0
                AnsiPortList | st: 8 | ed: 17 | cnt: 3
                  kind: OpenParenthesis | st: 8 | ed: 9
                  SeparatedList | st: 9 | ed: 16 | cnt: 1
                    ImplicitAnsiPort | st: 9 | ed: 16 | cnt: 3
                      SyntaxList | cnt: 0
                      VariablePortHeader | st: 9 | ed: 15 | cnt: 4
                        kind: InputKeyword | st: 9 | ed: 14
                        ImplicitType | st: 15 | ed: 15 | cnt: 3
                          SyntaxList | cnt: 0
                          kind: Placeholder | st: 15 | ed: 15
                      Declarator | st: 15 | ed: 16 | cnt: 3
                        kind: Identifier | st: 15 | ed: 16
                        SyntaxList | cnt: 0
                  kind: CloseParenthesis | st: 16 | ed: 17
                kind: Semicolon | st: 17 | ed: 18
              SyntaxList | st: 19 | ed: 26 | cnt: 1
                NetDeclaration | st: 19 | ed: 26 | cnt: 8
                  SyntaxList | cnt: 0
                  kind: WireKeyword | st: 19 | ed: 23
                  ImplicitType | st: 24 | ed: 24 | cnt: 3
                    SyntaxList | cnt: 0
                    kind: Placeholder | st: 24 | ed: 24
                  SeparatedList | st: 24 | ed: 25 | cnt: 1
                    Declarator | st: 24 | ed: 25 | cnt: 3
                      kind: Identifier | st: 24 | ed: 25
                      SyntaxList | cnt: 0
                  kind: Semicolon | st: 25 | ed: 26
              kind: EndModuleKeyword | st: 27 | ed: 36
            EmptyMember | st: 36 | ed: 37 | cnt: 3
              SyntaxList | cnt: 0
              TokenList | cnt: 0
              kind: Semicolon | st: 36 | ed: 37
          kind: EndOfFile | st: 37 | ed: 37
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
        CompilationUnit | st: 0 | ed: 41 | cnt: 2
          SyntaxList | st: 0 | ed: 41 | cnt: 4
            ModuleDeclaration | st: 0 | ed: 19 | cnt: 5
              SyntaxList | cnt: 0
              ModuleHeader | st: 0 | ed: 9 | cnt: 7
                kind: ModuleKeyword | st: 0 | ed: 6
                kind: Identifier | st: 7 | ed: 8
                SyntaxList | cnt: 0
                kind: Semicolon | st: 8 | ed: 9
              SyntaxList | cnt: 0
              kind: EndModuleKeyword | st: 10 | ed: 19
            EmptyMember | st: 19 | ed: 20 | cnt: 3
              SyntaxList | cnt: 0
              TokenList | cnt: 0
              kind: Semicolon | st: 19 | ed: 20
            ModuleDeclaration | st: 21 | ed: 40 | cnt: 5
              SyntaxList | cnt: 0
              ModuleHeader | st: 21 | ed: 30 | cnt: 7
                kind: ModuleKeyword | st: 21 | ed: 27
                kind: Identifier | st: 28 | ed: 29
                SyntaxList | cnt: 0
                kind: Semicolon | st: 29 | ed: 30
              SyntaxList | cnt: 0
              kind: EndModuleKeyword | st: 31 | ed: 40
            EmptyMember | st: 40 | ed: 41 | cnt: 3
              SyntaxList | cnt: 0
              TokenList | cnt: 0
              kind: Semicolon | st: 40 | ed: 41
          kind: EndOfFile | st: 41 | ed: 41
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

    let unit = ast::CompilationUnit::cast(root).unwrap();
    let module = unit
        .members()
        .children()
        .next()
        .unwrap()
        .as_module_declaration()
        .unwrap();
    dbg!(module.header().name().unwrap().range());

    let expected = expect![[r#"
        CompilationUnit | st: 1 | ed: 60 | cnt: 2
          SyntaxList | st: 1 | ed: 59 | cnt: 2
            ModuleDeclaration | st: 1 | ed: 58 | cnt: 5
              SyntaxList | cnt: 0
              ModuleHeader | st: 1 | ed: 12 | cnt: 7
                kind: ModuleKeyword | st: 1 | ed: 7
                kind: Identifier | st: 8 | ed: 9
                SyntaxList | cnt: 0
                AnsiPortList | st: 9 | ed: 11 | cnt: 3
                  kind: OpenParenthesis | st: 9 | ed: 10
                  SeparatedList | cnt: 0
                  kind: CloseParenthesis | st: 10 | ed: 11
                kind: Semicolon | st: 11 | ed: 12
              SyntaxList | st: 13 | ed: 48 | cnt: 1
                AlwaysCombBlock | st: 13 | ed: 48 | cnt: 3
                  SyntaxList | cnt: 0
                  kind: AlwaysCombKeyword | st: 13 | ed: 24
                  SequentialBlockStatement | st: 25 | ed: 48 | cnt: 7
                    SyntaxList | cnt: 0
                    kind: BeginKeyword | st: 25 | ed: 30
                    SyntaxList | st: 38 | ed: 44 | cnt: 1
                      ExpressionStatement | st: 38 | ed: 44 | cnt: 4
                        SyntaxList | cnt: 0
                        AssignmentExpression | st: 38 | ed: 43 | cnt: 4
                          IdentifierName | st: 38 | ed: 39 | cnt: 1
                            kind: Identifier | st: 38 | ed: 39
                          kind: Equals | st: 40 | ed: 41
                          SyntaxList | cnt: 0
                          IntegerLiteralExpression | st: 42 | ed: 43 | cnt: 1
                            kind: IntegerLiteral | st: 42 | ed: 43
                        kind: Semicolon | st: 43 | ed: 44
                    kind: EndKeyword | st: 45 | ed: 48
              kind: EndModuleKeyword | st: 49 | ed: 58
            EmptyMember | st: 58 | ed: 59 | cnt: 3
              SyntaxList | cnt: 0
              TokenList | cnt: 0
              kind: Semicolon | st: 58 | ed: 59
          kind: EndOfFile | st: 60 | ed: 60
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
