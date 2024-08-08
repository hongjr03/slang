include!(concat!(env!("OUT_DIR"), "/token.rs"));

#[cfg(test)]
mod tests {
    use crate::TokenKind;

    // Assuming the macro and TokenKind enum are defined in your crate.

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_punctuation_tokens() {
            assert_eq!(T!["'"], TokenKind::APOSTROPHE);
            assert_eq!(T![:=], TokenKind::COLON_EQUALS);
            assert_eq!(T![**], TokenKind::DOUBLE_STAR);
            assert_eq!(T![>=], TokenKind::GREATER_THAN_EQUALS);
        }

        #[test]
        fn test_operator_tokens() {
            assert_eq!(T![+], TokenKind::PLUS);
            assert_eq!(T![+=], TokenKind::PLUS_EQUAL);
            assert_eq!(T![->], TokenKind::MINUS_ARROW);
            assert_eq!(T![&&], TokenKind::DOUBLE_AND);
        }

        #[test]
        fn test_keyword_tokens() {
            assert_eq!(T![module], TokenKind::MODULE_KEYWORD);
            assert_eq!(T![function], TokenKind::FUNCTION_KEYWORD);
            assert_eq!(T![if], TokenKind::IF_KEYWORD);
            assert_eq!(T![endmodule], TokenKind::END_MODULE_KEYWORD);
        }

        #[test]
        fn test_system_names() {
            assert_eq!(T!["$unit"], TokenKind::UNIT_SYSTEM_NAME);
            assert_eq!(T!["$root"], TokenKind::ROOT_SYSTEM_NAME);
        }

        #[test]
        fn test_macro_tokens() {
            assert_eq!(T!["`\""], TokenKind::MACRO_QUOTE);
            assert_eq!(T!["``"], TokenKind::MACRO_PASTE);
        }
    }
}
