include!(concat!(env!("OUT_DIR"), "/syntax_kind.rs"));

pub mod cursor;
pub mod iter;

impl SyntaxKind {
    pub fn id(self) -> u16 {
        self.0
    }
}

impl TokenKind {
    pub fn id(self) -> u16 {
        self.0
    }
}
