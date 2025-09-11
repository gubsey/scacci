mod chess;
mod fen;
pub mod notation;
mod vec2;

pub use chess::*;

#[cfg(test)]
mod tests {
    use std::{
        io::{Write, stderr},
        str::FromStr,
    };

    use crate::notation::Notation;

    use super::*;
    #[test]
    fn en_passant() {
        let mut chess = Chess::from_fen("8/8/8/pP6/8/8/8/8 w - a6 0 1").unwrap();
        let m = "a6";
        eprintln!("##> {m} <##");
        eprintln!("{}", chess.to_fen());
        stderr().flush().unwrap();
        let note = Notation::from_str(m).unwrap();
        chess.move_by_note(note).unwrap();
    }
}
