use crate::{Piece, aparsetheid::*, chess};

#[derive(Debug, Clone, Copy)]
struct Rank;
impl Parser<u8, usize> for Rank {
    fn parse<S: Stream<u8>>(self, s: S) -> Option<(usize, S)> {
        Range(b'1'..=b'8').map(|x| (x - b'1') as usize).parse(s)
    }
}

#[derive(Debug, Clone, Copy)]
struct File;
impl Parser<u8, usize> for File {
    fn parse<S: Stream<u8>>(self, s: S) -> Option<(usize, S)> {
        Range(b'a'..=b'h').map(|x| (x - b'a') as usize).parse(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RankFile;
impl Parser<u8, [usize; 2]> for RankFile {
    fn parse<S: Stream<u8>>(self, s: S) -> Option<([usize; 2], S)> {
        doh!(xs = s;
            r <- Rank;
            f <- File;
        );
        Some(([r, f], xs))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PieceLetter;
impl Parser<u8, Piece> for PieceLetter {
    fn parse<S: Stream<u8>>(self, s: S) -> Option<(Piece, S)> {
        use chess::{Class::*, Color::*};
        let (x, xs) = Any.parse(s)?;
        let r = match x.to_ascii_lowercase() {
            b'p' => Pawn,
            b'b' => Bishop,
            b'n' => Knight,
            b'r' => Rook,
            b'q' => Queen,
            b'k' => King,
            _ => return None,
        };
        let c = if x.is_ascii_uppercase() { White } else { Black };
        Some((Piece(c, r), xs))
    }
}

pub enum Notation {
    Standard {
        from_rank: Option<usize>,
        from_file: Option<usize>,
        capture: bool,
        piece_rank: chess::Class,
        to: [usize; 2],
        promote: Option<chess::Class>,
    },
    KCastle,
    QCastle,
}

/*
(
    `Rank`?
    `File`?
    [BNRQK]?
    x?
    `Rank`
    `File`
    ([=/]?[BNRQK])?
    [+#]?
) OR (0-0) OR (0-0-0)
*/

struct NotationParser;
impl Parser<u8, Notation> for NotationParser {
    fn parse<S: Stream<u8>>(self, s: S) -> Option<(Notation, S)> {
        doh!(
            xs=s;
            r <- Rank;
            f <- File
        );
        todo!()
    }
}
